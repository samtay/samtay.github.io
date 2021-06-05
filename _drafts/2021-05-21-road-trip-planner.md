---
layout: post
title: Road Trip Planner
description: Planning a road trip with Datalog.
tags: [rust, datalog]
---

At the time of writing, my wife and I are in the process of converting a [2000
Blue Bird CSRE 3408](https://cptdb.ca/wiki/index.php/Blue_Bird_CS_Bus) into a
motor home. After spending a winter exploring the coast of Florida, we plan to
road trip across the continental US, stopping at national parks along the way.
Our road trip has constraints, such as:

1. Limiting the total road trip length,
2. Limiting the distance between stops,
3. Ensuring the park has a campground that allows our 34' motor home

For the final project of my Database Management Systems course, I chose to use
Datalog to find potential road trips that fit all of these constraints.

## Data

First, I had to gather park data. Fortunately, the National Park Service offers
a free [API](https://www.nps.gov/subjects/developer/api-documentation.htm) from
which I can get most of the necessary information. This is pretty quick, given
common tools like [curl](https://curl.se/) and
[jq](https://stedolan.github.io/jq/).

{% highlight bash %}
#!/usr/bin/env bash
set -eo pipefail

mkdir -p data

# get total number of campgrounds
total=$(curl \
  -H "X-Api-Key: $NPS_API_KEY" \
  -H "accept: application/json" \
  -X GET "https://developer.nps.gov/api/v1/campgrounds?limit=0" |
    jq -r '.total'
)

# fetch all those campgrounds
curl \
  -H "X-Api-Key: $NPS_API_KEY" \
  -H "accept: application/json" \
  -X GET "https://developer.nps.gov/api/v1/campgrounds?limit=$total" |
  jq -c '.data' > data/campgrounds.json
{% endhighlight %}

And we can do the same thing for their `parks` endpoint.

**Pro tip**: if you're like me and need to re-learn Bash syntax every time you
use it, check out [shellcheck](https://www.shellcheck.net/); when combined with
a filewatcher like [watchexec](https://github.com/watchexec/watchexec), this can
really speed up your Bash wpm. E.g.

{% highlight code %}
$ watchexec --watch bin -- shellcheck bin/*

In bin/fetch_nps_data line 16:
  -H 'X-Api-Key: $NPS_API_KEY' \
     ^-----------------------^ SC2016: Expressions don't expand in single quotes, use double quotes for that.
{% endhighlight %}

Next, a little more jq-fu to coerce some of this json into tab separated fact
files for Datalog:

{% highlight bash %}
#!/usr/bin/env bash
set -eo pipefail

cd data

# parks
filter=$(cat <<-jqfilter
    .[]
  | [.parkCode, .fullName]
  | @tsv
jqfilter
)
jq -r "$filter" parks.json > park.facts

# campgrounds
filter=$(cat <<-jqfilter
    .[]
  | select(.accessibility.rvAllowed == "1")
  | [.id, .parkCode, .name]
  | @tsv
jqfilter
)
jq -r "$filter" campgrounds.json > campground.facts

# campground locations
filter=$(cat <<-jqfilter
    .[]
  | select(
      .accessibility.rvAllowed == "1"
      and .latitude != ""
      and .longitude != ""
    )
  | [.id, .latitude, .longitude]
  | @tsv
jqfilter
)
jq -r "$filter" campgrounds.json > location.facts

# campground rv amenities
filter=$(cat <<-jqfilter
    .[]
  | select(.accessibility.rvAllowed == "1")
  | [.id,
     .accessibility.rvMaxLength,
     .amenities.internetConnectivity,
     .amenities.cellPhoneReception,
     .amenities.dumpStation
     ]
  | @tsv
jqfilter
)
jq -r "$filter" campgrounds.json > amenities.facts
{% endhighlight %}

### Distances

Of course, to plan a realistic road trip it would be best to use actual
directions from something like OpenStreetMap or Google Maps. But, since I have
time limitations, I'm just using the [haversine
formula](https://en.wikipedia.org/wiki/Haversine_formula) for now. Luckily, the
rust ecosystem is plentiful, so this doesn't take much code. First,
the types:

{% highlight rust %}
use geo::prelude::HaversineDistance;
use geo::{point, Point};
use serde::Deserialize;

#[derive(Clone, Debug, Deserialize, PartialEq)]
struct LocationRow {
    camp_id: String,
    latitude: f64,
    longitude: f64,
}

impl LocationRow {
    fn coordinate(&self) -> Point<f64> {
        point!(x: self.longitude, y: self.latitude)
    }

    /// Distance to another location in miles
    pub fn distance_to(&self, other: &LocationRow) -> f64 {
        const MILES_PER_METER: f64 = 0.000621371;
        let meters = self
            .coordinate()
            .haversine_distance(&other.coordinate());
        return meters * MILES_PER_METER;
    }
}
{% endhighlight %}

The `LocationRow` is exactly the type of tsv row we piped to `location.facts`.
So, we'll read each of those campgrounds' locations, generate all the pairs, and
write the distances to another facts file:

{% highlight rust %}
use csv;
use itertools::Itertools;

/// Generate distance.facts from the NPS location.facts file
fn generate_distances() -> Result<()> {
    let pairs = csv::ReaderBuilder::new()
        .has_headers(false)
        .delimiter(b'\t')
        .from_path("data/location.facts")?
        .into_deserialize::<LocationRow>()
        .filter_map(|x| x.ok())
        .combinations_with_replacement(2);
    let mut writer = csv::WriterBuilder::new()
        .has_headers(false)
        .delimiter(b'\t')
        .from_path("data/distance.facts")?;
    for pair in pairs {
        writer.write_record(&[
            &pair[0].camp_id,
            &pair[1].camp_id,
            &format!("{:.2}", &pair[0].distance_to(&pair[1])),
        ])?;
    }
    Ok(())
}
{% endhighlight %}

## Datalog

This road trip planner uses the
[souffle](https://souffle-lang.github.io/index.html) dialect of Datalog.  First,
I defined some types and let souffle know where to find the relation facts.

{% highlight bash %}
.type ParkId <: symbol
.type CampId <: symbol
.type Name <: symbol
.type Distance <: float

.decl camp(camp:CampId, park: ParkId, name:Name)
.input camp(filename="campground.facts")

.decl park(park:ParkId, name:Name)
.input park(filename="park.facts")

.decl distance(camp1:CampId, camp2:CampId, dist:Distance)
.input distance(filename="distance.facts")

.decl location(camp:CampId, lat:float, long: float)
.input location(filename="location.facts")

.decl amenities(camp:CampId, rv:number, internet:symbol, cell:symbol, dump:symbol)
.input amenities(filename="amenities.facts")
{% endhighlight %}

Next, some useful helper relations -- restrict our attention to campgrounds that
actually fit our RV, and have a commutative distance relation:

{% highlight c++ %}
// Campgrounds that fit our RV
// Note: some campgrounds just report 0 but allow RVs
.decl rv_camp(id:CampId)
rv_camp(id) :- camp(id, _, _),
  amenities(id, rv_len, _, _, _),
  (rv_len = 0 ; rv_len >= 34).

// Distances between campgrounds that fit our RV
.decl rv_dist(camp1:CampId, camp2:CampId, dist:Distance)
rv_dist(from, to, len) :-
  distance(from, to, len), rv_camp(from), rv_camp(to).
rv_dist(to, from, len) :-
  distance(from, to, len), rv_camp(from), rv_camp(to).
{% endhighlight %}

Next I define all possible road trip segments. These exceed 100mi to make useful
progress between stops, but are limited to 600mi so they can fit easily in one
weekend. Also, I need to make sure that the direction of these segments is
oriented in the general direction of the final destination. There's a few ways
to do this. We could restrict the segments in the general NW direction; that is,
make sure that northward progress exceeds eastward, and western progress exceeds
southward. (For example, it's okay to backtrack east 100mi if we make 500mi
progress north.)

{% highlight c++ %}
// Optimized road trip segments
// 1. Limit 600mi between stops
// 2. Exceed 100mi between stops
// 3. Go northwest (generally) to make progress from FL to WA
.decl segment(camp1:CampId, camp2:CampId, dist:Distance)
segment(from, to, len) :-
  rv_dist(from, to, len),
  100 <= len, len <= 600,
  location(from, f_lat, f_long),
  location(to, t_lat, t_long),
  (f_long - t_long > f_lat - t_lat),
  (t_lat - f_lat > t_long - f_long).
{% endhighlight %}

Another option would be to just make sure that we're closing in on the final
destination. For example, let's say we want to make it to an RV campground near
Mount Rainier. Then
{% highlight c++ %}
// End location near Mount Rainier
.decl camp_near_seattle(camp:CampId)
camp_near_seattle(c) :- camp(c, "mora", "Cougar Rock Campground").

// Optimized road trip segments
// 1. Limit 600mi between stops
// 2. Exceed 100mi between stops
// 3. Make progress towards final destination
.decl segment(camp1:CampId, camp2:CampId, dist:Distance)
segment(from, to, len) :-
  rv_dist(from, to, len),
  100 <= len, len <= 600,
  rv_dist(from, end, dist_from),
  rv_dist(to, end, dist_to),
  dist_from > dist_to,
  camp_near_seattle(end).
{% endhighlight %}
is also a reasonable `segment` defintion, and is more portable for finding road
trips to other locations. In addition, having Mount Rainier be a sink of this
directed graph alleviates the need for a flimsy stopping condition in the road
trip planner, as souffle can just iterate until the distance to the final
destination goes to zero.

Finally, the question remains how to assemble these segments into road trip
plans. There are a few options.

#### Naive Enumeration

This is perhaps the first obvious solution, essentially the transitive closure
of this directed graph of segments.

{% highlight c++ %}
.decl road_trip_segment(from:CampId, to:CampId, acc:Distance)
road_trip_segment(f, t, d) :-
  camp(f, "ever", "Flamingo Campground"),
  segment(f, t, d).
road_trip_segment(f, t, acc+d) :-
  road_trip_segment(_, f, acc),
  segment(f, t, d).
{% endhighlight %}

This doesn't generate single road trip plans, but rather generates all
campgrounds reachable along the segments defined above, starting from the
Flamingo Campground in the Everglades, and of course stopping at the sink Mount
Rainier (as long as it can indeed reach Mount Rainier with the segment
constraints). However, with the definitions thus far, the order of magnitude of
these combinations is infeasible to compute in souffle, at least on my laptop.

#### Functional Dependencies

After some digging in the documentation, I found that souffle recently added
support for [functional dependencies](https://souffle-lang.github.io/choice),
that is, we can express a dependency `from -> to` and non-deterministically
choose a single `to` campground for each `from`. This is currently unreleased,
and requires building souffle from a recent commit. The code is the same as
above except an extra `choice-domain from` clause:

{% highlight c++ %}
// Generate a non-deterministic path
.decl road_trip_segment(from:CampId, to:CampId, acc:Distance)
  choice-domain from
{% endhighlight %}

The plus side: this does result in a road trip from FL to WA that fits our
constraints.

| Stop | Park Campground | Distance |
|:---:|:---:|:---:|
| 1 | Everglades National Park: Flamingo Campground | 0 |
| 2 | Gulf Islands National Seashore: Fort Pickens Campground Loop A | 527.19 |
| 3 | Ozark National Scenic Riverways: Alley Spring Campground | 1056.47 |
| 4 | Lake Meredith National Recreation Area: Blue Creek | 1632.46 |
| 5 | Glen Canyon National Recreation Area: Beehives Campground | 2185.89 |
| 6 | Great Basin National Park: Baker Creek Campground | 2391.80 |
| 7 | Whiskeytown National Recreation Area: Brandy Creek RV | 2847.99 |
| 8 | North Cascades National Park: Colonial Creek South Campground | 3410.27 |
| 9 | Mount Rainier National Park: Cougar Rock Campground | 3546.77 |
{:.grid-table}

The downside is that this only results in a _single_ plan; although
the result is "non-deterministic", it is not actually _random_. Repeatedly
running the souffle planner always outputs the same plan. Thus, we cannot
perform any optimizations for preferred amenities or total trip length.
Furthermore, in some ways we just got lucky that this successfully resulted in a
plan. For instance, suppose this chosen path goes directly northwest for the
first 7 stops to Bighorn Canyon, but then encounters a northwestward dead end,
with no other campgrounds within 600mi to the west or north; then there would be
no more campgrounds to travel to that decrease the distance to the final
destination, i.e. Bighorn is a leaf in the directed graph of segments. However, it might
have been possible to avoid this dead end by going straight west a few
stops earlier.

#### Min/Max Choice

Instead of having souffle choose the next campground non-deterministically, I
could choose a specific one, for example to minimize the distance between stops.

{% highlight c++ %}
.decl road_trip_segment(from:CampId, to:CampId, acc:Distance)
  choice-domain from

road_trip_segment(f, t, d) :-
  camp(f, "ever", "Flamingo Campground"), segment(f, t, d).
road_trip_segment(f, t, acc+d) :-
  road_trip_segment(_, f, acc),
  d = min l : segment(f, t, l).
{% endhighlight %}

This generates the following plan, with many more stops than before:

| Stop | Park Campground | Distance |
|:---:|:---:|:---:|
| 1 | Everglades National Park: Flamingo Campground | 0 |
| 2 | Gulf Islands National Seashore: Fort Pickens Campground Loop A | 527.19 |
| 3 | Natchez Trace Parkway: Rocky Springs Campground, Milepost 54.8. | 768.80 |
| 4 | Hot Springs National Park: Gulpha Gorge Campground | 981.03 |
| 5 | Buffalo National River: Woolum | 1081.46 |
| 6 | Lake Meredith National Recreation Area: Cedar Canyon Campground | 1549.38 |
| 7 | Bandelier National Monument: Juniper Family Campground | 1831.56 |
| 8 | Mesa Verde National Park: Morefield Campground | 1989.07 |
| 9 | Curecanti National Recreation Area: Ponderosa Campground | 2092.99 |
| 10 | Arches National Park: Devils Garden Campground | 2216.85 |
| 11 | Dinosaur National Monument: Split Mountain Group Campground | 2333.38 |
| 12 | Grand Teton National Park: Gros Ventre Campground | 2564.26 |
| 13 | Craters Of The Moon National Monument & Preserve: Lava Flow Campground | 2709.51 |
| 14 | Glacier National Park: Two Medicine | 3057.03 |
| 15 | Lake Roosevelt National Recreation Area: North Gorge Campground | 3269.73 |
| 16 | North Cascades National Park: Colonial Creek South Campground | 3410.73 |
| 17 | Olympic National Park: Heart O' the Hills Campground | 3527.01 |
| 18 | Mount Rainier National Park: Cougar Rock Campground | 3643.42 |
{:.grid-table}

However, this suffers from the same drawbacks. To demonstrate, an attempt to
choose the max distance between stops results in a dead end half-way through the
trip:

{% highlight c++ %}
.decl road_trip_segment(from:CampId, to:CampId, acc:Distance)
  choice-domain from
road_trip_segment(f, t, d) :-
  camp(f, "ever", "Flamingo Campground"), segment(f, t, d).
road_trip_segment(f, t, acc+d) :-
  road_trip_segment(_, f, acc),
  d = max l : segment(f, t, l).
{% endhighlight %}
| Stop | Park Campground | Distance |
|:---:|:---:|:---:|
| 1 | Everglades National Park: Flamingo Campground | 0 |
| 2 | Gulf Islands National Seashore: Fort Pickens Campground Loop A | 527.19 |
| 3 | Ozark National Scenic Riverways: Pulltite Campground | 1068.55 |
| 4 | Sleeping Bear Dunes National Lakeshore: D. H. Day Campground | 1662.30 |
| 5 | Pictured Rocks National Lakeshore: Hurricane River Campground | 1784.65 |
{:.grid-table}

#### Recursive Types

The final part of this exploration leverages the souffle type system, which
allows for arbitrary records, algebraic data types, and recursion. For
convenience, I'll define a `Segment` representing an ordered pair of campgrounds
{% highlight c++ %}
.type Segment = [from:CampId, to:CampId]
{% endhighlight %}
and a cons list of them called `Path`
{% highlight c++ %}
.type Path = [tail:Path, head:Segment]
{% endhighlight %}

To demonstrate the utility here, let's look at the naive enumeration utilizing
these types:
{% highlight c++ %}
.decl road_trip(x: Path)
road_trip([nil, [f, t]]) :-
  camp(f, "ever", "Flamingo Campground"),
  segment(f, t, _).
road_trip([tail, [f,t]]) :-
  road_trip(tail),
  tail=[tail2, [start, f]],
  segment(f, t, _).
{% endhighlight %}
Of course, what I mentioned above is still true, as is; this generates way too many
segments to compute in a timely manner. But, notice the output is much
cleaner. Instead of a relation filled with arbitrary segments that would be
annoying to assemble into possible road trips, we actually have a relation that
is filled with individual road trips of type `Path`. Some of them might not make it
all the way to WA, as was shown in the max choice example above, but we could
filter those out afterwards, e.g.

{% highlight c++ %}
.decl successful_road_trip(x: Path)
.output successful_road_trip(IO=stdout)
successful_road_trip(path) :-
  road_trip(path),
  path=[tail, [f, end]],
  camp_near_seattle(end).
{% endhighlight %}

As a demonstration, the number of plans generated to go from Yellowstone to the
Badlands totals 29,376, with the first plans being more direct, such as:

<h5 style="text-align: center;">Plan 1</h5>

| Stop | Park Campground | Distance |
|:---:|:---:|:---:|
| 1 | Yellowstone National Park: Bridge Bay Campground | 0 |
| 2 | Badlands National Park: Cedar Pass Campground | 424.14 |
{:.grid-table}

and proceedingly more winding:

<h5 style="text-align: center;">Plan 100</h5>

| Stop | Park Campground | Distance |
|:---:|:---:|:---:|
| 1 | Yellowstone National Park: Mammoth Campground | 0 |
| 2 | Bighorn Canyon National Recreation Area: Horseshoe Bend Campgroun | 118.95 |
| 3 | Badlands National Park: Cedar Pass Campground | 441.72 |
{:.grid-table}

<h5 style="text-align: center;">Plan 1000</h5>

| Stop | Park Campground | Distance |
|:---:|:---:|:---:|
| 1 | Yellowstone National Park: Bridge Bay Campground | 0 |
| 2 | Dinosaur National Monument: Gates of Lodore Campground | 274.63 |
| 3 | Bighorn Canyon National Recreation Area: Barry's Landing & Trail Creek Campground | 578.70 |
| 4 | Theodore Roosevelt National Park: Juniper Campground | 868.24 |
| 5 | Badlands National Park: Cedar Pass Campground | 1142.63 |
{:.grid-table}

<h5 style="text-align: center;">Plan 10000</h5>

| Stop | Park Campground | Distance |
|:---:|:---:|:---:|
| 1 | Yellowstone National Park: Indian Creek Campground | 0 |
| 2 | Dinosaur National Monument: Split Mountain Group Campground | 316.09 |
| 3 | Yellowstone National Park: Lewis Lake Campground | 590.41 |
| 4 | Dinosaur National Monument: Gates of Lodore Campground | 851.58 |
| 5 | Yellowstone National Park: Pebble Creek Campground | 1147.70 |
| 5 | Theodore Roosevelt National Park: Roundup Group Horse Camp | 1496.24 |
| 5 | Badlands National Park: Cedar Pass Campground | 1733.97 |
{:.grid-table}

and so on. Clearly the 10,000th plan above is absurd, and the reason is _not_
just because I'm using haversine distances. The reason did not become clear to
me until I looked at these parks on a map. The main problem is the progress
condition just cares about any nonzero progress towards the destination; if
you imagine a circle spiraling in towards the final destination at the center,
you can see how this condition might result in unwanted road trip plans. That
is, the restriction I defined earlier as `100 <= segment_length <= 600` is
misleading, as it doesn't actually guarantee 100mi of progress.

And so this motivates the last improvement: instead of making sure we drive
100mi between stops, instead make sure that the next stop is at least 100mi
closer to the final destination.

{% highlight c++ %}
// Optimized road trip segments
// 1. Limit 600mi between stops
// 2. Make 100mi progress towards final destination
.decl segment(camp1:CampId, camp2:CampId, dist:Distance)
segment(from, to, len) :-
  rv_dist(from, to, len),
  len <= 600,
  rv_dist(from, end, dist_from),
  rv_dist(to, end, dist_to),
  dist_from - dist_to > 100,
  camp_near_seattle(end).
{% endhighlight %}

Huzzah! Now, the enumeration only outputs 17 plans, with at most one intermediary
stop. This is much more reasonable for two parks that are only 500mi from one
another, and the only reason there are even as many as 17 plans is simply
because each park has multiple RV campgrounds.

Furthermore, this last improvement allows me to accomplish what I set out to do:
changing the minimum progress to 400mi allows Souffle to output 3032 road trip
plans from the Everglades to Mount Rainier in under a second.

## CLI tool

I've parameterized the start and end locations into a small CLI tool to run this
[road trip planner](https://github.com/samtay/road-trip-planner):
road-trip-planner

{% highlight shell %}
❯ road-trip-planner --help
road-trip-planner 0.0.1
Sam Tay, samctay@pm.me
Generates road trip plans via national parks

USAGE:
    road-trip-planner [FLAGS] <from> <to>

ARGS:
    <from>    Starting park code (e.g. ever)
    <to>      Ending park code (e.g. olym)

FLAGS:
    -h, --help       Prints help information
    -l, --lucky      Output a single trip
        --min        Use minimum distance between stops (for --lucky)
    -r, --refresh    Use fresh NPS data
    -V, --version    Prints version information
{% endhighlight %}

## Conclusion

This was a fun exercie, but the resulting planner isn't as useful as it could
be. The main missing ingredients are ordering and choice. While souffle supports
a single arbitrary choice from a given domain, what I'd really like to be able
to do is specify preferences, i.e. first choose from campgrounds with wifi
service and dump stations, and if there are none, then settle for cell service,
etc.. Something like Postgres' `coalesce` function would make this planner much
more useful.

Future improvements I have in mind for this planner:

1. Include the cell, internet, and dump amenities in the enumeration output.
   These can then be externally counted, so that plans can be sorted by stops
   with the most cell service, or to filter plans that include at least one dump
   stop every 500mi, etc..
2. A few more things should be parameterized from the CLI, such as the RV
   length, minimum progress and maximum segment distance. The minimum progress
   turns out to be a very important parameter, and makes the difference of
   whether or not the computation finishes in a second or days.
3. Call out to an API for actual driving distance and/or time, instead of using
   the haversine approximation.

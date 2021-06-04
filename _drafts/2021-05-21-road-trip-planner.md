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
which I can get most of the necessary information. Of course, I could use some
lightweight Rust libraries to accomplish this menial work, but given that I only
have a weekend or two to write this, let's just hack up some scripts real quick
and rely on common tools like [curl](https://curl.se/) and
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
        let meters = self.coordinate().haversine_distance(&other.coordinate());
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
        .into_deserialize()
        .filter_map(|x: std::result::Result<LocationRow, csv::Error>| x.ok())
        .combinations(2);
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

During my DBMS course, we used
[souffle](https://souffle-lang.github.io/index.html) for the Datalog homework.
As I started writing the queries to generate a road trip, however, I hit a snag
or two.

First, I defined some types and let souffle know where to find the relation
facts.
{% highlight code %}
.type ParkId <: symbol
.type CampId <: symbol
.type Name <: symbol
.type Distance <: float

.decl camp(camp:CampId, park: ParkId, name:Name)
.input camp(filename="data/campground.facts")

.decl park(park:ParkId, name:Name)
.input park(filename="data/park.facts")

.decl distance(camp1:CampId, camp2:CampId, dist: Distance)
.input distance(filename="data/distance.facts")

.decl location(camp:CampId, lat:float, long: float)
.input location(filename="data/location.facts")
{% endhighlight %}

Next I define a reasonable park-to-park segment of the road trip

{% highlight code %}
// Distance between stops shouldn't exceed 500 miles
.decl segment_candidate_attempt(from:CampId, to:CampId, l:Distance)
segment_candidate_attempt(f, t, l) :-
  (distance(f,t,l) ; distance(t,f,l)),
  l <= 500.
{% endhighlight %}

Here's where the problem starts to surface, and it's an expressivity problem.
I need to assemble these segments into singular road trips. Since I'm starting
from Florida, I can start there:

{% highlight code %}
.decl road_trip_segment_attempt(from:CampId, to:CampId, accumLen: float)
.output road_trip_segment_attempt(IO=stdout)

road_trip_segment_attempt(f, t, d) :-
  camp(f, _, "Flamingo Campground"), segment_candidate_attempt(f, t, d).
{% endhighlight %}

But how can I finish this definition? I can try to just assemble segments
starting from Florida, limiting the total length of the road trip:

{% highlight code %}
road_trip_segment_attempt(f, t, acc+d) :-
  road_trip_segment_attempt(_, f, acc),
  segment_candidate_attempt(f, t, d),
  acc+d < 3000.
{% endhighlight %}

But this doesn't really give us a road trip from FL to WA. Instead, this results
in all paths starting in the Everglades with total weight less than 3000. Worse
still, there's no restriction on visiting past stops, so this relation will even
return trips that just go back and forth between the Everglades and the nearest
park until the distance traveled hits 3000mi (and unfortunately, that problem
can't be solved by a simple `!road_trip_segment_attempt(t, _, _)` as this would
introduce a cyclic negation, disallowed in souffle).

Luckily for me, my problem domain is narrow: I'm going from FL to WA. Thus, I
can be smarter about what constitutes a reasonable road trip segment:

{% highlight code %}
// Distance between stops shouldn't exceed 500 miles
// We're going from FL to WA so don't backtrack east
segment_candidate(f, t, l) :-
  (distance(f,t,l) ; distance(t,f,l)),
  l <= 500,
  location(f, _, f_long),
  location(t, _, t_long),
  f_long >= t_long.
{% endhighlight %}

It's important to note that this is not a great restriction to have. It's very
possible that I'm excluding reasonable roadtrips that make northward progress
at the expense of backtracking east. What I'm really missing in souffle is the
ability to require that our road trip relation _ends_ in WA.

However, even with this restriction, I still have a big problem. The
`road_trip_segment` relation is generating all the reasonable park-to-park
segments, but there's no way to narrow in on a _particular_ itinerary.
That is, for each `from: CampId`, there are many `to: CampId`'s. There might be
three parks I can reasonably travel to initially from the Everglades, and then
four parks I can reasonably travel to from each of those, etc. Yet, the whole point
of this exercise was to actually output a _plan_.

After some digging in the documentation, I found that souffle recently added
support for [functional dependencies](https://souffle-lang.github.io/choice),
which allows us to do exactly that; namely, express a dependency `from -> to`
and non-deterministically choose the `to` campground. This is currently
unreleased, and requires building souffle from a recent commit.

{% highlight code %}
// Generate a non-deterministic path
.decl road_trip_segment(from:CampId, to:CampId, dist: Distance, stop_ix: number) choice-domain from, to

// Start somewhere in the Everglades
road_trip_segment(f, t, d, 1) :-
  camp(f, "ever", _),
  segment_candidate(f, t, d).

road_trip_segment(f, t, acc+d, ix+1) :-
  road_trip_segment(_, f, acc, ix),
  segment_candidate(f, t, d).
{% endhighlight %}

I'll print out a demonstration of this, just as a sanity check that this matches
intuition:
{% highlight code %}
.decl ex(p1: Name, p2: Name, d: Distance, ix: number)
.output ex(IO=stdout)
ex(p1, p2, d, ix) :-
  road_trip_segment(f, t, d, ix),
  camp(f, pid1, _),
  camp(t, pid2, _),
  park(pid1, p1),
  park(pid2, p2).
{% endhighlight %}
and this results in the following plan:

| From | To | Distance | Stop |
|:---:|:---:|:---:|:---:|
| Everglades National Park | Big Cypress National Preserve | 74.79 | 1 |
| Big Cypress National Preserve | Gulf Islands National Seashore | 539.45 | 2 |
| Gulf Islands National Seashore | Gulf Islands National Seashore | 630.54 | 3 |
| Gulf Islands National Seashore | Ozark National Scenic Riverways | 1121.16 | 4 |
| Ozark National Scenic Riverways | Buffalo National River | 1217.57 | 5 |
| Buffalo National River | Lake Meredith National Recreation Area | 1703.88 | 6 |
| Lake Meredith National Recreation Area | Rocky Mountain National Park | 2100.08 | 7 |
| Rocky Mountain National Park | Bighorn Canyon National Recreation Area | 2459.57 | 8 |
| Bighorn Canyon National Recreation Area | Glacier National Park | 2821 | 9 |
| Glacier National Park | North Cascades National Park | 3146.20 | 10 |
| North Cascades National Park | Oregon Caves National Monument & Preserve | 3614.40 | 11 |
| Oregon Caves National Monument & Preserve | Redwood National and State Parks | 3674.95 | 12 |
| Redwood National and State Parks | Olympic National Park | 4107.31 | 13 |
{:.grid-table}

So, this _sort of_ works. The plus side: I now have a reasonable road trip plan that goes from
the Everglades up to the Olympic National Forest. There are thirteen stops, each
at national parks and no singular trips greater than 500mi.

But, there are significant drawbacks here:

1. No optimizations for preferred amenities.
2. No optimization for limiting total trip length.
3. No possible backtracking on longitude.
4. **Only a single plan output**

These drawbacks are related, and #4 is the clincher. Souffle refers to this
choice domain as "non-deterministic", but, the choice is not random. If this
program is run multiple times, the output is the same. What I'd really like is
to generate a set of many plans, so that I can explore and optimize among them
to find plans that are ideal for my particular needs.

# TODO
1. Choose to explore either crepe, datafrog, or differential-dataflow

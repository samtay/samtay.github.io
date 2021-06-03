---
layout: post
title: My first foray into Rust
description: A story of coming to Rust from Haskell.
tags: [rust, haskell, tui]
---

## The language

{% comment %}
After I found Haskell, I thought it would be the last language I'd ever write.
And to be clear, I'm still enamored with it as a _language_. But the other
things that come along with a language, such as its implementation, tooling, and
ecosystem, have a huge impact on your day-to-day interaction with it. And,
without trying to start any flame wars, I think it's fair to say Haskell has
some frustrating, long-standing issues.[^1] So for my own edification I decided
to explore a new language, and Rust caught my eye.
{% endcomment %}

Rust seems to have a lot of momentum and a strong community. For a young
language, I was pleasantly surprised by the ease of getting up and running with
`rustup` and `cargo`, solid beginner tutorials,[^2] and the community emphasis on
documentation. All of these factors made for a very ergonomic exploration of the
language. It was also great to see familiar friends from Haskell, such as
generics and traits. Traits are definitely not as powerful as Haskell's
typeclasses, but they provide plenty of mileage for writing elegant, type-safe
code.

For my first project, I made [so: a terminal interface for
StackOverflow](https://github.com/samtay/so):
![demo](https://raw.githubusercontent.com/samtay/so/master/assets/demo.gif)
This is a rewrite of a [Haskell implementation](https://github.com/samtay/so-hs)
I started a while ago, so it was a great way to see the two languages
side-by-side.

## Battling lifetimes

Lifetimes in Rust are notoriously tricky for beginners and comfort with them
probably only comes with experience. You can of course look at the [rust lang
book](https://doc.rust-lang.org/1.9.0/book/lifetimes.html) chapter, but there
you will find a series of `foo`, `baz` functions and single letter variable
assignments; for me personally, this makes it harder to grok the real
implications. So let me walk you through my actual experience, which I think
will be a better demonstration:

If I want to create a specific instance of
[`clap::App`](https://docs.rs/clap/2.33.1/clap/struct.App.html),
but it is used in multiple places, like once in `main` and once in `test`, my
immediate reaction is to make a function, so I don't have to duplicate code.
Let's do that:

{% highlight rust %}
use clap::{App, Arg, ArgMatches};

pub fn mk_app<'a, 'b>() -> App<'a, 'b> {
    App::new("so")
        .arg(
            Arg::with_name("site")
                .long("site")
                .short("s")
                .takes_value(true)
                .default_value("stackoverflow")
                .help("StackExchange site code to search"),
        )
        .arg(
            Arg::with_name("limit")
                .long("limit")
                .short("l")
                .takes_value(true)
                .default_value("10")
                .help("Question limit per site query")
        )
        .arg(
            Arg::with_name("query")
                .multiple(true)
                .index(1)
                .required(true)
        )
}
{% endhighlight %}
If you're a budding Rustacean, and wondering where `'a`, `'b` are coming
from, it might help to first read an overview of
[lifetimes](https://doc.rust-lang.org/1.8.0/book/lifetimes.html). For what it's
worth, this was the first time I encountered them writing `so`, and if you try
to write `pub fn mk_app() -> App`, the compiler will very helpfully guide you
towards the signature above.

But now it turns out, I want to be able to pull default CLI options
from a config file. No biggie, let's modify my function to take the
configuration:
{% highlight rust %}
pub struct Config {
    pub limit: u16,
    pub site: String,
}

mk_app<'a, 'b>(config: Config) -> App<'a, 'b>
{% endhighlight %}

This is great, I'll be able to mock configs in my tests and really hammer
this. Let's start by using the default value for the `site` parameter:
{% highlight rust %}
pub fn mk_app<'a, 'b>(config: Config) -> App<'a, 'b> {
    App::new("so")
        .arg(
            Arg::with_name("site")
                .long("site")
                .short("s")
                .takes_value(true)
                .default_value(config.site)
                .help("StackExchange site code to search"),
        )
        // elided, same as above
}
{% endhighlight %}
Here we run into a n00b Rust issue, but again, the compiler is quite helpful:
{% highlight shell %}
error[E0308]: mismatched types
  --> src/cli.rs:21:32
   |
21 |                 .default_value(config.site)
   |                                ^^^^^^^^^^^
   |                                |
   |                                expected `&str`, found struct `std::string::String`
   |                                help: consider borrowing here: `&config.site`
{% endhighlight %}
Ah yes, this makes sense, since earlier we were passing in
`"stackoverflow": &str`. So we follow the compiler's advice and borrow, using
`.default_value(&config.site)`. But next we see:
{% highlight shell %}
error[E0515]: cannot return value referencing local data `config.site`
  --> src/cli.rs:14:5
   |
14 | /     App::new("so")
15 | |         .arg(
16 | |             Arg::with_name("site")
17 | |                 .long("site")
...  |
22 | |                 .default_value(&config.site)
   | |                                ------------ `config.site` is borrowed here
...  |
39 | |                 .required(true),
40 | |         )
   | |_________^ returns a value referencing data owned by the current function
{% endhighlight %}
This makes sense, given Rust's [ownership
semantics](https://doc.rust-lang.org/1.8.0/book/ownership.html). The function
`mk_app` takes _ownership_ of `config`, but then it goes out of scope at the end
of the function, so the resource is deallocated. It would be _very bad_ if the
compiler didn't complain here, because the `&config.site` would be a dangling
pointer. Instead, we need to
[borrow](https://doc.rust-lang.org/1.8.0/book/references-and-borrowing.html) the
config:
{% highlight rust %}
pub fn mk_app<'a, 'b>(config: &Config) -> App<'a, 'b> {
{% endhighlight %}
Of course, this wouldn't be a true introductory Rust post if we didn't have a
few battles with the borrow checker. This signature is illegal because it is
[ambiguous](https://doc.rust-lang.org/nomicon/lifetime-elision.html):

{% highlight shell %}
error[E0621]: explicit lifetime required in the type of `config`
  --> src/cli.rs:14:5
   |
13 |   pub fn mk_app<'a, 'b>(config: &Config) -> App<'a, 'b> {
   |                                 ------- help: add explicit lifetime `'b` to the type of `config`: `&'b config::Config`
14 | /     App::new("so")
15 | |         .arg(
16 | |             Arg::with_name("site")
17 | |                 .long("site")
...  |
38 | |                 .required(true),
39 | |         )
   | |_________^ lifetime `'b` required
{% endhighlight %}
Here's an oddity that might actually be a bug in the compiler message. If you
add the `'b` lifetime so that `config: &'b Config`, you'll see that the lifetime
is invalid. If we instead use `config: &'a Config`, the compilation succeeds :tada:
and we get the desired functionality.[^3]

But we're not quite done, because we haven't specified the default configured
`limit`. And here's where things get interesting! The `config.limit` has type
`u16`, but as we saw above, the `default_value` function accepts a `&str`. (To
be pedantic, we saw that it accepts a `&'a str`.) Luckily, all of these integer
types have `ToString` implementations:
{% highlight rust %}
pub fn mk_app<'a, 'b>(config: &'a Config) -> App<'a, 'b> {
    let limit = config.limit.to_string();
    App::new("so")
        // elided site arg
        .arg(
            Arg::with_name("limit")
                .long("limit")
                .short("l")
                .number_of_values(1)
                .takes_value(true)
                .default_value(&limit)
                .help("Question limit per site query"),
        )
        // elided query arg
}
{% endhighlight %}
But we've run into the same problem as above when we tried to return a reference
to the owned `config`:
{% highlight shell %}
15 | /     App::new("so")
16 | |         .arg(
17 | |             Arg::with_name("site")
18 | |                 .long("site")
...  |
31 | |                 .default_value(&limit)
   | |                                ------ `limit` is borrowed here
...  |
38 | |                 .required(true),
39 | |         )
   | |_________^ returns a value referencing data owned by the current function
{% endhighlight %}
And again, it makes sense given Rust's semantics around ownership and memory
management. When we create the limit string `let limit =
config.limit.to_string();`, the current scope owns that `limit`, but the memory
is deallocated when it goes out of scope. So, just as above, if the borrow
checker _did_ let this code pass, we would have a dangling pointer within the
`App`.

It is my understanding that this is just how things are if a struct like `App`
requires references such as `&str` in its construction. Someone needs to own
those strings, and thus they need to be kept in scope, i.e. kept _alive_, for as
long as the `App` lives. Notice that if I wasn't dealing with a struct
definition from an external crate, I could just modify the struct to take a
`String` or a `Box` to put the parsed limit on the heap, instead of the stack,
so that its lifetime would persist past the current function call. So, this
isn't really a pitfall of Rust, but rather, Rust's semantics have guaranteed
that this library will only be used in the way the authors intended. I've since
reorganized my code so that the configuration retrieval and clap argument
parsing all happen in the same place, as below.
{% highlight rust %}
/// CLI opts
pub struct Opts {
    pub list_sites: bool,
    pub print_config_path: bool,
    pub update_sites: bool,
    pub set_api_key: Option<String>,
    pub query: Option<String>,
    pub config: Config,
}

/// Get CLI opts, starting with defaults produced
/// from `mk_config` and matching args with `get_matches`.
fn get_opts_with<F, G>(mk_config: F, get_matches: G) -> Result<Opts>
where
    F: FnOnce() -> Result<Config>,
    G: for<'a> FnOnce(App<'a, '_>) -> ArgMatches<'a>,
{
    let config = mk_config()?;
    let limit = &config.limit.to_string();
    let clapp = App::new("so")
        .arg(
            Arg::with_name("limit")
                .long("limit")
                .short("l")
                .number_of_values(1)
                .takes_value(true)
                .default_value(&limit)
                .help("Question limit per site query"),
        ); // other args elided for the sake of brevity
    let matches = get_matches(clapp);
    Ok(Opts {
        config: Config {
            limit: matches.value_of("limit"),
            // etc.
        },
        list_sites: matches.is_present("list-sites"),
        // etc.
    })
}

/// Get CLI opts with defaults pulled
/// from user configuration
pub fn get_opts() -> Result<Opts> {
    get_opts_with(Config::new, |a| a.get_matches())
}
{% endhighlight %}

The solution here can be seen directly in the type signature: the function of
type `G` where `G: for<'a> FnOnce(App<'a, '_>) -> ArgMatches<'a>` is the only
place where the `App`'s lifetime is mentioned, yet the function on the whole
just returns `Opts` with no `'a`. Thus, the `App` is allocated and completely
deallocated by the time this function completes, and there are no more dangling
pointers.

Honestly, it doesn't feel like idiomatic Rust to have these two higher-order
function arguments. But, it works, and it allows me to test the CLI interface
like I wanted.

{% highlight rust %}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::SearchEngine;

    fn defaults() -> Config {
        Config {
            api_key: Some(String::from("my key")),
            limit: 64,
            lucky: false,
            sites: vec![
                String::from("some"),
                String::from("sites"),
                String::from("yeah"),
            ],
            search_engine: SearchEngine::DuckDuckGo,
        }
    }

    fn mk_config() -> Result<Config> {
        Ok(defaults())
    }

    #[test]
    fn test_defaults() {
        let opts = get_opts_with(mk_config, |a| {
            a.get_matches_from(
                vec!["so", "how do I exit Vim"]
            )
        });

        assert_eq!(opts.unwrap().config, defaults());
    }

    #[test]
    #[should_panic]
    fn test_conflicts() {
        get_opts_with(mk_config, |a| {
            a.get_matches_from_safe(
                vec!["so", "--lucky", "--no-lucky"]
            ).unwrap()
        })
        .unwrap();
    }

    // etc.
}
{% endhighlight %}

## Async

Asynchronous programming in Rust was _very_ daunting at first. For one, there's
a mess of options and it was hard to know what to use (tokio vs. async-std,
etc.). And the tutorials and StackOverflow answers that you find for these
options are likely to be out of date, given the rapid development of these
various avenues for async support.

This is one area where I found the Haskell implementation was easier; there, I
know to reach for
[async](https://hackage.haskell.org/package/async-2.2.2/docs/Control-Concurrent-Async.html)
and the library is incredibly easy to use. But of course,
in some sense this is comparing apples to oranges; namely,
Haskell has a runtime and a garbage collector.[^4] It _better_ be easier to
write asynchronous code with such tools available at runtime!

That being said, once I decided to depend on tokio, it was [relatively
straight-forward](https://github.com/samtay/so/commit/ec92f930344d364e3be359a41aebea78f8205fa7)
to switch to tokio's runtime, and the compiler is pretty good at guiding you.

For a demonstration of some common async tasks,
[here](https://github.com/samtay/so/commit/5f88657a75c4443ba93936e0f14bb3be0435fd41#diff-639fbc4ef05b315af92b4d836c31b023R87-R92)
is the commit which gets results in the background, and
[here](https://github.com/samtay/so/commit/412676f8c99a93ee879c9c127a58b32dae50cdfa#diff-e2d87d7106c6198944f0115725f2a85bR114)
is where I shoot off an arbitrary number of parallel requests.

## Libraries

In my limited experience this past month, my take is that the libraries are
numerous and excellent. I thought `so` might be a bit too much to chew while
learning a new language, but it turns out it's mostly glue code and all the
heavy lifting is done by regularly maintained and _fairly_ well-documented
libraries. The current app is way faster and more feature-rich than my original
Haskell implementation, which I spent much more time on.

#### tui-rs
After an initial survey of the landscape, I went with
[tui-rs](https://github.com/fdehau/tui-rs) as a TUI framework. I quickly ran into
hurdles when it came time to deal with user input. That's not to say you _can't_ deal
with user input in tui-rs, but that you'll be doing a lot more manual work to do
so. And I'm not disparaging the library here; this just isn't part of its goals.
Here is an
[example](https://github.com/fdehau/tui-rs/blob/e789c671b038d516c347874e899be815b15caf25/examples/user_input.rs)
demonstrating the work involved.

#### cursive

I ended up cutting my losses and trying out
[cursive](https://github.com/gyscos/Cursive). This was _such_
a good decision. The documentation is sort-of-lacking, but I was pleasantly
surprised just how flexible this library is, in a general sense with its callback
structure, and in small ergonomic ways such as
accepting `Into<String>` etc. types of arguments in most of its API.
Suffice it to say, if you are looking to build a TUI with some level of user
interaction, cursive is a great choice. _But_ you might have to explore its
codebase rather than its
[docs.rs](https://docs.rs/cursive/0.15.0/cursive/index.html). As with most of
the TUI libraries I've come across, the
[examples](https://github.com/gyscos/cursive/tree/master/examples)  will also go
a long way.

#### Feature flags, dependencies, and orphans
Another thing I like about the ecosystem is the common motivation for
slim dependencies and supporting a wide range of high-to-low level library usage via
cargo [feature
flags](https://doc.rust-lang.org/stable/cargo/reference/features.html#the-features-section).
When libraries adhere to this pattern (which seems to be quite common), if you
want to pull in less dependencies and only depend on what you need, you can just
specify the proper feature flags for a library in your `Cargo.toml`, like this:
{% highlight rust %}
reqwest = { version = "0.10", features = ["gzip", "json"] }
{% endhighlight %}
For some great examples of this, you can see the options available on the
[reqwest](https://docs.rs/reqwest/0.10.6/reqwest/#optional-features) crate or,
taken to an even more extreme level,
the [tokio](https://docs.rs/tokio/0.2.21/tokio/index.html#feature-flags) crate.
I would love to see more of this in Haskell's libraries, starting with
[lens](https://github.com/ekmett/lens/).  (Currently, if you want less
dependencies you can depend on the duplicated
[microlens](https://github.com/monadfix/microlens) library.)

This pattern can also alleviate the pain of orphans. In Rust, orphans are strictly
disallowed. In Haskell, orphans result in warnings and thus programmers have
more options in how to deal with them. The default "correct" solution is to use a newtype
wrapper, but when this is cumbersome it's not hard to find entire modules like
{% highlight haskell %}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module My.Namespace.Orphans where
-- do bad things here
{% endhighlight %}
This is especially common with things like JSON serialization & deserialization.
The most common orphans I see in Haskell are
[`aeson`](https://hackage.haskell.org/package/aeson) instances, because if some
library `acme` that has nothing to do with JSON exports a datatype `Foo`, why
should they incur an `aeson` dependency for the unknown, possibly empty subset
of library consumers that might want to serialize and deserialize `Foo`?
**But** if `acme` instead added an optional library flag `json` (like the `reqwest`
library above), then `acme` can provide the _optional_ dependency and instances. It's a
win-win.

## Concluding remarks
As a language, Haskell is hard to beat. It's a thing of beauty. But the language
itself isn't everything, and in just about every other aspect (compiler,
tooling, ecosystem, etc.) I've had a more enjoyable time with Rust over the past
month. So I'm going to continue exploring it. I plan to start a new project at a
lower level, closer to hardware, to see Rust in its more typical domain. If
you have any project ideas please leave a comment.

## Footnotes
[^1]: I don't want to hate on the language, but at the same time I don't think it's all that controversial to simply _acknowledge_ issues such as community fragmentation around build tools (cabal, stack, nix), lack of IDE, etc.
[^2]: Shout out to [rustlings](https://github.com/rust-lang/rustlings) which really helped me get used to the syntax and semantics. If I find the time, I'd like to start an equivalent for Haskell.
[^3]: Note that which lifetime parameter to use only depends on the internals of `App` and exactly which parts of that struct have the lifetimes `'a` and `'b`.  The solution here implies that it is the _first_ lifetime parameter that specifies the lifetime of the borrowed default value strings. We could have also looked at the definition of `clap::App` to figure this out.
[^4]: Or more precisely, any implementation of Haskell.

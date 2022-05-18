---
layout: post
title: Solar Energy
description: An overview of our solar system
tags: []
mathjax: true
---

# Overview

I'm going to give an overview of our solar setup so that I can refer back to it
when I inevitably forget all the details a year or two from now. I'm not sure if
anyone else will ever read this, but if so, feel free to reach out with any
questions and I'll be happy to help clarify things or give advice.

At a high level, the idea behind solar power is quite simple, but there are a
few moving parts that can cause confusion once you dig in deeper.

  1. Energy from the sun is captured by solar panels at varying voltage.
  2. A solar charge controller converts this to a constant voltage usable by a bank of DC batteries.
  3. The batteries take in energy from the charge controller, and store it.
  4. A DC fuse block is connected to the batteries, and distributes power to DC appliances.

Really, that's the main story behind solar power. However, because AC is by far
the most common electrical current (think of your typical 120V residential
outlets in the U.S), there's an addendum:

  1. An inverter takes DC current from the batteries and makes it usable as AC current.
  2. AC breaker box takes power from the inverter and provides it to AC appliances.

### Solar Panels

< insert img >

We have eight 400W Aptos (DNA-144-MF23-400W) solar panels for a total of 3200W
on the roof. These are arranged as a 2s4p array, which means there are four
pairs of panels, with each pair (a.k.a. _string_) wired in [series](), and the
four strings wired in [parallel](). In choosing this configuration, there are
two main considerations.

First is the effects on how the wattage is factored into potential and current.
Recall
\\[ W = V \cdot A \\]
In short, wiring in series causes the potential (voltage) to increase, and
wiring in parallel causes the current (amperage) to increase. Calculating these
values is important when planning for wire sizes and choosing a charge
controller.

The second consideration concerns shading. It is important to note that a single
solar panel consists of individual photovoltaic (sp?) _cells_ wired in series.
For example a typical residential 24V nominal panel has 72 cells in series.[^1]
When the panel is partially shaded, these series connections result in the power
output of the whole panel being drammatically reduced, by a factor much greater
than the fraction of the panel in shade.[^2] When you connect a string of panels
in series, it's similar to creating one large solar panel, and partial shading
across one of the panels will affect the entire string. Parallel connections do
_not_ have this issue; if you have four panels wired in parallel and one is
completely shaded, or even disconnected, the other three will still operate
independently.

It seems many people learn about partial shading and immediately conclude that
parallel connections are superior, but this is not necessarily true, and in fact
I would argue that in most cases, series connections are the better choice. The
reason is that, for efficiency's sake, having a higher voltage is a big win.

For one, the charge controller typically needs the panel
voltage to be at least double[^3] the battery voltage before it can start converting
any power. Consider a 1000W array of five 200W panels, each with 20Voc and 10Isc,
powering a 12V battery bank. If these five panels were in parallel, the combined
voltage even in ideal conditions is quite low at 20V. As a result, the
batteries might not start charging until very late in the morning and might stop
early in the afternoon. Similarly, there might be periods of time on a cloudy
day where no solar power is converted, due to low voltage. On the other hand, if
wired in series, the combined voltage in ideal conditions is 100V, and reaching
the threshold voltage of 20V (? find it) is likely to happen soon after sunrise
and last until close to sunset.

## Footnotes
[^1]: Our panels actually have 144 cells in 2p72s, but this detail isn't so important.
[^2]: Is this true?
[^3]: Is this true? Link to spec?

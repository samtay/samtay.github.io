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

We have eight 400W Aptos (DNA-144-MF23-400W) solar panels for a total of 3200W
on the roof.
{% include image.html path="solar/panels.jpg" path-detail="solar/panels.jpg" alt="Rooftop solar panels" %}
These are arranged as a 2s4p array, which means there are four
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

TODO describe our actual voltage and amperage at STC.

TODO possibly describe fusing requirements and how parallel requires fuses based
on Nate's explanation.

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

### Solar Charge Controller (SCC)

We have a Victron SmartSolar MPPT Tr VE.Can 150V | 100A Solar Charge Controller.
In general, you do want an MPPT (Maximum Power Point Tracking) controller as
they can harvest more energy, however
they are more expensive than PWM (Pulse Width Modulation) controllers. Our SCC
can be configured for 12V, 24V, 36V, and 48V battery banks. The important
decision when it comes to choosing an MPPT SCC for your setup are the two
numbers above: 150|100.

The first number is the maximum input voltage on the panels. Recall our array
(in idealized standard conditions) has an open circuit voltage \\(V_{VOC} = 97.1\\). It's
important to have a firm margin between the controller voltage limit and your
\\(V_{VOC}\\) since the actual voltage of your panels can increase past this number
in e.g. cold weather. Notice that I arranged my panels with the _maximum_ series
connections, as a 3s2p array would put us far too close to the 150V limit. This
would have been possible had we gotten the `250V | 100A` SCC from Victron.

The second number is the maximum output amperage to the batteries. This is the
number that determines how much you can harvest from the sun at any given moment.
The `100A` is the largest SCC that they make, and it is quite expensive! We
could have paid less for, say a `50A` or `85A`, but as with the rest of our
solar system, I erred on the size of "go big or go home". Given the number of
our panels, either way we have an "over-paneled" setup; that is, the maximum
wattage I will ever see from our SCC is less than the total panel wattage. is
The max SCC wattage output is its maximum amperage output multiplied
by the voltage of our battery bank, typically around 27V, for a total of 2700W.
However, I have seen the value peak over 2900W on a good day. This is fine from
the SCC's perspective, as any extra potential solar energy simply won't be
harvested.

Notice the maximum output of the SCC is in terms of _amperage_ and not
_wattage_. Clever readers will notice that maximum wattage output of a given
controller could be increased by increasing the voltage! And, this is correct,
we could have had an equivalent power output with a `150V | 50A` controller and
a 48V battery bank. In general, you should prefer the highest voltage battery
bank with which you are comfortable; the higher the volts, the greater
efficiency all around. However, this being our first solar setup, I was a bit
paranoid about high voltage systems (and, it's possible my fears were misplaced).
If I build a stationary solar system next, I will certainly up the voltage.

### Battery Bank

## Footnotes
[^1]: Our panels actually have 144 cells in 2p72s, but this detail isn't so important.
[^2]: Is this true?
[^3]: Is this true? Link to spec?

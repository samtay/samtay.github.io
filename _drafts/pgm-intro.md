---
layout: post
title: 'Graphical Models: Part 1'
description: Introduction to probabalistic graphical models
tags: [pgm, probability]
mathjax: true
---

This week I will start my second quarter as a PhD student at the University of
Washington and I will be studying Graphical Models with [Sewoong
Oh](https://homes.cs.washington.edu/~sewoong/) over in the CS department, a class
which I've been looking forward to for a number of reasons, chief among
them being that I like the opportunity to employ (seemingly disparate) pure
math techniques to unsuspecting applied problem domains; in this case,
solving probabilistic inference problems via graph theory.

This is the first of a series designed to introduce the concept of probabalistic graphical models, motivate their definitions, demonstrate their mathematical beauty, and (time-permitting) show their implementation in Haskell.

**Disclaimer**: this *Part 1* introductory post is rooted in the excessively optimistic assumption that I will have time to write subsequent posts that expand from this introduction, and the less excessively optimistic assumption that this expansion can be written to a broader audience than fellow graduate students enrolled in UW CSE 515.

**Prerequisites** are unfortunately yet to be determined. Since I've only just started this class it is impossible to know exactly how much of it can be disseminated to a broader audience. At the very least, you will need
1. Mathematical maturity, at least enough to understand the definition of a [graph](https://en.wikipedia.org/wiki/Graph_theory) <!-- or link to my own definition -->
2. Probability at the [undergraduate level](https://projects.iq.harvard.edu/stat110/home).

# Motivation

Let \\(\mathcal{X}\\) be some alphabet[^1] and consider a probability
distribution over the sample space \\(\mathcal{X}^n\\) with probability mass
function \\(\mu\\). That is, letting \\((X_1,\ldots,X_n)\\) denote a random
vector over this distribution,

$$ \mu(x_1, \ldots, x_n) = \mathbb{P}(X_1 = x_1, \ldots, X_n = x_n). $$

In this fully general setting where the \\(X_i\\)'s can be arbitrarily interdependent,
notice that \\(\mu\\) must be specified by \\(O(|\mathcal{X}|^n)\\) values. However,
if the \\(X_i\\)'s are independent, there is a tremendous
simplification as this distribution factors into \\(\mathbb{P}(X_1 = x_1)
\cdots \mathbb{P}(X_n = x_n)\\),
and in this case, \\(\mu\\) is specified by only \\(O(n|\mathcal{X}|)\\)
values. By exploiting this admittedly best-case independence scenario, our
computations regarding \\(\mu\\) reduce from exponential to linear in the
dimension \\(n\\).

This is the main idea behind our graphical model representations. While it is
rare for the \\(X_i\\) to be completely independent, it is often the
case that some of the \\(X_i\\) are conditionally independent (or at least, the
assumption of such conditional independence is a reasonable approximation),
which allows a reduction in complexity and thus more efficient and feasible
algorithms.

### Example
Let's take a small detour to look an illustrative example. Lately in U.S.
domestic politics, partisanship and party alliance seem to increasingly determine an
individual's views on a given issue. This observation suggests the
approximation that issues are conditionally independent given the party
alliance is known. So suppose we pick an American at random and consider the
probability space induced by three <!-- TODO count -->
random variables *Party* \\((P)\\), *Gun Control* \\((G)\\), and *Health Care*
\\((H)\\). For the sake of simplicity we will treat these as binary random variables,
i.e.

\\[ Val(P) = \\{p^0, p^1\\} \quad Val(G) = \\{g^0, g^1\\} \quad Val(H) = \\{h^0, h^1\\} \\]

where

\begin{align}
p^0 &:= \text{republican} \\\
p^1 &:= \text{democrat} \\\
g^0 &:= \text{does not support stricter gun laws} \\\
g^1 &:= \text{supports stricter gun laws} \\\
h^0 &:= \text{does not support medicare for all} \\\
h^1 &:= \text{supports medicare for all}
\end{align}

As discussed above, a fully general probablistic model allows for arbitrary \\(\mathbb{P}(P,G,H)\\) probabilities for the \\(2^n\\) possible outcomes in this space.[^3]

TODO draw example table of distribution

<!--

TODO maybe avoid this and just start with a graph, explain if it factors then good
Notice that by successively applying the definition of the conditional probability mass function[^2]
[^2]: \\(\mathbb{P}(A, B) = \mathbb{P}(A)\mathbb{P}(B\mid A)\\)
we can derive another general expression

$$ \mu(x_1, \ldots, x_n) = \mu(x_1) \prod_{i = 2}^n \mu(x_i \mid x_1, \ldots, x_{i-1}). $$

# Bayesian Networks

 -->
[^1]: Typically a finite set, e.g. \\(\\{0,1\\}\\).
[^3]: Technically, because these probabilities must sum to one, the distribution is fully specified by the first \\(2^n - 1\\) values.

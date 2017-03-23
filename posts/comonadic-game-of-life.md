---
title: Who needs HashLife when you have Comonads?
description: Improving a naive implementation of the Game of Life with an elegant and performant solution.
toc: yes
date: 2017-03-17
---

## Introduction

I'm going to talk a little bit about Conway's Game of Life,
Comonads in practical use,
and the performance improvement that they have to offer.
If you already know what the GoL is, skip the introduction,
and if you're already familiar with comonads and how they
are defined in Haskell, feel free to skip down to the performance section.

### What is the Game of Life?
Conway's [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)
is a cellular automaton of simple cells, each following simple rules,
from which very complex behavior emerges under the right conditions.
It is one of many examples of [complex systems](https://en.wikipedia.org/wiki/Complex_systems).

In a nutshell, there is a 2D grid of cells, each of which has two possible states: alive or dead.
The grid evolves in discrete steps of time `t`. At time `t = 0`,
we give the board some initial state. We can pick certain cells to be alive or dead,
or let a computer randomly decide what the grid should look like.
For all `t > 0`, the grid evolves to step `t + 1` based on these simple rules:

* Any live cell with exactly two or three live neighbours stays alive.
* Any dead cell with exactly three live neighbours becomes alive.
* All other cells die.

### A naive implementation
Yesterday I finished a little [terminal application](https://github.com/samtay/conway/tree/v0.0.0)
to play around with the GoL (link set to "initial" version before comonads).
As you can guess from the rules above, the GoL is very easy to program;
the difficulty is in programming it *efficiently*. One well known method
of computing the game is known as HashLife, which is a pretty objectively complex
technique. (Someone did this, or some of it, in Haskell
[here](http://dotat.at/prog/life/hslife.hs).)

In my first pass at this, instead of creating a custom data structure directly
I opted to leverage [grid](http://hackage.haskell.org/package/grid) which is
a really cool library that is useful for exploring mathematical grids/graphs/lattices:
```haskell
import Math.Geometry.Grid (Index)
import Math.Geometry.Grid.Octagonal (TorOctGrid)
import Math.Geometry.GridMap.Lazy (LGridMap)

data St = Alive | Dead
type Board = LGridMap TorOctGrid St
type Cell = Index Board
```
It was nice to do this first because I got **a lot** for free.
Essentially my board looks like a mapping of `(x,y)` coordinates to cell states.
In fact, the `toList` function that we get from the `Grid` typeclass  confirms this:
```haskell
λ> toList $ blinker 3 3
[((0,0),Dead),((0,1),Alive),((0,2),Dead),((1,0),Dead),((1,1),Alive),((1,2),Dead),((2,0),Dead),((2,1),Alive),((2,2),Dead)]
```
I even get a `neighbours` function that returns all 8 neighbours of a cell
along with many more useful functions, so implementing
game evolution was very straightforward:
```haskell
step :: Board -> Board
step b = GM.mapWithKey rule b
  where rule :: Cell -> St -> St
        rule c Dead
          | liveNeighbors c == 3 = Alive
          | otherwise            = Dead
        rule c Alive
          | liveNeighbors c == 2 = Alive
          | liveNeighbors c == 3 = Alive
          | otherwise            = Dead

        liveNeighbors :: Cell -> Int
        liveNeighbors c = population $
          GM.filterWithKey (const . (`elem` neighbours b c)) $ b

population :: Board -> Int
population = sum . map fn . GM.elems
  where fn Alive = 1
        fn Dead  = 0
```
Furthermore, using the toroidal style of grid allows modular boundaries
which is how I wanted to implement this version.

So, you can see I was able to speed through the actual GoL logic since
most of the tedious legwork was done in the grid package.
My real challenge and where I spent the most effort was in the
frontend, rendering and handling user interaction from a terminal.
I chose to use [brick](http://hackage.haskell.org/package/brick)
which is a *fantastic* package that provides
a high level declarative API to develop terminal interface applications
along with a number of useful widgets - not to mention 17 awesome
demo programs, great documentation, and a responsive google group.
If you're curious, [this](https://github.com/samtay/conway/blob/v0.0.0/app/Main.hs)
is how I rendered the above implementation using the brick library.
But, this post is not about brick. Maybe that will come in the future.

## Comonads
Like any good Haskeller I'd like to leverage whatever abstractions I can
to improve the elegance and performance of this codebase.
As it turns out, cellular automata are well represented by comonads.

### Definition
Let's consider what the *dual* of the `Monad` type looks like:
```haskell
-- Monad definition (adapted)
class Functor => Monad m where
  return :: a -> m a
  join   :: m (m a) -> m a
  bind   :: (a -> m b) -> (m a -> m b)

-- Comonad definition
-- Minimum defintion: extract, (duplicate | extend)
class Functor => Comonad m where
  extract   :: m a -> a       -- aka co-return
  duplicate :: m a -> m (m a) -- aka co-join
  extend    :: (m b -> a) -> (m b -> m a) -- aka co-bind
```
As anyone else on the internet would say, the *dual* of something
is when its "arrows are flipped around", which at first sounds like handwavey
nonsense. Head [here](http://blog.ezyang.com/2012/10/duality-for-haskellers/)
for an excellent explanation of duality and how it applies to types in Haskell.
I don't want to get lost in the forest or duplicate content on the internet,
so click that link or be satisfied with the fact that the arrows are literally
flipped in the type signatures above.

I don't want to get bogged down in category theory land -
if you want to go down that path, see my [resources](#further-reading).
Instead, let's just build up intuition with some examples.

### Examples
The intuition we are trying to garner is that while monads *produce* effectful computations,
comonads are *consumed* in context-sensitive computations.
They usually come in handy when there is some large data structure that is composed
of small, similar computations. Sound familiar?

#### Stream
This is probably the simplest example, almost canonical to comonads:
```haskell
data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Comonad Stream where
  extract :: Stream a -> a
  extract (Cons x _) = x

  duplicate :: Stream a -> Stream (Stream a)
  duplicate xxs@(Cons _ xs) = Cons xxs (duplicate xs)

  extend :: (Stream a -> b) -> Stream a -> Stream b
  extend f xxs@(Cons _ xs) = Cons (f xxs) (extend f xs)
```
So `extract` is like `head` and `duplicate` is like `tails`.
`extend` on the other hand looks a little `fmap`-y:
```haskell
fmap   :: (       a -> b) -> Stream a -> Stream b
extend :: (Stream a -> b) -> Stream a -> Stream b
```
Well, it's sort of similar to `fmap`
but the type signature is slightly different
in that the function argument `f :: Stream a -> b` accepts its first
argument already of type `Stream a`.
Consequently, `f` can *know* or be *context-aware* of the comonadic structure
when it produces its return value of type `b`. This is where the power of
comonad really shines. In this case, the context that `f` is aware of
at *each* function call when mapping over the stream is a current element
`x` (we'll say at the current "cursor") along with the whole tail of the list from
`x` onwards.

This observation lends itself to the intuition we set out to build, namely that
monads *produce* additional context
while comonads are *consumed* within a context.
```haskell
bind   :: (a -> m b) -> m a -> m b
extend :: (m a -> b) -> m a -> m b
```
Note that `bind` accepts a function `g :: a -> m b`
that takes an `a` value and *produces* a contextual value `m b`.
Contrast this with `extend` accepting a function `f :: m a -> b`
which has `f` *consuming* the contextual value `m a`.

#### Zipper
```haskell
-- (elems to the left) (cursor) (elems to the right)
data Zipper a = Zipper [a] a [a]

-- moving the cursor (not changing any values, per se)
left, right :: Zipper a -> Zipper a
left  (Zipper (l:ls) x rs) = Zipper ls l (x:rs)
right (Zipper ls x (r:rs)) = Zipper (x:ls) r rs

instance Functor Zipper where
  fmap f (Zipper l x r) = Zipper (fmap f l) (f x) (fmap f r)

instance Comonad Zipper where
  extract (Zipper _ x _) = x
  duplicate z = Zipper (iterate' left z) z (iterate' right z)
    where iterate' f = drop 1 . iterate f
```

Here's what this looks like in practice:
```haskell
λ> let z = Zipper [(-1),(-2)..] 0 [1..]
λ> let take' n (Zipper l x r) = (Zipper (take n l) x (take n r))
λ> take' 5 z
Zipper [-1,-2,-3,-4,-5] 0 [1,2,3,4,5]

-- duplicate
λ> take' 5 $ fmap (take' 5) $ duplicate $ z
Zipper
  [ Zipper [-2,-3,-4,-5,-6] (-1) [0,1,2,3,4]
  , Zipper [-3,-4,-5,-6,-7] (-2) [-1,0,1,2,3]
  , Zipper [-4,-5,-6,-7,-8] (-3) [-2,-1,0,1,2]
  , Zipper [-5,-6,-7,-8,-9] (-4) [-3,-2,-1,0,1]
  ,Zipper [-6,-7,-8,-9,-10] (-5) [-4,-3,-2,-1,0]
  ]
  (Zipper [-1,-2,-3,-4,-5] 0 [1,2,3,4,5])
  [ Zipper [0,-1,-2,-3,-4] 1 [2,3,4,5,6]
  , Zipper [1,0,-1,-2,-3] 2 [3,4,5,6,7]
  , Zipper [2,1,0,-1,-2] 3 [4,5,6,7,8]
  , Zipper [3,2,1,0,-1] 4 [5,6,7,8,9]
  , Zipper [4,3,2,1,0] 5 [6,7,8,9,10]
  ]

-- extend . extract
λ> take' 5 $ extend extract z
Zipper [-1,-2,-3,-4,-5] 0 [1,2,3,4,5] -- this makes sense, also is a law!

-- extend (without leveraging context)
λ> take' 5 $ extend (\(Zipper _ x _) -> 2 * x) z
Zipper [-2,-4,-6,-8,-10] 0 [2,4,6,8,10]

-- extend (looking to the immediate left and right)
λ> take' 5 $ extend (\(Zipper (l:_) x (r:_)) -> l + 2 * x + r) z
Zipper [-4,-8,-12,-16,-20] 0 [4,8,12,16,20]
λ> take' 5 $ extend (\(Zipper (l:_) x (r:_)) -> concat . intersperse "," . map show $ [l,x,r]) z
Zipper
  ["-2,-1,0","-3,-2,-1","-4,-3,-2","-5,-4,-3","-6,-5,-4"]
  "-1,0,1"
  ["0,1,2","1,2,3","2,3,4","3,4,5","4,5,6"]
```

Hopefully these examples show how comonads are a very fitting solution
to computing cellular autamata. Again, refer to [resources](#further-reading)
if you are unsatisfied, as there's plenty of content to read up on.

### Applying to the Game of Life
I want to change as little as possible from my current implementation -
ideally just swap out the data structure and change very little in my frontend and test suite.

## Performance
### Initial
Here are some profiling details from the first implementation,
which mapped across the board while performing lookups to retrieve the neighborhood values:

* Time: **13.87s**

```bash
$ time ./Spec +RTS -hc -p -K100M
...
... 13.87s user 0.02s system 100% cpu 13.875 total
```

* Memory:

```bash
# getting a web friendly view of heap profiler
$ hp2ps -e8in -c Spec.hp
$ convert Spec.ps initial-heap-profiling.png
```
![](/img/comonadic-gol/initial-heap.png)

Quite a bit of memory spent in the `step` function.

### Comonads to the rescue

## Further reading
For more in-depth reading on category theory and comonads, here are my sources:

* [Duality for Haskellers](http://blog.ezyang.com/2012/10/duality-for-haskellers/) - EZ Yang
* [Flipping arrows in coBurger King](http://blog.ezyang.com/2010/07/flipping-arrows-in-coburger-king/) - EZ Yang
* [Comonad presentation](https://www.youtube.com/watch?v=F7F-BzOB670) - Kenny Foner
* [ComonadSheet source code](https://github.com/kwf/ComonadSheet) - Kenny Foner
* [Another comonad presentation](https://www.slideshare.net/davidoverton/comonad) - David Overton

---
layout: post
title: 'Adventures in Refactoring'
description: Spelunking into stale code two years later.
tags: [haskell]
mathjax: false
---

This week I decided to revisit my halfway-finished (i.e. abandonded) TUI project:
[so](https://github.com/samtay/so). My last commit to this project was in May
2018, which was before I joined SimSpace[^1]
where I was exposed to myriad new techniques, from clever
[constraint](https://hackage.haskell.org/package/constraints-0.12/docs/Data-Constraint.html)
programming to new [language extensions](/posts/deriving-via-use-case), in addition to
debates regarding larger-scale patterns such as how to achieve error handling
with the fewest headaches possible. Needless to say, after working with such
smart and talented folks, I am a better developer now than I was before.[^2]

Lately I've been craving some Haskell time, and pondering my decision to enter
a Statistics graduate program :thinking:, ergo diving back into `so`. I was
happy to find code that horrified me and was in dire need of refactoring.
Unfortunately I was a bit overzealous and ended up just issuing a massive
[*Revive in
2020*](https://github.com/samtay/so/commit/dd90673b5cb705118dc7a7c616c58cbe275e9942)
commit, before thinking that such refactoring might be of interest to budding
Haskell developers. Let's dive into some of these changes.

### Default Extensions
This one may be obvious to most, but I previously had never used the
`default-extensions` clause in cabal. I always just included a massive number
of `{-# LANGUAGE Extension #-}` directives at the top of every file, having to
add them ad hoc when `ghcid` told me to do so. If you have a preferred set of
extensions that, in your own opinion, are not controversial and should be
enabled unanimously, stick them in your cabal file [like
this](https://github.com/samtay/so/commit/dd90673b5cb705118dc7a7c616c58cbe275e9942#diff-36f413166e2fc25f29ab47d0e5789fecR29-R45)
so they are enabled for every module automatically.

### Bracket Pattern
I had a clear case of a bracket pattern in my loading
animation:
{% highlight haskell %}
waitWithLoading :: Async a -> IO a
waitWithLoading a = do
  loadingThreadId <- forkIO showLoadingAnimation
  res             <- wait a
  killThread loadingThreadId
  A.clearLine
  A.setCursorColumn 0
  A.showCursor
  return res
{% endhighlight %}
Here I have some `a :: Async a` that the user is waiting for, and I'm providing
a nice animation for them while they wait. Once I get the `res :: a`, I need to
clear and clean up the animation.  Whenever we want to execute some `action`
which should always be preceeded
and followed by some other `pre_action` and `post_action`,
[bracket](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#v:bracket)
is the way to go:
{% highlight haskell %}
waitWithLoading a =
  bracket
    (forkIO showLoadingAnimation)
    (\tid -> do
      killThread tid
      A.clearLine
      A.setCursorColumn 0
      A.showCursor)
    (const $ wait a)
{% endhighlight %}
While the before and after code may look functionally the
same, there is actually an important difference in behavior: exception
handling. In my previous version, if `wait a` threw an exception, the animation
would not be cleaned up! As the documentation for bracket explains, in the
second version if `wait a` throws an exception, the animation clean up still
gets called, and _then_ the exception is re-thrown to the same scope. This is
the correct behavior.

I found another, slightly subtler, instance of bracket in a state-monadic context:
{% highlight haskell %}
queryLucky :: App (Either Error (Question [] Markdown))
queryLucky = do
  initialState <- get
  put $ initialState & sOptions . oLimit .~ 1
  result <- query
  put initialState
  return $ case result of
    Right []    -> Left NoResultsError
    Right (q:_) -> Right q
    Left e      -> Left e
{% endhighlight %}
Here `queryLucky` is a hopeful version of `query` that just grabs the first
search result from the StackExchange API. Consequently, instead of requesting
the usual number of question results, we limit the request to a single
question, so as to have a more efficient API call. So I want to run an inner
`query :: App b` action with a temporarily modified configuration state,
setting the limit to one, and `put` the initial state back in place once I'm
done. This should trigger your bracket spidey-sense! What could go wrong if
`query` throws an exception? The caller of `queryLucky` is left with what was
supposed to be a *temporarily* modified state! So let's bracket this:
{% highlight haskell %}
queryLucky :: App (Either Error (Question [] Markdown))
queryLucky = do
  initialState <- get
  result <- bracket_
      (modify $ sOptions . oLimit .~ 1)
      (put initialState)
      query
  return $ case result of
    Right []    -> Left NoResultsError
    Right (q:_) -> Right q
    Left e      -> Left e
{% endhighlight %}
Now regardless of what happens in `query`, we are assured that `put
initialState` will be called afterwards. Note here that
`bracket_ :: App a -> App c -> App b -> App b` is a [generic
 version](https://hackage.haskell.org/package/exceptions-0.10.4/docs/Control-Monad-Catch.html#v:bracket_)
 that runs in the `MonadMask App` instance, rather than `IO`. It is also a
 shorthand variant of `bracket` which doesn't require passing values to the
 second and third actions, like we had to do above with the forked thread ID.
Also, see in the same module other useful variants of
the bracket pattern, such as `bracketOnError` or `onException` which only run
the "release" action when an exception is thrown.

### LambdaCase

### Error Handling


### Footnotes
[^1]: [SimSpace](https://angel.co/company/simspace/jobs/64261-software-engineer-backend) was a fantastic place to work, and they are still hiring! I highly encourage you to apply if you love programming in Haskell and want to work remotely (or not).
[^2]: Hopefully this will always be true.

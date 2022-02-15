---
layout: post
title: 'Adventures in Refactoring'
description: Spelunking into stale code two years later.
tags: [haskell]
mathjax: false
---

I recently decided to revisit my halfway-finished (i.e. abandonded) TUI
project: [so](https://github.com/samtay/so-hs). My last commit to this project was
in May 2018, which was before I joined SimSpace where I was exposed to myriad
new techniques, from clever
[constraint](https://hackage.haskell.org/package/constraints-0.12/docs/Data-Constraint.html)
programming to new [language extensions](/posts/deriving-via-use-case), in
addition to debates regarding larger-scale patterns such as how to achieve
error handling with the fewest headaches possible. I've found that whenever I
enter a new environment and get exposed to other, smarter peoples'
perspectives, my old code looks more and more wretched. So I was not surprised
that upon revisiting `so`, I found code that horrified me and was in dire need
of refactoring, some of which may be of interest to budding Haskell developers.
So, if you're interested, let's dive into some of these changes, enumerated
here for convenience:
- [Bracket Pattern](#bracket-pattern)
- [LambdaCase](#lambdacase)
- [Error Handling](#error-handling)
- [NonEmptyList](#nonemptylist)
- [Type Synonyms](#type-synonyms)

{% comment %}
### Default Extensions
This one may be obvious to most, but I previously had never used the
`default-extensions` clause in cabal. I always just included a massive number
of `{-# LANGUAGE Extension #-}` directives at the top of every file, having to
add them ad hoc when `ghcid` told me to do so. If you have a preferred set of
extensions that, in your own opinion, are not controversial and should be
enabled unanimously, stick them in your cabal file [like
this](https://github.com/samtay/so-hs/commit/dd90673b5cb705118dc7a7c616c58cbe275e9942#diff-36f413166e2fc25f29ab47d0e5789fecR29-R45)
so they are enabled for every module automatically.

In fact, if you are starting a new Haskell package, you should probably be
using `hpack` which greatly simplifies the management of cabal files. (This is
also the new default for `stack new` projects.)
{% endcomment %}

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
clear and clean up the animation.  Whenever you want to execute some `action`
which should always be preceded
and followed by some other `pre_action` and `post_action` respectively,
[bracket](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#v:bracket)
is the way to go:
{% highlight haskell %}

bracket :: IO a        -- first "acquire resource"
        -> (a -> IO b) -- last "release resource"
        -> (a -> IO c) -- computation to run in-between
        -> IO c

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
same, there is an important difference in behavior: exception
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
[`LambdaCase`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/lambda_case.html)
is one of my favorite extensions. It's also arguably the smallest
and simplest one available. Chances are if you are reading this, you are
already aware of it, but I thought I'd include it here just in case.
All it does is enable the `\case` syntactic sugar which is short for `\a ->
case a of`, but it cleans up code surprisingly well.
Here is an example of an unnecessarily named function:
{% highlight haskell %}
execPrompt :: Async (Either Error [Question [] Markdown]) -> App ()
execPrompt aQuestions = liftIO $
  waitWithLoading aQuestions >>= runner
  where
    runner qs =
      if null qs
        then exitWithError "No results found. Try a different question."
        else void $ runStateT (runByline runPrompt) (initPromptState qs)
{% endhighlight %}
The `runner` function is poorly named and an unnecessary binding. Here's a
version with LambdaCase, which let's the information flow a bit more cleanly:
{% highlight haskell %}
execPrompt :: Async (Either Error [Question [] Markdown]) -> App ()
execPrompt aQuestions = liftIO $
  waitWithLoading aQuestions >>= \case
    [] -> exitWithError "No results found. Try a different question."
    qs -> void $ runStateT (runByline runPrompt) (initPromptState qs)
{% endhighlight %}

### Error Handling
What jumped out the most to me, and probably to you as well by now, is just how many
`App (Either Error a)` types are floating around.
While there's nothing inherently wrong with passing
`Either e a` types around, even frequently, generally speaking this is a code
smell, particularly if they are nested directly within an `App`-esque monad
stack. The issue is that, in most of these places along the application flow, I
don't particularly care about the `Either e` context and am simply bookkeeping to
have errors bubble up to a top-level (or nearer-to-top-level) error handler.

There are a number of ways for me to avoid this bookkeeping and replace `App (Either
Error a)`'s with `App a`'s, so as to have a cleaner flow of information.
Because my errors were really only used closer to the `main` entry point, I
chose to use exceptions. In particular because I am using asynchronous
processes for a friendlier user interface, it made sense to have a
[`MonadMask`](https://hackage.haskell.org/package/exceptions-0.10.4/docs/Control-Monad-Catch.html#t:MonadMask)
instance. Similar refactoring could be accomplished with
[`MonadError`](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#t:MonadError),
or even just putting errors into your `MonadState` transformer; it really just
depends on the use case and style preference.[^3]

For this code, the `Either Error a` was only coming from one place, namely the
decoding of the JSON response from Stack Exchange:
{% highlight haskell %}
seRequest
  :: String
  -> [W.Options -> W.Options]
  -> App (Either Error [Question [] Text])
seRequest resource optMods = do
  -- eliding irrelevant code
  decoded <- makeRequest resource optMods >>= thenDecodeIt
  return $ case decoded of
    Left e   -> Left . JSONError $ T.pack e
    Right [] -> Left NoResultsError
    Right qs -> Right qs
{% endhighlight %}
So all that changes is the last return statement, and of course the type signature:
{% highlight haskell %}
seRequest
  :: String
  -> [W.Options -> W.Options]
  -> App [Question [] Text]
seRequest resource optMods = do
 -- eliding again
 case decoded of
    Left e   -> throwM $ JSONError $ T.pack e
    Right [] -> throwM NoResultsError
    Right qs -> pure qs
{% endhighlight %}
With these changes, the rest of the code between `main` and `seRequest` can be
cleaner and just concern itself with the underlying question and answer data
coming from the StackExchange API. At the top level, instead of dealing with
the `Either Error` we deal with the exception. Originally the code looked something like:
{% highlight haskell %}
-- Simplified main example
main :: IO ()
main = do
  q <- getQuery
  runApp q >>= \case
    Left e    -> exitPrintingError e
    Right qas -> print qas
{% endhighlight %}
and it changed to this:
{% highlight haskell %}
-- Simplified main example
main :: IO ()
main = do
  q   <- getQuery
  qas <- catch (runApp q) exitPrintingError
  print qas
{% endhighlight %}
Furthermore, even if a few of the intermediary functions along my application
flow required the `Either Error` context, there would still likely be a net
benefit to change to exceptions. I could still reference any possible errors
within the application flow, and choose whether or not the exception should
bubble up, like so:
{% highlight haskell %}
intermediary :: App a
intermediary = do
  txt <- catch getQuestionTxt $ \case
    NoResultsError -> "No results, try another query."
    e              -> throwM e
  displayText txt
{% endhighlight %}
In this example I want to continue the same application flow if the error is
simply that the query returned no results from the search API, a reasonable
thing to do.[^4] But for other bona fide exceptions such as a JSON decoding
issue, I `throwM e` to continue to let the exception bubble up, halting
execution until the next exception catcher. Some readers may be opposed to
this latter version aesthetically, and I sympathize, but if it cleans up the
ugly juggling of `Either` types in 10 other locations, the maintenance benefit
outweighs the "purity" aesthetic.

To play devil's advocate, I do think in the current state of
the codebase, `MonadError e m` is a great fit, especially given that I am only
dealing with a single type of error. And for the scope of this application, now
and in the future, that's probably a fine choice. However, after seeing the
headaches that can come from using `MonadError` as an application gets more
complex, and the reconciliation required of different types of errors in
various parts of a growing application, I personally feel some bias against the approach.

I've chosen to go over this section rather painstakingly because, in my
experience, it seems exceptions in Haskell are rarely found in beginner to
intermediate level code. I'm not really sure why this is; it could be
influenced by the available learning materials, or perhaps it inherently feels a little
"dirty" to use exceptions, or any dynamically scoped features, in such a pure
language, but I hope to disavow you of
this notion by referring you to Mark Karpov's Exceptions tutorial, or
at the very least the [Motivation for
exceptions](https://markkarpov.com/tutorial/exceptions.html#motivation-for-exceptions)
section.

### NonEmptyList
I think this was on my TODO list even back in 2018, but of course if I am
throwing a `NoResultsError`, why am I using a data type `[]` that allows for no
results? And worse, having to redundantly handle the `[]` case in three locations? Enter
[`NonEmptyList`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List-NonEmpty.html#t:NonEmpty)
which allows for safer code and peace of mind. Now instead of handling
the empty case at the prompt
{% highlight haskell %}
showLuckyAnswer :: Question [] Markdown -> IO ()
showLuckyAnswer question =
  case question ^. qAnswers of
    [] -> exitWithError "No answers found." -- this line never runs
    (answer:_) -> putMdLn answer
{% endhighlight %}
redundantly, since such a situation would have thrown an error earlier,
we can type-safely access the first element of the answers list:
{% highlight haskell %}
import qualified Data.List.NonEmpty as NonEmpty

showLuckyAnswer :: Question NonEmpty Markdown -> IO ()
showLuckyAnswer question =
  putMdLn . NonEmpty.head $ question ^. qAnswers
{% endhighlight %}


### Type Synonyms
Good type synonyms can do wonders for readability, particularly for type
constructors with common or canonical parameters. Consider
the noise of all the `Question [] Markdown` types you've seen in this post thus
far. I suppose now is a good time as any to explain the need for such a
parameterization: the `Question` datatype is defined as
{% highlight haskell %}
data Question t a = Question
  { _qId      :: Int
  , _qScore   :: Int
  , _qAnswers :: t (Answer a)
  , _qTitle   :: Text
  , _qBody    :: a
  } deriving (Functor)

data Answer a = Answer
  { _aId       :: Int
  , _aScore    :: Int
  , _aBody     :: a
  , _aAccepted :: Bool
  } deriving (Show, Functor)
{% endhighlight %}
and `t` is parameterized because, while most of the application iterates over
answers in a `NonEmpty` container, there are areas within the brick
implementation that need to be able to iterate over questions and answers in
brick's [list widget](https://hackage.haskell.org/package/brick-0.52/docs/Brick-Widgets-List.html)
container. Because they are nested, without this parameterization, we would
essentially need to duplicate datatypes for the brick interface. The `a` is
just the content type of the answers, which is initially just raw `Text` but
later is parsed into a `Markdown` AST.

But, as noted above, that means I've added this type parameter noise everywhere
in my application when only *one place*, the brick implementation, uses a
different parameter. We can de-noise this using a type synonym:
{% highlight haskell %}
type Question = Question' NonEmpty

data Question' t a = Question
  { _qId      :: Int
  , _qScore   :: Int
  , _qAnswers :: t (Answer a)
  , _qTitle   :: Text
  , _qBody    :: a
  } deriving (Functor)
{% endhighlight %}
Now I can use `Question Text` or `Question Markdown` in all of the above code
snippets. Furthermore, I can add another type synonym specific to the brick
interface to use there:
{% highlight haskell %}
module Interface.Brick
  ( execBrick
  ) where

type BQuestion = Question' (GenericList Name Vector) Markdown
{% endhighlight %}

### Footnotes
[^3]: See [here](https://wiki.haskell.org/Error_vs._Exception) for a more detailed look at the distinction between errors and exceptions, and [here](https://markkarpov.com/tutorial/exceptions.html) for an excellent dive into community opinions on the matter.

[^4]: In my case, `NoResultsError` is still worthwhile as an exception because the application immediately bails on no results. Again, there's no single right way to handle this, but I tend to agree with those reading who think that `NoResultsError` should be classified separately from say `JSONDecodingError`.

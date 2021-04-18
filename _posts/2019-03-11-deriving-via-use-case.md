---
layout: post
title: A small use case for Deriving Via
description: Reducing boilerplate in exception hierarchies via Deriving Via.
tags: [haskell]
redirect_from:
  - /articles/deriving-via.html
  - /posts/deriving-via
---

## Introduction

I am going to show a small but practical scenario for which
[DerivingVia](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/deriving_via.html),
a language extension introduced in GHC 8.6, is a natural solution.
The extension is aptly named, as it extends the customizability of deriving
clauses, namely to allow you to specify that a type should derive a typeclass
instance *via* another type's instance.

As is typical for me as a lowly software engineer that writes Haskell, as opposed to a
computer scientist researching programming language theory, the description of the
language extension initially seemed opaque. In hindsight, I would argue it is one of the
simple-yet-still-incredibly-useful extensions.

## Motivation

The motivating example that I recently came across at work is implementing an exception
hierarchy. In fact, I wasn't even writing a full blown exception hierarchy. All I wanted
to do was have an exception handler that catches all exceptions that have a certain
typeclass `HasHttpStatus` which looks like

{% highlight haskell %}
data HttpStatus = HttpStatus
  { httpStatusCode    :: Int
  , httpStatusMessage :: String
  }

class HasHttpStatus e where
  toHttpStatus :: e -> HttpStatus
{% endhighlight %}

Now if I have some exceptions with natural http statuses, such as

{% highlight haskell %}
data NotFoundError = NotFoundError
  deriving Exception

instance HasHttpStatus NotFoundError
  where toHttpStatus _ = Status 404 "Not Found"

data UnauthorizedError = UnauthorizedError
  deriving Exception

instance HasHttpStatus UnauthorizedError
  where toHttpStatus _ = Status 401 "Unauthorized"
{% endhighlight %}

Then in my route handler, surrounding my application business logic, I want to catch such
errors so I can return the appropriate http status:

{% highlight haskell %}
insertEntityRoute :: IO ()
insertEntityRoute = catchAppErrors $ do
  ...

catchAppErrors :: IO () -> IO ()
catchAppErrors = flip catches
  [ Handler $ \e :: SomeHttpException ->
      sendHttpStatus (toHttpStatus e)
  , Handler $ \e :: SomeException     -> do
      logError ("something awful has happened: " ++ show e)
      sendHttpStatus (Status 500 "Internal Server Error")
  ]

{% endhighlight %}

My first attempt simply mimicked the definition of the `Exception` and `SomeException`
types.

{% highlight haskell %}
data SomeHttpException
  =  forall e. (Exception e, HasHttpStatus e)
  => SomeHttpException e
  deriving Typeable

instance Exception SomeHttpException where

instance Show SomeHttpException where
  showsPrec p (SomeHttpException e) = showsPrec p e

instance HasHttpStatus SomeHttpException where
  toHttpStatus (SomeHttpException e) = toHttpStatus e
{% endhighlight %}

This is on the right track, but I initially (and ignorantly) thought this would be enough
to use my handler above. Of course, nothing was being caught by the `SomeHttpException`
handler. Instead, it became clear after looking in the
[Control.Exception](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#t:Exception)
documentation that I would need to write custom `Exception`
instances for all of my http errors. So my final pull request looked just like what the
documentation advises (and is what is most likely familiar to anyone who has implemented
an exception hierarchy in Haskell):

{% highlight haskell %}
httpExceptionToException
  :: (Exception e, HasHttpStatus e)
  => e
  -> SomeException
httpExceptionToException = toException . SomeHttpException

httpExceptionFromException
  :: Exception e
  => SomeException
  -> Maybe e
httpExceptionFromException e = do
  SomeHttpException x <- fromException e
  cast x

instance Exception NotFoundError where
  toException = httpExceptionToException
  fromException = httpExceptionFromException

instance Exception UnauthorizedError where
  toException = httpExceptionToException
  fromException = httpExceptionFromException
{% endhighlight %}

With these `Exception` instances, my handler defined above works as expected. I didn't
like having to duplicate these instances (in my case there were quite a few more instances
to write), but I trusted that this was the accepted solution since it was in the
documentation and I came up dry googling for better solutions.

## DerivingVia
However, a colleage at work ([**@asivitz**](https://github.com/asivitz) on GitHub, for credit) spotted the
duplicate instances and refactored this by enabling `DerivingVia`. With this approach, we
need only write an `Exception` instance for one newtype wrapper, and then we can auto-derive
the rest of our http error instances via the newtype wrapper:

{% highlight haskell %}
newtype HttpException e = HttpException e

instance Show e => Show (HttpException e) where
  showsPrec p (HttpException e) = showsPrec p e

instance HasHttpStatus e => HasHttpStatus (HttpException e) where
  toHttpStatus (HttpException e) = toHttpStatus e

instance (Show e, HasHttpStatus e) => Exception (HttpException e)
  where
    toException = httpExceptionToException
    fromException = httpExceptionFromException
{% endhighlight %}

Now we can use this `HttpException` wrapper instance in our deriving clauses.
The error types will instead be defined as:

{% highlight haskell %}
data NotFoundError = NotFoundError
  deriving Exception via (HttpException NotFoundError)

data UnauthorizedError = UnauthorizedError
  deriving Exception via (HttpException UnauthorizedError)
{% endhighlight %}

The syntax is wonderfully clear here. It is evident when reading the deriving
clause above that the exception instance for `e` is precisely the one that we
have defined for `HttpException e`.  So our `SomeHttpException` handler will
still catch both of these exceptions, but we get to remove our explicit
typeclass implementations.

## Conclusion
The lesson here is that if you find yourself writing identical typeclass instances over and
over, possibly accumulating a mountain of boilerplate in your codebase: stop, drop, and
derive via.

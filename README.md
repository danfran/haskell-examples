# Haskell Examples

## Arrows

A few simple examples applying arrows to functions and combining them using:

Combinator|	What it does (specialised to (->)) |Alternatives
----------|------------------------------------|------------
(>>>)	|flip |(.)
first	|\f (x, y) -> (f x, y)	|first (Data.Bifunctor)
second	|\f (x, y) -> (x, f y)	|fmap; second (Data.Bifunctor)
(***)	|\f g (x, y) -> (f x, g y)	|bimap (Data.Bifunctor)
(&&&)	|\f g x -> (f x, g x)	|liftA2 (,) (Control.Applicative)
left	|Maps over Left case.	|first (Data.Bifunctor)
right	|Maps over Right case.	|fmap; second (Data.Bifunctor)
(+++)	|Maps over both cases.	|bimap (Data.Bifunctor)
(&#124;&#124;&#124;)	|Eliminates Either.	|either (Data.Either)
app	|\(f, x) -> f x	uncurry |($)


## Xkcd Downloader

Functional Reactive Programming example with Reactive Banana to download feeds from Xkcd.com.


## Twitter Trends

OAuth authentication for the Twitter's API to download latests trends. It requires 5 parameters. You can find more info how to get them [here](https://dev.twitter.com/rest/reference/get/trends/place).


## Hangman

Classical game created using a State Monad (StateT). The words are loaded from a file that contains a list of english words.

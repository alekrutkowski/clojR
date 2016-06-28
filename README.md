Some Clojure-inspired functions (macros) in R
================
Aleksander Rutkowski
2016-06-28

[Clojure](https://clojure.org/) is a modern [Lisp](https://en.wikipedia.org/wiki/Lisp_%28programming_language%29) dialect. Due to its Lisp/[Scheme](https://en.wikipedia.org/wiki/Scheme_%28programming_language%29) roots, R is somewhat similar (functional programming with immutable data; macros a.k.a. non-standard evaluation a.k.a. code as data a.k.a. homoiconicity), but lacks some useful and terse language constructs. This is an attempt to implement them in R.

The implemented functions (macros):

-   `cond` and related -- see <https://clojuredocs.org/clojure.core/cond>
-   `case` and related -- see <https://clojuredocs.org/clojure.core/case>
-   `as->` -- see <https://clojuredocs.org/clojure.core/as-%3E>
-   `->` -- see <https://clojuredocs.org/clojure.core/-%3E>
-   `->>` -- see <https://clojuredocs.org/clojure.core/-%3E%3E>
-   `doc` -- see <https://clojuredocs.org/clojure.repl/doc>

New functions (macros) may be added gradually in the future.

### Installation

``` r
devtools::install_github('alekrutkowski/clojR')
```

### Demo

``` r
library(clojR)
```

#### `cond` and related

``` r
# Also vectorised `condv` is available (based on base::ifelse).
# Example based on the first example from
# https://clojuredocs.org/clojure.core/cond
# Transformed to:
# if (n < 0) "negative" else if (n > 0) "positive" else "zero"
`pos-neg-or-zero` <- function(n)
                     cond(n < 0, 'negative',
                          n > 0, 'positive',
                                 'zero')
`pos-neg-or-zero`(5)
```

    ## [1] "positive"

``` r
`pos-neg-or-zero`(-1)
```

    ## [1] "negative"

``` r
`pos-neg-or-zero`(0)
```

    ## [1] "zero"

``` r
# condv(n < 0, 'negative',
#       n > 0, 'positive',
#              'zero')
# would transform to:
# ifelse(n < 0, "negative", ifelse(n > 0, "positive", "zero"))
```

#### `case` and related

``` r
# More powerful than base::switch
# not only letters or integers can be used as keys:
f <- base::mean
case(f,
     stats::median, 1,
     base::mean,    2,
                    3)
```

    ## [1] 2

``` r
# Different vectorised variants of case
# (using `ifelse` instead of `if` under the hood):

x <- c(1/0, as.numeric(NA), NaN, 0)

# `casevid`
# based on vectorised base::identical
casevid(x,
        Inf,            "infinity",
        as.numeric(NA), "not available",
                        "other")
```

    ## [1] "infinity"      "not available" "other"         "other"

``` r
# `caseveq`
# based on `==` (equality)
# less general than `casevid` but likely faster
caseveq(x,
        Inf,            "infinity",
        as.numeric(NA), "not available",
                        "other")
```

    ## [1] "infinity" NA         NA         NA

#### Threading (the same role as the pipe operators)

``` r
# Possible alternatives to pipe operators
# like %>% in package `magrittr`

# `Thread-first` -- transformed to:
# sum(3 - 400, 7, 8)
`->`(3,
     `-`(400),
     sum(7,8))
```

    ## [1] -382

``` r
# `Thread-last` -- transformed to:
# sum(7, 8, 400 - 3)
`->>`(3,
     `-`(400),
     sum(7,8))
```

    ## [1] 412

``` r
# `Thread-as` -- transformed to:
# sum(c(mean(c(1, 1000)), 5)) - 13
`as->`(1000, nn,
       mean(c(1,nn)),
       sum(c(nn,5)),
       `-`(nn,13))
```

    ## [1] 492.5

#### [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)-related

``` r
# Extraction of inline "docstrings":
# an informal documentation, e.g. for
# functions not in a package.
# Use the double hash character (##)
# for the comments to be detected by `doc`.
fff <- function(x = 1) {
    ## This is my function:
    ## argument x, default 1.
    ## returns x + 10.
    x + 10
}
doc(fff)
```

    ##  This is my function:
    ##  argument x, default 1.
    ##  returns x + 10.

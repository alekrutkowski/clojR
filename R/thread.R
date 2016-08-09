#' Threading a value, bound to a symbol, through expressions
#'
#' See \url{https://clojuredocs.org/clojure.core/as->}.
#' @param value An initial value.
#' @param symbol An unquoted symbol name which will be replaced
#' by the values returned by the previous expressions.
#' @param ... The expressions, most likely including the
#' symbol defined in \code{symbol} (possibly nested).
#' @examples
#' `as->`(1000, nn,
#'        mean(c(1,nn)),
#'        sum(c(nn,5)),
#'        `-`(nn,13))
#' # Is transformed to:
#' # sum(c(mean(c(1, 1000)), 5)) - 13
#' # an evaluates to 492.5.
#' @rdname thread-as
#' @export
`as->` <- function(value, symbol, ...)
    threadFactory(ENV=parent.frame(), substiSym, substitute(value), substitute(symbol), NULL, ...)

#' Threading a value, as the first argument, through expressions
#'
#' See \url{https://clojuredocs.org/clojure.core/->}.
#' @param value An initial value.
#' @param ... The expressions.
#' @examples
#' `->`(3,
#'      `-`(400),
#'      sum(7,8))
#' # Is transformed to:
#' # sum(3 - 400, 7, 8)
#' # an evaluates to -382.
#' @rdname thread-first
#' @export
`->` <- function(value, ...)
    threadFactory(ENV=parent.frame(), substiPos, substitute(value), NULL, insertFirst, ...)

#' Threading a value, as the last argument, through expressions
#'
#' See \url{https://clojuredocs.org/clojure.core/->>}.
#' @param value An initial value.
#' @param ... The expressions.
#' @examples
#' `->>`(3,
#'      `-`(400),
#'      sum(7,8))
#' # Is transformed to:
#' # sum(7, 8, 400 - 3)
#' # an evaluates to 412.
#' @rdname thread-last
#' @export
`->>` <- function(value, ...)
    threadFactory(ENV=parent.frame(), substiPos, substitute(value), NULL, insertLast, ...)


threadFactory <- function(ENV, substiFUN, value, symbol, insertFUN, ...)
    substitute(list(...)) %>%
        as.list %>%
        tail(-1) %>% # %T>% {(.) %>% lapply(as.list) %>% str} %>%
        substiFUN(value, symbol, insertFUN) %>%
        eval(ENV)


# For `as->` --------------------------------------------------------------

substiSym <- function(listL, value, symbol, ._)
    listL %>%
    Reduce(function(a,b)
        recurSubsti(b, a, symbol),
        x=.,
        init=value)

recurSubsti <- function(b, a, symbol)
    b %>%
    as.list %>%
    lapply(function(el)
        `if`(identical(el, symbol),
             a,
             `if`(length(el)>1,
                  recurSubsti(el, a, symbol),
                  el))) %>%
    as.call


# For `->` and `->>` ------------------------------------------------------

substiPos <- function(listL, value, ._, insertFUN)
    listL %>%
    Reduce(function(a,b)
        b %>%
            callIfLambda %>%
            as.list %>%
            insertFUN(a) %>%
            as.call,
        x=.,
        init=value)

insertFirst <- function(List, val)
    c(head(List,1),
      val,
      tail(List,-1))

insertLast <- function(List, val)
    c(List,
      val)

isLambda <- function(qexpr)
    qexpr %>%
    as.list %>%
    extract2(1) %>%
    equals(quote(`function`))

callIfLambda <- function(qexpr)
    `if`(qexpr %>% isLambda,
         qexpr %>%
             as.expression %>%
             as.call,
         qexpr)

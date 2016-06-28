iden <- Vectorize(identical, vectorize.args=c('x','y'), SIMPLIFY=TRUE)

#' Equivalent of nested if ... else ... calls
#'
#' See \url{https://clojuredocs.org/clojure.core/cond}.
#' @param ... An odd number of expressions. A set of test/expression
#' pairs plus an expression to be evalauted if all tests fail. Equivalent
#' of nested \code{if ... else ...} calls.
#' @examples
#' cond(x < 5, 'a',
#'      x < 2, 'b',
#'      x > 1, 'c',
#'             'd')
#' # Is transformed to:
#' # if (x < 5) "a" else
#' #     if (x < 2) "b" else
#' #         if (x > 1) "c" else "d"
#' @export
cond <- function(...)
    cond_Factory(ENV=parent.frame(), vectorised=FALSE, ...)

#' Equivalent of nested ifelse calls (vectorised clojR::cond)
#'
#' See \url{https://clojuredocs.org/clojure.core/cond}.
#' @param ... An odd number of expressions. A set of test/expression
#' pairs plus an expression to be evalauted if all tests fail. Equivalent
#' of nested \code{\link[base]{ifelse}} calls.
#' @examples
#' condv(x < 5, 'a',
#'       x < 2, 'b',
#'       x > 1, 'c',
#'              'd')
#' # Is transformed to:
#' # ifelse(x < 5, "a",
#' #        ifelse(x < 2, "b",
#' #               ifelse(x > 1, "c",
#' #                      "d")))
#' @export
condv <- function(...)
    cond_Factory(ENV=parent.frame(), vectorised=TRUE, ...)

#' A powerful replacement for base::switch
#'
#' See \url{https://clojuredocs.org/clojure.core/case}.
#' @param obj An object/value.
#' @param ... An odd number of expressions. A set of test/expression
#' pairs plus an expression to be evalauted if all tests fail. Equivalent
#' of nested \code{if (identical(obj, a)) b else c} calls.
#' @examples
#' case(x,
#'      list(1,2), "abc",
#'      c('a','b'), 123,
#'      NULL, "oh no!",
#'      "something else")
#' # Is transformed to:
#' # if (identical(x, list(1, 2))) "abc" else
#' #     if (identical(x, c("a","b"))) 123 else
#' #         if (identical(x, NULL)) "oh no!" else
#' #             "something else"
#' @export
case <- function(obj, ...)
    case_Factory(ENV=parent.frame(), obj=obj, comparFun=quote(identical),
                 vectorised=FALSE, ...)

#' A vectorised version of clojR::case (with vectorised identity function)
#'
#' See \url{https://clojuredocs.org/clojure.core/case}.
#' @param obj An object/value.
#' @param ... An odd number of expressions. A set of test/expression
#' pairs plus an expression to be evalauted if all tests fail. Equivalent
#' of nested \code{ifelse(iden(x,a), b, c)} calls, where \code{x} are elements
#' of the \code{obj} vector and iden is a vectorised base::identity function.
#' @examples
#'  casevid(x,
#'           Inf,            "infinity",
#'           as.numeric(NA), "not available",
#'                           "other")
#' # Is transformed to:
#' # ifelse(iden(x, Inf), "infinity",
#' #        ifelse(iden(x, as.numeric(NA)), "not available",
#' #               "other"))
#' @export
casevid <- function(obj, ...)
    case_Factory(ENV=parent.frame(), obj=obj, comparFun=quote(iden),
                 vectorised=TRUE, ...)

#' A vectorised version of clojR::case (with `==` function)
#'
#' See \url{https://clojuredocs.org/clojure.core/case}.
#' @param obj An object/value.
#' @param ... An odd number of expressions. A set of test/expression
#' pairs plus an expression to be evalauted if all tests fail. Equivalent
#' of nested \code{ifelse(x == a, b, c)} calls, where \code{x} are elements
#' of the \code{obj} vector.
#' @examples
#'  caseveq(x,
#'           Inf,            "infinity",
#'           as.numeric(NA), "not available",
#'                           "other")
#' # Is transformed to:
#' # ifelse(x == Inf, "infinity",
#' #        ifelse(x == as.numeric(NA), "not available",
#' #                                     "other"))
#' @export
caseveq <- function(obj, ...)
    case_Factory(ENV=parent.frame(), obj=obj, comparFun=quote(`==`),
                 vectorised=TRUE, ...)

cond_case_Factory <- function(ENV, obj, message_infix, vectorised, comparFun, ...)  {
    IF <- if (vectorised)
        quote(ifelse) else quote(`if`)
    substitute(list(...)) %>%
        as.list %T>%
        {if (length(.) < 4)
            stop(paste('\ncase requires',
                       message_infix,
                       'at least 3 other arguments!'),
                 call.=FALSE)} %>%
        tail(-1) %T>%
        {if (length(.) %% 2 != 1)
            stop(paste('\ncase requires',
                       message_infix,
                       'an uneven number of arguments!'),
                 call.=FALSE)} %>%
        split(((seq_along(.) + 1)/2) %>%
                  floor) %>%
        rev %>%
        {c(.[[1]], tail(., -1))} %>%
        Reduce(function(x,y)
            `if`(comparFun %>% is.null,
                 bquote(.(IF)(.(y[[1]]),
                              .(y[[2]]), .(x))),
                 bquote(.(IF)(.(comparFun)(.(obj),.(y[[1]])),
                              .(y[[2]]), .(x)))),
            .) %>%
        eval(ENV)
}

cond_Factory <- function(ENV, vectorised, ...)
    cond_case_Factory(ENV, obj=NULL, message_infix="", comparFun=NULL,
                      vectorised=vectorised, ...)

case_Factory <- function(ENV, obj, comparFun, vectorised, ...)
    cond_case_Factory(ENV, obj=obj, message_infix="an object plus",
                      comparFun=comparFun,
                      vectorised=vectorised, ...)




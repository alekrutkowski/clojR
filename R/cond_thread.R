#' Conditionally evaluate expressions and thread them
#'
#' See \url{https://clojuredocs.org/clojure.core/cond->}. NOTE:
#' This R version is more powerful (general) than the Clojure version as it is threading the
#' \code{obj} value not only through the right-side expresions but also
#' the conditions/tests (while in Clojure these are static conditions).
#' @param obj An object/value.
#' @param ... An even number of expressions. A set of test/expression pairs.
#' @examples
#' # Adapted from the first example at https://clojuredocs.org/clojure.core/cond->
#'
#' `cond->`(1,               # we start with 1
#'          `==`(1), `+`(1), # 1==1 is true so 1+1 is evaluated and yields 2 which is threaded further
#'          `<`(0), `*`(42), # 2<0 is false so the operation is skipped
#'          `==`(2), `*`(3)) # 2==2 is true so 2*3 is evaluated and it finally yields 6
#'
#' # A version closer in spirit to the Clojure example -- the constants need to be wrapped
#' # in anonymous functions or in a function which ignores its first argument
#' # (see `constant` below):
#'
#' `cond->`(1,                          # we start with 1
#'          function(x) TRUE, `+`(1),   # the condition is true so 1+1 yields 2
#'          function(x) FALSE, `*`(42), # the condition is false so the operation is skipped
#'          function(x) 2==2, `*`(3))   # 2==2 so it yields 6
#'
#' constant <- function(ignore_me, v) v
#'
#' `cond->`(1,                        # we start with 1
#'          constant(TRUE), `+`(1),   # the condition is true so 1+1 yields 2
#'          constant(FALSE), `*`(42), # the condition is false so the operation is skipped
#'          constant(2==2), `*`(3))   # 2==2 so it yields 6
#' @rdname cond-thread
#' @export
`cond->` <- function(obj, ...)
    `cond->_Factory`(ENV=parent.frame(), obj, vectorised=FALSE, ...)

#' A vectorised version of clojR::`cond->`
#'
#' See \code{\link[clojR]{`cond->`}}. This function (\code{`condv->`})
#' uses internally \code{ifelse} instead of \code{if}.
#' @param obj An object/value.
#' @param ... An even number of expressions. A set of test/expression pairs.
#' @rdname condv-thread
#' @export
`condv->` <- function(obj, ...)
    `cond->_Factory`(ENV=parent.frame(), obj, vectorised=TRUE, ...)

`cond->_Factory` <- function(ENV, obj, vectorised, ...) {
    IF <- if (vectorised)
        quote(ifelse) else quote(`if`)
    Reduce(function(x,y)
        bquote(.(IF)(clojR::`->`(.(x), .(y[[1]])),
                     clojR::`->`(.(x), .(y[[2]])),
                    .(x))),
        x = substitute(list(...)) %>%
            as.list %>%
            tail(-1) %T>%
            {if (length(.) < 2)
                stop(paste('\n`cond->` requires',
                           'an object plus',
                           'at least 2 other arguments!'),
                     call.=FALSE)
            } %T>%
            {if (length(.) %% 2 == 1)
                stop(paste('\n`cond->` requires',
                           'an object plus',
                           'an even number of other arguments!'),
                     call.=FALSE)} %>%
            split(seq_len(length(.)/2) %>%
                      rep.int(2) %>%
                      sort),
        init = obj) %>%
        eval(envir = ENV)
}

# # Tests:
# hasName_nn <- function(zz) 'nn' %in% names(zz)
#     (function(aa) `cond->`(aa,
#                            hasName_nn, dplyr::mutate(mm=nn+1),
#                            is.null, cbind(data.frame(nn=100,mm=102))))(data.frame(nn=0))
#     (function(aa) `cond->`(aa,
#                            function(zz) 'nn' %in% names(zz), dplyr::mutate(mm=nn+1),
#                            is.null, cbind(data.frame(nn=100,mm=102))))(data.frame(nn=0))

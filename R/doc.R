#' @import magrittr
NULL

#' Display in-body function documentation (a "docstring")
#'
#' See \url{https://clojuredocs.org/clojure.repl/doc}.
#' The "docstring" is extracted from the comments starting
#' with a double hash character (##).
#' @param f A function.
#' @examples
#' fff <- function(x = 1) {
#'     ## This is my function:
#'     ## argument x, default 1.
#'     ## returns x + 10.
#'     x + 10
#' }
#' doc(fff)
#' # displays in the console:
#'
#' # This is my function:
#' # argument x, default 1.
#' # returns x + 10
#' @export
doc <- function(f)
    capture.output(f %>% print) %>%
    Filter(function(x)
        grepl('^.*##',x),.) %>%
    gsub('^.*##',"",.) %>%
    cat(sep='\n')

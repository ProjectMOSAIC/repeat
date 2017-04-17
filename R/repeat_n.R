#' Replication for randomization and resampling
#'
#' Replication for randomization and resampling
#'
#' @param expr and expression to be repeated
#' @param n an integer number of times to repeat
#' @param cull a function for culling the results of each evaluation of expr
#' @param names names for columns of the returned object
#' @param ... additional arguments (currently ignored)
#' @return a list or data frame containing the (culled) restuls of the
#'   repetition
#' @export
#' @importFrom rlang quo
#' @importFrom dplyr bind_rows

repeat_n <- function(expr, n = 1,
                     cull = cull_for_repeat, combine = combine_for_repeat,
                     names = NULL, ...) {
  qexpr <- rlang::quo(expr)
  results <- if("package:parallel" %in% search() ) {
    parallel::mclapply( integer(n), function(...) { cull(rlang::eval_tidy(qexpr)) } )
  } else {
    lapply( integer(n), function(...) { cull(rlang::eval_tidy(qexpr)) } )
  }
  combine(results)
}

#' @export
cull_for_repeat <- function(object, ...) {
  UseMethod("cull_for_repeat")
}

#' @export
cull_for_repeat.default <- function(object, ...) {
  object
}

#' @export
cull_for_repeat.cointoss <- function(object, ...) {
  return( data.frame(n=attr(object,'n'),
                     heads=sum(attr(object,'sequence')=='H'),
                     tails=sum(attr(object,'sequence')=='T'),
                     prop=sum(attr(object,'sequence')=="H") / attr(object,'n')
  ) )
}

#' @export
combine_for_repeat <- function(object, ...) {
  UseMethod("combine_for_repeat")
}

#' @export
combine_for_repeat.default <- function(object, ...) {
  object
}

#' @export
combine_for_repeat.list <- function(object, ...) {
  dplyr::bind_rows(object)
}

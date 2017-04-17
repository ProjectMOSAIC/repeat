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
                     cull = "default", combine = "default",
                     names = NULL, ...) {
  if (is.character(cull) && cull == "default") {
    cull <- function(x) x
  }
  if (is.character(combine) && combine == "default") {
    combine <- function(x) dplyr::bind_rows(x)
  }
  qexpr <- rlang::quo(expr)
  results <- lapply( integer(n), function(...) { rlang::eval_tidy(qexpr) } )
  results <- lapply(results, cull)
  combine(results)
}
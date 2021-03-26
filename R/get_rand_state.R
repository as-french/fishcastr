#' Extract random state of global environment
#'
#' This function extracts the current random state 'seed' of the global
#' environment and is used to store the random state before a simulation before
#' restoring it with set_rand_state after the simulation. (code from
#' https://www.r-bloggers.com/2019/08/local-randomness-in-r/).
#'
#' @return The random seed of the global environment
#' @export
get_rand_state <- function() {
  # Using `get0()` here to have `NULL` output in case object doesn't exist.
  # Also using `inherits = FALSE` to get value exactly from global environment
  # and not from one of its parent.
  get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
}

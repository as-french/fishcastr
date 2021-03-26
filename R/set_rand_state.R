#' Restore the random state of global environment
#'
#' This function restores a previously extracted random state 'seed' of the
#' global environment.(code from
#' https://www.r-bloggers.com/2019/08/local-randomness-in-r/).
#'
#' @param state The random seed of the global environment
#' @return The random seed of the global environment
#' @export
set_rand_state <- function(state) {
  # Assigning `NULL` state might lead to unwanted consequences
  if (!is.null(state)) {
    assign(".Random.seed", state, envir = .GlobalEnv, inherits = FALSE)
  }
}

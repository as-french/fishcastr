#' Extract random state of global environment
#'
#' @description This function extracts the current random state 'seed' of the
#'   global environment and is used to store the random state before a
#'   simulation before restoring it with set_rand_state after the simulation.
#'   Code attributed to Evgeni Chasnovski and accessed at
#'   https://www.r-bloggers.com/2019/08/local-randomness-in-r/. Code was
#'   originally posted by Evgeni Chasnovski at
#'   http://www.questionflow.org/2019/08/13/local-randomness-in-r/ under
#'   creative commons  (CC-BY-SA 4.0;
#'   https://creativecommons.org/licenses/by-sa/4.0/).
#'
#' @return The random seed of the global environment
#' @export
get_rand_state <- function() {
  # Using `get0()` here to have `NULL` output in case object doesn't exist.
  # Also using `inherits = FALSE` to get value exactly from global environment
  # and not from one of its parent.
  get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
}

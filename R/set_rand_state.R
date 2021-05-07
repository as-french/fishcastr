#' Restore the random state of global environment
#'
#' @description This function restores a previously extracted random state
#'   'seed' of the global environment.  Code attributed to Evgeni Chasnovski and
#'   accessed at https://www.r-bloggers.com/2019/08/local-randomness-in-r/. Code
#'   was originally posted by Evgeni Chasnovski at
#'   http://www.questionflow.org/2019/08/13/local-randomness-in-r/ under
#'   creative commons  (CC-BY-SA 4.0;
#'   https://creativecommons.org/licenses/by-sa/4.0/).
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

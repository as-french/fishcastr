#' Calculate the bootstrap percentile intervals of a selected summary statistic
#' (e.g., median)
#'
#' @description This function takes a vector of values and calculates a
#'   bootstrap percentile or multiple percentiles of a summary statistic.
#'
#' @param x A vector of more than one element.
#' @param n An integer of number of boot resamples.
#' @param average_FUN A summary statistic (e.g., median).
#' @param percentile A value or concatenated list of values bounded by 0 and 1;
#'   can calculate multiple percentiles at one time (e.g., c(0.025,0.975)).
#' @param parallel Not implemented. Experimental.
#' @return A user specified percentile or concatenated list of percentiles.
#' @examples
#' \dontrun{
#'  data_counts <-
#'    readRDS(paste0(
#'     system.file("vignettes", package = "fishcastr"),
#'     "/vignette_data/data_ssmolt_enviro_1981_2019.rds"
#'   ))
#'
#'  data_counts_subset <- data_counts[data_counts$salmonid_year %in% c(1981:2018),]
#'
#' mean_terciles <- round(calculate_terciles(bio_year = data_counts_subset$salmonid_year,
#'                                           counts = data_counts_subset$ssmolt,
#'                                           yday = data_counts_subset$salmonid_yday,
#'                                           terciles = c(1/3,2/3))[["CI95_se_terciles"]][1,])
#' #print(mean_terciles)
#' tercile_data <- terciles_plot_data(unreliable_years = c(2000,2004,2008),
#'                              bio_year = data_counts_subset$salmonid_year,
#'                              counts = data_counts_subset$ssmolt,
#'                              yday = data_counts_subset$salmonid_yday,
#'                              terciles = c(1/3,2/3),
#'                              plot = FALSE,
#'                              mean_tercile_dates = mean_terciles)
#' #print(tercile_data)
#' upper_CI_boot <- apply(X = tercile_data,
#'                        MARGIN = 2,
#'                        FUN = boot_average_CI,
#'                        n = 999,
#'                        average_FUN = median,
#'                        percentile = c(0.025,0.975)) # upper CI
#' #print(upper_CI_boot)
#' }
#' @export
boot_average_CI <- function(x, n, average_FUN, percentile, parallel = FALSE){
  old_state <- get_rand_state()
  on.exit(set_rand_state(old_state))
  na_rem <- TRUE
  # resample x and calculate average_type "n" times,
#  suppressMessages(require(boot))
  #  ave <- vector(length = n)
  #  for(i in 1:n){
  #    set.seed(i)
  #    ave[i] <- average_FUN(sample(x = x, size = length(x), replace = TRUE), na.rm = na_rem)
  #}

  #  ###
  ave_list <- lapply(1:n, function(z){
    #if(parallel == FALSE){
    set.seed(z)
    #}
    res <- average_FUN(base::sample(x = x, size = length(x), replace = TRUE), na.rm = na_rem)
  })
  ave <- do.call(c, ave_list)
  ###
  #ave
  # extract percentiles
  result <- quantile(ave, probs = percentile)
  #  result
  return(result)
}

#' Function summary_stats
#'
#' @param df a data frame of charges associated with hospital care,
#' labeled by Diagnosis-Related Groups (DRG) and containing a column labeled
#' "Average.Medicare.Payments"
#' @param stat a metric to be calculated for the "Average.Medicare.Payments"
#' column of the given data frame by Diagnosis-Related Group. Accepts argument
#' of 'mean', 'median' or 'stdev'.
#'
#' @return the mean, median, or standard deviation of "Average.Medicare.Payments"
#' @export
#'
#' @examples
#' drg = data(DRG_data.csv)
#' summary_stats(drg, 'stdev')

library(dplyr)

summary_stats = function(df, stat) {
  # Calculate mean, median, and standard deviation of Average Medicare Payments
  # column of a data frame by DRG
  stats = df %>%
    group_by(DRG.Definition) %>%
    summarize(mean = mean(Average.Medicare.Payments),
              median = median(Average.Medicare.Payments),
              stdev = sd(Average.Medicare.Payments))
  # Return only specified metric
  if (stat == 'mean'){
    stats %>% select(c(DRG.Definition, mean))
  }
  else if (stat == 'median'){
    stats %>% select(c(DRG.Definition, median))
  }
  else if (stat == 'stdev') {
    stats %>% select(c(DRG.Definition, stdev))
  }
}

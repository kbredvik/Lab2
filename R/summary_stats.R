#' Function summary_stats
#'
#' @param df a data frame of charges associated with hospital care, including a
#' column labeled "DRG.Definition" that lists the Diagnosis-Related Group (DRG)
#' for each charge, and a column labeled "Average.Medicare.Payments"
#' @param stat a metric to be calculated for the "Average.Medicare.Payments"
#' column of the given data frame by Diagnosis-Related Group. Accepts argument
#' of 'mean', 'median' or 'stdev'.
#'
#' @return a data frame containing one column of Diagnosis-Related Group labels
#' and one column containing either the mean, median, or standard deviation
#' of the input variable "Average.Medicare.Payments", to 3 decimal places.
#' @export
#'
#' @examples
#' Example data set retrieved from:
#' https://data.cms.gov/Medicare-Inpatient/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3
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
    stats %>% select(DRG.Definition, mean)
  }
  else if (stat == 'median'){
    stats %>% select(DRG.Definition, median)
  }
  else if (stat == 'stdev') {
    stats %>% select(DRG.Definition, stdev)
  }
  # Round to 3 decimal places
  stats = mutate_if(stats,
                    is.numeric,
                    round,
                    digits = 3)
  return(stats)
}

#' Function plot_charges
#'
#' @param df a data frame of charges associated with hospital care,
#' labeled by Diagnosis-Related Groups (DRG)
#' @param payment_group a category of hospital-associated charges or payments
#'
#' @return a ggplot boxplot showing medical charges or payments made vs.
#' Diagnosis-Related Group (DRG), with medical charges on a logarithmic scale
#' (base 10).
#' @export
#'
#' @examples
#' drg = data(DRG_data.csv)
#' plot_charges(drg, 'Average.Covered.Charges')

library(ggplot2)
library(dplyr)

plot_charges = function(df, payment_group) {
  toplot = df %>%
    # Group data by DRG code
    group_by(DRG.Definition) %>%
    ggplot(aes(x = DRG.Definition,
               y = gsub('\\.', ' ', get(payment_group)))) +
    # Create boxplot of payments or charges vs. DRG code
    geom_boxplot() +
    # Add graph title
    ggtitle(paste(toString(payment_group))) +
    # Change y axis to logarithmic scale
    scale_y_continuous(trans = 'log10') +
    # Add axis labels
    ylab(toString(gsub('\\.', ' ', get(payment_group)))) +
    xlab('Diagnosis-Related Group Codes')
}

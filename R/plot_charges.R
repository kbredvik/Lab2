#' Function plot_charges
#'
#' @param df a data frame of charges associated with hospital care, with
#' a column labeled "DRG.Definition" containing Diagnosis-Related Groups (DRG).
#' @param payment_group a category of hospital-associated charges or payments,
#' corresponding to the name of a column in the data frame provided by the user.
#'
#' @return a ggplot boxplot showing medical charges or payments made vs.
#' Diagnosis-Related Group (DRG), with medical charges on a logarithmic scale
#' (base 10).
#' @export
#'
#' @examples
#' Example data set retrieved from:
#' https://data.cms.gov/Medicare-Inpatient/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3
#' drg = data(DRG_data.csv)
#' plot_charges(drg, 'Average.Covered.Charges')

library(ggplot2)
library(dplyr)

plot_charges = function(df, payment_group) {
  # Initialize payment group variable
  payment_group = payment_group
  toplot = df %>%
    # Group data by DRG code
    group_by(DRG.Definition) %>%
    # Change y axis to logarithmic scale
    ggplot(aes(x = DRG.Definition,
               y = log10(get(payment_group)))) +
    # Create boxplot of payments or charges vs. DRG code
    geom_boxplot(outlier.size = 0.5) +
    # Add graph title
    ggtitle(paste(toString(gsub('\\.',
                                ' ',
                                payment_group)))) +
    # Flip x and y axes
    coord_flip() +
    # Reformat y axis (DRG codes)
    theme(axis.text.y = element_text(size = 3),
          axis.ticks.y = element_blank()) +
    # Add axis labels
    ylab(paste0(toString(gsub('\\.',
                       ' ',
                       payment_group)),
         ", log($)")) +
    xlab('Diagnosis-Related Group (DRG) Code')
  toplot
}


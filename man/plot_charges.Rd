% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_charges.R
\name{plot_charges}
\alias{plot_charges}
\title{Plot Charges}
\usage{
plot_charges(df, payment_group)
}
\arguments{
\item{df}{a data frame of charges associated with hospital care, with
a column labeled "DRG.Definition" containing Diagnosis-Related Groups (DRG).}

\item{payment_group}{a category of hospital-associated charges or payments,
corresponding to the name of a column in the data frame provided by the user.}
}
\value{
a ggplot boxplot showing medical charges or payments made vs.
Diagnosis-Related Group (DRG), with medical charges on a logarithmic scale.
}
\description{
Box plot of log(medical charges) by DRG.
}
\examples{
Example data set retrieved from:
https://data.cms.gov/Medicare-Inpatient/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3
drg = data(DRG_data.csv)
plot_charges(drg, 'Average.Covered.Charges')
}

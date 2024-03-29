% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/incident_sampling.R
\name{sample_mod_id}
\alias{sample_mod_id}
\title{Sample specific incident data from module assignment data}
\usage{
sample_mod_id(inc_id_template, mod_id_data)
}
\arguments{
\item{inc_id_template}{inc_id data frame that has been sampled for the disired
incident; output from \code{sample_inc_id()}}

\item{mod_id_data}{seasonal module id assignment data}
}
\value{
data frame; same dimensions and columns/rows as the inc_id_template
}
\description{
Sample specific incident data from module assignment data
}
\examples{
\dontrun{
inc_id_highline <- sample_inc_id(inc_id_2017, inc_info_2017, 'HIGHLINE')
mod_id_highline <- sample_mod_id(inc_id_highline, mod_id_2017)
}
}

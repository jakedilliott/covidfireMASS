% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agent_fxns.R
\name{vaccinate}
\alias{vaccinate}
\title{Find out which agents to vaccinate and assign immunity}
\usage{
vaccinate(input_df, vax_efficacy = 0.95, vax_df = NULL)
}
\arguments{
\item{input_df}{data frame containing agent roster}

\item{vax_efficacy}{proportion of vaccinated agents the gain immunity}

\item{vax_df}{1 row data frame, colnames are res_gaccs, row values are the total number of vaccinated agents after vaccination for that time step. if the vax_df cell for gacc NM-SWC is 100 and 50 agents are already vaccinated, this function will return 50 ids to vaccinate}
}
\value{
nested list of res_ids; output$vaccinated and output$immune
}
\description{
Find out which agents to vaccinate and assign immunity
}
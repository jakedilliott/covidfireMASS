
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covidfireMASS

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/327392364.svg)](https://zenodo.org/badge/latestdoi/327392364)
[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-%602.10%60-6666ff.svg)](https://cran.r-project.org/)
<!-- badges: end -->

## Covid Fire Multi Agent Seasonal Simulation

An Agent-Based Model that simulates COVID dynamics within wildfire
firefighter camps. This model assumes a hub-and-spoke camp model, which
implements module based infection dynamics and quarantine protocols.
This work is an extension of the Susceptible Infected Recovered(SIR)
model, adding Exposed, Asymptomatic, and Quarantined agent health
states. Hence the SEIRAQ model.

## Installation

You can install the development version of covidfireMASS from
[GitHub](https//:github.com) with:

``` r
if (!require("remotes", character.only = TRUE)) {
  install.packages("remotes", dependencies = TRUE)
}
remotes::install_github("jakedilliott/covidfireMASS")
```

## Necessary Data

### Incident Assignments

-   Each row is a unique fire fighter/resource
-   res_id: numeric, resource ID number
-   res_gacc: character, resource home GACC (Geographic Area
    Coordination Center)
-   Each column to the right of the second column is labeled with a date
    and each cell contains an incident that shows which fire each
    resource was assigned to on that day. On days where a resource was
    not on a fire inc_id needs to be 0.

Example: Resource 1001 was on incident 1046 from 01/01/2020 - 01/01/2020

| res_id | res_gacc | X2020.01.01 | X2020.01.02 | X2020.01.03 | …   |
|-------:|:---------|------------:|------------:|------------:|:----|
|   1001 | NM-SWC   |        1046 |        1046 |        1046 | …   |
|   1002 | OR-NWC   |        1055 |        1055 |           0 | …   |
|   1003 | OR-NWC   |           0 |           0 |        1055 | …   |

Incident Assignments Table

### Module Assignments

-   res_id and res_gacc columns here are the same as the module
    assignments data
-   Each column to the right of the second column is labeled with a date
    and each cell contains a module ID that describes which module (team
    or crew) each resource was assigned to each day. On days where a
    resource is not on a fire mod_id needs to be 0. These module ID’s
    are only unique when paired with the incident of each agent on that
    day, there are modules with the same ID across multiple fires.

Example: Resource 1001 was on module E-1 from 01/01/2020 - 01/03/2020

| res_id | res_gacc | X2020.01.01 | X2020.01.02 | X2020.01.03 | …   |
|-------:|:---------|:------------|:------------|:------------|:----|
|   1001 | NM-SWC   | E-1         | E-1         | E-1         | …   |
|   1002 | OR-NWC   | O-12        | O-12        | 0           | …   |
|   1003 | OR-NWC   | 0           | 0           | C-8         | …   |

Module Assignments Table

### Incident Information

inc_id \| inc_number \| inc_name \| inc_gacc \| inc_lat \| inc_lon \|
max_team_type \| first_day \| last_day \|

## Minimal Example

``` r
library(covidfireMASS)

# Import Data
# This example uses a '.rda' file that includes preconfigured input data
load("sim_inputs_2017_complete.rda")

# Running simulations
# Default simulation run, see ?seasonal_sim for default inputs
sim1 <- seasonal_sim(inc_id_2017, mod_id_2017, inc_info_2017, overhead_ids_2017,
                     vax_df = vax_plan_base_2017)

# seasonal_sim outputs a data frame with the following columns when the 
# `raw` option is TRUE:
#
# res_id     res_gacc     inc_id     mod_id        leader    state
# "numeric"  "character"  "numeric"  "character"  "logical"  "character"
#
# quarantine  q_days     vaccinated  vax_rate   time 
# "logical"   "numeric"  "logical"   "numeric"  "numeric"

# If the `raw` options is set to FALSE (the default), a summarised
# output in a wider format will be returned
```

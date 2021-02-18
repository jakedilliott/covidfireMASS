
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covidfireMASS

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
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
devtools::install_github("jakedilliott/covidfireMASS")
```

## Necessary Data

### Incident Assignments

-   Each row is a unique fire fighter/resource
-   res\_id: numeric, resource ID number
-   res\_gacc: character, resource home GACC (Geographic Area
    Coordination Center)
-   Each column to the right of the second column is labeled with a date
    and each cell contains an incident that shows which fire each
    resource was assigned to on that day. On days where a resource was
    not on a fire inc\_id needs to be 0.

Example: Resource 1001 was on incident 1046 from 01/01/2020 - 01/01/2020

| res\_id | res\_gacc | X2020.01.01 | X2020.01.02 | X2020.01.03 | …   |
|--------:|:----------|------------:|------------:|------------:|:----|
|    1001 | NM-SWC    |        1046 |        1046 |        1046 | …   |
|    1002 | OR-NWC    |        1055 |        1055 |           0 | …   |
|    1003 | OR-NWC    |           0 |           0 |        1055 | …   |

Incident Assignments Table

### Module Assignments

-   res\_id and res\_gacc columns here are the same as the module
    assignments data
-   Each column to the right of the second column is labeled with a date
    and each cell contains a module ID that describes which module (team
    or crew) each resource was assigned to each day. On days where a
    resource is not on a fire mod\_id needs to be 0. These module ID’s
    are only unique when paired with the incident of each agent on that
    day, there are modules with the same ID across multiple fires.

Example: Resource 1001 was on module E-1 from 01/01/2020 - 01/03/2020

| res\_id | res\_gacc | X2020.01.01 | X2020.01.02 | X2020.01.03 | …   |
|--------:|:----------|:------------|:------------|:------------|:----|
|    1001 | NM-SWC    | E-1         | E-1         | E-1         | …   |
|    1002 | OR-NWC    | O-12        | O-12        | 0           | …   |
|    1003 | OR-NWC    | 0           | 0           | C-8         | …   |

Module Assignments Table

### Incident Information

inc\_id \| inc\_number \| inc\_name \| inc\_gacc \| inc\_lat \| inc\_lon
\| max\_team\_type \| first\_day \| last\_day \|

## Minimal Example

``` r
library(covidfireMASS)
library(readr)
library(dplyr)
library(ggplot2)

#### Import Data ####
inc_assignments <- read_csv("path/to/inc_assignments.csv")
mod_assignments <- read_csv("path/to/mod_assignments.csv")
inc_info <- read.csv("path/to/inc_info.csv")

#### Running Simulations ####
# Default simulation run, see ?seasonal_sim for default inputs
sim1 <- seasonal_sim(inc_assignments, mod_assignments, inc_info)

# Reducing vaccination rate for the Southwest GACC
# and specifying an initial vaccinated population.
sim2 <- seasonal_sim(inc_assignments, mod_assignments, inc_info,
                     varying_vax = list(gacc = "NM-SWC", rate = 0.005),
                     R_init = 5000)
                     
sapply(sim2, class)

# Seasonal sim outputs a data frame with the following columns
# res_id     res_gacc     inc_id     mod_id        leader    state
# "numeric"  "character"  "numeric"  "character"  "logical"  "character"
#
# quarantine  q_days     vaccinated  vax_rate   time 
# "logical"   "numeric"  "logical"   "numeric"  "numeric"

#### Visualization ####
# Graphing quarantined work force (i.e. all agents that are on an incident
# and in quarantine/isolation)

to_plot <- sim2 %>%
  filter(inc_id > 0, quarantined) %>%
  count(time, res_gacc)
  
p <- ggplot(to_plot, aes(time, n, fill = res_gacc)) +
  geom_col(position = "stack")
```

NOTE: ***include example figure with the graph here***

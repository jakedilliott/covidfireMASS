
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

## Covid Fire Multi Agent Seasona Simulation

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

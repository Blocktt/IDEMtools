
# README- MassIBItools
================

A shiny app used to calculate IBI scores for given sites using benthic macroinvertebrate taxonomic information.

## Badges

[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/Blocktt/MassIBItools/graphs/commit-activity)
[![GitHub
license](https://img.shields.io/github/license/Blocktt/MassIBItools)](https://github.com/Blocktt/MassIBItools/blob/master/LICENSE)
[![GitHub
issues](https://img.shields.io/github/issues-raw/Blocktt/MassIBItools)](https://github.com/Blocktt/MassIBItools/issues)
[![Github all
releases](https://img.shields.io/github/downloads/Blocktt/MassIBItools/total)](https://github.com/Blocktt/MassIBItools/releases)

## Installation

``` r
library(devtools)  #install if needed
Sys.setenv("TAR" = "internal")  # needed for R v3.6.0
install_github("Blocktt/MassIBItools", force=TRUE, build_vignettes=TRUE)
```

## Purpose

Functions to aid the Massachusetts DEP in bioassessment and IBI scoring using their updated index. Program uses Erik Leppo's BioMonTools for calculations (https://github.com/leppott/BioMonTools). 

## Status

In development.

## Usage

A Shiny app purpose built for MA DEP to run IBI metric scoring. 

## Documentation

Vignette and install guide updates are planned for the future.

## Issues

<https://github.com/Blocktt/MassIBItools/issues>


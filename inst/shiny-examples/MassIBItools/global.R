# Shiny Global File

# Packages
library(shiny)
library(DT)
library(ggplot2)
library(readxl)
library(reshape2)
library(dplyr)
library(utils)
library(BioMonTools)
library(knitr)
library(maps)
library(rmarkdown)
library(markdown)
library(tidyr)
library(leaflet)
library(shinyjs) # used for download button enable
library(mapview) # used to download leaflet map
library(stringr)
library(shinythemes)


# Drop-down boxes
MMIs <- c("IDEM_2017_Bugs", "IDEM_2017_Fish")
Community <- c("bugs", "fish")


# File Size
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10MB.
options(shiny.maxRequestSize = 25*1024^2)

# define which metrics michigan wants to keep in indices

BugMetrics <- c("nt_total"
                 ,"pi_EphemNoCaeBae"
                 ,"pi_ffg_filt"
                 ,"pi_ffg_shred"
                 ,"pi_OET"
                 ,"pi_Pleco"
                 ,"pi_tv_intol"
                 ,"pt_EPT"
                 ,"pt_ffg_pred"
                 ,"pt_NonIns"
                 ,"pt_POET"
                 ,"pt_tv_intol"
                 ,"pt_tv_toler"
                 ,"pt_volt_semi"
                 ,"x_Becks")# END BugMetrics

FishMetrics <- c("nt_total"
                ,"pi_EphemNoCaeBae"
                ,"pi_ffg_filt"
                ,"pi_ffg_shred"
                ,"pi_OET"
                ,"pi_Pleco"
                ,"pi_tv_intol"
                ,"pt_EPT"
                ,"pt_ffg_pred"
                ,"pt_NonIns"
                ,"pt_POET"
                ,"pt_tv_intol"
                ,"pt_tv_toler"
                ,"pt_volt_semi"
                ,"x_Becks")# END FishMetrics

#### GIS/Map data ####

dir_data <- file.path(".","GIS_Data")

## Indiana State Basins
IN_StateBasins <- rgdal::readOGR(file.path(dir_data, "IN_StateBasins_20210113.shp"))

## Indiana 2017 Bug IBI Site Classes

IN_BugClasses <- rgdal::readOGR(file.path(dir_data, "IN_BugClasses_20210113.shp"))




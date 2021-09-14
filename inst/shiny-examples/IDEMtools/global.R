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
library(capture)

# Drop-down boxes
MMI <- c("IDEM_2021_Diatoms")
Community <- c("algae")


# File Size
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10MB.
options(shiny.maxRequestSize = 25*1024^2)

# define which metrics to keep in indices

DiatomMetrics <- c("nt_LOW_N"
                   ,"nt_LOW_P"
                   ,"pi_Tol_13"
                   ,"pt_Achnan_Navic"
                   ,"pt_BC_12"
                   ,"pt_O_345"
                   ,"pt_PT_12"
                   ,"pt_SALINITY_34"
                   ,"pt_Sens_810")# END DiatomMetrics

# BugMetrics <- c("nt_total"
#                 ,"pi_EphemNoCaeBae"
#                 ,"pi_ffg_filt"
#                 ,"pi_ffg_shred"
#                 ,"pi_OET"
#                 ,"pi_Pleco"
#                 ,"pi_tv_intol"
#                 ,"pt_EPT"
#                 ,"pt_ffg_pred"
#                 ,"pt_NonIns"
#                 ,"pt_POET"
#                 ,"pt_tv_intol"
#                 ,"pt_tv_toler"
#                 ,"pt_volt_semi"
#                 ,"x_Becks")# END BugMetrics
#
# FishMetrics <- c("nt_total"
#                  ,"pi_EphemNoCaeBae"
#                  ,"pi_ffg_filt"
#                  ,"pi_ffg_shred"
#                  ,"pi_OET"
#                  ,"pi_Pleco"
#                  ,"pi_tv_intol"
#                  ,"pt_EPT"
#                  ,"pt_ffg_pred"
#                  ,"pt_NonIns"
#                  ,"pt_POET"
#                  ,"pt_tv_intol"
#                  ,"pt_tv_toler"
#                  ,"pt_volt_semi"
#                  ,"x_Becks")# END FishMetrics


#### GIS/Map data ####

dir_data <- file.path(".","GIS_Data")

jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"
#https://stackoverflow.com/questions/47343316/shiny-leaflet-easyprint-plugin

## Indiana State Basins
IN_StateBasins <- rgdal::readOGR(file.path(dir_data, "IN_StateBasins_20210113.shp"))

## Indiana 2017 Bug IBI Site Classes

IN_BugClasses <- rgdal::readOGR(file.path(dir_data, "IN_BugClasses_20210113.shp"))




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
library(htmlwidgets)


# Drop-down boxes
MMIs <- c("IEPA_2021_Bugs")
Community <- c("bugs")


# File Size
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10MB.
options(shiny.maxRequestSize = 25*1024^2)

# define which metrics michigan wants to keep in indices

BugMetrics <- c("nt_ECT"
                ,"pi_Dipt"
                ,"pi_ffg_filt"
                ,"nt_habit_climb"
                ,"pi_tv_toler")# END BugMetrics

#### GIS/Map data ####

dir_data <- file.path(".","GIS_Data")

jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"
#https://stackoverflow.com/questions/47343316/shiny-leaflet-easyprint-plugin

## Illinois 2021 Bug IBI Site Classes

IL_BugClasses <- rgdal::readOGR(file.path(dir_data, "IEPA_SiteClasses.shp"))




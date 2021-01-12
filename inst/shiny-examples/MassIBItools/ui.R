#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Packages
# library(shiny)
# library(DT)
# library(ggplot2)
# library(readxl)
# library(reshape2)
# library(dplyr)
# library(utils)
# library(BioMonTools)
# library(knitr)
# library(maps)
# library(rmarkdown)
# library(tidyr)
# library(plotly)
# library(shinyjs) # used for download button enable

# Source Pages ####

# Load files for individual screens

tab_Background <- source("external/tab_Background.R", local = TRUE)$value
tab_Instructions <- source("external/tab_Instructions.R", local = TRUE)$value
tab_Calculator <- source("external/tab_Calculator.R", local = TRUE)$value
tab_DataExplorer <- source("external/tab_DataExplorer.R", local = TRUE)$value


# Define UI
shinyUI(navbarPage(theme = shinytheme("united"), "Indiana Stream IBI Calculator v0.1.0.900"
                   ,tab_Background()
                   ,tab_Instructions()
                   ,tab_Calculator()
                   ,tab_DataExplorer()
          )## navbarPage~END
)## shinyUI~END

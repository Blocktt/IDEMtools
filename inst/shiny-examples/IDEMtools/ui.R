#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Source Pages ####

# Load files for individual screens

tab_Background <- source("external/tab_Background.R", local = TRUE)$value
tab_Instructions <- source("external/tab_Instructions.R", local = TRUE)$value
tab_Calculator <- source("external/tab_Calculator.R", local = TRUE)$value
tab_DataExplorer <- source("external/tab_DataExplorer.R", local = TRUE)$value
tab_SiteClasses <- source("external/tab_SiteClasses.R", local = TRUE)$value
tab_StoryMaps <- source("external/tab_StoryMaps.R", local = TRUE)$value

# Define UI
shinyUI(navbarPage(theme = shinytheme("cerulean"), "Indiana DEM IBI Calculator v0.1.2.9000"
                   ,tab_Background()
                   ,tab_Instructions()
                   ,tab_Calculator()
                   ,tab_DataExplorer()
                   ,tab_SiteClasses()
                   ,tab_StoryMaps()
          )## navbarPage~END
)## shinyUI~END

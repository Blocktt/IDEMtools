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

# test for rendering markdown
# fn_html <- file.path(".", "Extras", "App_Instructions.rmd")
# rmdfiles <- c("App_Background_test.rmd")
# sapply(rmdfiles, knit, quiet = T)



# Define UI
shinyUI(navbarPage(theme = shinytheme("united"), "Massachusetts Stream IBI Calculator v0.1.2.920",
                   tabPanel("Background",

                            img(src = "MassDEPlogo.png"),
                            # withMathJax(includeMarkdown("App_Background.md")),
                            # withMathJax(includeMarkdown("App_Background.rmd"))
                            #https://groups.google.com/g/shiny-discuss/c/0UiiHcdhN4k
                            #https://stackoverflow.com/questions/34256810/knitr-rmarkdown-pander-minimal-md-html-for-body-body-only
                            includeHTML("App_Background.html"),
                            img(src = "figure1_alt.png", height = 580, width = 1000)

                   ), #tabPanel ~END
                   tabPanel("Instructions",

                            img(src = "MassDEPlogo.png"),
                            # withMathJax(includeMarkdown("App_Instructions.md"))
                            includeHTML("App_Instructions.html")

                   ), #tabPanel ~END
                   tabPanel("Calculator",
                            # SideBar
                            sidebarLayout(
                                sidebarPanel(
                                    # 0. Progress
                                    h3("App Steps")
                                    # , htmlOutput("vig")
                                    #, p("1_LoadData, 2_CalcMetrics, 3_CalcIndex, 4_DownloadResults")
                                    , h4("1. Load File")
                                    , h5("Select file parameters")
                                    , radioButtons('sep', 'Separator',
                                                   c(Comma=',',
                                                     Semicolon=';',
                                                     Tab='\t'),
                                                   ',')
                                    , radioButtons('quote', 'Quote',
                                                   c(None='',
                                                     'Double Quote'='"',
                                                     'Single Quote'="'"),
                                                   '"')
                                    #, tags$hr()
                                    , fileInput('fn_input', 'Choose file to upload',
                                                accept = c(
                                                    'text/csv',
                                                    'text/comma-separated-values',
                                                    'text/tab-separated-values',
                                                    'text/plain',
                                                    '.csv',
                                                    '.tsv'
                                                )
                                    )##fileInput~END
                                    #, tags$hr()
                                    , h4("2. Calculate IBI")
                                    #, selectInput("MMI", "Select an IBI to calculate:",
                                                  #choices=MMIs)
                                    , h5("Mass Kick IBI or Multihabitat IBI - specified in INDEX_REGION field of input")
                                    , actionButton("b_Calc", "Calculate Metric Values and Scores")
                                    , tags$hr()
                                    , h4("3. Download Results")

                                    # Button
                                    , p("Select button to download zip file with input and results.")
                                    , p("Check 'results_log.txt' for any warnings or messages.")
                                    , useShinyjs()
                                    , shinyjs::disabled(downloadButton("b_downloadData", "Download"))

                                )##sidebarPanel~END
                                , mainPanel(
                                    tabsetPanel(type="tabs"
                                                # , tabPanel("TESTING"
                                                #            , htmlOutput("vig")
                                                #            , textOutput("fn_input")
                                                #            , useShinyjs()
                                                #            , runcodeUI(code="shinyjs::alert('Hello!')")
                                                #            )
                                                # , tabPanel("Directions"
                                                #            , p("Import data file."))
                                                , tabPanel("Data, Import"
                                                           #, tableOutput('df_import'))
                                                           , DT::dataTableOutput('df_import_DT'))
                                                # , tabPanel("Data QC"
                                                #            , h4("QC on the imported data")
                                                #             , p("Future implementation."))
                                                # , tabPanel("Results, Metric Values"
                                                #            , DT::dataTableOutput('df_metric_values'))
                                                # , tabPanel("Results, Metric Scores"
                                                #            , DT::dataTableOutput('df_metric_scores'))
                                                # , tabPanel("Results, Plot"
                                                #            #, plotlyOutput("plot_IBI", , height = "90%")
                                                #            , imageOutput("plot_IBI")
                                                #            )
                                    )##tabsetPanel~END
                                )##mainPanel~END

                            )##sidebarLayout~END

            ),## tabPanel~END
            tabPanel("Data Explorer"
                     , titlePanel("Data Explorer - Explore Your Results!")
                     , h5("The map and plot below will be generated once metric values and scores have been calculated.")
                     , h5("Sites are clustered when zoomed out for increased visibility - zoom in for added detail!")
                     , h5("Sites are color coded by their Index Score value - click on a site for more info!")
                     , sidebarLayout(
                       sidebarPanel(
                         helpText("Use the drop down menu to select a Sample ID.")
                         ,selectInput("siteid.select", "Select Sample ID:"
                                       , "")##selectInput~END
                         , p("After choosing a Sample ID, the map will zoom to its location and the plot will display scoring.")
                         , br()
                         # , helpText("Click below to download PDF of map")
                         # , downloadButton(outputId = "map_down", label = "Map Download")
                         , plotOutput("DatExp_plot")
                         , plotOutput("Index_plot")

                       )##sidebarPanel.END
                       , mainPanel(
                         tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
                         , leafletOutput("mymap", height = "85vh")

                       )##mainPanel.END
                     )#sidebarLayout.End
            )## tabPanel~END
          )## navbarPage~END
)## shinyUI~END

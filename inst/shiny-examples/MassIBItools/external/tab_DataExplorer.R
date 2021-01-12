function(){
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
               , plotOutput("DatExp_plot")
               , plotOutput("Index_plot")

             )##sidebarPanel.END
             , mainPanel(
               tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
               , leafletOutput("mymap", height = "85vh")

             )##mainPanel.END
           )#sidebarLayout.End
  )## tabPanel~END
}##FUNCTION~END

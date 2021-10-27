function(){
  tabPanel("StoryMaps",
           mainPanel(
             h3("An ArcGIS StoryMap that summarizes the development of the IDEM Diatom IBI.")
             , h3(tags$a(href= "https://storymaps.arcgis.com/stories/98d52e3c1f004d658a8d452a0c0b4aea", "Direct Link"))
             , htmlOutput("StoryMap_Tech")

           )##mainPanel~END
  ) #tabPanel ~END
}##FUNCTION~END

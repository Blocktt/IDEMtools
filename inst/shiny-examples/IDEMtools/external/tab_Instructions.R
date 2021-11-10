function(){
  tabPanel("Instructions",
           mainPanel(
             img(src = "IDEM_logo.png", height = 175)
             ,br()
             ,includeHTML("www/App_Instructions.html")

           )##mainPanel~END
  ) #tabPanel ~END
}##FUNCTION~END

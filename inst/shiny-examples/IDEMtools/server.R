#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # map and plots require df_metsc
    map_data <- reactiveValues(df_metsc = NULL)

    # Misc Names ####
    output$fn_input_display <- renderText({input$fn_input}) ## renderText~END


    # df_import ####
    output$df_import_DT <- renderDT({
        # input$df_import will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.


        inFile <- input$fn_input

        shiny::validate(
            need(inFile != "", "Please select a data set") # used to inform the user that a data set is required
        )

        if (is.null(inFile)){
            return(NULL)
        }##IF~is.null~END

        # Read user imported file
        df_input <- read.csv(inFile$datapath, header = TRUE,
                             sep = input$sep,
                             quote = input$quote, stringsAsFactors = FALSE)

        required_columns <- c("INDEX_NAME"
                              ,"INDEX_REGION"
                              ,"STATIONID"
                              ,"BFI"
                              ,"LAT"
                              ,"LONG"
                              ,"COLLDATE"
                              ,"SAMPLEID"
                              ,"TAXAID"
                              ,"EXCLUDE"
                              ,"NONTARGET"
                              ,"N_TAXA"
                              ,"ORDER"
                              ,"FAMILY"
                              ,"GENUS"
                              ,"BC_USGS"
                              ,"PT_USGS"
                              ,"O_USGS"
                              ,"SALINITY_USGS"
                              ,"P_USGS"
                              ,'N_USGS'
                              ,"TOLVAL")

        column_names <- colnames(df_input)

        # QC Check for column names
        col_req_match <- required_columns %in% column_names
        col_missing <- required_columns[!col_req_match]

        shiny::validate(
            need(all(required_columns %in% column_names)
                 , paste0("Error\nChoose correct data separator; otherwise, you may have missing required columns\n"
                          , paste("Required columns missing from the data:\n")
                          , paste("* ", col_missing, collapse = "\n")))
        )##END ~ validate() code

        ########################### MAP and PLOT Observer
        observe({
          inFile<- input$fn_input
          if(is.null(inFile))
            return(NULL)

          df_input

          updateSelectInput(session, "siteid.select"
                            , choices = as.character(
                              sort(unique(df_input[, "SAMPLEID"]))))
        }) ## observe~END



        # Add "Results" folder if missing
        boo_Results <- dir.exists(file.path(".", "Results"))
        if(boo_Results==FALSE){
            dir.create(file.path(".", "Results"))
        }

        # Remove all files in "Results" folder
        fn_results <- list.files(file.path(".", "Results"), full.names=TRUE)
        file.remove(fn_results)

        # Write to "Results" folder - Import as TSV
        fn_input <- file.path(".", "Results", "data_import.tsv")
        write.table(df_input, fn_input, row.names=FALSE
                    , col.names=TRUE, sep="\t")

        # Copy to "Results" folder - Import "as is"
        file.copy(input$fn_input$datapath
                  , file.path(".", "Results", input$fn_input$name))

        return(df_input)

    }##expression~END
    , filter="top", options=list(scrollX=TRUE)

    )##output$df_import_DT~END

    # b_Calc ####
    # Calculate IBI (metrics and scores) from df_import
    # add "sleep" so progress bar is readable
    observeEvent(input$b_Calc, {
        shiny::withProgress({
            #
            # Number of increments
            n_inc <- 8

            # sink output
            file_sink <- file(file.path(".", "Results", "results_log.txt")
                              , open = "wt")
            sink(file_sink, type = "output", append = TRUE)
            sink(file_sink, type = "message", append = TRUE)

            # Log
            message("Results Log from IDEMtools Shiny App")
            message(Sys.time())
            inFile <- input$fn_input
            message(paste0("file = ", inFile$name))

            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Data, Initialize")
            Sys.sleep(0.25)

            # Read in saved file (known format)
            df_data <- NULL  # set as null for IF QC check prior to import
            fn_input <- file.path(".", "Results", "data_import.tsv")
            df_data <- read.delim(fn_input, stringsAsFactors = FALSE, sep="\t")

            # QC, FAIL if TRUE
            if (is.null(df_data)){
                return(NULL)
            }

            # QC, FFG symbols
            # FFG_approved <- c("CG", "CF", "PR", "SC", "SH")
            # df_QC <- df_data
            # df_QC[df_QC==""] <- NA
            #
            #
            # N_FFG_wrong <- sum((na.omit(df_QC$FFG) %in% FFG_approved) == 0)
            # if(N_FFG_wrong>0){
            #     message(paste0(N_FFG_wrong, "taxa have the incorrect FFG descriptor, please use the following:"))
            #     message("Replace 'Collector' with 'CG'")
            #     message("Replace 'Filterer' with 'CF'")
            #     message("Replace 'Predator' with 'PR'")
            #     message("Replace 'Scraper' with 'SC'")
            #     message("Replace 'Shredder' with 'SH'")
            #     message("Failure to change FFG to correct coding scheme will result in incorrect metric calculations")
            # }


            # QC, N_TAXA = 0
            N_Taxa_zeros <- sum(df_data$N_TAXA == 0, na.rm = TRUE)
            if(N_Taxa_zeros>0){
                message("Some taxa in your dataset have a count (N_TAXA) of zero. Values for TAXAID with N_TAXA = 0 will be removed before calculations.")
            }

            # QC, Exclude as TRUE/FALSE
            Exclude.T <- sum(df_data$EXCLUDE==TRUE, na.rm=TRUE)
            if(Exclude.T==0){##IF.Exclude.T.START
                message("EXCLUDE column does not have any TRUE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
            }##IF.Exclude.T.END

            # QC, NonTarget as TRUE/FALSE
            NonTarget.F <- sum(df_data$NONTARGET==FALSE, na.rm=TRUE)
            if(NonTarget.F==0){##IF.Exclude.T.START
                message("NONTARGET column does not have any FALSE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
            }##IF.Exclude.T.END


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Calculate, Metrics (takes ~ 30-45s)")
            Sys.sleep(0.5)

            # convert Field Names to UPPER CASE
            names(df_data) <- toupper(names(df_data))

            # QC, Required Fields
            col.req <- c("INDEX_REGION","SAMPLEID","TAXAID","EXCLUDE","NONTARGET"
                         ,"N_TAXA","PHYLUM","ORDER","FAMILY","GENUS","BC_USGS"
                         ,"TROPHIC_USGS","SAP_USGS","PT_USGS","O_USGS","SALINITY_USGS"
                         ,"BAHLS_USGS","P_USGS","N_USGS","HABITAT_USGS","N_FIXER_USGS"
                         ,"MOTILITY_USGS","SIZE_USGS","HABIT_USGS","MOTILE2_USGS"
                         ,"TOLVAL","DIATOM_ISA","DIAT_CL","POLL_TOL","BEN_SES"
                         ,"DIATAS_TP","DIATAS_TN","DIAT_COND","DIAT_CA","MOTILITY"
                         ,"NF")
            col.req.missing <- col.req[!(col.req %in% toupper(names(df_data)))]

            # Add missing fields
            df_data[,col.req.missing] <- NA
            warning(paste("Metrics related to the following fields are invalid:"
                          , paste(paste0("   ", col.req.missing), collapse="\n"), sep="\n"))

            # calculate values and scores in two steps using BioMonTools
            # save each file separately

            # create long name version of Index Regions for client comprehension

            df_data$INDEX_REGION_LONG <- ifelse(df_data$INDEX_REGION == "HiN", "High_Lithologic_Nitrogen",
                                                ifelse(df_data$INDEX_REGION == "LoN", "Low_Lithologic_Nitrogen",
                                                                     "Unknown"))

            # columns to keep
            keep_cols <- c("BFI", "LAT", "LONG", "STATIONID"
                           , "COLLDATE", "INDEX_REGION_LONG")

            # metric calculation
            df_metval <- suppressWarnings(
              metric.values(fun.DF = df_data
                            , fun.Community = "algae"
                            , fun.MetricNames = DiatomMetrics
                            , fun.cols2keep= keep_cols
                            , boo.Shiny = TRUE))


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Metrics have been calculated!")
            Sys.sleep(1)

            # Log
            message(paste0("Chosen IBI from Shiny app = ", MMI))


            #
            # Save
            fn_metval <- file.path(".", "Results", "results_metval.csv")
            write.csv(df_metval, fn_metval, row.names = FALSE)

            #
            # QC - upper case Index.Name
            names(df_metval)[grepl("Index.Name"
                                   , names(df_metval))] <- "INDEX.NAME"


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Adjust, Metrics")
            Sys.sleep(0.50)

            # Adjust diatom metrics according to BFI field

            df_metval <- df_metval %>%
              group_by(SAMPLEID) %>%
              mutate(pt_BC_12_adj =
                       ifelse(BFI < 30
                              , pt_BC_12 - 10.5
                              , pt_BC_12 - 15.1))


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Calculate, Scores")
            Sys.sleep(0.50)



            # Metric Scores

            # Thresholds
            fn_thresh <- file.path(system.file(package="BioMonTools")
                                   , "extdata", "MetricScoring.xlsx")
            df_thresh_metric <- read_excel(fn_thresh, sheet="metric.scoring")
            df_thresh_index <- read_excel(fn_thresh, sheet="index.scoring")

            # metrics for scores

            myMetrics <- c("nt_LOW_N"
                           ,"nt_LOW_P"
                           ,"pi_Tol_13"
                           ,"pt_Achnan_Navic"
                           ,"pt_BC_12_adj" # Calculated in app, not BioMonTools
                           ,"pt_O_345"
                           ,"pt_PT_12"
                           ,"pt_SALINITY_34"
                           ,"pt_Sens_810")# END myMetrics

            # run scoring code
            df_metsc <- metric.scores(DF_Metrics = df_metval
                                      , col_MetricNames = myMetrics
                                      , col_IndexName = "INDEX_NAME"
                                      , col_IndexRegion = "INDEX_REGION"
                                      , DF_Thresh_Metric = df_thresh_metric
                                      , DF_Thresh_Index = df_thresh_index
                                      , col_ni_total = "ni_total")

            df_metsc <- df_metsc %>%
              mutate(INDEX_REGION = replace(INDEX_REGION, INDEX_REGION == "HIN", "HiN")) %>%
              mutate(INDEX_REGION = replace(INDEX_REGION, INDEX_REGION == "LON", "LoN"))


            # Save
            fn_metsc <- file.path(".", "Results", "results_metsc.csv")
            write.csv(df_metsc, fn_metsc, row.names = FALSE)

            # MAP and Plot requires df_metsc
            map_data$df_metsc <- df_metsc


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Create, summary report (~ 20 - 40 sec)")
            Sys.sleep(0.75)

            # Render Summary Report (rmarkdown file)
            # rmarkdown::render(input = file.path(".", "Extras", "Summary_IN.rmd")
            #                   , output_format = "word_document"
            #                   , output_dir = file.path(".", "Results")
            #                   , output_file = "results_summary_report"
            #                   , quiet = TRUE)

            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Ben's code is magical!")
            Sys.sleep(0.75)


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Create, Zip")
            Sys.sleep(0.50)

            # Create zip file
            fn_4zip <- list.files(path = file.path(".", "Results")
                                  , pattern = "^results_"
                                  , full.names = TRUE)
            zip(file.path(".", "Results", "results.zip"), fn_4zip)

            # enable download button
            shinyjs::enable("b_downloadData")

            # #
            # return(myMetric.Values)
            # end sink
            #flush.console()
            sink() # console
            sink() # message
            #
        }##expr~withProgress~END
        , message = "Calculating IBI"
        )##withProgress~END
    }##expr~ObserveEvent~END
    )##observeEvent~b_CalcIBI~END


    # Downloadable csv of selected dataset
    output$b_downloadData <- downloadHandler(
        # use index and date time as file name

        filename = function() {
            paste(input$MMI, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip", sep = "")
        },
        content = function(fname) {##content~START

            # Create Zip file
            file.copy(file.path(".", "Results", "results.zip"), fname)

            #
        }##content~END
        #, contentType = "application/zip"
    )##downloadData~END


    # Data Explorer Tab ####

    # create quantile color palette to change color of markers based on index values
    # scale_range <- c(0,100)
    # at <- c(0, 35, 55, 75, 100)
    # qpal <- colorBin(c("red","yellow", "green"), domain = scale_range, bins = at)

    output$mymap <- renderLeaflet({

      req(!is.null(map_data$df_metsc))

      df_data <- map_data$df_metsc

      # create Narratives

      Nar_Map <- factor(c("Exceptional"
                          ,"Satisfactory"
                          ,"Moderately Degraded"
                          ,"Severely Degraded"))

      Narratives <- ifelse(df_data$Index_Nar == "Exceptional", "Exceptional",
                           ifelse(df_data$Index_Nar == "Satisfactory", "Satisfactory",
                                  ifelse(df_data$Index_Nar == "Moderately Degraded", "Moderately Degraded",
                                         "Severely Degraded")))

      Narratives <- factor(Narratives, levels = c("Exceptional"
                                                  ,"Satisfactory"
                                                  ,"Moderately Degraded"
                                                  ,"Severely Degraded"))


      pal <- colorFactor(
        palette = c('green', 'yellow', 'orange', 'red'),
        domain = Narratives,
        ordered = TRUE
      )

      # create Region_Name column to combine Index_Regions

      # df_data$Region_Name <- ifelse(df_data$INDEX_REGION == "Bugs_N", "N",
      #                                     ifelse(df_data$INDEX_REGION == "Bugs_NC", "NC",
      #                                            ifelse(df_data$INDEX_REGION == "Bugs_SW", "SW",
      #                                                   ifelse(df_data$INDEX_REGION == "Bugs_SE", "SE",
      #                                                          "Unknown"))))

      # subset data by Index_Region

      HiN_data <- df_data %>%
        filter(INDEX_REGION == "HiN")

      LoN_data <- df_data %>%
        filter(INDEX_REGION == "LoN")

      leaflet() %>%
        addTiles() %>%
        addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>%
        addProviderTiles("CartoDB.Positron", group="Positron") %>%
        addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite") %>%
        addPolygons(data = IN_StateBasins
                    , color = "blue"
                    , weight = 5
                    , fill = FALSE
                    , label = IN_StateBasins$Monitoring
                    , group = "State Basins"

        ) %>%
        addPolygons(data = IN_BugClasses
                    , color = "green"
                    , weight = 3
                    , fill = FALSE
                    , label = IN_BugClasses$Location
                    , group = "Bug Site Classes"

        ) %>%
        addCircleMarkers(data = HiN_data, lat = ~LAT, lng = ~LONG
                         , group = "High Nitrogen"
                         , popup = paste("SampleID:", HiN_data$SAMPLEID, "<br>"
                                         ,"Site Class:", HiN_data$INDEX_REGION, "<br>"
                                         ,"Coll Date:", HiN_data$COLLDATE, "<br>"
                                         ,"Station ID:", HiN_data$STATIONID, "<br>"
                                         ,"<b> Index Value:</b>", round(HiN_data$Index, 2), "<br>"
                                         ,"<b> Narrative:</b>", HiN_data$Index_Nar)
                         , color = "black", fillColor = ~pal(Index_Nar)
                         , fillOpacity = 1, stroke = TRUE
                         , clusterOptions = markerClusterOptions()

        ) %>%
        addCircleMarkers(data = LoN_data, lat = ~LAT, lng = ~LONG
                         , group = "Low Nitrogen"
                         , popup = paste("SampleID:", LoN_data$SAMPLEID, "<br>"
                                         ,"Site Class:", LoN_data$INDEX_REGION, "<br>"
                                         ,"Coll Date:", LoN_data$COLLDATE, "<br>"
                                         ,"Station ID:", LoN_data$STATIONID, "<br>"
                                         ,"<b> Index Value:</b>", round(LoN_data$Index, 2), "<br>"
                                         ,"<b> Narrative:</b>", LoN_data$Index_Nar)
                         , color = "black", fillColor = ~pal(Index_Nar)
                         , fillOpacity = 1, stroke = TRUE
                         , clusterOptions = markerClusterOptions()

        ) %>%
        addLegend(pal = pal,
                  values = Narratives,
                  position = "bottomright",
                  title = "Index Narratives",
                  opacity = 1) %>%
        addLayersControl(overlayGroups = c("High Nitrogen", "Low Nitrogen"
                                           , "State Basins", "Bug Site Classes")
                         ,baseGroups = c("Esri WSM"
                                         , "Positron", "Toner Lite")
                         ,options = layersControlOptions(collapsed = TRUE))%>%
        hideGroup(c("State Basins", "Bug Site Classes")) %>%
        addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap)
      # %>%
      #   onRender( # used for making download button https://stackoverflow.com/questions/47343316/shiny-leaflet-easyprint-plugin
      #     "function(el, x) {
      #       L.easyPrint({
      #         sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
      #         filename: 'mymap',
      #         exportOnly: true,
      #         hideControlContainer: true
      #       }).addTo(this);
      #       }"
      #   ) ##onRender~END

      }) ##renderLeaflet~END

    # Map that filters output data to only a single site
   observeEvent(input$siteid.select,{
      req(!is.null(map_data$df_metsc))

      df_data <- map_data$df_metsc

      #
      df_filtered <- df_data[df_data$SAMPLEID == input$siteid.select, ]

      #
      # get centroid (use mean just in case have duplicates)
      view.cent <- c(mean(df_filtered$LONG), mean(df_filtered$LAT))
      #
      # modify map
      leafletProxy("mymap") %>%
        #clearShapes() %>%  # removes all layers
        removeShape("layer_site_selected") %>%
        #addPolylines(data=filteredData()
        addCircles(data=df_filtered
                   , lng=~LONG
                   , lat=~LAT
                   , popup= paste("SampleID:", df_filtered$SAMPLEID, "<br>"
                                 ,"Site Class:", df_filtered$INDEX_REGION, "<br>"
                                 ,"<b> Index Value:</b>", round(df_filtered$Index, 2), "<br>"
                                 ,"<b> Narrative:</b>", df_filtered$Index_Nar)
                   , color = "black"
                   , group = "Sites_selected"
                   , layerId = "layer_site_selected"
                   , radius=30) %>%

        setView(view.cent[1], view.cent[2], zoom = 16) # 1= whole earth

    }) ## observeEvent(input$siteid.select) ~ END



    ## Plots ####

    df_sitefilt <- reactive({
      req(!is.null(map_data$df_metsc))

      df_all_scores <- map_data$df_metsc

      df_all_scores[df_all_scores$SAMPLEID == input$siteid.select, ]
    })## reactive~ END


    output$DatExp_plot <- renderPlot({
      if (is.null(df_sitefilt()))
        return(NULL)

      df_selected_site <- df_sitefilt()

      df_trim <- df_selected_site %>%
        select_if(!is.na(df_selected_site)) %>%
        select(-c(Index_Nar)) %>%
        select(SAMPLEID, Index, starts_with("SC_"))%>%
        rename_at(vars(starts_with("SC_")),
                  funs(str_replace(., "SC_", "")))

      df_grph_input <- df_trim %>%
        pivot_longer(!SAMPLEID, names_to = "Variable", values_to = "Score")

      df_grph_input <- as.data.frame(df_grph_input)

      # shape palette
      shape_pal <- c("Index" = 16
                     ,"nt_LOW_N" = 15
                     ,"nt_LOW_P" = 15
                     ,"pi_Tol_13" = 15
                     ,"pt_Achnan_Navic" = 15
                     ,"pt_BC_12_adj" = 15 # Calculated in app, not BioMonTools
                     ,"pt_O_345" = 15
                     ,"pt_PT_12" = 15
                     ,"pt_SALINITY_34" = 15
                     ,"pt_Sens_810" = 15)

      # size palette
      size_pal <- c("Index" = 10
                    ,"nt_LOW_N" = 5
                    ,"nt_LOW_P" = 5
                    ,"pi_Tol_13" = 5
                    ,"pt_Achnan_Navic" = 5
                    ,"pt_BC_12_adj" = 5 # Calculated in app, not BioMonTools
                    ,"pt_O_345" = 5
                    ,"pt_PT_12" = 5
                    ,"pt_SALINITY_34" = 5
                    ,"pt_Sens_810" = 5)

      ggplot(df_grph_input, aes(x=Variable, y = Score, shape = Variable))+
        geom_point(aes(size = Variable))+
        scale_size_manual(values=size_pal)+
        scale_shape_manual(values=shape_pal)+
        ylim(0,100)+
        labs(y = "Scores",
             x = "")+
        coord_flip()+
        scale_x_discrete(limits = rev(levels(as.factor(df_grph_input$Variable))))+
        theme(text = element_text(size = 12),
              axis.text = element_text(color = "black", size = 12),
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(color = "black"),
              legend.position = "none")

    }) ## renderPlot ~ END

    output$Index_plot <- renderPlot({
      if (is.null(df_sitefilt()))
        return(NULL)

      df_all_scores <- map_data$df_metsc

      df_selected_site <- df_sitefilt()

      site_region <- as.character(df_selected_site$INDEX_REGION)

      df_sub_regions <- df_all_scores[df_all_scores$INDEX_REGION == site_region,]

      ggplot()+
        geom_boxplot(data = df_sub_regions
                     , aes(x = INDEX_REGION, y = Index), width = 0.25)+
        geom_point(data = df_selected_site
                   , aes(x = INDEX_REGION, y = Index), size = 5)+
        labs(y = "Index Scores of Input Data Frame",
             x = "Index Region")+
        ylim(0,100)+
        theme(text = element_text(size = 12),
              axis.text = element_text(color = "black", size = 12),
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(color = "black"),
              legend.position = "none")


    }) ## renderPlot ~ END

    # Site Class Identifier ####

    ## df_import ####
    output$df_site_import_DT <- renderDT({
      # input$df_import will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.

      inFile <- input$fn_input_siteclass

      # shiny::validate(
      #   need(inFile != "", "Please select a data set") # used to inform the user that a data set is required
      # )

      if (is.null(inFile)){
        return(NULL)
      }##IF~is.null~END

      # Read user imported file
      df_input_sc <- read.csv(inFile$datapath, header = TRUE,
                           sep = input$sep_sci,
                           quote = input$quote_sci, stringsAsFactors = FALSE)

      required_columns <- c("COMID"
                            ,"STATIONID")

      column_names <- colnames(df_input_sc)

      # QC Check for column names
      col_req_match <- required_columns %in% column_names
      col_missing <- required_columns[!col_req_match]

      shiny::validate(
        need(all(required_columns %in% column_names)
             , paste0("Error\nChoose correct data separator; otherwise, you may have missing required columns\n"
                      , paste("Required columns missing from the data:\n")
                      , paste("* ", col_missing, collapse = "\n")))
      )##END ~ validate() code

      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(".", "Results_SiteClass"))
      if(boo_Results==FALSE){
        dir.create(file.path(".", "Results_SiteClass"))
      }

      # Remove all files in "Results" folder
      fn_results <- list.files(file.path(".", "Results_SiteClass"), full.names=TRUE)
      file.remove(fn_results)

      # Write to "Results" folder - Import as TSV
      fn_input <- file.path(".", "Results_SiteClass", "data_import_siteclassidentifier.tsv")
      write.table(df_input_sc, fn_input, row.names=FALSE
                  , col.names=TRUE, sep="\t")

      # Copy to "Results" folder - Import "as is"
      file.copy(input$fn_input_siteclass$datapath
                , file.path(".", "Results_SiteClass", input$fn_input_siteclass$name))

      return(df_input_sc)

    }##expression~END
    , filter="top", options=list(scrollX=TRUE)

    )##output$df_import_DT~END


    ## c_Calc ####
    observeEvent(input$c_Calc, {
      shiny::withProgress({
        #
        # Number of increments
        n_inc <- 4

        # sink output
        file_sink <- file(file.path(".", "Results_SiteClass", "results_log_sci.txt")
                          , open = "wt")
        sink(file_sink, type = "output", append = TRUE)
        sink(file_sink, type = "message", append = TRUE)

        # Log
        message("Results Log from IDEMtools Site Class Identifier")
        message(Sys.time())
        inFile <- input$fn_input_siteclass
        message(paste0("file = ", inFile$name))

        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Data, Initialize")
        Sys.sleep(0.25)

        # Read in saved file (known format)
        df_data <- NULL  # set as null for IF QC check prior to import
        fn_input <- file.path(".", "Results_SiteClass", "data_import_siteclassidentifier.tsv")
        df_data <- read.delim(fn_input, stringsAsFactors = FALSE, sep="\t")

        # QC, FAIL if TRUE
        if (is.null(df_data)){
          return(NULL)
        }

        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Join with COMIDs")
        Sys.sleep(0.5)

        # convert Field Names to UPPER CASE
        names(df_data) <- toupper(names(df_data))

        # Read in saved COMID file
        df_COMIDs <- NULL  # set as null for IF QC check prior to import
        fn_input <- file.path(".", "Extras", "tables", "df_COMIDs.csv")
        df_COMIDs <- read.csv(fn_input, stringsAsFactors = FALSE, sep=",")


        ### Join tables ####

        df_combined <- dplyr::left_join(df_data, df_COMIDs
                                         , by = "COMID")


        ### QC Results ####

        N_SiteClass_zeros <- sum(is.na(df_combined$SiteClass))
        if(N_SiteClass_zeros>0){
          message(paste("Some input COMIDs did not match with the site class identifier table.\n")
                  ,paste("Number of mismatches:", N_SiteClass_zeros,"\n")
                  ,paste("Please check that input COMIDs are correct.\n")
                  ,paste("Contact Ben Block (Ben.Block@tetratech.com) if problem persists.")
          )# message~ END
        }#IF statement ~END

#         if(N_SiteClass_zeros>0){
#           message("Some input COMIDs did not match with the site class identifier table.
#                   QC check the COMIDs for accuracy.
#                   Contact Ben Block (Ben.Block@tetratech.com) if problem persists.")
#         }#IF statement ~END


        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Join Completed!")
        Sys.sleep(1)

        # Save
        fn_siteclasses <- file.path(".", "Results_SiteClass"
                               , "results_SiteClassesIdentified.csv")
        write.csv(df_combined, fn_siteclasses, row.names = FALSE)

        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Create, Zip")
        Sys.sleep(0.50)

        # Create zip file
        fn_4zip <- list.files(path = file.path(".", "Results_SiteClass")
                              , pattern = "^results_"
                              , full.names = TRUE)
        zip(file.path(".", "Results_SiteClass", "results.zip"), fn_4zip)

        # enable download button
        shinyjs::enable("c_downloadData")

        sink() # console
        sink() # message
        #
      }##expr~withProgress~END
      )##withProgress~END
    }##expr~ObserveEvent~END
    )##observeEvent~b_CalcIBI~END


    ## Download dataset ####
    output$c_downloadData <- downloadHandler(
      # file name

      filename = function() {
        paste("Site_Class_Identifer_Results_"
              , format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip", sep = "")
      },
      content = function(fname) {##content~START

        # Create Zip file
        file.copy(file.path(".", "Results_SiteClass", "results.zip"), fname)

        #
      }##content~END
    )##downloadData~END

    # StoryMaps ####

    ## Technical ####
    #https://stackoverflow.com/questions/33020558/embed-iframe-inside-shiny-app
    #https://stackoverflow.com/questions/59628035/r-shiny-how-to-fill-out-the-entire-space-of-the-browser-window-with-an-iframe
    output$StoryMap_Tech <- renderUI({
      Technical <- paste0("https://storymaps.arcgis.com/stories/98d52e3c1f004d658a8d452a0c0b4aea")
      my_Technical <- tags$iframe(src=Technical, style='width:90vw;height:90vh;')
      my_Technical
    })## renderUI ~ END

})##shinyServer~END

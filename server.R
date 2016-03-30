shinyServer(function(input, output, session){
  
  output$Map <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron", options = tileOptions(minZoom = 6, maxZoom = 13)) %>% 
      setView(lng=-82.7, lat=40.6, zoom=6) %>%
          addCircleMarkers(data = sitesonly, radius = 10,
                           color = "navy", stroke=FALSE, fillOpacity=0.5, layerId = sitesonly$location)
      })
    
 # highlights selected marker
  observeEvent(input$Map_marker_click, {
    p <- input$Map_marker_click
    leafletProxy("Map") %>% 
      setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>%
      addCircleMarkers(p$lng, p$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId ="Selected")
    
  })

  # updates location bar with clicked site
  observeEvent(input$Map_marker_click, {
    p <- input$Map_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location)) updateSelectInput(session, "location", selected=p$id)
      if(!is.null(input$location) && input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })

  # marks selected site on map from location bar
  observeEvent(input$location, {
    if(!is.null(input$Map_marker_click)){
      p <- input$Map_marker_click
      p2 <- subset(sitesonly, location==input$location)
      if(input$location != p$id){
        leafletProxy("Map") %>% 
          setView(lng=p2$lng, lat=p2$lat, input$Map_zoom) %>%
          addCircleMarkers(p2$lng, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId ="Selected")
      }
    }else{
      p2 <- subset(sitesonly, location==input$location)
      leafletProxy("Map") %>% 
        setView(lng=p2$lng, lat=p2$lat, input$Map_zoom) %>%
        addCircleMarkers(p2$lng, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId ="Selected")
    }
    
  })
  
  get_df <- reactive({
    if(is.null(input$location)){
      return(NULL)
    }else{
      if(input$location == ""){
        return(NULL)
      }else{
        sites2map[which(sites2map$location == input$location), ]
      }
    }
  })
  
  output$yearOutput <- renderUI({
    if(is.null(input$location)){
      return(NULL)
    }else{
      selectInput("year", "Years",
                     c("ALL", sort(unique(get_df()$Year))),
                     selected = "ALL", multiple = FALSE)
    }
  })
  
  d2_yr <- reactive({ 
    if (is.null(input$year)) {
      return(NULL)
    }else{
      if (input$year == "ALL"){
        ret <- get_df()
      }else{
        ret <- subset(get_df(), Year==input$year) 
      }  
    }
    return(ret)
  })
  
  d3_spec.yr <- reactive({
    if(is.null(input$year)){
      return(NULL)
    }else{
      if (input$year == "ALL"){
        ret <- all.counts %>%
          filter(location == input$location) %>%
          select(CommonName, GrandTotal) %>%
          arrange(-GrandTotal)
      }else{
        ret <- ann.counts %>% 
          filter(location == input$location, Year == input$year) %>%
          select(CommonName, TotalCount) %>%
          arrange(-TotalCount)
      }
      return(ret)
    }
  })
  
  output$graph1a <- renderPlot({

      if(is.null(d2_yr())){
        return(NULL)
      }else{
        if (input$year == "ALL"){
          grandrich <- d2_yr() %>% 
            filter(variable == "grand.richness") %>%
            slice(1L) %>%
            select(value)
          nyears <- length(unique(d2_yr()$Year))
          grtitle <- paste(grandrich, "species observed over", nyears, "years", sep = " ")
          richness <- d2_yr() %>% filter(variable == "year.richness")
          gr1a <- ggplot(data = richness, aes(x = Year, y = value)) 
          gr1a + geom_point() + xlim(1995, 2014) + ylim(0, max(richness$value)) + ggtitle(grtitle)
        }else{
          yrrich <- d2_yr() %>%
            filter(variable == "year.richness") %>%
            slice(1L) %>%
            select(value)
          nweeks <- length(unique(d2_yr()$Week))
          grtitle <- paste(yrrich, "species observed over", nweeks, "weeks", sep = " ")
          richness <- d2_yr() %>% filter(variable == "surv.richness")
          gr1a <- ggplot(data = richness, aes(x = Week, y = value))
          gr1a + geom_point() + xlim(1, 31) + ylim(0, max(richness$value)) + ggtitle(grtitle)
        }
      }
  })
  
  output$graph1b <- renderPlot({
    
    if(is.null(d2_yr())){
      return(NULL)
    }else{
        if (input$year == "ALL"){
          grandtotal <- d2_yr() %>% 
            filter(variable == "grand.total.counted") %>%
            slice(1L) %>%
            select(value)
          nyears <- length(unique(d2_yr()$Year))
          grtitle <- paste(grandtotal, "butterflies observed over", nyears, "years", sep = " ")
          total <- d2_yr() %>% filter(variable == "year.total.counted")
          gr1b <- ggplot(data = total, aes(x = Year, y = value))
          gr1b + geom_point() + xlim(1995, 2014) + ylim(0, max(total$value)) + ggtitle(grtitle)
        }else{
          yrtotal <- d2_yr() %>% 
            filter(variable == "year.total.counted") %>%
            slice(1L) %>%
            select(value)
          nweeks <- length(unique(d2_yr()$Week))
          grtitle <- paste(yrtotal, "butterflies observed over", nweeks, "weeks", sep = " ")
          total <- d2_yr() %>% filter(variable == "surv.total.counted")
          gr1b <- ggplot(data = total, aes(x = Week, y = value)) 
          gr1b + geom_point() + xlim(1, 31) + ylim(0, max(total$value)) + ggtitle(grtitle)
        }
      }
  })
  # 
  # output$table <- renderTable({
  #   d3_spec.yr()
  # })
  # 
  output$table <- DT::renderDataTable({
    d3_spec.yr()
  })
  
  #####################################
  # TAB 2
  ##################################
  

  # update sites on map to include only where species selected is present

  speciesData <- reactive({
    if(is.null(input$species)){
      return(NULL)
    }else{
      if(input$species == ""){
        return(NULL)
      }else{
      ann.counts %>% filter(CommonName == input$species)
      }
    }
  })
  
  speciesIndex <- reactive({
    if(is.null(input$species)){
      return(NULL)
    }else{
      if(input$species == ""){
        return(NULL)
      }else{
      spec.sites %>% filter(CommonName == input$species)
      }
    }
  })
  
  mapData <- reactive({
    if(is.null(input$species)){
      return(NULL)
    }else{
      if(input$species == ""){
        return(NULL)
      }else{
      siteocc %>% filter(CommonName == input$species)
      }
    }
  })
  
  output$siteOutput <- renderUI({
    if(is.null(speciesData())){
      return(NULL)
    }else{
    selectInput("location2", "Location",
                c("", sort(unique(speciesData()$location))),
                # selected = "")
                selected = sort(unique(speciesData()$location))[1])
    }
  })
  
  output$Map2 <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron", options = tileOptions(minZoom = 6, maxZoom = 13)) %>% 
      setView(lng=-82.7, lat=40.6, zoom=6) 
  })
  
  
  observe({
    if(!is.null(mapData())){
      binpal <- colorBin("Reds", mapData()$Occupancy, 3, pretty = TRUE)
      leafletProxy("Map2", data = mapData()) %>%
        removeMarker(layerId = sitesonly$location) %>%
        addCircleMarkers(radius = ~10, color = "black",
                         fillColor = ~binpal(Occupancy),
                         stroke=TRUE, fillOpacity=.6, opacity = .5, 
                         layerId = mapData()$Description.x)
    }
  })
  
  
  # highlights selected marker
  observeEvent(input$Map2_marker_click, {
    p <- input$Map2_marker_click
    leafletProxy("Map2") %>% 
      setView(lng=p$lng, lat=p$lat, input$Map2_zoom) %>%
      addCircleMarkers(p$lng, p$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId ="Selected")
    
  })
  
  # updates location bar with clicked site
  observeEvent(input$Map2_marker_click, {
    p <- input$Map2_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location2)) updateSelectInput(session, "location2", selected=p$id)
      if(!is.null(input$location2) && input$location2!=p$id) updateSelectInput(session, "location2", selected=p$id)
    }
  })
  
  # marks selected site on map from location bar
  observeEvent(input$location2, {
    if(!is.null(input$Map2_marker_click)){
      p <- input$Map2_marker_click
      p2 <- subset(sitesonly, location==input$location2)
      if(input$location2 != p$id){
        leafletProxy("Map2") %>% 
          setView(lng=p2$lng, lat=p2$lat, input$Map2_zoom) %>%
          addCircleMarkers(p2$lng, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId ="Selected")
      }
    }else{
      p2 <- subset(sitesonly, location==input$location2)
      leafletProxy("Map2") %>% 
        setView(lng=p2$lng, lat=p2$lat, input$Map2_zoom) %>%
        addCircleMarkers(p2$lng, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId ="Selected")
    }
    
  })

  
  # this should be simplified in the data passed to app!!!!
  get_plot_data <- reactive({
    if(is.null(input$species)){
      return(NULL)
    }else{
      if(input$species == ""){
        return(NULL)
      }else{
        if(is.null(input$location2)){
          spec.trend %>%
            filter(CommonName == input$species) %>%
            mutate(IndexValue = exp(CollInd),
                   Index = "Statewide_index") %>%
            arrange(Year)
        }else{
          dat1 <- spec.trend %>%
            filter(CommonName == input$species) %>%
            mutate(Statewide_index = exp(CollInd))
          
          dat2 <- speciesIndex() %>% 
            filter(location == input$location2) %>%
            mutate(Site_index = TrpzInd)
          
          dat3 <- speciesData() %>%
            filter(location == input$location2) %>%
            mutate(Raw_count = TotalCount)
          
          dat4 <- merge(dat1, dat2, all.x = TRUE)
          dat5 <- merge(dat4, dat3, by = c("CommonName","Year"), all.x = TRUE, all.y = TRUE)
          dat6 <- dat5 %>%
            select(CommonName, Year, Raw_count, Site_index, Statewide_index) %>%
            gather(Index, IndexValue, Raw_count:Statewide_index)
          return(dat6)
        }
      }
    }  
  })
  
  
  output$graph2a <- renderPlot({
    if(is.null(get_plot_data())){
      return(NULL)
    }else{
      gr2a <- ggplot(data = get_plot_data(), aes(x = Year, y = IndexValue, group = Index, color = Index))
      gr2a + geom_point(size = 4) + facet_wrap( ~ Index, ncol = 1, scales = "free_y") + xlim(1995, 2014) + 
        expand_limits(x = 1995, y = 0) + theme(legend.position="none")
    }
  })
  
  
################################################################
  # tab 3: Phenology plots

  # update sites on map to include only where species selected is present
  
  mapData3 <- reactive({
    if(is.null(input$species3)){
      return(NULL)
    }else{
      if(input$species3 == ""){
        return(NULL)
      }else{
        phenology %>% dplyr::filter(sp == input$species3) %>%
          group_by(SiteID) %>%
          mutate(YearsData = length(unique(Year))) %>%
          ungroup() %>%
          select(Description.x, lat, lng, YearsData) %>%
          distinct()
      }
    }
  })
  
  output$siteOutput3 <- renderUI({
    if(is.null(mapData3())){
      return(NULL)
    }else{
      selectInput("location3", "Location",
                     c("ALL", sort(mapData3()$Description.x)),
                     selected = "ALL")
                     # selected = sort(mapData3()$Description.x)[1])
    }
  })
  
  output$Map3 <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron", options = tileOptions(minZoom = 6, maxZoom = 13)) %>% 
      setView(lng=-82.7, lat=40.6, zoom=6) 
  })
  
  output$yearOutput3 <- renderUI({
    if(is.null(input$location3)){
      return(NULL)
    }else{
      if(input$location3 == "ALL"){
        yearphen <- unique(phenology$Year[which(phenology$sp == input$species3)])
      }else{
        yearphen <- unique(phenology$Year[which(phenology$sp == input$species3 & 
                                                  phenology$Description.x == input$location3)])
      }
      selectInput("year3", "Years", c("ALL", sort(yearphen)),selected = "ALL")
    }
  })
  
  observe({
    if(!is.null(mapData3())){
      binpal <- colorBin("Reds", mapData3()$YearsData, 3, pretty = TRUE)
      leafletProxy("Map3", data = mapData3()) %>%
        removeMarker(layerId = sitesonly$location) %>%
        addCircleMarkers(radius = ~10, color = "black",
                         fillColor = ~binpal(YearsData),
                         stroke=TRUE, fillOpacity=.6, opacity = .5, 
                         layerId = mapData3()$Description.x)
    }
  })
  
  
  # highlights selected marker
  observeEvent(input$Map3_marker_click, {
    p <- input$Map3_marker_click
    leafletProxy("Map3") %>% 
      setView(lng=p$lng, lat=p$lat, input$Map3_zoom) %>%
      addCircleMarkers(p$lng, p$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId ="Selected")
    
  })
  
  
  
  # updates location bar with clicked site
  observeEvent(input$Map3_marker_click, {
    p <- input$Map3_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location3)) updateSelectInput(session, "location3", selected=p$id)
      if(!is.null(input$location3) && input$location3!=p$id) updateSelectInput(session, "location3", selected=p$id)
    }
  })
  
  # marks selected site on map from location bar
  observeEvent(input$location3, {
    if(!is.null(input$Map3_marker_click)){
      p <- input$Map3_marker_click
      p2 <- subset(mapData3(), Description.x == input$location3)
      if(input$location3 != p$id){
        leafletProxy("Map3") %>% 
          setView(lng=p2$lng, lat=p2$lat, input$Map3_zoom) %>%
          addCircleMarkers(p2$lng, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId ="Selected")
      }
    }else{
      p2 <- subset(mapData3(), Description.x == input$location3)
      leafletProxy("Map3") %>% 
        setView(lng=p2$lng, lat=p2$lat, input$Map3_zoom) %>%
        addCircleMarkers(p2$lng, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId ="Selected")
    }
    
  })

  
  get_phen_spec <- reactive({
    if(is.null(input$species3)){
      return(NULL)
    }else{
      if(input$species3 == ""){
        return(NULL)
      }else{
        phenology %>% filter(sp == input$species3) %>%
          mutate(SiteYear = paste(SiteID, Year, sep = "_"))
      }
    }
  })
  
  get_phen_site <- reactive({
    if(is.null(input$location3)){
      return(NULL)
    }else{
      if(input$location3 == "ALL"){
        return(NULL)
      }else{
        get_phen_spec() %>% filter(Description.x == input$location3)
      }
    }
  })
  
  get_phen_year <- reactive({
    if(is.null(input$year3)){
      return(NULL)
    }else{
      if(input$year3 != "ALL" & input$location3 == "ALL"){
        get_phen_spec() %>% filter(Year == input$year3)
      }else{
        if(input$year3 == "ALL" & input$location3 == "ALL"){
          return(NULL)
        }else{
          if(input$year3 != "ALL" & input$location3 != "ALL"){
            get_phen_site() %>% filter(Year == input$year3)
          }else{
            if(input$year3 == "ALL" & input$location3 != "ALL"){
              return(NULL)
            }
          }
        }
      }
    }
  })
  
  get_temp_spec <- reactive({
    if(is.null(mapData3())){
      return(NULL)
    }else{
    out <- temperature[which(temperature$Description.x %in% mapData3()$Description.x), ]
    out$SiteYear <-  paste(out$SiteID, out$year, sep = "_")
    return(out)
    }
  })

  get_temp_site <- reactive({

      if(input$location3 == "ALL"){
        return(NULL)
      }else{
        get_temp_spec() %>% dplyr::filter(Description.x == input$location3)
      }

  })
  
  get_temp_year <- reactive({

      if(input$year3 != "ALL" & input$location3 == "ALL"){
        get_temp_spec() %>% dplyr::filter(year == input$year3)
      }else{
        if(input$year3 == "ALL" & input$location3 == "ALL"){
          return(NULL)
        }else{
          if(input$year3 != "ALL" & input$location3 != "ALL"){
            get_temp_site() %>% dplyr::filter(year == input$year3)
          }else{
            if(input$year3 == "ALL" & input$location3 != "ALL"){
              return(NULL)
            }
          }
        }
      }
    
  })
  
  
  get_gdd_spec <- reactive({
    if(is.null(mapData3())){
      return(NULL)
    }else{
    out <- gdd[which(gdd$Description.x %in% mapData3()$Description.x), ]
    out$SiteYear <-  paste(out$SiteID, out$year, sep = "_")
    return(out)
    }
  })
  
  get_gdd_site <- reactive({
    if(is.null(input$location3)){
      return(NULL)
    }else{
      if(input$location3 == "ALL"){
        return(NULL)
      }else{
        get_gdd_spec() %>% dplyr::filter(Description.x == input$location3)
      }
    }
  })
  
  get_gdd_year <- reactive({
    if(is.null(input$year3)){
      return(NULL)
    }else{
      if(input$year3 != "ALL" & input$location3 == "ALL"){
        get_gdd_spec() %>% dplyr::filter(year == input$year3)
      }else{
        if(input$year3 == "ALL" & input$location3 == "ALL"){
          return(NULL)
        }else{
          if(input$year3 != "ALL" & input$location3 != "ALL"){
            get_gdd_site() %>% dplyr::filter(year == input$year3)
          }else{
            if(input$year3 == "ALL" & input$location3 != "ALL"){
              return(NULL)
            }
          }
        }
      }
    }
  })
  
  
  output$graph3phen <- renderPlot({
    if(is.null(get_phen_spec())){
      return(NULL)
    }else{
        
        if(input$location3 == "ALL" & input$year3 == "ALL"){
          gr3phen <- ggplot(data = get_phen_spec(), aes(x = Day_of_Year, y = Gamma)) + theme_bw()
          gr3phen + geom_line(aes(group = SiteYear), color = "#a6bddb") +
            stat_summary(geom="smooth", fun.y="mean", color = "#2b8cbe")
        }else{
          if(input$location3 == "ALL" & input$year3 != "ALL"){
            gr3phen <- ggplot(data = get_phen_spec(), aes(x = Day_of_Year, y = Gamma)) + theme_bw()
            gr3phen + geom_line(aes(group = SiteYear), color = "#a6bddb") +
              stat_summary(geom="smooth", fun.y="mean", color = "#2b8cbe") +
              geom_line(data = get_phen_year(), aes(group = Description.x), color = "#fa9fb5", size = 1.2) +
              stat_summary(data = get_phen_year(), geom = "smooth", fun.y = "mean", color = "#c51b8a", size = 1.75)
            
          }else{
            if(input$location3 != "ALL" & input$year3 == "ALL"){
              gr3phen <- ggplot(data = get_phen_spec(), aes(x = Day_of_Year, y = Gamma)) + theme_bw()
              gr3phen + geom_line(aes(group = SiteYear), color = "#a6bddb") +
                stat_summary(geom="smooth", fun.y="mean", color = "#2b8cbe") +
                geom_line(data = get_phen_site(), aes(group = Year), color = "#fec44f", size = 1.2) +
                stat_summary(data = get_phen_site(), geom = "smooth", fun.y = "mean", color = "#d95f0e", size = 1.5) 
              
            }else{
              gr3phen <- ggplot(data = get_phen_spec(), aes(x = Day_of_Year, y = Gamma)) + theme_bw()
              gr3phen + geom_line(aes(group = SiteYear), color = "#a6bddb") +
                stat_summary(geom="smooth", fun.y="mean", color = "#2b8cbe") +
                geom_line(data = get_phen_site(), aes(group = Year), color = "#fec44f", size = 1.2) +
                stat_summary(data = get_phen_site(), geom = "smooth", fun.y = "mean", color = "#d95f0e", size = 1.2) +
                geom_line(data = get_phen_year(), color = "#c51b8a", size = 1.75)
            }
          }
        }
      }
    })

  output$graph3a <- renderPlot({

    if(is.null(get_temp_spec())){
      return(NULL)
      }else{
      if(input$location3 == "ALL" & input$year3 == "ALL"){
        gr3a <- ggplot(data = get_temp_spec(), aes(x = month, y = MeanTemperature)) + theme_bw()
        gr3a + geom_jitter(alpha = 0.3, color = "#a6bddb") +
          stat_summary(aes(group = month), fun.y = "mean", color = "#2b8cbe", geom = "point", size = 3)
      }else{
        if(input$location3 == "ALL" & input$year3 != "ALL"){
          gr3a <- ggplot(data = get_temp_spec(), aes(x = month, y = MeanTemperature)) + theme_bw()
          gr3a + geom_jitter(alpha = 0.3, color = "#a6bddb") +
            stat_summary(aes(group = month), fun.y = "mean",  color = "#2b8cbe", geom = "point", size = 1) +
            geom_jitter(data = get_temp_year(), alpha = 0.9, color = "#fa9fb5", size = 3) +
            stat_summary(data = get_temp_year(), aes(group = month), fun.y = "median", color = "#c51b8a", geom = "point", size = 4)

        }else{
          if(input$location3 != "ALL" & input$year3 == "ALL"){
            gr3a <- ggplot(data = get_temp_spec(), aes(x = month, y = MeanTemperature)) + theme_bw()
            gr3a + geom_jitter(alpha = 0.3, color = "#a6bddb") +
              stat_summary(aes(group = month), fun.y = "mean",  color = "#2b8cbe",geom = "point", size = 1) +
              geom_jitter(data = get_temp_site(), alpha = 0.9, color = "#fec44f", size = 3) +
              stat_summary(data = get_temp_site(), aes(group = month), fun.y = "median", color = "#d95f0e", geom = "point", size = 4)

          }else{
            gr3a <- ggplot(data = get_temp_spec(), aes(x = month, y = MeanTemperature)) + theme_bw()
            gr3a + geom_jitter(alpha = 0.3, color = "#a6bddb") +
              stat_summary(aes(group = month), fun.y = "mean",  color = "#2b8cbe",geom = "point", size = 1) +
              geom_jitter(data = get_temp_site(), alpha = 0.9, color = "#fec44f", size = 3) +
              stat_summary(data = get_temp_site(), aes(group = month), fun.y = "median", color = "#d95f0e", geom = "point", size = 3) +
              geom_jitter(data = get_temp_year(), aes(group = month), color = "#c51b8a", size = 5)
        }
      }
    }
    }
  })
  
  #gdd
  output$graph3b <- renderPlot({
    if(is.null(get_gdd_spec())){
      return(NULL)
    }else{
    #   if(is.null(input$location3) | is.null(input$year3)){
    #     return(NULL)
    #   }else{
        if(input$location3 == "ALL" & input$year3 == "ALL"){
          gr3b <- ggplot(data = get_gdd_spec(), aes(x = Day_of_Year, y = Cumulative_degree_days)) + theme_bw()
          gr3b + geom_line(aes(group = SiteYear), color = "#a6bddb") +
            stat_summary(geom="smooth", fun.y="mean", color = "#2b8cbe")
        }else{
          if(input$location3 == "ALL" & input$year3 != "ALL"){
            gr3b <- ggplot(data = get_gdd_spec(), aes(x = Day_of_Year, y = Cumulative_degree_days)) + theme_bw()
            gr3b + geom_line(aes(group = SiteYear), color = "#a6bddb") +
              stat_summary(geom="smooth", fun.y="mean", color = "#2b8cbe") + 
              geom_line(data = get_gdd_year(), aes(group = Description.x), color = "#fa9fb5", size = 1.2) +
              stat_summary(data = get_gdd_year(), geom = "smooth", fun.y = "mean", color = "#c51b8a", size = 1.75)
            
          }else{
            if(input$location3 != "ALL" & input$year3 == "ALL"){
              gr3b <- ggplot(data = get_gdd_spec(), aes(x = Day_of_Year, y = Cumulative_degree_days)) + theme_bw()
              gr3b + geom_line(aes(group = SiteYear), color = "#a6bddb") +
                stat_summary(geom="smooth", fun.y="mean", color = "#2b8cbe") + 
                geom_line(data = get_gdd_site(), aes(group = year), color = "#fec44f", size = 1.2) +
                stat_summary(data = get_gdd_site(), geom = "smooth", fun.y = "mean", color = "#d95f0e", size = 1.75) 
              
            }else{
              gr3b <- ggplot(data = get_gdd_spec(), aes(x = Day_of_Year, y = Cumulative_degree_days)) + theme_bw()
              gr3b + geom_line(aes(group = SiteYear), color = "#a6bddb") +
                stat_summary(geom="smooth", fun.y="mean", color = "#2b8cbe") + 
                geom_line(data = get_gdd_site(), aes(group = year), color = "#fec44f") +
                stat_summary(data = get_gdd_site(), geom = "smooth", fun.y = "mean", color = "#d95f0e") +
                geom_line(data = get_gdd_year(), color = "#c51b8a", size = 1.75)
            }
          }
        }
    #   }
    }
  })
  
})

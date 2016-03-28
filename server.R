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
      selectizeInput("year", "Years",
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
    selectizeInput("location2", "Location",
                sort(unique(speciesData()$location)),
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
      if(is.null(input$location2)){
        spec.trend %>%
          filter(CommonName == input$species) %>%
          mutate(IndexValue = exp(CollInd),
                 Index = "UKBMS_collated") %>%
          arrange(Year)
      }else{
        dat1 <- spec.trend %>%
          filter(CommonName == input$species) %>%
          mutate(UKBMS_collated = exp(CollInd))
        
        dat2 <- speciesIndex() %>% 
          filter(location == input$location2) %>%
          mutate(UKBMS_site = TrpzInd)
        
        dat3 <- speciesData() %>%
          filter(location == input$location2) %>%
          mutate(Raw_count = TotalCount)
           
        dat4 <- merge(dat1, dat2, all.x = TRUE)
        dat5 <- merge(dat4, dat3, by = c("CommonName","Year"), all.x = TRUE, all.y = TRUE)
        dat6 <- dat5 %>%
          select(CommonName, Year, UKBMS_collated, UKBMS_site, Raw_count) %>%
          gather(Index, IndexValue, UKBMS_collated:Raw_count)
        return(dat6)
      }
    }
  })
  
  
  output$graph2a <- renderPlot({
   gr2a <- ggplot(data = get_plot_data(), aes(x = Year, y = IndexValue, group = Index, color = Index))
   gr2a + geom_point(size = 5)
  })
  
  
})

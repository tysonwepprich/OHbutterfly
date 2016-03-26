shinyServer(function(input, output, session){
  
  output$Map <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron", options = tileOptions(minZoom = 6, maxZoom = 13)) %>% 
      setView(lng=-82.7, lat=40.6, zoom=6) %>%
      addCircleMarkers(data=sites2map, radius = ~10, 
                       # color = ~palfun(PopClass), 
                       colo = "navy",
                       stroke=FALSE, fillOpacity=0.5, layerId = ~location)
  })
  
  # if site is clicked, remove and replace with different marker, recenter map
  observeEvent(input$Map_marker_click, {
    p <- input$Map_marker_click
    if(p$id=="Selected"){
      leafletProxy("Map") %>% removeMarker(layerId="Selected")
    } else {
      leafletProxy("Map") %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% 
        addCircleMarkers(p$lng, p$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected")
    }
  })
  
  # if site is clicked, make it selected in the input bar for site
  observeEvent(input$Map_marker_click, {
    p <- input$Map_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location)) updateSelectInput(session, "location", selected=p$id)
      if(!is.null(input$location) && input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })
  
  observeEvent(input$location, {
    p2 <- subset(sites2map, location==input$location)[1,]
    leafletProxy("Map") %>% setView(lng=p2$lng, lat=p2$lat, input$Map_zoom) %>% 
      addCircleMarkers(p2$lng, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected")
  })
  
  get_ddf <- reactive({
    if(!is.null(input$variable)){
      sites2map[which(sites2map$location == input$location & variable == input$variable), ]
    }else{
      sites2map[which(sites2map$location == input$location & variable == "surv.richness"), ]
    }
  })
  
  observe({
    updateSelectInput(session, "year", choices = sort(unique(get_ddf()$Year)))
  })
  
  d2_yr <- reactive({ 
    if (is.null(input$year)) {
      return(NULL)
    }else{
      subset(get_ddf(), Year==input$year) 
    }    
  })
  
  d3_spec.yr <- reactive({
    if(is.null(input$year)){
      return(NULL)
    }else{
      spec.sites %>% 
        filter(location == input$location, Year == input$year) %>%
        select(CommonName, RawSum)
    }
  })
  
  output$graph <- renderPlot({
    plot(d2_yr()$Week, d2_yr()$value)
    # main = paste("Total species in", input$year, "is", d3_var()$year.richness[1], sep = " "))
  })
  
  output$table <- DT::renderDataTable({
    d3_spec.yr()
  })
  
  #####################################
  # TAB 2
  ##################################
  
  # update sites on map to include only where species selected is present
  
  output$Map2 <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron", options = tileOptions(minZoom = 6, maxZoom = 13)) %>% 
      setView(lng=-82.7, lat=40.6, zoom=6)
  })
  
  speciesData <- reactive({
    if(is.null(input$species)){
      return(NULL)
    }else{
    subset(spec.sites, CommonName == input$species)
    }
  })
  
  output$siteOutput <- renderUI({
    if(is.null(input$species)){
      return(NULL)
    }else{
    selectizeInput("location2", "Location",
                sort(unique(speciesData()$location)),
                selected = "")
    }
  })
  
  observeEvent(input$species, {
    leafletProxy("Map2") %>%
      clearShapes() %>%
      addCircleMarkers(data=speciesData(), radius = ~10, 
                       colo = "navy",
                       stroke=FALSE, fillOpacity=0.5, layerId = ~location)
  })
  
  
  observeEvent(input$Map2_marker_click, {
    p <- input$Map2_marker_click
    if(p$id=="Selected"){
      leafletProxy("Map2") %>% removeMarker(layerId="Selected")
    } else {
      leafletProxy("Map2") %>% setView(lng=p$lng, lat=p$lat, input$Map2_zoom) %>% 
        addCircleMarkers(p$lng, p$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected")
    }
  })
  
  observeEvent(input$Map2_marker_click, {
    p <- input$Map2_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location2)) updateSelectInput(session, "location2", selected=p$id)
      if(!is.null(input$location2) && input$location!=p$id) updateSelectInput(session, "location2", selected=p$id)
    }
  })
  
  observeEvent(input$location2, {
    p2 <- subset(spec.sites, location==input$location2)[1,]
      leafletProxy("Map2") %>% setView(lng=p2$lng, lat=p2$lat, input$Map2_zoom) %>% 
        addCircleMarkers(p2$lng, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected")
  })

  
  get_trenddf <- reactive({
    if(is.null(input$species)){
      return(NULL)
    }else{
    spec.trend %>%
        filter(CommonName == input$species) %>%
        mutate(StatewideIndex = exp(CollInd)) %>%
        arrange(Year)
    }
  })
  
  get_spec_site <- reactive({
    if(is.null(input$species)){
      return(NULL)
    }else{
      if(is.null(input$location2)){
        return(NULL)
      }else{
        speciesData() %>% 
          filter(location == input$location2) %>%
          arrange(Year)
      }
    }
  })
  
  
  output$graph2a <- renderPlot({
    plot(get_trenddf()$Year, get_trenddf()$StatewideIndex, type = "l", col = "blue")
  })
  
  output$graph2b <- renderPlot({
    plot(get_spec_site()$Year, get_spec_site()$TrpzInd, type = "l", col = "red")
  })
  
  
})

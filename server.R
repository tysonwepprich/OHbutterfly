shinyServer(function(input, output, session){

  output$Map <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron", options = tileOptions(minZoom = 6, maxZoom = 13)) %>% 
      setView(lng=-82.7, lat=40.6, zoom=6) %>%
      addCircleMarkers(data=sites2map, radius = ~10, 
                       # color = ~palfun(PopClass), 
                       colo = "navy",
                       stroke=FALSE, fillOpacity=0.5, layerId = ~location)
  })
  
  observeEvent(input$Map_marker_click, {
    p <- input$Map_marker_click
    if(p$id=="Selected"){
      leafletProxy("Map") %>% removeMarker(layerId="Selected")
    } else {
      leafletProxy("Map") %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% 
        addCircleMarkers(p$lng, p$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected")
    }
  })
  
  observeEvent(input$Map_marker_click, {
    p <- input$Map_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location)) updateSelectInput(session, "location", selected=p$id)
      if(!is.null(input$location) && input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })
  
  observeEvent(input$location, {
    # p <- input$Map_marker_click
    p2 <- subset(sites2map, location==input$location)[1,]
    # if(nrow(p2)==0){
    #   leafletProxy("Map") %>% removeMarker(layerId="Selected")
    # } else if(input$location!=p$id){
      leafletProxy("Map") %>% setView(lng=p2$lng, lat=p2$lat, input$Map_zoom) %>% 
        addCircleMarkers(p2$lng, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected")
    # }
  })
  
  get_ddf <- reactive({
    sites2map[which(sites2map$location == input$location & variable == input$variable), ]
  })
  
  observe({
    updateSelectInput(session, "year", choices = sort(unique(get_ddf()$Year)))
  })
  
  d2_yr <- reactive({ subset(get_ddf(), Year==input$year) })

  
  output$graph <- renderPlot({
    plot(d2_yr()$Week, d2_yr()$value)
    
    # main = paste("Total species in", input$year, "is", d3_var()$year.richness[1], sep = " "))
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
    subset(spec.sites, CommonName == input$species)
  })
  
  observeEvent(input$species, {
    leafletProxy("Map2") %>%  addCircleMarkers(data=speciesData(), radius = ~10, 
                                               colo = "navy",
                                               stroke=FALSE, fillOpacity=0.5, layerId = ~location)
    })
  
  observe({
    updateSelectInput(session, "location2", choices = sort(unique(speciesData()$location)))
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
      if(is.null(input$location)) updateSelectInput(session, "location2", selected=p$id)
      if(!is.null(input$location) && input$location!=p$id) updateSelectInput(session, "location2", selected=p$id)
    }
  })
  
  observeEvent(input$location2, {
    # p <- input$Map2_marker_click
    p2 <- subset(spec.sites, location==input$location2)[1,]
    # if(nrow(p2)==0){
    #   leafletProxy("Map2") %>% removeMarker(layerId="Selected")
    # } else if(input$location2 != p$id){
      leafletProxy("Map2") %>% setView(lng=p2$lng, lat=p2$lat, input$Map2_zoom) %>% 
        addCircleMarkers(p2$lng, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected")
    # }
  })
  
  observeEvent(input$species, {
    leafletProxy("Map2") %>%
      clearShapes() %>%
      addCircleMarkers(data=speciesData(), radius = ~10, 
                       colo = "navy",
                       stroke=FALSE, fillOpacity=0.5, layerId = ~location)
  })
  
  
  get_trenddf <- reactive({
    spec.trend[which(spec.trend$CommonName == input$species), ]
  })
  
  
  output$graph2 <- renderPlot({
    par(mfrow=c(1,2))
    plot(get_trenddf()$Year, get_trenddf()$CollInd, type = "l", col = "blue")
    observeEvent(input$species,{
      plot(speciesData()$Year, speciesData()$TrpzInd, col = "red")
    })
  })
  
  
})

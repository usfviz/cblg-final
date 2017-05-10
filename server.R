shinyServer(function(input, output) {
  
  # ------------------------------------------------------- #
  # PANEL 1: MAP OF SF
  # ------------------------------------------------------- #
  # Subset Data ---------------------------------------------------------------------- #
  
  subsetData <- reactive({
    if (input$family == 'ALL') {
      new_data <- data.in[(data.in$year == input$time),]
    } else {
      new_data <- data.in[(data.in$year == input$time) & (data.in$family == input$family),]
    }
    new_data$this.year <- factor(ifelse(new_data$year == input$time, 'green', 'grey'))
    return(new_data)
  })

  # Make Color Palette --------------------------------------------------------------- #
  pal <- colorFactor(tree.green.pal , domain = unique(data.in$family))
  outline <- colorFactor(c("transparent",'red') , domain = c('Public', 'Private'))
  
 
  
  getColor <- function(quakes) {
    sapply(quakes$mag, function(mag) {
      if(mag <= 4) {
        "green"
      } else if(mag <= 5) {
        "orange"
      } else {
        "red"
      } })
  }
  
  # Text Output ---------------------------------------------------------------------- #
  output$year.facts <-  renderUI({
    HTML(paste0('<b>Year: </b>', input$time, "<br> <b>Trees Planted: </b>", nrow(subsetData())))
  })
  
  # Main Map  ----------------------------------------------------------------------- #
  output$sf.trees <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      setView( -122.4347, 37.78, zoom = 12) 
  })
  
  observe({
    leafletProxy("sf.trees") %>%
      clearMarkers() %>%
      addCircleMarkers(data = subsetData(),
                       lng         = ~Longitude,
                       lat         = ~Latitude,
                       radius      = 5,
                       color       = ~pal(family),
                       stroke      = FALSE,
                       fillOpacity = .5,
                       popupOptions = popupOptions(closeOnClick = F),
                       popup       = ~paste0("<b><font color = '#36802D'> Scientific Name: </font></b>", species.science,
                                            "<br><b><font color = '#36802D'> Common Name: </font></b>", species.common),
                       group      = "default"
                       )
  })
  
  observe({
    
    if (input$care %in% c('Public', 'Private')) {
      data.highlight <- subsetData() %>% filter(Owner == input$care)
      leafletProxy("sf.trees") %>%   
        clearGroup("owner") %>%
        addCircleMarkers(data = data.highlight,
                                     lng         = ~Longitude,
                                     lat         = ~Latitude,
                                     radius      = 5,
                                     fillCol     = FALSE,
                                     fillOpacity = 0,
                                     color       = 'blue',
                                     stroke      = TRUE,
                                     weight      = 1,
                                     group       = "owner"
        )
    } else if (input$care == 'All'){
      leafletProxy("sf.trees") %>%  clearGroup("owner")
    }
  })
  

  # ------------------------------------------------------- #
  # PANEL 2: BAR PLOT OF SPECIES
  # ------------------------------------------------------- #
  
  selected <- reactive({ 
    years.and.species[(years.and.species$species.common %in% input$types) & (years.and.species$year <= input$time),]
  })
  
  output$bars <- renderPlot({ggplot(selected(), aes(x = species.common, y = count, fill = decade)) + geom_col() + scale_fill_brewer(palette = 'GnBu') + labs(x = "Species", y = "Count", title = "Trees Over Time") + scale_y_continuous(limits = c(0, y_max))
  })
  
  # ------------------------------------------------------- #
  # PANEL 3: NEIGHBORHOOD DENSITY MAP
  # ------------------------------------------------------- #
  
  time.tree.density <- reactive({
    tree.density <- tree.list %>% 
    filter(year <= input$time) %>%
    group_by(hood.names) %>%
    summarise(n_trees  = n(),
              max_area = max(scaled.area), 
              density  = n_trees / max_area) %>%
    filter(hood.names != '')  %>%
    rename(Name = hood.names)
  
  sf.hoods.only <- merge(sf.hoods.only, tree.density, by="Name", all=T)
  
  return(sf.hoods.only)
  })
  
  output$hood.map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    setView( -122.4347, 37.78, zoom = 13)
  })
  
  observe({
    bins <- c(0, 2000, 4000, 6000, Inf)
    pal <- colorBin("Greens", domain = time.tree.density()$density, bins = bins)
    
    leafletProxy("hood.map") %>%
    clearShapes() %>%
    addPolygons(data= time.tree.density(),
                color = ~pal(density), weight = .5, fillOpacity = .5,
                highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
                label = ~time.tree.density()@data$Name,
                labelOptions = labelOptions(noHide = T)) %>%
      addLegend(pal = pal, 
                title = "Neighborhood Tree Density", 
                values = time.tree.density()@data[['density']], 
                opacity = 0.7, 
                position = 'topright',
                layerId = "hood.stuff")
    
  })
 # ------------------------------------------------------- #
  # PANEL 3 (or 4?): BUBBLE PLOT OF NEIGHBORHOODS
  # ------------------------------------------------------- #
  
  bubble <- reactive({
    
  bubble.labs <- list(Median.income = "Income", Area = "Area", Invasive.pct = "Percent Invasive", Population = "Population", Tree.count = "Tree count", Pop.density = "Population density", Tree.density = "Tree density")
    
    x = names(bubble.labs[bubble.labs == input$bub.x])
    y = names(bubble.labs[bubble.labs == input$bub.y])
    color = names(bubble.labs[bubble.labs == input$bub.color])
    size = names(bubble.labs[bubble.labs == input$bub.size])
    
    selected <- cbind(df.full, data.frame(x = df.full[,x], y = df.full[,y], size = df.full[,size], color = df.full[,color]))
    
    xmin <- min(selected$x)
    xmax <- max(selected$x)
    ymin <- min(selected$y)
    ymax <- max(selected$y)
    
    all_values <- function(x) {
      if(is.null(x)) return(NULL)
      row <- df.full[df.full$Neighborhood == x$Neighborhood, ]
      paste0("Neighborhood: <b>", row$Neighborhood, "</b><br>",
             "Median income: <b>$", format(as.integer(row$Income), big.mark = ','), "</b><br>",
             "Trees: <b>", format(row$Tree.count, big.mark = ','), "</b><br>",
             "Population: <b>", format(row$Population, big.mark = ','), "</b>")
    }
    
    selected %>%
      ggvis(~x, ~y) %>% 
      layer_points(fill = ~color, size = ~size, key := ~Neighborhood) %>%
      add_tooltip(all_values, "click") %>% 
      add_tooltip(function(data){
        paste0("<t><b>", data$Neighborhood, "</b>")
      }, "hover") %>%
      add_axis("x", values = list(xmin, xmax), title = input$bub.x) %>% 
      add_axis("y", values = list(ymin, ymax), title = input$bub.y) %>%
      add_legend("fill", title = input$bub.color) %>%
      hide_legend("size") %>%
      scale_numeric("fill", range = c("yellowgreen", "darkgreen"))
  })
  bubble %>% bind_shiny('ggvis', 'ggvis_ui')
})

## Create Drill Down Functionality using Plotly
function(input, output, session) {
  
  ## Set the Initial Drill Down Level
  drillDown <- reactiveValues(level="Region",
                              level_id = 1,
                              event_data = NULL)
  
  ## Change Level When Drill Down Level Id Changes
  levels <- c("Region", "Country", "City")
  observe({
    drillDown[["level"]] <- levels[drillDown[["level_id"]]]
  })
  
  ## Reactive Function to Set Data Based on Drill Down Level
  chartData <- reactive({
    if(drillDown[["level"]]=="Region"){
      dt <- cities[,list(Population = sum(Population)),list(Region)]
      names(dt)[1] <- "Category"
    }else{
      if(drillDown[["level"]]=="Country"){
        dt <- cities[Region == drillDown[["selected"]], list(Population = sum(Population)),list(Country)]
        names(dt)[1] <- "Category"
      } else{
        if(drillDown[["level"]]=="City"){
          dt <- cities[Country==drillDown[["selected"]], list(Population = sum(Population)), list(City)]
          names(dt)[1] <- "Category"
        } else return(NULL)
      } 
    }
    return(dt)
  })
  
  ## Plot the Data
  output$plot <- renderPlotly({
    plot_ly(chartData(), x = ~Population, y=~Category, type="bar", orientation = "h", 
            hoverinfo = "text", text=~paste0(drillDown[["level"]],": ",Category,
                                             "<br> Population: ", format(Population,scientific = F, big.mark = ",")),
            marker = list(color = 'rgb(223, 159, 155)',
                          line = list(color = 'rgb(18, 7, 6)', width = 0.5))) %>% config(displayModeBar=T) %>%
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          zeroline = FALSE,
                          domain = c(0.15, 1)),
             yaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE),
             margin = list(l = 40, r = 10, t = 10, b = 40),
             showlegend = FALSE)     %>%
      # labeling the y-axis
      add_annotations(xref = 'paper', yref = 'Category', x = 0.14, y = ~Category,
                      xanchor = 'right',
                      text = ~Category,
                      font = list(family = 'calibre', size = 12,
                                  color = 'rgb(255, 21, 0)'),
                      showarrow = FALSE, align = 'right')
  })
  
  ## Go Back Link
  output$go_back <- renderUI({
    if(drillDown[["level_id"]] != 1){
      actionLink("go_back", "Go Back",  icon("angle-double-left"))
    }
  })
  
  observeEvent(input$go_back, {
    drillDown[["level_id"]] <- drillDown[["level_id"]] - 1
    drillDown[["selected"]] <- ifelse(drillDown[["level_id"]] == 1, "", drillDown[["previous_selection"]])
    
  })
  
  ## Change Drill Down Level Based on Plotly Click Event Data
  observeEvent(event_data("plotly_click"),{
    if(drillDown[["level_id"]] == length(levels)) return(NULL)
    drillDown[["level_id"]] <- drillDown[["level_id"]] + 1 
    drillDown[["previous_selection"]] <- drillDown[["selected"]]
    drillDown[["selected"]] <- as.character(chartData()[as.numeric(event_data("plotly_click")[2])+1, "Category"])
  })
  
  ## Display Click (For Debugging)
  output$level_id <- renderPrint({
    cat("level_id: ", drillDown[["level_id"]])
  })
  
  output$level <- renderPrint({
    cat("level: ",drillDown[["level"]])
  })
  output$selected <- renderPrint({
    cat("selected: ", drillDown[["selected"]])
  })
  
}
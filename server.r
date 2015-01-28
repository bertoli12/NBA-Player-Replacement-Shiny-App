library(shiny)
library(RSQLite)
library(RODBC)
library(ggvis)
library(jpeg)
library(png)
library(RANN)
library(data.table)

dt.raw <- as.data.table(read.csv('L:\\public\\share\\hcms\\Analyst-Shared\\Peter Bertoli\\Garbage FIles\\Shiny\\Summary.csv'))

shinyServer(function(input, output, session) {
  
  ### Tooltip ###
  Player_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$Player)) return(NULL)
    
    all_Player <- isolate(dt.cluster.cln())
    Player.tt <- all_Player[all_Player$Player == x$Player, ]
    
    paste0("<b>",Player.tt$Player,"</b><br>",
           Player.tt$Team)  
  }
  
  
  ### Cluster Analysis ###
  dt.cluster.cln <- reactive({
      if (is.null(input$Player))
        return()
    
      Player.test <- input$Player
      metric.list <- as.character(input$metrics)
      metric.list <- append(as.character(list('Player','Team')),metric.list)
      n <- input$matches
      
      dt.query <- subset(dt.raw, select = c(metric.list))   ### Create Data Frames of Selected Metrics
      dt.test <- dt.query[Player.test == Player]
      dt.test$Player <- NULL
      dt.test$Team <- NULL
      dt.query.cln <- dt.query[Player.test != Player]
      dt.query.cln$Player <-NULL
      dt.query.cln$Team <-NULL
      
      cluster <- nn2(dt.query.cln,query=dt.test,k=n+1)   ### Run Cluster
      
      matches <- as.data.frame(cluster$nn.idx)   ### Get Nearest Neighbors
      matches.cln <- matches[1,]
      names(matches.cln) <- NULL
      matches.cln <- unlist(c(matches.cln))
      
      distance <- as.data.frame(cluster$nn.dists)   ### Get Distances
      distance.cln <- distance[1,]
      names(distance.cln) <- NULL
      distance.cln <- unlist(c(distance.cln))
      
      dt.test <- dt.query[Player.test == Player]   ### Start Final Output
      dt.query.cln <- dt.query[Player.test != Player]
     
      for(i in matches.cln){
        dt.test <- rbind(dt.test,dt.query[i:i,])} 
      
      row <- 2
      dt.test$Distance[1] <- 0

      for(i in distance.cln){
        dt.test$Distance[row] <- i
        row <- row + 1}

      for(i in 2:nrow(dt.test)-1){        
        player.row <- i      
        if (dt.test$Player[player.row] == Player.test & dt.test$Distance[player.row] > 0){
          dt.test <- dt.test[-c(player.row), ]
        }
      }
      
      dt.cluster <- dt.test
    })
  
  
    ### Metrics List Output ###
  
    output$xvar <- renderUI({
      selectInput("xvar", "X-axis variable",choices = append(as.character(list('Distance')),input$metrics), selected = "Distance")
    })
    
    output$yvar <- renderUI({
      selectInput("yvar", "Y-axis variable",choices = append(as.character(list('Distance')),input$metrics), selected = "GameAvgPTS")
    })
  
    ### GGVIS Plot ###  
    PlayerChart <- reactive({
      if (is.null(input$Player))
        return()
      
      xvar_name <- names(metric.list)[metric.list == input$xvar]
      yvar_name <- names(metric.list)[metric.list == input$yvar]

      xvar <- prop("x", as.symbol(input$xvar))
      yvar <- prop("y", as.symbol(input$yvar))      
      
      dt.cluster.cln %>%
        ggvis(x = xvar, 
              y = yvar) %>%
        layer_points(size := 75, 
                     size.hover := 500,
                     size.brush := 500,
                     fillOpacity := 0.25, 
                     fillOpacity.hover := 0.8,
                     fillOpacity.brush := 0.8,
                     key := ~Player,
                     stroke = ~Team,
                     strokeWidth := 3) %>%
        add_axis("x", title = "") %>%
        add_legend("stroke", title = "Team") %>%
        add_axis("x", 
                 title = xvar_name,
                 title_offset = 50) %>%
        add_axis("y", 
                 title = yvar_name,
                 title_offset = 50) %>%
        add_tooltip(Player_tooltip,"hover") %>%
        add_axis("y", title = "") %>%
        set_options(hover_duration = 200) %>%
        set_options(width = 1000, height = 450)
    })
  
    PlayerChart %>% bind_shiny("PlayerChart")
    
    output$cluster.tbl <- renderDataTable({
      data <- dt.cluster.cln()
    },options = list(paging = FALSE))
  
})

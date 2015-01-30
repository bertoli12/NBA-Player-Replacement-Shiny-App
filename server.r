library(shiny)
library(ggvis)
library(RANN)
library(data.table)

dt.raw <- as.data.table(read.csv("Summary.csv"))
PlayerNames <- as.character(dt.raw$Player)

shinyServer(function(input, output, session) {
  
  ### Tooltip ###
  Player_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$Player)) return(NULL)
    
    all_Player <- isolate(dt.cluster.cln())
    Player.tt <- all_Player[all_Player$Player == x$Player, ]
    
    paste0("<b>",Player.tt$Player,"</b><br>",
           "Team: ",Player.tt$Team,"<br>",
           "Position: ",Player.tt$Position,"<br>",
           "Salary: ",Player.tt$Salary,"<br>"
           )  
  }
  
  
  ### Cluster Analysis ###
  dt.cluster.cln <- reactive({
    input$goButton
    
    metric.list <- append(as.character(list('Player','Team','Position','Salary')),as.character(input$metrics))
    n <- input$matches
    
    isolate({
        Player.test <- input$Player

        dt.query <- subset(dt.raw, select = c(metric.list))   ### Create Data Frames of Selected Metrics
        dt.test <- dt.query[Player.test == Player]
        dt.test$Player <- NULL
        dt.test$Team <- NULL
        dt.test$Position <- NULL
        dt.test$Salary <- NULL
        dt.query.cln <- dt.query[Player.test != Player]
        dt.query.cln$Player <-NULL
        dt.query.cln$Team <-NULL
        dt.query.cln$Position <-NULL
        dt.query.cln$Salary <-NULL
        
        cluster <- nn2(dt.query.cln,query=dt.test,k=n)   ### Run Cluster
        
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
  })
  
  ### Full Data Set for Graph ###
  
  dt.final.cln <- reactive({
    input$goButton
    input$matches
    input$metrics
    
    dt.graph <- isolate(dt.cluster.cln())
    players <- as.character(dt.graph$Player)
    dt.final <- dt.raw[0:0]
    dt.final$Distance <- 0
    
    for(i in players){
      dt.row <- dt.raw[i == Player]
      dt.distance <- dt.graph[i == Player]
      dt.row$Distance <- dt.distance$Distance
      dt.final <- rbind(dt.final,dt.row)
    }    
    dt.final
  })
  
  output$test.tbl <- renderDataTable({
    if (is.null(dt.cluster.cln()))
      return()
    
    data <- dt.final.cln()
  },options = list(paging = FALSE))
  
    ### GGVIS Plot ###  
    PlayerChart <- reactive({
      if (is.null(dt.cluster.cln()))
        return()
        
      metric.list <- append(as.character(list('Player','Team','Salary','Position')),as.character(input$metrics))      
                         
      xvar_name <- names(metric.list)[metric.list == input$xvar]
      yvar_name <- names(metric.list)[metric.list == input$yvar]
      
      x.axis <- prop("x", as.symbol(input$xvar))
      y.axis <- prop("y", as.symbol(input$yvar))  
      
      dt.final.cln %>%
        ggvis(x = x.axis, 
              y = y.axis) %>%
        layer_points(size := 75, 
                     size.hover := 500,
                     size.brush := 500,
                     fillOpacity := 0.25, 
                     fillOpacity.hover := 0.8,
                     fillOpacity.brush := 0.8,
                     key := ~Player,
                     stroke = ~Position,
                     strokeWidth := 3) %>%
        add_axis("x", title = "") %>%
        add_legend("stroke", title = "Position") %>%
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
      if (is.null(dt.cluster.cln()))
        return()
      
      data <- dt.cluster.cln()
    },options = list(paging = FALSE))
  
})

library(shiny)
library(ggvis)
library(RANN)
library(data.table)

dt.raw <- as.data.table(read.csv("Summary.csv"))
dt.shot <- as.data.table(read.csv("ShotData.csv"))
dt.shotPlayer <- as.data.table(read.csv("shotPlayer.csv"))
dt.shot$shot_distance <- as.numeric(dt.shot$shot_distance)
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
  
  Shot_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$ID)) return(NULL)
    
    all_Shots <- isolate(dt.bestPlayer.cln())
    Shot.tt <- all_Shots[all_Shots$ID == x$ID, ]
    
    paste0("<b>",Shot.tt$player,"</b><br>",
           "Shot Type: ",Shot.tt$type,"<br>",
           "Result: ",Shot.tt$result,"<br>",
           "Distance from Hoop: ",Shot.tt$shot_distance,"<br>"
    )  
  }
  
  
  ### Cluster Analysis ###
  dt.cluster.cln <- reactive({
    input$goButton
    
    metric.list <- append(as.character(list('Player','Team','Position','Salary')),as.character(input$metrics))
    n <- input$matches
    if(length(as.character(input$positions)) == 0){
      positions <- as.character(list('PG','SG','SF','PF','C'))
    }else{
      positions <- as.character(input$positions)
    }
    
    isolate({
        Player.test <- input$Player

        dt.query <- subset(dt.raw, select = c(metric.list))   ### Create Data Frames of Selected Metrics
        dt.query <- dt.query[Position %in% positions | Player.test == Player]
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
    input$positions
    
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
  


### Shot Selection Tab ###

    dt.shotData.cln <- reactive({
      input$goButton2
      
      isolate({
        shot.team <- input$shotTeam
        shot.side <- as.character(input$shotSide)
        
        if(length(as.character(input$shotType)) == 0){
          shotType <- as.character(list('Jump Bank Shot','Jump Shot'))
        }else{
          shotType <- as.character(input$shotType)
        }
        
        distance.short <- as.numeric(as.character(input$shotDistance)[1])
        distance.long <- as.numeric(as.character(input$shotDistance)[2])  
        distance.average <- (distance.short + distance.long)/2
      
        shotData <- dt.shot[type %in% shotType & 
                              shot_distance >= distance.short & 
                              shot_distance <= distance.long & 
                              shot_distance <= distance.long & 
                              side %in% shot.side &
                              team == shot.team]
        shotData$factor <- 0
        playersToFactor <- NULL
        
        
        for(i in 1:nrow(shotData)){
          if(shotData$player[i] %in% playersToFactor){
            playersToFactor <- playersToFactor
          }else{
            playersToFactor <- as.character(c(playersToFactor, as.character(shotData$player[i])))
          }
        }
        
        Player <- playersToFactor
        Inflection_Percentage <- NULL
        Significance <- NULL
        AIC <- NULL
        
        for(i in playersToFactor){
          shotData$factor <- 0
          name <- as.character(i)  
          
          for (i in 1:nrow(shotData)){
            if(shotData$player[i] == name){
              shotData$factor[i] <- 1
            }
          }
          
          reg <- glm(result ~ shot_distance + factor, data = shotData, family=binomial(link="probit"),na.action=na.pass)
          
          inflection.base <- as.numeric(pnorm(coef(reg)[1] + (coef(reg)[2] * distance.average)))
          inflection.max <- as.numeric(pnorm(coef(reg)[1] + (coef(reg)[2] * distance.average) + coef(reg)[3]))
          inflection.total <- round(as.numeric(inflection.max - inflection.base),digits=3)
          
          Inflection_Percentage <- as.character(c(Inflection_Percentage, as.character(inflection.total)))
          Significance <- as.character(c(Significance, as.character(round(coef(summary(reg))[,4][3],digits=5))))
          AIC <- as.character(c(AIC, as.character(round(AIC(reg),digits=1))))
        }
        
        dt.summary <- as.data.table(cbind(Player,Inflection_Percentage,Significance,AIC))        
      })
    })
    
    ### Full Team Output ###
    output$shot.tbl <- renderDataTable({
      if (is.null(dt.shotData.cln()))
        return()
      
      data <- dt.shotData.cln()
    },options = list(paging = FALSE))


    ### Best Player Algorithm ###
    output$dt.bestPlayer <- renderText({
      input$goButton2
      
      dt.summaryTeam <- isolate(dt.shotData.cln())
      coef <- 0
      bestPlayer <- NULL
      
      for(i in 1:nrow(dt.summaryTeam)){
        if(dt.summaryTeam$Inflection_Percentage[i] > coef & dt.summaryTeam$Significance[i] < .1){
          coef <- dt.summaryTeam$Inflection_Percentage[i]
          bestPlayer <- dt.summaryTeam$Player[i]
        }
      }
      
      if(coef <= 0){
        outputText <- HTML("<h3><p class=text-danger><strong>There is no player with a significant advantage for this shot...try adjusting the inputs to be more inclusive.<strong></p><h3>")
      }else{
        outputText <- HTML(paste("<h3><p class=text-primary><strong>The best player to take this shot is ",bestPlayer,"<strong></p><h3>"))
      }      
      outputText
    })

    ### Best Player Shot Chart ###
    dt.bestPlayer.cln <- reactive({
      input$goButton2
      
      isolate({
        shot.team <- input$shotTeam
        shot.side <- as.character(input$shotSide)
        
        if(length(as.character(input$shotType)) == 0){
          shotType <- as.character(list('Jump Bank Shot','Jump Shot'))
        }else{
          shotType <- as.character(input$shotType)
        }
        
        distance.short <- as.numeric(as.character(input$shotDistance)[1])
        distance.long <- as.numeric(as.character(input$shotDistance)[2])
        
        dt.summaryTeam <- isolate(dt.shotData.cln())
        coef <- 0
        bestPlayer <- NULL
        
        for(i in 1:nrow(dt.summaryTeam)){
          if(dt.summaryTeam$Inflection_Percentage[i] > coef & dt.summaryTeam$Significance[i] < .1){
            coef <- dt.summaryTeam$Inflection_Percentage[i]
            bestPlayer <- dt.summaryTeam$Player[i]
          }
        }
        
        if(coef > 0){        
          bestPlayerData <- dt.shot[type %in% shotType & 
                                    shot_distance >= distance.short & 
                                    shot_distance <= distance.long & 
                                    shot_distance <= distance.long & 
                                    side %in% shot.side &
                                    player == bestPlayer]
        }else{
          bestPlayerData <- dt.shot[type %in% shotType & 
                                    shot_distance >= distance.short & 
                                    shot_distance <= distance.long & 
                                    shot_distance <= distance.long & 
                                    side %in% shot.side &
                                    team == shot.team]
        }        
        bestPlayerData        
        })
      })

    ### GGVIS Plot ###  
    bestPlayerChart <- reactive({
      if (is.null(dt.bestPlayer.cln()))
        return()
      
      dt.bestPlayer.cln %>%
        ggvis(x = ~original_x, 
              y = ~original_y) %>%
        layer_points(size := 5, 
                     size.hover := 20,
                     size.brush := 20,
                     fillOpacity := 0.25, 
                     fillOpacity.hover := 0.8,
                     fillOpacity.brush := 0.8,
                     key := ~ID,
                     stroke = ~result,
                     strokeWidth := 3) %>%
        add_legend("stroke", title = "Shot Result") %>%
        add_axis("x", 
                 title = "") %>%
        add_axis("y", 
                 title = "") %>%
        add_tooltip(Shot_tooltip,"hover") %>%
        set_options(hover_duration = 50) %>%
        set_options(width = 1000, height = 450)    
    })  
    
    bestPlayerChart %>% bind_shiny("bestPlayerChart")

})

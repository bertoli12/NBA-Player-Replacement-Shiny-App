library(shiny)
library(ggvis)
library(RANN)
library(data.table)


dt.raw <- as.data.table(read.csv("Summary.csv"))

PlayerNames <- as.character(dt.raw$Player)
metricsToInclude <- as.character(colnames(dt.raw))
metricsToInclude <- metricsToInclude[metricsToInclude != 'Team' 
                                     & metricsToInclude != 'Player'
                                     & metricsToInclude != 'Salary'
                                     & metricsToInclude != 'Position']

shinyUI(
  fluidPage(theme = "bootstrap.css",
    titlePanel(h1(tags$p(class="text-primary",tags$strong('NBA Replacement Player Locator'))),
               'NBA Player Finder'
               ),
    sidebarLayout(
        sidebarPanel(
        tags$head(
          tags$style(type="text/css", "select { width: 500px; }"),
          tags$style(type="text/css", ".col-sm-4 { width: 25%; }")
          ),
        h4('Use the inputs below to select desired metrics and hit the refresh button to update. Hover over the data in the graph for more info:'),
        br(),
        tags$button(class="btn btn-danger",id="goButton",class="btn action-button shiny-bound-input","Refresh"),
        br(),
        h3(''),
        selectInput('Player', h4(tags$p(class="text-danger",'Player Name: ')), c(Choose='', PlayerNames), selected = "Nerlens Noel"),
        h4(''),
        selectInput('metrics', 
                    h4(tags$p(class="text-danger",'Metrics to Include: ')),
                    c(Choose= '',metricsToInclude), 
                    multiple=TRUE, 
                    selectize=TRUE, 
                    selected =  list('Player','GameAvgPTS','GameAvgAST','GameAvgMin','GameAvgBlk', 'Age')),
        selectInput("xvar", h4(tags$p(class="text-danger",'X-Axis Variable: ')),choices = append(as.character(list('Distance')),metricsToInclude), selected = "Distance"),
        selectInput("yvar", h4(tags$p(class="text-danger",'Y-Axis Variable: ')),choices = append(as.character(list('Distance')),metricsToInclude), selected = "GameAvgPTS"), 
        sliderInput("matches", h4(tags$p(class="text-danger",'Number of Player Matches: ')), 
                    min = 0, max = 50, value = 20, step= 1),
        br(),        
        h4(tags$p(class="text-danger",'Created by Peter Bertoli')),
        HTML("<a href = https://github.com/bertoli12/NBA-Player-Replacement-Shiny-App target=_blank> Source Code </a>")
      ),
    mainPanel(
      HTML('<div class="alert alert-dismissable alert-info">
            <button type="button" class="close" data-dismiss="alert">×</button>
            <h4>This tool is designed to allow users to find players of similiar caliber based on 2014 YTD game averages of the user input metrics. It uses a dynamic 
            <a href="http://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm" class="alert-link">
            nearest neighbor cluster</a> algorithm. Please contact <a href="mailto:peter.j.bertoli@gmail.com?subject=NBA Shiny App" class="alert-link">
            Peter</a> for questions.<h4>
            </div>'),
      br(),
      h3('Replacement Player Graph'),
      ggvisOutput("PlayerChart"),
      br(),
      h3('Searchable Data Table'),
      dataTableOutput(outputId="cluster.tbl")
     )
    )
  )
)
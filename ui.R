library(shiny)
library(ggvis)
library(RANN)
library(data.table)


dt.raw <- as.data.table(read.csv("Summary.csv"))
dt.shotPlayer <- as.data.table(read.csv("shotTeam.csv"))
dt.shotTypes <- as.data.table(read.csv("shotType.csv"))

PlayerNames <- as.character(dt.raw$Player)
shotTeamNames <- as.character(dt.shotPlayer$team)
shotTypes <- as.character(dt.shotTypes$type)
metricsToInclude <- as.character(colnames(dt.raw))
metricsToInclude <- metricsToInclude[metricsToInclude != 'Team' 
                                     & metricsToInclude != 'Player'
                                     & metricsToInclude != 'Salary'
                                     & metricsToInclude != 'Position']

shinyUI(fluidPage(theme = "bootstrap.css",
        navbarPage("NBA Data Science Tools:",
                   tabPanel("Player Replacement",
                     sidebarLayout(
                       sidebarPanel(
                         tags$head(
                           tags$style(type="text/css", "select { width: 500px; }"),
                           tags$style(type="text/css", ".col-sm-4 { width: 25%; }")
                           ),
                         h4('Use the inputs below to select desired metrics and hit the refresh button to update players. Hover over the data in the graph for more info:'),
                         br(),
                         tags$button(class="btn btn-danger",id="goButton",class="btn action-button shiny-bound-input","Change Player"),
                         br(),
                         h3(''),
                         selectInput('Player', h4(tags$p(class="text-danger",'Player Name: ')), c(Choose='', PlayerNames), selected = "Nerlens Noel"),
                         h4(''),
                         selectInput('metrics', 
                                     h4(tags$p(class="text-danger",'Metrics to Include: ')),
                                     c(Choose= '',metricsToInclude), 
                                     multiple=TRUE, 
                                     selectize=TRUE, 
                                     selected =  list('Player','WinsProduced','GameAvgREB','GameAvgBLK', 'Age')),
                         selectInput("xvar", h4(tags$p(class="text-danger",'X-Axis Variable: ')),choices = append(as.character(list('Distance')),metricsToInclude), selected = "Distance"),
                         selectInput("yvar", h4(tags$p(class="text-danger",'Y-Axis Variable: ')),choices = append(as.character(list('Distance')),metricsToInclude), selected = "WinsProduced"), 
                         sliderInput("matches", h4(tags$p(class="text-danger",'Number of Player Matches: ')), 
                                     min = 0, max = 50, value = 20, step= 1),
                         selectInput('positions', 
                                     h4(tags$p(class="text-danger",'Positions to Include: ')),
                                     c(Choose= '',as.character(list('PG','SG','SF','PF','C'))), 
                                     multiple=TRUE, 
                                     selectize=TRUE, 
                                     selected =  as.character(list('PF','C'))),
                         br(),        
                         h4(tags$p(class="text-danger",'Created by Peter Bertoli')),
                         HTML("<a href = https://github.com/bertoli12/NBA-Player-Replacement-Shiny-App target=_blank> Source Code </a>"),
                         br(),
                         h6(HTML('This tool is designed to allow users to find players of similiar caliber based on 2014 YTD metrics that the user is interested in replacing. It uses a dynamic 
                                <a href="http://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm" class="alert-link">
                                nearest neighbor cluster</a> algorithm. Please contact <a href="mailto:peter.j.bertoli@gmail.com?subject=NBA Shiny App" class="alert-link">
                                Peter</a> for questions.'))
                         ),
                       mainPanel(
                                HTML('<div class="alert alert-dismissable alert-info">
                                <button type="button" class="close" data-dismiss="alert">×</button>
                                <h4>Please give the tool a moment to load. Be sure to check out the shot finder tool by using the navigation bar at the top of the page!<h4>
                                </div>'),
                                br(),
                                h3('Replacement Player Graph'),
                                ggvisOutput("PlayerChart"),
                                br(),
                                h3('Searchable Data Table'),
                                dataTableOutput(outputId="cluster.tbl")
                                )
                       )
                     ),
                   tabPanel("Shot Finder",
                            sidebarLayout(
                              sidebarPanel(
                                tags$head(
                                  tags$style(type="text/css", "select { width: 500px; }"),
                                  tags$style(type="text/css", ".col-sm-4 { width: 25%; }")
                                ),
                                h4('Use the inputs to select desired the shot type and location:'),
                                h5('WARNING: Results may take a minute!'),
                                br(),
                                tags$button(class="btn btn-danger",id="goButton2",class="btn action-button shiny-bound-input","Refresh All"),
                                br(),
                                h3(''),
                                selectInput('shotTeam', h4(tags$p(class="text-danger",'Team Name: ')), c(Choose='', shotTeamNames), selected = "PHI"),
                                sliderInput("shotDistance", h4(tags$p(class="text-danger",'Shot Distance Range: ')), 
                                            min = 0, max = 40, value = c(22,26), step= 1),
                                checkboxGroupInput("shotSide", label = h4(tags$p(class="text-danger",'Shot Location: ')), 
                                                   choices = list("Left Side" = "Left", "Right Side" = "Right"),
                                                   selected = c("Left","Right")),
                                selectInput('shotType', 
                                            h4(tags$p(class="text-danger",'Potential Shot Types: ')),
                                            c(Choose= '',shotTypes), 
                                            multiple=TRUE, 
                                            selectize=TRUE, 
                                            selected =  as.character(list('Jump Bank Shot','Jump Shot'))),
                                br(),
                                h4(tags$p(class="text-danger",'Created by Peter Bertoli')),
                                HTML("<a href = https://github.com/bertoli12/NBA-Player-Replacement-Shiny-App target=_blank> Source Code </a>"),
                                br(),
                                h6('This tool is designed to allow coaches to find the player who has the highest probability of hitting a shot from a designed play. 
                                    It looks at all of a teams shots from the 2014-2015 season YTD and then performs GLM regressions for each player who has attemped a similiar shot to determine who should get the ball under a given circumstance.'),
                                h6('A player must take at least 10% of a teams shots of the desired type and location in order to qualify.')
                              ),
                              mainPanel(
                                htmlOutput("dt.bestPlayer"),
                                h3('Player Shot Chart'),
                                ggvisOutput("bestPlayerChart"),
                                br(),
                                h3('Regression Outputs'),
                                h5('Use this table to find out how everyone matches up'),
                                dataTableOutput(outputId="shot.tbl")
                                )
                            )
                   )
           )
        )
        )
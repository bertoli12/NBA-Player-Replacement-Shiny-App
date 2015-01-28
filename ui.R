library(shiny)
library(RSQLite)
library(RODBC)
library(ggvis)
library(jpeg)
library(png)
library(RANN)
library(data.table)

dt.raw <- as.data.table(read.csv('L:\\public\\share\\hcms\\Analyst-Shared\\Peter Bertoli\\Garbage FIles\\Shiny\\Summary.csv'))
PlayerNames <- as.character(dt.raw$Player)
metricsToInclude <- as.character(colnames(dt.raw))
metricsToInclude <- metricsToInclude[metricsToInclude != 'Team' & metricsToInclude != 'Player']

shinyUI(
  fluidPage(
    titlePanel(h1(''),
               h1('')
               ),
    sidebarLayout(
        sidebarPanel(
        selectInput('metrics', 
                    'Metrics to Include: ',
                    c(Choose= '',metricsToInclude), 
                    multiple=TRUE, 
                    selectize=TRUE, 
                    selected =  list('Player','GameAvgPTS','GameAvgBLK')),
        selectInput("xvar", "X-axis variable",metricsToInclude, selected = "GameAvgPTS"),
        selectInput("yvar", "Y-axis variable",metricsToInclude, selected = "GameAvgPTS"),
        uiOutput("xvar"),
        uiOutput("yvar"), 
        sliderInput("matches", "Select Number of Neighbors:", 
                    min = 0, max = 50, value = 10, step= 1),
        selectInput('Player', 'Player Name: ', c(Choose='', PlayerNames), selectize=TRUE, selected = "James Harden")
      ),
    mainPanel(
      textOutput("output.grouping.list.cln"),
      ggvisOutput("PlayerChart"),
      h2(''),
      dataTableOutput(outputId="cluster.tbl")
     )
    )
  )
)
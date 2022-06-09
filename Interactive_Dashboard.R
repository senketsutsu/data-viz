library(shiny)
library(shinydashboard)
library(flexdashboard)
library(ggplot2)
library(plotly)

###PREPARE DATA###
games <- read.csv("2022-01-08.csv", header = TRUE, sep = ',')
subc <- read.csv("subcategories_p.csv", header = TRUE, sep = ",")
games_detailed_info <- read.csv("games_detailed_info.csv", header = TRUE, sep = ",")

games <- merge(games,subc,by="ID")
games <- merge(games,games_detailed_info,by="ID")

Q1 <- quantile(games$Year, .25)
Q3 <- quantile(games$Year, .75)
IQR <- IQR(games$Year)
no_outliers <- subset(games, games$Year > (Q1 - 1.5*IQR) & games$Year < (Q3 + 1.5*IQR))

games_1000 <- subset(no_outliers, no_outliers$Rank <= 1000)
###

ui <- dashboardPage(
  dashboardHeader(title = span("Mom, let's get BGG!", style = "font-weight: bold")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("fa-solid fa-window-maximize")),
      menuItem("Sketch", tabName = "sketch", icon = icon("fa-solid fa-pen")),
      menuItem("About", tabName = "description", icon = icon("fa-solid fa-file"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",       
              splitLayout(
                # Object 1
                verticalLayout(
                  box(
                    width = "50%",
                    tags$div(
                      style = "display: flex; align-items: center; text-align: center",
                      p(style = "flex: 1; font-size: 150%; white-space: pre-line; color: rgb(60,141,188); font-weight: bold", "So many games to choose from..."),
                    ),
                    plotOutput("gamePlot", click = "plot1_click", brush = brushOpts( id = "plot1_brush"))
                  ),
                  box(
                    width = "50%",
                    dataTableOutput("gameTable")
                  )
                ),
                # Object 2
                box(tittle = "Selected Game", solidHeader = TRUE, width = "50%", hight = "100vh",
                    tags$div(
                      style = "display: flex; align-items: center; text-align: center",
                      p(style = "flex: 1; font-size: 150%; white-space: pre-line; color: rgb(60,141,188); font-weight: bold", "Maybe this one?"),
                    ),
                    tags$div(
                      style = "display: flex; align-items: center; text-align: center",
                      p(style = "flex: 1; font-size: 75%; white-space: pre-line; color: rgb(60,141,188)", "(click on the chart to see data about a given game)"),
                    ),
                    br(),
                    uiOutput("gameImg"),
                    br(),br(),
                    uiOutput("gameRank"),
                    uiOutput("gameCat"),
                    br(),
                    tags$div(
                      style = "display: flex; align-items: center; text-align: center; padding-bottom: 0px",
                      gaugeOutput("avgRate"),
                      gaugeOutput("avgRateBuy")
                    ),
                    uiOutput("gameDesc"),
                    br(),
                    
                    
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "sketch",
              tags$div(
                style = "display: flex; flex-direction: column; align-items: center; text-align: center",
                h2(style = "flex: 1; font-size: 250%; white-space: pre-line; color: rgb(60,141,188); font-weight: bold; text-align: center","The Sketches"),
                br(),
                br(),
                h3(style = "flex: 1; white-space: pre-line; color: rgb(0,105,145); font-size: 100%","Our first concept for this project was slightly different from the final one."),
                br(),tags$img(src="first_sketch.JPG", width = "75%", align = "center"),br(),
                h3(style = "flex: 1; white-space: pre-line; color: rgb(0,105,145); font-size: 100%","We changed it due to difficulties, we faced while executing our primary concept."),
                br(),tags$img(src="final_concept.jpg", width = "75%", align = "center"),br(),
                h3(style = "line-height: 1.6; flex: 1; white-space: pre-line; color: rgb(0,105,145); font-size: 100%",
                   "We got rid of the keywords and decided to show some other information instead of them such as the game category and the place in the ranking.
                   We also switched from the rating distribution graph to two gauges showing average rating and average rating among the buyers.
                   We also changed the placement of some of the objects to better show elements of the new concept."),
              )
              
      ),
      # Third tab content
      tabItem(tabName = "description",
              tags$div(
                style = "display: flex; flex-direction: column; align-items: center; text-align: center",
                h2(style = "flex: 1; font-size: 250%; white-space: pre-line; color: rgb(60,141,188); font-weight: bold; text-align: center","Description"),
                br(),
                br(),
                h2(style = "flex: 1; font-size: 150%; white-space: pre-line; color: rgb(60,141,188); font-weight: bold; text-align: center","1. Main goal"),
                h3(style = "line-height: 1.6; flex: 1; white-space: pre-line; color: rgb(0,105,145); font-size: 100%",
                   "Our main purpose for this project was to create user friendly application comparing numerous board games.
                   We wanted to make accessing the basic informations about games easier.
                   We were mostly inspired by a website which ranks board games and we wanted to improve the way the games ranking are shown there."),
                br(),
                h2(style = "flex: 1; font-size: 150%; white-space: pre-line; color: rgb(60,141,188); font-weight: bold; text-align: center","2. Execution"),
                br(),tags$img(src="execution.jpg", width = "75%", align = "center"),br(),
                h3(style = "line-height: 1.6; flex: 1; white-space: pre-line; color: rgb(0,105,145); font-size: 100%",
                   "a) a graph showing games place in the ranking as well as the year of their production, is also color-coded based on their category
                   
                   b) a datatable which shows basic informations (Rank, Name, Year, Average) about chosen games on the graph (if none were chosen than about all) 
                   
                   (the next points consider only the situation when one game was chosen on the graph, when no game was chosen or multiple games were chosen, this part of the dashboard is empty)
                   
                   c) an image showing how the game looks (due to poor quality of the image given in the data set this object is quite small)
                   
                   d) the name of currently chosen game
                   
                   e) the category and the place in the ranking of the chosen game
                   
                   f) a visualization showing the average rating of this game, the color of the visualisation changes based on the rating, if the rating is below 6 then the color is red, if it's between 6 and 7.5 the color is yellow and when it's above 7.5 then the color is green
                   
                   g) a visualization showing the average rating of this game among the people who bought it, the color of the visualisation changes based on the rating, if the rating is below 6 then the color is red, if it's between 6 and 7.5 the color is yellow and when it's above 7.5 then the color is green
                   
                   h) description of the game which is based on one of the opinions of the customers about this game
                   
                   
                   "),
                
              ),
              tags$div(
                style = "display: flex; flex-direction: column; align-items: right; text-align: right",
                br(),tags$a(tags$img(src="mini-logo.JPG", width = "5%", align = "center"),href = "https://www.put.poznan.pl/"),br(),
              )
              
      )
    )
  )
)

server <- function(input, output) {
  
  # selectedData <- reactive({
  #   data <- brushedPoints(games_1000, input$plot1_brush)
  #   if (nrow(data) == 0)
  #     data <- games_1000
  #   data
  # })
  
  output$gamePlot<- renderPlot({
    
    ggplot(games_1000, aes(x=Year, y=Rank, color=Category, text = paste("Title:",Name))) +
      geom_point(alpha=0.5) +
      scale_y_reverse() +
      theme_bw() +
      theme(legend.title = element_blank())
    
    
  })
  
  output$gameTable <- renderDataTable({
    data <- brushedPoints(games_1000, input$plot1_brush)
    if (nrow(data) == 0)
      data <- games_1000
    data <- data[, c("Rank","Name","Year","Average")]
    data <- data[order(data$Rank,decreasing = FALSE),]
    #selectedData()
  }, options = list(pageLength = 5, scrollX = TRUE))
  
  output$gameImg <- renderUI({
    scr <- nearPoints(games_1000, input$plot1_click)[1, "Thumbnail"]
    if(nrow(nearPoints(games_1000, input$plot1_click)) == 0)
      tags$div(
        p("No_game")
      )
    if(nrow(nearPoints(games_1000, input$plot1_click)) > 0)
      
      tags$div(
        style = "display: flex; align-items: center; text-align: center",
        tags$img(src=scr, style="display: block; height: 64px; width: 64px"),
        p(style = "flex: 1; font-size: 180%; white-space: pre-line; font-weight: bold; color: rgb(0,105,145)", nearPoints(games_1000, input$plot1_click)[1, "Name"])
      )
  })
  
  output$gameDesc <- renderUI({
    game_des <- nearPoints(games_1000, input$plot1_click)[1, "description"]
    if(nrow(nearPoints(games_1000, input$plot1_click)) == 0)
      tags$div(
        p("No_game")
      )
    if(nrow(nearPoints(games_1000, input$plot1_click)) > 0)
      tags$div(
        tags$div(
          style = "text-align: center;",
          p(style = "font-size: 125%; white-space: pre-line; color: rgb(60,141,188); text-transform : uppercase; font-weight: bold", "Game description")
        ),
        tags$div(
          style = "text-align: center; overflow: auto; height: 50vh",
          p(style = "flex: 1; font-size: 100%; white-space: pre-line", game_des )
        ))
  })
  
  output$gameCat <- renderUI({
    cat_now <- nearPoints(games_1000, input$plot1_click)[1, "Category"]
    if(nrow(nearPoints(games_1000, input$plot1_click)) == 0)
      tags$div(
        p("No_game")
      )
    if(nrow(nearPoints(games_1000, input$plot1_click)) > 0)
      
      tags$div(
        style = "display: flex; align-items: center; text-align: center",
        p(style = "flex: 1; font-size: 90%; white-space: pre-line; color: rgb(60,141,188)", "category:"),
        p(style = "flex: 1; font-size: 90%; white-space: pre-line; color: rgb(60,141,188)", cat_now)
      )
  })
  
  output$gameRank <- renderUI({
    cat_now <- nearPoints(games_1000, input$plot1_click)[1, "Rank"]
    if(nrow(nearPoints(games_1000, input$plot1_click)) == 0)
      tags$div(
        p("No_game")
      )
    if(nrow(nearPoints(games_1000, input$plot1_click)) > 0)
      
      tags$div(
        style = "display: flex; align-items: center; text-align: center",
        p(style = "flex: 1; font-size: 90%; white-space: pre-line; color: rgb(60,141,188)", "rank:"),
        p(style = "flex: 1; font-size: 90%; white-space: pre-line; color: rgb(60,141,188)", cat_now)
      )
  })
  
  output$avgRate <- renderGauge({
    if(nrow(nearPoints(games_1000, input$plot1_click)) > 0)
      gauge(
        nearPoints(games_1000, input$plot1_click)[1, "average"],
        min = 0,
        max = 10,
        gaugeSectors(
          success =c(7.5 , 10), 
          warning =c(6 , 7.5), 
          danger  =c(0, 6)
        ),
        label = "Average rating"
      )
  })
  
  output$avgRateBuy <- renderGauge({
    if(nrow(nearPoints(games_1000, input$plot1_click)) > 0)
      gauge(
        nearPoints(games_1000, input$plot1_click)[1, "bayesaverage"],
        min = 0,
        max = 10,
        gaugeSectors(
          success =c(7.5 , 10), 
          warning =c(6 , 7.5), 
          danger  =c(0, 6)
        ),
        label = "Bayers rating"
      )
  })
  
 
  
  
}

shinyApp(ui, server)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# Load data
d <- read.csv("C:\\Users\\singh\\OneDrive\\Desktop\\apps\\Gunnels.csv") %>%
  select(-rownames)

ui <- dashboardPage(
  
  skin = "black",
  
  dashboardHeader(title = "Gunnel Habitat Dashboard"),
  
  dashboardSidebar(
    width = 230,
    
    sliderInput("time", "Time:",
                min(d$Time), max(d$Time),
                value = c(min(d$Time), max(d$Time))),
    
    selectInput("habitat", "Habitat:",
                choices = c("Amphiso","Subst","Pool","Water","Cobble"))
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
      body { background-color: #1e1e2f; color: white; }

      .content-wrapper, .right-side {
        background-color: #1e1e2f;
      }

      .box {
        background-color: #2c2f48;
        border-radius: 10px;
      }

      .small-box {
        border-radius: 10px;
      }

      h3, .box-title {
        font-weight: bold;
        color: white;
      }
    "))),
    
    fluidRow(
      valueBoxOutput("total", width = 3),
      valueBoxOutput("presence", width = 3),
      valueBoxOutput("slope", width = 3),
      valueBoxOutput("fromlow", width = 3)
    ),
    
    fluidRow(
      box(plotOutput("trend"), width = 6, solidHeader = TRUE, status = "primary"),
      box(plotOutput("bar"), width = 6, solidHeader = TRUE, status = "success")
    ),
    
    fluidRow(
      box(plotOutput("scatter"), width = 6, solidHeader = TRUE, status = "warning"),
      box(plotOutput("hist"), width = 6, solidHeader = TRUE, status = "danger")
    )
  )
)

server <- function(input, output) {
  
  df <- reactive({
    d %>% filter(Time >= input$time[1], Time <= input$time[2])
  })
  
  # KPI CARDS
  output$total <- renderValueBox({
    valueBox(nrow(df()), "Total Records", icon = icon("database"), color = "blue")
  })
  
  output$presence <- renderValueBox({
    valueBox(round(mean(df()$Gunnel),2), "Gunnel Presence",
             icon = icon("fish"), color = "green")
  })
  
  output$slope <- renderValueBox({
    valueBox(round(mean(df()$Slope),2), "Avg Slope",
             icon = icon("chart-line"), color = "purple")
  })
  
  output$fromlow <- renderValueBox({
    valueBox(round(mean(df()$Fromlow),2), "Avg Fromlow",
             icon = icon("water"), color = "yellow")
  })
  
  # PLOTS
  
  output$trend <- renderPlot({
    ggplot(df(), aes(Time, Gunnel)) +
      geom_line(color = "#4db8ff", size = 1) +
      ggtitle("Gunnel Over Time") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "white"),
        text = element_text(color = "white"),
        panel.background = element_rect(fill = "#2c2f48"),
        plot.background = element_rect(fill = "#2c2f48")
      )
  })
  
  output$bar <- renderPlot({
    ggplot(df(), aes(x = .data[[input$habitat]], y = Gunnel)) +
      stat_summary(fun = mean, geom = "bar", fill = "#00cc99") +
      ggtitle("Habitat Impact") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "white"),
        text = element_text(color = "white"),
        panel.background = element_rect(fill = "#2c2f48"),
        plot.background = element_rect(fill = "#2c2f48")
      )
  })
  
  output$scatter <- renderPlot({
    ggplot(df(), aes(Fromlow, Slope, color = Gunnel)) +
      geom_point(size = 3) +
      ggtitle("Fromlow vs Slope") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "white"),
        text = element_text(color = "white"),
        panel.background = element_rect(fill = "#2c2f48"),
        plot.background = element_rect(fill = "#2c2f48")
      )
  })
  
  output$hist <- renderPlot({
    ggplot(df(), aes(Slope)) +
      geom_histogram(fill = "#ff9933", bins = 20) +
      ggtitle("Slope Distribution") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "white"),
        text = element_text(color = "white"),
        panel.background = element_rect(fill = "#2c2f48"),
        plot.background = element_rect(fill = "#2c2f48")
      )
  })
}

shinyApp(ui, server)
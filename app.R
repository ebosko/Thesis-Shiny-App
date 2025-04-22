library(shiny)
library(tidyverse)
library(readxl)
library(MASS)
library(ggplot2)

#load data
#read in data
tib <- read_excel('C:/Users/Eddie/OneDrive/Desktop/BIOSTAT/Thesis/Thesis/DataProcessed/2025.4.2.xlsx', sheet = "tib.long")

ggo <- read_excel('C:/Users/Eddie/OneDrive/Desktop/BIOSTAT/Thesis/Thesis/DataProcessed/2025.4.2.xlsx', sheet = "ggo.long")

cons <- read_excel('C:/Users/Eddie/OneDrive/Desktop/BIOSTAT/Thesis/Thesis/DataProcessed/2025.4.2.xlsx', sheet = "cons.long")

bronch <- read_excel('C:/Users/Eddie/OneDrive/Desktop/BIOSTAT/Thesis/Thesis/DataProcessed/2025.4.2.xlsx', sheet = "bronch.long")

atel <- read_excel('C:/Users/Eddie/OneDrive/Desktop/BIOSTAT/Thesis/Thesis/DataProcessed/2025.4.2.xlsx', sheet = "atel.long")

ln <- read_excel('C:/Users/Eddie/OneDrive/Desktop/BIOSTAT/Thesis/Thesis/DataProcessed/2025.4.2.xlsx', sheet = "ln.long")

thin <- read_excel('C:/Users/Eddie/OneDrive/Desktop/BIOSTAT/Thesis/Thesis/DataProcessed/2025.4.2.xlsx', sheet = "thin.long")

thick <- read_excel('C:/Users/Eddie/OneDrive/Desktop/BIOSTAT/Thesis/Thesis/DataProcessed/2025.4.2.xlsx', sheet = "thick.long")

demo <- read.csv('C:/Users/Eddie/OneDrive/Desktop/BIOSTAT/Thesis/Thesis/DataRaw/demographic_final.csv')

# UI
ui <- fluidPage(
  titlePanel("CT Score Distribution by Lobe"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature", "Choose a CT Feature:",
                  choices = c("Bronchiectasis", "Atelectasis", "GGO", "TIB", "Consolidation"))
    ),
    mainPanel(
      plotOutput("scorePlot")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$scorePlot <- renderPlot({
    tib %>%
      filter(feature == input$feature) %>%
      ggplot(aes(x = Attribute, fill = factor(Value))) +
      geom_bar(position = "stack") +
      labs(
        title = paste("Distribution of", input$feature, "Ratings by Lobe"),
        x = "Lobe",
        y = "Count",
        fill = "Rating"
      ) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)

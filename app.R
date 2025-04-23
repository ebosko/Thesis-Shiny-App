library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggiraph)

#load data
#read in data
tib <- read_excel('2025.4.2.xlsx', sheet = "tib.long")

ggo <- read_excel('2025.4.2.xlsx', sheet = "ggo.long")

cons <- read_excel('2025.4.2.xlsx', sheet = "cons.long")

bronch <- read_excel('2025.4.2.xlsx', sheet = "bronch.long")

atel <- read_excel('2025.4.2.xlsx', sheet = "atel.long")

ln <- read_excel('2025.4.2.xlsx', sheet = "ln.long")

thin <- read_excel('2025.4.2.xlsx', sheet = "thin.long")

thick <- read_excel('2025.4.2.xlsx', sheet = "thick.long")

# Combine all features into one long dataset with a new column indicating the feature
tib$feature <- "tib"
ggo$feature <- "ggo"
cons$feature <- "cons"
bronch$feature <- "bronch"
atel$feature <- "atel"
ln$feature <- "ln"
thin$feature <- "thin"
thick$feature <- "thick"

# Feature map for user-friendly display
feature_map <- list(
  "Tree-in-bud" = "tib",
  "Ground-glass opacities" = "ggo",
  "Consolidation" = "cons",
  "Bronchiectasis" = "bronch",
  "Atelectasis" = "atel",
  "Large nodules" = "ln",
  "Thin wall cavities" = "thin",
  "Thick wall cavities" = "thick"
)

# Custom color palette
score_colors <- c("0" = "#be64ac", "1" = "#5ec9c2", "2" = "#3b9142", "3" = "#e34a33")

ui <- fluidPage(
  titlePanel("CT Feature Score Distributions by Lobe"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature", "Choose a CT Feature:", choices = names(feature_map)),
      selectInput("rater", "Choose Rater:", choices = c("Overall", "JW", "VH"))
    ),
    mainPanel(
      plotlyOutput("barPlot", height = "550px")
    )
  )
)

server <- function(input, output) {
  output$barPlot <- renderPlotly({
    # Get data
    feature_data <- get(feature_map[[input$feature]])
    
    # Apply rater filter
    if (input$rater != "Overall") {
      feature_data <- feature_data %>% filter(rater == input$rater)
    }
    
    # Prepare for plotting
    plot_data <- feature_data %>%
      count(lobe, score) %>%
      mutate(score = factor(score, levels = c(3, 2, 1, 0)))
    
    # Make ggplot object
    p <- ggplot(plot_data, aes(x = lobe, y = n, fill = score, text = paste0(
      "Lobe: ", lobe, "<br>",
      "Score: ", score, "<br>",
      "Count: ", n
    ))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = score_colors) +
      labs(
        title = paste("Distribution of", input$feature, "Scores by Lobe",
                      if (input$rater != "Overall") paste("(", input$rater, ")") else ""),
        x = "Lobe", y = "Count", fill = "Score"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
}

shinyApp(ui = ui, server = server)

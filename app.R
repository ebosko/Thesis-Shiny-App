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

# Feature mapping
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

score_colors <- c("0" = "#be64ac", "1" = "#5ec9c2", "2" = "#3b9142", "3" = "#e34a33")

ui <- fluidPage(
  titlePanel("CT Feature Score Distributions by Lobe"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature", "Choose a CT Feature:", choices = names(feature_map)),
      selectInput("rater",   "Choose Rater:"      , choices = c("Overall", "JW", "VH")),
      tags$hr(),
      h4("Lung lobe visual"),
      imageOutput("lungImage", height = "400px")
    ),
    mainPanel(
      plotlyOutput("barPlot", height = "550px")
    )
  )
)

server <- function(input, output, session) {
  
  # ---------------------------------------------------------------
  #   1.  STACKED BAR (unchanged, except keep key = tolower(lobe))
  # ---------------------------------------------------------------
  output$barPlot <- renderPlotly({
    df <- get(feature_map[[input$feature]])
    if (input$rater != "Overall") df <- dplyr::filter(df, rater == input$rater)
    
    plot_data <- df |>
      count(lobe, score) |>
      mutate(score = factor(score, levels = c(3, 2, 1, 0)))
    
    p <- ggplot(plot_data,
                aes(x = lobe,
                    y = n,
                    fill = score,
                    key = tolower(lobe),        # <── key we will read back
                    text = paste0("Lobe: ", lobe,
                                  "<br>Score: ", score,
                                  "<br>Count: ", n))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = score_colors) +
      labs(title = paste("Distribution of", input$feature, "scores by lobe",
                         if (input$rater != "Overall") paste0(" (", input$rater, ")") else ""),
           x = "Lobe", y = "Count", fill = "Score") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text", source = "barplot") |>
      config(displayModeBar = FALSE)
  })
  
  # ---------------------------------------------------------------
  #   2.  REACTIVE IMAGE
  # ---------------------------------------------------------------
  output$lungImage <- renderImage({
    
    ev <- event_data("plotly_click", source = "barplot")
    # If nothing clicked yet → NULL
    lobe_click <- if (is.null(ev) || is.null(ev$key)) NULL else ev$key
    
    # Build the file name
    file_name <- if (is.null(lobe_click) || lobe_click == "")
      "lung_default.png"
    else
      paste0("lung_", lobe_click, ".png")   # lobe_click already lower-case
    
    # Give Shiny an *absolute* path
    list(src         = normalizePath(file.path("www", file_name)),
         contentType = "image/png",
         width       = "100%",
         alt         = paste("Lung diagram:", lobe_click %||% "default"))
  }, deleteFile = FALSE)
}

shinyApp(ui, server)

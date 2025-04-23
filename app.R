library(shiny)
library(tidyverse)
library(readxl)
library(MASS)
library(ggplot2)

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

# Combine all datasets into one
all_data <- bind_rows(tib, ggo, cons, bronch, atel, ln, thin, thick)

# Define UI
ui <- fluidPage(
  titlePanel("CT Feature Score Distributions by Lobe"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("feature", "Choose a CT Feature:",
                  choices = c("Tree-in-Bud" = "tib",
                              "Ground Glass Opacities" = "ggo",
                              "Consolidation" = "cons",
                              "Bronchiectasis" = "bronch",
                              "Atelectasis" = "atel",
                              "Large Nodules" = "ln",
                              "Thin Wall Cavity" = "thin",
                              "Thick Wall Cavity" = "thick")),
      
      selectInput("rater", "Choose Rater:",
                  choices = c("Overall" = "overall", "JW" = "JW", "VH" = "VH"),
                  selected = "overall")
    ),
    
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$barPlot <- renderPlot({
    df <- all_data %>%
      filter(feature == input$feature)
    
    if (input$rater != "overall") {
      df <- df %>% filter(rater == input$rater)
    }
    
    df %>%
      mutate(score = factor(score, levels = c(3, 2, 1, 0))) %>%
      ggplot(aes(x = lobe, fill = score)) +
      geom_bar(position = "stack") +
      labs(
        title = paste("Distribution of", toupper(input$feature), "Scores by Lobe"),
        x = "Lobe",
        y = "Count",
        fill = "Score"
      ) +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

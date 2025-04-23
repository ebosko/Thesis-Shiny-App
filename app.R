# app.R  –  full dashboard: bar  +  lung  +  detail  +  heat-maps --------------
# -----------------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)
library(psych)         

# ── 1  DATA ───────────────────────────────────────────────────────────────────
excel_path <- "2025.4.2.xlsx" 

# load sheets
tib    <- read_excel(excel_path, sheet = "tib.long")
ggo    <- read_excel(excel_path, sheet = "ggo.long")
cons   <- read_excel(excel_path, sheet = "cons.long")
bronch <- read_excel(excel_path, sheet = "bronch.long")
atel   <- read_excel(excel_path, sheet = "atel.long")
ln     <- read_excel(excel_path, sheet = "ln.long")
thin   <- read_excel(excel_path, sheet = "thin.long")
thick  <- read_excel(excel_path, sheet = "thick.long")

# lobe order
lobe_levels <- c("LLL","LLS","LUS","RLL","RML","RUL")

# feature map for sidebar → data frame name
feature_map <- list(
  "Tree-in-bud"            = "tib",
  "Ground-glass opacities" = "ggo",
  "Consolidation"          = "cons",
  "Bronchiectasis"         = "bronch",
  "Atelectasis"            = "atel",
  "Large nodules"          = "ln",
  "Thin wall cavities"     = "thin",
  "Thick wall cavities"    = "thick"
)

score_colors <- c("0"="#be64ac","1"="#5ec9c2","2"="#3b9142","3"="#e34a33")

# ---- ordinal_all for violin & heat-map  -------------------------------------
ordinal_all <- bind_rows(
  tib    %>% mutate(feature="TIB"),
  ggo    %>% mutate(feature="GGO"),
  bronch %>% mutate(feature="BRONCH"),
  cons   %>% mutate(feature="CONS"),
  atel   %>% mutate(feature="ATEL")
) %>%
  mutate(
    score   = as.numeric(score),
    lobe    = factor(lobe, levels = lobe_levels),
    feature = factor(feature, levels = c("TIB","GGO","CONS","ATEL","BRONCH")),
    rater   = factor(rater, levels = c("JW","VH"))
  )

# ───────────────────── HEAT-MAPS  (overall mean + |gap|)  ────────────────────
## ---------- ORDINAL ----------------------------------------------------------
ordinal_heat <- ordinal_all %>%
  group_by(lobe, feature, rater) %>%
  summarise(mean_rater = mean(score, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = rater, values_from = mean_rater) %>%
  mutate(
    mean_score = (JW + VH) / 2,
    gap        = abs(JW - VH)                    # absolute difference
  )

p_ord <- ggplot(
  ordinal_heat,
  aes(feature, lobe, fill = mean_score,
      text = sprintf(
        "Feature: %s<br>Lobe: %s<br>JW mean = %.2f<br>VH mean = %.2f<br>Overall mean = %.2f<br>|Gap| = %.2f",
        feature, lobe, JW, VH, mean_score, gap))
) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_distiller(
    palette  = "YlOrRd", direction = 1,
    name     = "Mean score",
    limits   = c(0, 3)
  ) +
  labs(
    title = "Mean ordinal score by CT feature and lobe",
    x = "CT feature", y = "Lung lobe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ordinal_heatmap <- ggplotly(p_ord, tooltip = "text") %>%
  config(displayModeBar = FALSE)


## ---------- BINARY -----------------------------------------------------------
binary_all <- bind_rows(
  ln    %>% mutate(feature = "Large nod"),
  thin  %>% mutate(feature = "Thin cav"),
  thick %>% mutate(feature = "Thick cav")
) %>%
  mutate(lobe = factor(lobe, levels = lobe_levels))

binary_heat <- binary_all %>%
  group_by(lobe, feature, rater) %>%
  summarise(mean_rater = mean(as.numeric(score)), .groups = "drop") %>%
  pivot_wider(names_from = rater, values_from = mean_rater) %>%
  mutate(
    prop_present = (JW + VH) / 2,
    gap          = abs(JW - VH)
  )

grey_teal <- scales::gradient_n_pal(c("#f0f0f0", "#02818a"))

p_bin <- ggplot(
  binary_heat,
  aes(feature, lobe, fill = prop_present,
      text = sprintf(
        "Feature: %s<br>Lobe: %s<br>JW prop = %.2f<br>VH prop = %.2f<br>Overall prop = %.2f<br>|Gap| = %.2f",
        feature, lobe, JW, VH, prop_present, gap))
) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_gradientn(
    colours = grey_teal(seq(0, 1, length.out = 7)),
    name    = "Proportion",
    limits  = c(0, 1)
  ) +
  labs(
    title = "Proportion present (binary features) by CT feature and lobe",
    x = "CT feature", y = "Lung lobe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

binary_heatmap <- ggplotly(p_bin, tooltip = "text") %>%
  config(displayModeBar = FALSE)




# ── 2  UI ─────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("CT Feature Score Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature","CT Feature:",choices = names(feature_map)),
      selectInput("rater","Rater:",choices = c("Overall","JW","VH")),
      tags$hr(),
      h4("Lung lobe visual"),
      imageOutput("lungImage", height = "400px")
    ),
    mainPanel(
      plotlyOutput("barPlot",   height = "500px"),
      plotOutput ("detailPlot", height = "450px"),
      plotlyOutput("heatOrdinal", height = "420px"),
      plotlyOutput("heatBinary",  height = "420px")
    )
  )
)

# ── 3  SERVER ────────────────────────────────────────────────────────────────
server <- function(input, output, session){
  
  selected_lobe <- reactiveVal(NULL)
  
  # ---- stacked bar ---------------------------------------------------------
  output$barPlot <- renderPlotly({
    df <- get(feature_map[[input$feature]])
    if(input$rater!="Overall") df <- filter(df,rater==input$rater)
    df <- mutate(df,lobe=factor(lobe,levels=lobe_levels))
    
    plot_data <- df %>% count(lobe,score) %>%
      mutate(score=factor(score,levels=c(3,2,1,0)))
    
    p <- ggplot(plot_data,
                aes(lobe,n,fill=score,key=tolower(lobe),
                    text=sprintf("Lobe: %s<br>Score: %s<br>Count: %s",lobe,score,n)))+
      geom_bar(stat="identity",position="stack")+
      scale_fill_manual(values=score_colors)+
      labs(title=paste("Distribution of",input$feature,
                       if(input$rater!="Overall")paste0(" (",input$rater,")")),
           x="Lobe",y="Count",fill="Score")+ theme_minimal()
    
    ggplotly(p,tooltip="text",source="barplot")%>%config(displayModeBar=FALSE)
  })
  
  observeEvent(event_data("plotly_click",source="barplot"),
               selected_lobe(event_data("plotly_click",source="barplot")$key))
  
  # ---- lung diagram --------------------------------------------------------
  output$lungImage <- renderImage({
    lobe <- selected_lobe()
    fname <- if(is.null(lobe)||lobe=="")"lung_default.png" else paste0("lung_",lobe,".png")
    list(src=normalizePath(file.path("www",fname)),
         contentType="image/png",width="100%",
         alt=paste("Lung:",lobe%||%"default"))
  },deleteFile=FALSE)
  
  # ---- detail view (linked) -------------------------------------------------
  # ---- detail view (linked) -------------------------------------------------
  output$detailPlot <- renderPlot({
    
    # 1. subset to the sidebar feature + rater -------------------------------
    feat_code <- toupper(feature_map[[input$feature]])
    df <- filter(ordinal_all, feature == feat_code)
    if (input$rater != "Overall") df <- filter(df, rater == input$rater)
    
    # 2. guard: if nothing left, show a blank panel --------------------------
    if (nrow(df) == 0) {
      return(
        ggplot() + theme_void() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No data for this selection", size = 6)
      )
    }
    
    # 3. decide if this is a binary feature ----------------------------------
    unique_scores <- sort(unique(df$score))
    is_binary <- length(unique_scores) == 2 && all(unique_scores %in% 0:1)
    
    # 4. plot ----------------------------------------------------------------
    if (is_binary) {
      ggplot(df, aes(rater, score, colour = rater)) +
        geom_jitter(width = 0.15, height = 0.05, alpha = 0.6, size = 1) +
        geom_boxplot(width = 0.25, outlier.shape = NA, alpha = 0.8) +
        facet_grid(~ lobe, switch = "x") +
        scale_y_continuous(breaks = unique_scores, limits = c(-0.2, 1.2)) +
        scale_colour_brewer(palette = "Set1") +
        labs(
          title = paste("Binary score distribution –", input$feature),
          x = "Rater", y = "Score"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          strip.text  = element_text(size = 9),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
    } else {
      ggplot(df, aes(rater, score, fill = rater)) +
        geom_violin(scale = "area", bw = 0.4, trim = FALSE,
                    alpha = 0.55, colour = NA) +
        geom_boxplot(width = 0.10, outlier.shape = NA, alpha = 0.9) +
        facet_grid(~ lobe, switch = "x") +
        scale_y_continuous(breaks = 0:3, limits = c(-0.4, 3.4)) +
        scale_fill_brewer(palette = "Set1") +
        labs(
          title = paste("Score distribution –", input$feature),
          x = "Rater", y = "Score"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          strip.text  = element_text(size = 9),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
    }
  })
  
  
  # ---- heat-maps (static objects) ------------------------------------------
  output$heatOrdinal <- renderPlotly({ ordinal_heatmap })
  output$heatBinary  <- renderPlotly({ binary_heatmap  })
}

shinyApp(ui, server)

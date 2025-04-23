# app.R ── interactive stacked bar + lung image + linked detail (violin / box-dot)
# -------------------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)

# ── DATA ───────────────────────────────────────────────────────────────────────
excel_path <- "2025.4.2.xlsx"   # adjust if needed

tib    <- read_excel(excel_path, sheet = "tib.long")
ggo    <- read_excel(excel_path, sheet = "ggo.long")
cons   <- read_excel(excel_path, sheet = "cons.long")
bronch <- read_excel(excel_path, sheet = "bronch.long")
atel   <- read_excel(excel_path, sheet = "atel.long")
ln     <- read_excel(excel_path, sheet = "ln.long")
thin   <- read_excel(excel_path, sheet = "thin.long")
thick  <- read_excel(excel_path, sheet = "thick.long")

lobe_levels <- c("LLL","LLS","LUS","RLL","RML","RUL")

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

ordinal_all <- bind_rows(
  tib    %>% mutate(feature="TIB"),
  ggo    %>% mutate(feature="GGO"),
  bronch %>% mutate(feature="BRONCH"),
  cons   %>% mutate(feature="CONS"),
  atel   %>% mutate(feature="ATEL")
) %>% mutate(
  score   = as.numeric(score),
  lobe    = factor(lobe, levels=lobe_levels),
  feature = factor(feature, levels=c("TIB","GGO","CONS","ATEL","BRONCH")),
  rater   = factor(rater,  levels=c("JW","VH"))
)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("CT Feature Score Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature","CT Feature:",choices=names(feature_map)),
      selectInput("rater","Rater:",choices=c("Overall","JW","VH")),
      tags$hr(),
      h4("Lung lobe visual"),
      imageOutput("lungImage",height="400px")
    ),
    mainPanel(
      plotlyOutput("barPlot",height="500px"),
      plotOutput ("detailPlot",height="450px")
    )
  )
)

# ── SERVER ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session){
  
  selected_lobe <- reactiveVal(NULL)
  
  # stacked bar ---------------------------------------------------------------
  output$barPlot <- renderPlotly({
    df <- get(feature_map[[input$feature]])
    if (input$rater!="Overall") df <- filter(df,rater==input$rater)
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
           x="Lobe",y="Count",fill="Score")+
      theme_minimal()
    
    ggplotly(p,tooltip="text",source="barplot")%>%config(displayModeBar=FALSE)
  })
  
  observeEvent(event_data("plotly_click",source="barplot"),
               selected_lobe(event_data("plotly_click",source="barplot")$key))
  
  # lung diagram --------------------------------------------------------------
  output$lungImage <- renderImage({
    lobe <- selected_lobe()
    fname <- if(is.null(lobe)||lobe=="")"lung_default.png" else paste0("lung_",lobe,".png")
    list(src=normalizePath(file.path("www",fname)),
         contentType="image/png",width="100%",alt=paste("Lung:",lobe%||%"default"))
  },deleteFile=FALSE)
  
  # detail plot (violin or dot-box) ------------------------------------------
  output$detailPlot <- renderPlot({
    feat_code <- toupper(feature_map[[input$feature]])
    df <- filter(ordinal_all,feature==feat_code)
    if(input$rater!="Overall") df <- filter(df,rater==input$rater)
    
    unique_scores <- sort(unique(df$score))
    is_binary <- length(unique_scores)==2 && all(unique_scores%in%0:1)
    
    if(is_binary){
      ggplot(df,aes(rater,score,colour=rater))+
        geom_jitter(width=.15,height=.05,alpha=.6,size=1)+
        geom_boxplot(width=.25,outlier.shape=NA,alpha=.8)+
        facet_grid(~lobe,switch="x")+
        scale_y_continuous(breaks=unique_scores,limits=c(-.2,1.2))+
        scale_colour_brewer(palette="Set1")+
        labs(title=paste("Binary score distribution –",input$feature),
             x="Rater",y="Score")+
        theme_minimal(base_size=12)+
        theme(strip.text=element_text(size=9),
              axis.text.x=element_text(angle=45,hjust=1),
              legend.position="none")
    } else {
      ggplot(df,aes(rater,score,fill=rater))+
        geom_violin(scale="area",bw=.4,trim=FALSE,alpha=.55,colour=NA)+
        geom_boxplot(width=.10,outlier.shape=NA,alpha=.9)+
        facet_grid(~lobe,switch="x")+
        scale_y_continuous(breaks=0:3,limits=c(-.4,3.4))+
        scale_fill_brewer(palette="Set1")+
        labs(title=paste("Score distribution –",input$feature),
             x="Rater",y="Score")+
        theme_minimal(base_size=12)+
        theme(strip.text=element_text(size=9),
              axis.text.x=element_text(angle=45,hjust=1),
              legend.position="none")
    }
  })
}

shinyApp(ui, server)

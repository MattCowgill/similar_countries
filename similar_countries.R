library(tidyverse)
library(grattantheme)
library(shiny)
library(grid)
#shinyWidgets is used to customise appearance of sliders
library(shinyWidgets)


# import data

load("oecd_data.Rda")


# function to define style for sliderbar text

grattan_slider <- function(variable, title, subtitle, default = 1){
  
  
  sliderInput(inputId = variable, 
              label = HTML(paste0(tags$span(style = "color:#6A737B", title),
                                  tags$br(),
                                  tags$span(style = "font-weight:normal; color:#6A737B; font-size:12px", subtitle))),
              min = 0, max = 2,
              value = default, step = 0.1)
}


# UI ----
ui <- fluidPage(
  
  chooseSliderSkin("Flat", color = "#F68B33"),

  # App title ----
  titlePanel(title = "", windowTitle = "Similarity scores for OECD countries"),
  
  # Title on page
  
  fluidRow(column(12,
                  span("Which countries in the OECD are most like Australia?",
                       style = "color:#6A737B; font-weight:bold; font-size:24px"))),
  
  # Intro text
  fluidRow(
    column(12,
           span("Move the sliders to choose which criteria should be used to judge countries' similarity to Australia, and how much weight those criteria should be given. Based on your weights, and countries' real data, the chart will then show you which countries are most similar to Australia.", style = "color:#6A737B")
           )
  ),
  
  br(),
  
  fluidRow(
    column(12,
           span("Choose your weights:", style = "color:#6A737B;font-weight:bold;font-size:18px"))
  ),
  
  fluidRow(
    column(12,
           span("0 = criterion isn't used; 1 = normal; 2 = extra-important", style = "color:#6A737B;font-weight:regular;font-size:12px")
    )
  ),
  br(),
  # Sidebar layout 
  fluidRow(
    
    # Sidebar with sliders
    column(2, 
           #"sidebar1",
      
      # sliderInput("lab_prod", span("Labour productivity (GDP per hour worked):", style = "color:#6A737B"),
      #             min = 0, max = 2,
      #             value = 1, step = 0.1),
      
      grattan_slider(var = "lab_prod",
                     title = "Labour productivity",
                     subtitle = "GDP per hour worked (total economy)"),
      
      grattan_slider(var = "hours_per_cap",
                     title = "Labour utilisation",
                     subtitle = "Hours worked per person (total economy)"),
      
      grattan_slider(var = "gov_spend",
                     title = "Govt. spending",
                     subtitle = "Govt. expenditure as % of GDP"),
      
      grattan_slider(var = "product_reg",
                     title = "Market regulation",
                     subtitle = "OECD index of product mkt regulation"),
      
      grattan_slider(var = "trade_share",
                     title = "International trade",
                     subtitle = "Exports + imports as % of GDP"),
      
      grattan_slider(var = "urban_popshare",
                     title = "Urbanisation",
                     subtitle = "Proportion of pop. in urban areas")
      
      
    ),
    
    column(2, 
           #"sidebar2",
           
           grattan_slider(var = "gini_net",
                          title = "Income inequality",
                          subtitle = "Gini coefficient of net income"),
           
           grattan_slider(var = "poverty_rate",
                          title = "Poverty rate",
                          subtitle = "Proportion of pop <50% median income"),
           
           grattan_slider(var = "pop_size",
                          title = "Population size",
                          subtitle = "Number of people in country (log)",
                          default = 1),
        
           grattan_slider(var = "pop_share_65plus",
                          title = "Old-age population",
                          subtitle = "Proportion of population aged 65+"),
           
           grattan_slider(var = "foreign_perc",
                          title = "Foreign-born pop.",
                          subtitle = "% of people aged 15+ born outside country"),
           
           grattan_slider(var = "lfpr_gap",
                          title = "Participation gap",
                          subtitle = "Male minus female labour force part. rate")
          

           ),
    

    
    # Main panel for displaying outputs ----
    column(8,
      #"main",

      # tableOutput("values"),
      # br(),
      plotOutput("Main_plot", height = "600px", width = "100%"),
      br()
      #plotOutput("Plot_by_subject", height = "1500px", width = "100%")
      
    )
  )
)

# server logic -
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      subject = c("lab_prod", 
                  "gini_net", 
                  "poverty_rate", 
                  "hours_per_cap", 
                  "pop_size",
                  "pop_share_65plus",
                  "foreign_perc",
                  "trade_share",
                  "lfpr_gap",
                  "urban_popshare",
                  "product_reg",
                  "gov_spend"),
      weight = c(input$lab_prod, 
                 input$gini_net, 
                 input$poverty_rate, 
                 input$hours_per_cap,
                 input$pop_size,
                 input$pop_share_65plus,
                 input$foreign_perc,
                 input$trade_share,
                 input$lfpr_gap,
                 input$urban_popshare,
                 input$product_reg,
                 input$gov_spend),
      stringsAsFactors = FALSE)
    
  })
  
  # Calculate weighted metrics
  
  wt_data <- reactive({
    
    left_join(oecd_data, sliderValues(),
              by = "subject") %>%
      # calculate weighted distance per metric
    mutate(distance = weight * (scaled_obs - AUS) ^ 2)
     
  })
  
  # Calculate summary of weighted distance by country
  wt_summ <- reactive({
    
    wt_data() %>%
      filter(!is.na(weight)) %>%
      group_by(country) %>%
      summarise(distance = sqrt(sum(distance))) %>%
      arrange(distance)
    
  })
  
  # Show the weights in an HTML table ----
  # output$values <- renderTable({
  #   sliderValues()
  # })
  
  # do a plot 
  output$Main_plot <- renderPlot({
    
    p <- wt_summ() %>%
      ggplot(aes(x = reorder(country, -distance),
               y = distance)) +
      geom_col(width = 1) +
      coord_flip() +
      scale_y_continuous_grattan() +
      theme_grattan(flipped = TRUE, base_size = 16) +
      theme(axis.ticks = element_blank() ) +
      labs(y = "Difference from Australia",
           title = "Which countries are most like Australia?",
           subtitle = "Similarity to Australia based on weighted criteria\n(lower value means more similar)",
           caption = "Notes: Similarity measured using weighted Euclidean distance on metrics standardised using z-scores.\nSource: OECD Stat and Grattan analysis")
      
    # need to tinker with the grobs to properly left-align the plot title
    g <- ggplotGrob(p)
    
    g$layout$l[g$layout$name == "title"] <- 1
    g$layout$l[g$layout$name == "subtitle"] <- 1
    g$layout$l[g$layout$name == "caption"] <- 1

    grid::grid.draw(g)
    
      
  }, height = 650)
  
}

# Create Shiny app ----
shinyApp(ui, server)

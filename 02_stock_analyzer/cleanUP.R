# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(rsconnect)
library(plotly)
library(tidyverse)
library(lubridate)
library(quantmod)
library(rvest)
library(glue)

source(file = "00_scripts/stock_analysis_functions.R")

# UI ----
ui <- fluidPage(

  title = "Stock Analyser",
    
    # 1.0 HEADER ----
    div(
      h1("Stock Analzer"),
      p("This is my second shiny project")
    ),

    # 2.0 APPLICATION UI -----
  
  
  div(#START OF BIG DIV
    
    
    column(width = 4, 
           wellPanel(
             
             pickerInput(
               p("Stock Index"),
               inputId = "index_selection", 
               choices = c("DAX", "DOW", "SP500","NASDAQ"),
               multiple = F,
               selected = FALSE,
               options = pickerOptions(
                 #`actions-box` = TRUE,
                 actionsBox = FALSE,
                 liveSearch = TRUE,
                 size = 10,
               )
             ),
             uiOutput("indices"),
             hr(),
             actionButton("analyze" ,label="analyze", icon("download")),
             p(textOutput("selected_symbol")),
             hr(),
             sliderInput("in_mavg_short", "Short Moving Average:",
                         min = 5, max = 40,
                         value = 20),
             sliderInput("int_mavg_long", "Long Moving Average:",
                         min = 50, max = 120,
                         value = 50),
           )
           
    ),
   
  
    column(width = 8,
           
          div(h4(textOutput("plot_header"))),
          verbatimTextOutput(outputId = "stock_data"),
          renderPrint(stock_data_tbl()),
          plotlyOutput(outputId = "plotly_plot")
          
          
           
      ), #END OF COLUMN8
   


    # 3.0 ANALYST COMMENTARY ----
    column(width = 12, 
           #COMMENT
           h4("Analyst Commentary"),
           verbatimTextOutput(outputId = "stock_data3"),
           renderPrint(commentrxt()),
    )
    
  ) #END OF BIG DIV
    

    
) #END OF FLUIDPAGE    

# SERVER ----
server <- function(input, output, session) {
  
  #REACTIVE FOR EXTRACTING JUST SYMBOL WITH ACTIONSBUTTON
  stock_symbol <- eventReactive(input$analyze, {
    input$stock_selection})
  
  output$selected_symbol <- renderText({stock_symbol()%>%get_symbol_from_user_input})
  
  
  #REACTIVE FOR PLOT TITLE
  stock_symbol2 <- eventReactive(input$analyze, {
    input$stock_selection}, ignoreNULL = FALSE )
  output$plot_header <- renderText(stock_symbol2())

  
  
  shortavg <- eventReactive(input$analyze, {
    input$in_mavg_short})
  
  stock_data_tbl <- reactive({
    stock_symbol() %>% get_symbol_from_user_input %>%
      get_stock_data(from = today() - days(180), 
                     to   = today(),
                     mavg_short = ,
                     input$in_mavg_long)
    
  })
  output$stock_data <- renderPrint({stock_data_tbl()})
  
  ##REACTIVE PLOT
  output$plotly_plot <- renderPlotly({
    stock_data_tbl() %>%
      plot_stock_data()
                      })
  
  
  
  
  stock_data_tbl2 <- reactive({
    stock_symbol() %>% get_symbol_from_user_input %>%
      get_stock_data(from = today() - days(180), 
                     to   = today(),
                     input$in_mavg_short,
                     input$in_mavg_long)%>%generate_commentary(stock_symbol()%>%get_symbol_from_user_input)
    
  })
  ##REACTIVE FOR COMMENT SECTION
  commentrxt <- reactive({
    stock_data_tbl2()
    
  })
  
  output$stock_data3 <- renderPrint({commentrxt()})
  
  
  
 #### stock index to stock list 
  
  stock_list_tbl <- eventReactive(input$analyze, {
    
    input$index_selection%>%get_stock_list()
    
    }, ignoreNULL = FALSE)

  
  output$indices <- renderUI({
    choices = stock_list_tbl() %>% purrr::pluck("label")
    
    pickerInput(
      inputId = "stock_selection", 
      label = "Stocks",
      choices = choices,
      multiple = FALSE,
      selected = FALSE,
      options = pickerOptions(
        actionsBox = FALSE,
        liveSearch = TRUE,
        size = 10
      )
    )
  })
  
  
  
}

# RUN APP ----
shinyApp(ui = ui, server = server)

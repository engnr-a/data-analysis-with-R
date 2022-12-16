# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)

source(file = "00_scripts/stock_analysis_functions.R")

# UI ----
ui <- fluidPage(title = "Stock Analyzer",
                # 1.0 HEADER ----
                div(
                  h1("Stock Analzer"),
                  p("This is my second shiny project")
                ),
                
                
                
                
                # 2.0 APPLICATION UI -----
                div(
                  column(width = 4, 
                         wellPanel(
                                        pickerInput(
                                                      p("Stock List (Pick one to Analyze)"),
                                                      inputId = "stock_selection", 
                                                      choices = get_stock_list(),
                                                      multiple = F,
                                                      selected = NULL,
                                                      options = pickerOptions(
                                                                               #`actions-box` = TRUE,
                                                                                actionsBox = FALSE,
                                                                                liveSearch = TRUE,
                                                                                size = 10,
                                                                                )
                                                                               ),
                           
                                                    actionButton("analyze" ,label="analyze", icon("download")),
                                                    p(
                             
                                                      textOutput("selected_symbol")
                             
                                                      ),
                             
                                                     ),
                        #actionButton(inputId ="analyze" ,"Analyze", icon("download")), 
                        
                        
                      
                  ),
                      
                  
                  column(width = 8, 
                         div(
                           
                           div(renderPrint(stock_data_tbl())
                             
                            ),
                    
                    # Add content here
                           div(
                               
                               h4(textOutput("plot_header")),
                               div(
                                 
                                 plot_stock_data()
                                 )
                               )
                    
                  ))
                ),
             
                
                
                # 3.0 ANALYST COMMENTARY ----
                
                column(width = 12, 
                       div(generate_commentary(stock_data_tbl, user_input = user_input))
                ))
                
                
                
                             
                


   

    

    

# SERVER
server <- function(input, output, session) {
  
  
  stock_symbol <- eventReactive(input$analyze, {
      input$stock_selection})
  
  output$selected_symbol <- renderText({stock_symbol()})
  output$plot_header <- renderText(stock_symbol())
  
  
  
  stock_data_tbl <- reactive({
    
    stock_symbol() %>% 
      get_stock_data(from = today() - days(180), 
                     to   = today(),
                     mavg_short = 20,
                     mavg_long  = 50)
    
  })
  output$stock_data <- renderPrint({stock_data_table()})
  
  
  
  
 
}


# RUN APP ----
shinyApp(ui = ui, server = server)

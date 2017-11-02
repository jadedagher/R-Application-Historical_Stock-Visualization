# Load packages ----
library(shiny) #interface library 
library(quantmod) #finance data library

# User interface ----
ui <- fluidPage(
  shinyUI(
    
#------------------------------------- Beginning of Page 1
    
      navbarPage("Stock Visualization",
  
            tabPanel("Equity",

  
              titlePanel("Stock Equity Tracker"),
              br(),
  
              tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
              ),
  
              sidebarLayout(
                
                sidebarPanel(
                  
                  helpText("Select a stock to examine. 
                           Information will be collected from Google finance. For example: AAPL, GOOG, AMZN"),
                  
                  textInput("symb", "Symbol", "FB"),
                  
                  dateRangeInput("dates", 
                                 "Date range",
                                 start = "2013-01-01", 
                                 end = as.character(Sys.Date())),
                  
                  selectInput("selectData", "Select your chart data", 
                              choices = list("Openning price" = 1, "High" = 2, "Low" = 3, "Closing price" = 4, "Volumes" = 5), 
                              selected = 4),
                  
                  hr(),
                  radioButtons("radioIndicators", label = h5("Indicators"),
                               choices = list("MACD indicators" = 1, "BBAND indicators" = 2, "EMA indicators" = 3), selected = 1),
                  
                  hr(),
                  br(),
                  downloadButton("downloadData", "Download data"),
                  br(),
                  helpText("Jade Dagher", align = "right")
                
                ), #end sidebarPanel Page 1
                
                
                mainPanel(
                  
                  tabsetPanel(
                    tabPanel(
                      "Chart", 
                      br(),
                      plotOutput("plot"),
                      br(),
                      verbatimTextOutput("summary")
                    ), #end tabPanel1 Page 1
                    
                    tabPanel(
                      "Table",
                      br(),
                      fluidRow(
                        column(12,
                          DT::dataTableOutput("table")
                        ) #end column
                      ) #end fluidRow
                    ) #end tabPanel2 Page 1
                  ) #end tabsetPanel Page 1
                ) #end mainPanel Page 1
              ) #end sidebarLayout Page 1
            ), #end tabPanel 1
  
#------------------------------------- End Page 1
  
#------------------------------------- Beginning of Page 2 
  
          #Page 2 
          tabPanel("Forex exchange",
             
            titlePanel("Forex Exchange Tracker"),
            br(), 
    
            sidebarLayout(
              
              sidebarPanel(
                helpText("Select a Currency to examine. 
                         Information will be collected from Google finance. For example: EUR/USD, USD/GBP, GBP/JPY"),
                
                textInput("cur", "Currency Exchange", "BTC/EUR"),
                
                helpText("Min date = current date - 180 days"),
                dateRangeInput("datesfx", 
                               "Date range",
                               start = Sys.Date() - 179, 
                               end = as.character(Sys.Date())),
                
                hr(),
                radioButtons("radioIndicatorsfx", label = h5("Indicators"),
                             choices = list("MACD indicators" = 1, "BBAND indicators" = 2, "EMA indicators" = 3), selected = 1),
                hr(),
                br(),
                downloadButton("downloadDatafx", "Download data"),
                br(),
                helpText("Jade Dagher", align = "right")
              ), #end sidebarPanel Page 2
            
              mainPanel(
              
                tabsetPanel(
                  tabPanel(
                    "Chart", 
                    br(),
                    plotOutput("plotfx"),
                    br(),
                    verbatimTextOutput("summaryfx")
                  
                  ), #end tabPanel1 Page 1
                  
                  tabPanel(
                    "Table",
                    br(),
                    fluidRow(
                      column(12,
                             DT::dataTableOutput("tablefx"))
                    )
                  ) #end tabPanel2 Page 2
                ) #end tabsetPanel Page 2
              ) #end mainPanel Page 2
            ) #end sidebarLayout Page 2
          ) #end tabPanel 2

#------------------------------------- End Page 2 

      ) #end navbarPage
  ) #end shinyUI
); #end fluidPage 


#----------------------------------------------------------------End UI 

# Server logic
server <- function(input, output) {
  
  #equity data import with quantmod library
  dataInput <- reactive({
    getSymbols(input$symb, src = "google", 
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  #forex data import with quantmod library
  dataInputfx <- reactive({
    getFX(input$cur,
               from = input$datesfx[1],
               to = input$datesfx[2],
               src = "google",
               auto.assign = FALSE)
  })
  
  #function to dowload equity data 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataInput(), file, row.names = FALSE)
    }
  )
  
  #function to dowload forex data
  output$downloadDatafx <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataInputfx(), file, row.names = FALSE)
    }
  )
    
  output$plot <- renderPlot({
    
    #equity plot data choice (Openning, High, Low, Closing, Volume)
    if (input$selectData == 1) {
      chartSeries(Op(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL) 
    }else if (input$selectData == 2) {
      chartSeries(Hi(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    }else if (input$selectData == 3) {
      chartSeries(Lo(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    }else if (input$selectData == 4) {
      chartSeries(Cl(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    }else if (input$selectData == 5) {
      chartSeries(Vo(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    }else{
      chartSeries(Lo(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    }
    
    #equity plot data indicator choice 
    if (input$radioIndicators == 1){
      return(addMACD())
    }else if (input$radioIndicators == 2){
      return(addBBands())
    }else if (input$radioIndicators == 3){
      return(addEMA())
    }else{
      return(print("Indicator error"))
    }
    
  })
  
  
  output$plotfx <- renderPlot({
    
    #forex plot 
    chartSeries(dataInputfx(), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    
    #forex plot data indicator choice 
    if (input$radioIndicatorsfx == 1){
      return(addMACD())
    }else if (input$radioIndicatorsfx == 2){
      return(addBBands())
    }else if (input$radioIndicatorsfx == 3){
      return(addEMA())
    }else{
      return(print("Indicator error"))
    }
    
  })
  
  #equity data summary display
  output$summary <- renderPrint({
    dataset <- dataInput()
    summary(dataset)
  })
  
  #forex data summary display
  output$summaryfx <- renderPrint({
    dataset <- dataInputfx()
    summary(dataset)
  })
  
  #equity data table display
  output$table <- DT::renderDataTable(DT::datatable({
    data <- dataInput()[,1:5]
    data
  }))
  
  #forex data table display
  output$tablefx <- DT::renderDataTable(DT::datatable({
    data <- dataInputfx()
    data
  }))
  
} #end server


# Run the app with shiny
shinyApp(ui, server)




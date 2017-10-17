# Load packages ----
library(shiny)
library(quantmod)

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
               Information will be collected from Google finance."),
      
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
      downloadButton("downloadData", "Download data")
    
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel(
          "Chart", 
          br(),
          plotOutput("plot"),
          br(),
          verbatimTextOutput("summary")), 
        
        tabPanel(
          "Table",
          br(),
          fluidRow(
            column(12,
            DT::dataTableOutput("table"))
          )
        )
      )
  ) #end mainPanel Page 1
  
  
  ) #end sidebarLayout Page 1
  
  
  ),#End tabPanel 1
  
#------------------------------------- End Page 1
  
#------------------------------------- Beginning of Page 2 
  
  #Page 2 
  tabPanel("Forex exchange",
             
    titlePanel("Forex Exchange Tracker"),
    br(), 
    
    sidebarLayout(
      
      sidebarPanel(
        helpText("Select a Currency to examine. 
                 Information will be collected from Google finance."),
        
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
        downloadButton("downloadDatafx", "Download data")
      ),
    mainPanel(
      
      tabsetPanel(
        tabPanel(
          "Chart", 
          br(),
          plotOutput("plotfx"),
          br(),
          verbatimTextOutput("summaryfx")), 
        
        tabPanel(
          "Table",
          br(),
          fluidRow(
            column(12,
                   DT::dataTableOutput("tablefx"))
          )
        )
      )
    ) #end mainPanel Page 1
    ) #end sidebarLayout Page 2
             
) #End tabPanel 2

#------------------------------------- End Page 2 

) #End navbarPage

) #End shinyUI

); #End fluidPage 


#----------------------------------------------------------------End UI 

# Server logic
server <- function(input, output) {
  
  #equity
  dataInput <- reactive({
    getSymbols(input$symb, src = "google", 
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  #forex
  dataInputfx <- reactive({
    getFX(input$cur,
               from = input$datesfx[1],
               to = input$datesfx[2],
               src = "google",
               auto.assign = FALSE)
  })
  
  #equity
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataInput(), file, row.names = FALSE)
    }
  )
  
  #forex
  output$downloadDatafx <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataInput(), file, row.names = FALSE)
    }
  )
    
  #equity
  output$plot <- renderPlot({
    
    if (input$selectData == 1) {
      chartSeries(Op(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL) 
    }
    if (input$selectData == 2) {
      chartSeries(Hi(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    }
    if (input$selectData == 3) {
      chartSeries(Lo(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    }
    if (input$selectData == 4) {
      chartSeries(Cl(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    }
    if (input$selectData == 5) {
      chartSeries(Vo(dataInput()), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    }
    
    if (input$radioIndicators == 1){return(addMACD())}
    if (input$radioIndicators == 2){return(addBBands())}
    if (input$radioIndicators == 3){return(addEMA())}
    
  })
  
  #forex
  output$plotfx <- renderPlot({
    
    chartSeries(dataInputfx(), theme = chartTheme("black"), 
                  type = "line", TA = NULL)
    if (input$radioIndicatorsfx == 1){return(addMACD())}
    if (input$radioIndicatorsfx == 2){return(addBBands())}
    if (input$radioIndicatorsfx == 3){return(addEMA())}
  })
  
  #equity
  output$summary <- renderPrint({
    dataset <- dataInput()
    summary(dataset)
  })
  
  #forex
  output$summaryfx <- renderPrint({
    dataset <- dataInputfx()
    summary(dataset)
  })
  
  #equity
  output$table <- DT::renderDataTable(DT::datatable({
    data <- dataInput()[,1:5]
    data
  }))
  
  #forex
  output$tablefx <- DT::renderDataTable(DT::datatable({
    data <- dataInputfx()
    data
  }))


}

# Run the app
shinyApp(ui, server)


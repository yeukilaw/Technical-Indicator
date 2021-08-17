library(shiny)
library(TTR)
library(quantmod)
library(DT)

#================= ui ==========================

sui = fluidPage(
  titlePanel(h3('Stock Analysis with Technical Indicators')),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4('Stock Selection'),
      br(),
      textInput(inputId = 'stocknr', label = 'Stock Symbol', value = '0001.HK'),
      
      dateRangeInput(inputId = 'date', label = 'Date', 
                     start = '1999-01-01', end = as.character(Sys.Date())),
      
      selectInput(inputId = 'period', label = 'Periodicity',
                  choices = c('Daily' = 'daily',
                              'Weekly' = 'weekly',
                              'Monthly' = 'monthly')
                  ),
      
      br(),
      h4('Chart Option'),
      br(),
      
      numericInput(inputId = 'n', label = 'MA Periods:', 
                   min = 1, max = 500, value = 10),
      
      selectInput(inputId = 'chartType', label = 'Choose Chart Type',
                  choices = c('Line' = 'line',
                              'Barchart' = 'bars',
                              'Candlestick' = 'candlesticks'),
                  selected = 'Line'),
      
      selectizeInput(inputId = 'techind', label = 'Choose Technical Indicators',
                  choices = c('SMA', 'EMA', 'MACD', 'RSI', 'Momentum'),
                  multiple = T),
      
      selectInput(inputId = 'bgcol', label = 'Background Colour', 
                  choices = c('Black' = 'black',
                              'White' = 'white'),
                  selected = 'Black'),
      
      br(),
      submitButton('Submit')
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title = 'Plot', plotOutput(outputId = 'distplot')),
        tabPanel(title = 'Summary', 
                 fixedRow(
                   h4("Today's Price: "),
                   tableOutput(outputId='today')),
                 br(),
                 fixedRow(
                   column(5,
                          h5("SMA Indicators: "),
                          tableOutput(outputId='sma')),
                   column(5,
                          h5("RSI Indicators: "),
                          tableOutput(outputId='rsi'))
                 ),
                 br(),
                 fixedRow(
                   h5("MACD Indicators: "),
                   tableOutput(outputId = 'macd'))
        ),
        tabPanel(title = 'History', DTOutput(outputId = 'pricerecord'))
      )
    
      )
  )
)

#================= server ==========================

sserv = function(input, output) {
  dataInput = reactive({
    getSymbols(input$stocknr, src = "yahoo",
               from = input$date[1],
               to = input$date[2],
               periodicity = input$period,
               auto.assign = FALSE)
  })
    
  output$distplot = renderPlot({
    chartSeries(dataInput(),type = input$chartType, theme=chartTheme(input$bgcol),
                TA = 'addVo()', name = input$stocknr)
    
    nlag = input$n
    if ('SMA' %in% input$techind)  print(addSMA(nlag))
    if ('EMA' %in% input$techind)  print(addEMA(nlag))
    if ('MACD' %in% input$techind)  print(addMACD())
    if ('Momentum' %in% input$techind)  print(addMomentum(nlag))
    if ('RSI' %in% input$techind)  print(addRSI(nlag))
    
  })
  
  #============== summary ==================
  
  # download this day's data
  todayData = reactive({
    getSymbols(input$stocknr, src = "yahoo", auto.assign = FALSE)
  })
  
  df = reactive ({
    
    inp = na.omit(todayData()[,4])
    
    Today = round(as.numeric(last(inp)),3)
    SMA10 = round(last(SMA(inp, 10)),3)
    SMA20 = round(last(SMA(inp, 20)),3)
    SMA50 = round(last(SMA(inp, 50)),3)
    SMA100 = round(last(SMA(inp, 100)),3)
    SMA250 = round(last(SMA(inp, 250)),3)
    
    RSI7 = round(last(RSI(inp, 7)),3)
    RSI10 = round(last(RSI(inp, 10)),3)
    RSI14 = round(last(RSI(inp, 14)),3)
    RSI20 = round(last(RSI(inp, 20)),3)
    RSI50 = round(last(RSI(inp, 50)),3)
    
    MACD8 = round(last(MACD(inp, 8, 17)),3)
    MACD12 = round(last(MACD(inp, 12, 25)),3)
    MACD21 = round(last(MACD(inp, 21, 55)),3)
    
    rn = c('Today','SMA10','SMA20','SMA50','SMA100', 'SMA250',
           'RSI7','RSI10','RSI14','RSI20','RSI50',
           'MACD8','MACD12','MACD21')
    
    tidf = list(Today,
                matrix(c(rn[2:6],c(SMA10,SMA20,SMA50,SMA100,SMA250)), 
                       ncol=2, dimnames = list(rn[2:6],c('SMA','Price'))),
                matrix(c(rn[7:11],c(RSI7,RSI10,RSI14,RSI20,RSI50)), 
                       ncol=2, dimnames = list(rn[7:11],c('RSI','Price'))),
                matrix(c(rn[12:14],c(MACD8,MACD12,MACD21)),
                       ncol=3, dimnames = list(rn[12:14],c(' ','MACD','Signal'))))
    names(tidf) = c('Today', 'SMA', 'RSI', 'MACD')
    
    return(tidf)
  })
  
  output$today = renderTable(getQuote(input$stocknr)[-1])
  output$sma = renderTable(df()$SMA)
  output$rsi = renderTable(df()$RSI)
  output$macd = renderTable(df()$MACD)
  
  #================= history ==========================
  
  histtable = reactive({
    inp = na.omit(dataInput())
    histmat = as.data.frame(inp)
    colnames(histmat) = c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
    return(histmat)
  })
  
  output$pricerecord = renderDT(histtable())
  
}

#================= runApp ==========================

app = shinyApp(
  ui = sui,
  server = sserv
)
runApp(app)


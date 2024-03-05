#Load libraries
library(shiny)
library(ggplot2)
library(ggcorrplot)
library(readr)
library(readxl)
library(haven)
library(lubridate)
library(shinyWidgets)
library(DT)
library(dplyr)

#Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .summary-container {
        padding: 10px;
        background-color: #ffffff;
        border-radius: 8px; 
        box-shadow: 0 4px 8px 1px;
        margin-top: 10px;
        border: 1px solid #ddd;
      }

      /* Title styling */
      .summary-title {
        margin-bottom: 20px;
        color: #337ab7;
        font-size: 20px;
        font-weight: bold;
        text-align: center;
      }
    "))),
  #set background color
  setBackgroundColor(
    color = c("#EAEDED","#9CB6E4"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel("Exploratory Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      helpText("*Select file of any format: csv, xls/xlsx, rds, sas7bdat, xpt"),
      fileInput("dataFile", "Choose File",
                accept = c(".csv", ".xlsx", ".xls", ".rds", ".sas7bdat", ".xpt")),
      conditionalPanel(
        condition = "input.tabSelected === 'Descriptive Statistics' 
                   | input.tabSelected === 'Visualization'
                   | input.tabSelected === 'Outliers'",
      selectInput("column", "Select Column", choices = NULL)
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabSelected",
        tabPanel("Descriptive Statistics", 
                 br(),
                 uiOutput("summaryContainer")
                 ),
        tabPanel("Visualization", br(), plotOutput("plot")),
        tabPanel("Outliers", br(), uiOutput("outliers"),br(), uiOutput("boxPlot")),
        tabPanel("Correlation Matrix", br(), plotOutput("corrPlot",width = "100%", height = "600px")),
        tabPanel("Missing Values", br(),
                 uiOutput("missingval_inputs")
                 ),
        tabPanel("Structure",br(),
                 uiOutput("structure_inputs")
                 ),
        tabPanel("Summary",br(),
                 uiOutput("summary_inputs")
        )
      )
    )
  )
)

#Define server
server <- function(input, output, session) {
  # Reactive expression to read the uploaded file based on extension
  uploadedData <- reactive({
    req(input$dataFile)
    switch(tools::file_ext(input$dataFile$name),
           csv = read.csv(input$dataFile$datapath),
           xls = read_excel(input$dataFile$datapath),
           xlsx = read_excel(input$dataFile$datapath),
           rds = readRDS(input$dataFile$datapath),
           sas7bdat = haven::read_sas(input$dataFile$datapath),
           xpt = haven::read_xpt(input$dataFile$datapath),
           stop("Unsupported file format"))
  })
  
  # Update the columns based on the uploaded data
  observe({
    df <- uploadedData()
    if (is.null(df)) return(NULL)
    updateSelectInput(session, "column", choices = colnames(df))
  })
  
  #Dynamic inputs for Descriptive Stats
  output$summaryContainer <- renderUI({
    df <- uploadedData()
    if (is.null(df)) return()
    div(class = "summary-container",
        h4("Column Summary", class = "summary-title"),
        verbatimTextOutput("stats")
    )
  })
  
  #Dynamic inputs for Dataset missing values
  output$missingval_inputs <- renderUI({
    df <- uploadedData()
    if (is.null(df)) return()
    div(class = "summary-container",
        h4("Missing Values", class = "summary-title"),
        verbatimTextOutput("missingValues")
    )
  })
  
  #Dynamic inputs for Dataset Structure
  output$structure_inputs <- renderUI({
    df <- uploadedData()
    if (is.null(df)) return()
    div(class = "summary-container",
        h4("Dataset Structure", class = "summary-title"),
        verbatimTextOutput("dataStructure")
    )
  })
  
  #Dynamic inputs for Dataset summary
  output$summary_inputs <- renderUI({
    df <- uploadedData()
    if (is.null(df)) return()
    div(class = "summary-container",
        h4("Dataset Summary", class = "summary-title"),
        verbatimTextOutput("dataSummary")
    )
  })
  
  #Descriptive statistics
  output$stats <- renderPrint({
    df <- uploadedData()
    if (is.null(df)) return()
    
    col <- input$column
    if (is.null(col)) return()
    
    data <- df[[col]]
    
    if (is.numeric(data)) {
      # Numeric stats
      stats <- list(
        Count = length(data),
        Min = min(data, na.rm = TRUE),
        Max = max(data, na.rm = TRUE),
        Sum = sum(data, na.rm = TRUE),
        Mean = mean(data, na.rm = TRUE),
        Median = median(data, na.rm = TRUE),
        Mode = names(sort(table(data), decreasing = TRUE))[1],
        Range = diff(range(data, na.rm = TRUE)),
        Quantiles=quantile(data, probs = c(0.25,0.75), na.rm = TRUE),
        IQR = IQR(data, na.rm = TRUE),
        Variance = var(data, na.rm = TRUE),
        StdDev = sd(data, na.rm = TRUE))
    } else {
      # Categorical stats
      freq <- table(data)
      modeValue <- names(sort(table(data), decreasing = TRUE))[1]
      
      stats <- list(
        Count = sum(freq),
        Mode = modeValue,
        Frequencies = freq
      )
    }
    stats
  })
  
  #Visualization
  output$plot <- renderPlot({
    df <- uploadedData()
    if (is.null(df)) return()
    
    col <- input$column
    if (is.null(col)) return()
    
    data <- df[[col]]
    
    if (is.numeric(data)) {
      # Plot for numeric data
      ggplot(data.frame(data), aes(x = data)) +
        geom_histogram(binwidth = (max(data, na.rm = TRUE) - min(data, na.rm = TRUE)) / 30, fill = "skyblue", color = "black") +
        labs(title=paste0("Histogram of ",col), x=col,y="Count")+
        theme(text = element_text(size=13),axis.text.x = element_text(size=13),axis.text.y = element_text(size=13))
    } else if (is.character(data)) {
      # Plot for categorical data
      ggplot(data.frame(data), aes(x = data)) +
        geom_bar(fill = "skyblue", color = "black") +
        geom_text(stat='count', aes(label=..count..), vjust=-0.5) + # Add bar counts
        labs(title=paste0("Barplot of ", col), x=col, y="Count") +
        theme(axis.text.x = element_text(size=13),
              text = element_text(size=13),
              axis.text.y = element_text(size=13))
      
    } else if (is.Date(data)) {
      # Plot for Date data
      ggplot(data.frame(data), aes(x =data)) +
        geom_histogram(stat = "count", bins = 30) + # Adjust bins as needed
        labs(title=paste0("Histogram of ",col), x=col,y="Count")+
        theme(text = element_text(size=13),axis.text.x = element_text(size=13),axis.text.y = element_text(size=13))
    }
  })
  
  #Correlation Matrix
  output$corrPlot <- renderPlot({
    df <- uploadedData()
    if (!is.null(df) && ncol(df) > 1) {
      numericdf <- df[sapply(df, is.numeric)]
      if (ncol(numericdf) > 1) {
        corrMatrix <- cor(numericdf, use = "complete.obs")
        ggcorrplot(corrMatrix, lab = TRUE)
      }
    }
  })
  
  #Show boxplot
  output$boxPlot <- renderUI({
    df <- uploadedData()
    if (is.null(df)) return(NULL)
    
    selectedCol <- input$column
    if (selectedCol %in% names(df) && is.numeric(df[[selectedCol]])) {
      plotOutput("box_plot")
    }
  })
  
  #Box Plot for Outlier Detection
  output$box_plot <- renderPlot({
    df <- uploadedData()
    if (is.null(df) || is.null(input$column) || !is.numeric(df[[input$column]])) return()
    ggplot(df, aes_string(x = factor(0), y = input$column)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      labs(x = NULL, y = input$column) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  })
  
  #Detect Outliers
  output$outliers <- renderUI({
    df <- uploadedData()
    col <- input$column
    var_data <- df[[col]]
    if (!is.numeric(var_data)) {
      return(tags$div("Selected column is not numeric.", 
                      style = "color: #D9534F; background-color: #F2DEDE; padding: 10px; border-radius: 5px; margin: 10px 0;"))
    }
    
    qnt <- quantile(var_data, probs=c(.25, .75), na.rm = TRUE)
    imin <- qnt[1] - 1.5 * IQR(var_data, na.rm = TRUE)
    imax <- qnt[2] + 1.5 * IQR(var_data, na.rm = TRUE)
    outliers <- var_data[!is.na(var_data) & (var_data > imax | var_data < imin)]
    
    if(length(outliers) > 0) {
      outliersText <- paste("Outliers:", toString(unique(outliers)))
      tags$p(outliersText, style = "color: black;")
    } else {
      tags$div("No outliers detected.", style = "color: black;")
    }
  })
  
  #Missing Values
  output$missingValues <- renderPrint({
    df <- uploadedData()
    if (!is.null(df)) {
      sapply(df, function(x) sum(is.na(x)))
    }
  })
  
  #Dataset Summary
  output$dataSummary <- renderPrint({
    df <- uploadedData()
    if (!is.null(df)) {
      summary(df)
    }
  })
  
  #Dataset Structure
  output$dataStructure <- renderPrint({
    df <- uploadedData()
    if (!is.null(df)) {
      str(df)
    }
  })
}

shinyApp(ui = ui, server = server)
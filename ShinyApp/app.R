library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Pre-processing of single line reading"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # X screen Resolution:
      numericInput("ResX", "X screen resolution (in pixels):", 1024, min = 1, max = 10000),
      verbatimTextOutput("value1"),

      # Y screen Resolution:
      numericInput("ResY", "Y screen resolution (in pixels):", 768, min = 1, max = 10000),
      verbatimTextOutput("value2"),
      # 
      # Y screen Resolution:
      numericInput("maxtrial", "Maximum number of trials:", 0, min = 1, max = 10000),
      verbatimTextOutput("value3"),
      
      # Y screen Resolution:
      numericInput("tBlink", "Blink detection cut-off (in ms):", 50, min = 1, max = 500),
      verbatimTextOutput("value4"),
      
      # asc file input:
      fileInput("file1", "Choose .asc file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary")
      
      # Output: HTML table with requested number of observations ----
      #textOutput("summary")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # set max seze to 100 MB to allow uploading larger files:
  options(shiny.maxRequestSize=100*1024^2)
  
  output$summary <- renderPrint({
    dataFile= NULL
    file1 = input$file1
    if (is.null(file1)) {
      return(cat("Please upload data file"))
    }
    
    dataFile= readLines(file1$datapath)
      #dataset <- datasetInput()
      #summary(dataset)
    head(dataFile)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
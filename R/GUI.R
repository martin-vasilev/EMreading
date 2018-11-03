#' 
#' Graphical user interface for data pre-processing using R shiny
#' 
#' @author Martin R. Vasilev

GUI <- function() {
  require(shiny)
  shinyApp(
    ui = fluidPage(
      
      # App title ----
      titlePanel("Pre-processing of single line reading data"),
      h4('Currently works only with data recorded with Eyetrack'),
      
      # Sidebar layout with a input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          # X screen Resolution:
          numericInput("ResX", "X screen resolution (in pixels):", 1920, min = 1, max = 10000),
          verbatimTextOutput("value1"),
          
          # Y screen Resolution:
          numericInput("ResY", "Y screen resolution (in pixels):", 1080, min = 1, max = 10000),
          verbatimTextOutput("value2"),
          # 
          # Y screen Resolution:
          numericInput("maxtrial", "Maximum number of trials:", 120, min = 1, max = 10000),
          verbatimTextOutput("value3"),
          
          # Y screen Resolution:
          numericInput("tBlink", "Blink detection cut-off (in ms):", 50, min = 1, max = 500),
          verbatimTextOutput("value4"),
          
          # asc file input:
          fileInput("file1", "Choose .asc file",
                    multiple = T,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          textInput("filename", "Output file name", "raw_fix.csv")
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          useShinyalert(),
          tags$head(tags$style(type="text/css", "
                               #loadmessage {
                               position: fixed;
                               top: 0px;
                               left: 0px;
                               width: 100%;
                               padding: 5px 0px 5px 0px;
                               text-align: center;
                               font-weight: bold;
                               font-size: 100%;
                               color: #000000;
                               background-color: #CCFF66;
                               z-index: 105;
}
")),
          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                           tags$div("Processing data...",id="loadmessage")),
          h4('Extracted data output:'),
          # Output: Verbatim text for data summary ----
          tableOutput('table'),
          conditionalPanel("output.fileUploaded",
                           downloadButton("downloadData", "Download Data"))
          #downloadButton("downloadData", "Download Data")
          
          )
        )
        ), 
    
    server = function(input, output) {
      
      # set max seze to 100 MB to allow uploading larger files:
      options(shiny.maxRequestSize=100*1024^2)
      
      
      datasetInput <- reactive({
        
        raw_fix<- NULL
        plot=TRUE
        dataFile= NULL
        file1 = input$file1
        
        if (is.null(file1)) {
          return(cat("Please upload data file"))
        }else{
          
          
          #AlldataFiles = readLines(file1$datapath)
          
          source("https://raw.githubusercontent.com/martin-vasilev/EMreading/master/R/utility.R")
          
          for(i in 1:nrow(file1)){
            dataFile<- readLines(input$file1[[i, 'datapath']])
            inFile <- input$file1
            filename= stringi::stri_extract_first(str = inFile$name[i], regex = ".*(?=\\.)")
            
            #head(dataFile)
            trial_db<- trial_info(dataFile, input$maxtrial)
            #head(trial_db)
            
            for(j in 1:nrow(trial_db)){ # for each item
              if(j!= nrow(trial_db)){
                time= 1000*2
              }else{
                time= 1000*2
              }
              shinyalert(
                title = paste("Subject", i, "Trial", j, "Filename: ", filename),
                text = "",
                closeOnEsc = TRUE,
                closeOnClickOutside = FALSE,
                html = FALSE,
                type = "info",
                showConfirmButton = FALSE,
                showCancelButton = FALSE,
                timer = time,
                imageUrl = "",
                animation = F
              )
              
              text<- get_text(dataFile[trial_db$ID[j]:trial_db$start[j]]) # get text details (Eyetrack)
              
              if(text[1]!=0){ # if trial contained text
                try(coords<- get_coord(text)) # extract text coordinates
                map<- coord_map(coords, x=input$ResX, y= input$ResY) # map them to pixels on the screen
                
                # Extract raw fixations from data and map them to the text:
                try(raw_fix_temp<- parse_fix(dataFile, map, coords, trial_db[j,], i,
                                             input$ResX, input$ResY, input$tBlink, SL= TRUE))
                
                # Combine fixations:
                if(is.null(raw_fix_temp)){
                  next;
                }
                #raw_fix_temp$sub<- i
                raw_fix_temp$dataFile= filename
                
                raw_fix<- rbind(raw_fix, raw_fix_temp)
                
                
              } else{ # if there was no text in trial, just extract fixations
                try(raw_fix_temp<- parse_fix(dataFile, map=0, coords=0, trial_db[j,], i, input$ResX,
                                             input$ResY, input$tBlink, hasText=FALSE, SL= TRUE))
                if(is.null(raw_fix_temp)){
                  next;
                }
                #raw_fix_temp$sub<- i
                raw_fix_temp$dataFile= filename
                raw_fix<- rbind(raw_fix, raw_fix_temp)
                # create picture of fixations:
                
                #if(length(raw_fix_temp)>1 & plot==TRUE){ # if data was extracted from trial
                #  plot_fix(coords, raw_fix_temp, i, j, ResX, ResY, hasText = FALSE)
                #}
              }
              
              #cat(toString(j)); cat(" ")
            } # end of item loop
          }
          
          
        }
        return(raw_fix)
        
      })
      
      output$table <- renderTable({
        head(datasetInput())
      })
      
      output$downloadData <- downloadHandler(
        filename = function() {
          input$filename
        },
        content = function(file) {
          write.csv(datasetInput(), file)
        }
      )
      
      getData <- reactive({
        if(is.null(input$file1))
        {return(NULL)}
        else {return(1)}
      })
      
      output$fileUploaded <- reactive({
        return(!is.null(getData()))
      })
      outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
      
    }
  )
}
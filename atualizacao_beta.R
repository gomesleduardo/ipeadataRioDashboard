library(shiny)
library(readxl)
library(magrittr)
library(shinythemes)
library(rJava)
library(xlsxjars)
library(xlsx)
library(stringi)

# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("simplex"),
  
  # App title ----
  titlePanel("Update - Ipeadata Macro (v0.0.1)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose .xls file to upload",
                multiple = FALSE,
                accept = c("text/xls",
                           "text/comma-separated-values,text/plain",
                           ".xls")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Texto sobre o botao
      tags$h5("Check values"),
      
      # Botao de acao
      actionButton("chec", "Check"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Texto sobre o botao
      tags$h5("Update database"),
      
      # Botao de acao
      actionButton("gener", "Update", class = "btn-primary")

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      # tableOutput("contents")
      DT::dataTableOutput("contents"),
      h4("Warnings"),
      verbatimTextOutput("txtout")
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  # Tabela que sera exibida
  output$contents <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)

    tryCatch(
      {
        # df <- read.csv(input$file1$datapath,
        #                header = input$header,
        #                sep = input$sep,
        #                quote = input$quote)
        
        # ----- Lendo xls
        df <- data.frame(read_excel(input$file1$datapath))
        
        
        # df <- data.frame(read_excel("C:/Users/b207056565/Desktop/TERC_generica.xls"))
        names(df)[1] <- "VALDATA"
        names(df)[- 1] <- toupper(names(df)[- 1])
        nvar <- ncol(df)
        df[, 1] <- as.character(as.Date(df[, 1], origin = "1900-01-01"))
        for (i in 2:nvar) {
          df[, i] <- as.numeric(df[, i])
        }
        df2 <- df
        for (i in 2:nvar) {
          df[, i] <- round(as.numeric(df[, i]), 2)
        }
        df2[, 1] <- as.Date(df2[, 1], origin = "1900-01-01")
        
        inFile <- input$file1
        file_name <- stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)")
        
        # file_name <- iconv(file_name, to="ASCII//TRANSLIT")
        
        file_name <- gsub(" ", "_", file_name)
        
        if (input$gener == 1) {
          
          write.xlsx(df2,
                     file.path("", "", "Srjn3", 
                               "Area_Corporativa", "Projeto_IPEADATA",
                               "ETL", "Generica", paste0(file_name, ".xls")),
                     sheetName = "Generica", row.names = FALSE, showNA = FALSE)
          
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(df)
    
  }, options = list(searching = FALSE))
  
  # Texto informativo dinamico conforme avanco do processo
  output$txtout <- renderText({
    
    texto <- ifelse(is.null(input$file1), 
                    "----- No file selected.",
                    "----- Uploaded successfully.")
    
    texto <- ifelse(input$chec == 1 & !is.null(input$file1), 
                    paste0(texto, "\n----- Checking values."), 
                    texto)
    
    inFile <- input$file1
    file_name <- stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)")

    # file_name <- iconv(file_name, to="ASCII//TRANSLIT")

    file_name <- gsub(" ", "_", file_name)

    # if (input$gener == 1) {
    # 
    #   write.xlsx(output$contents,
    #              file.path("", "", "Srjn3",
    #                        "Area_Corporativa", "Projeto_IPEADATA",
    #                        "ETL", "Generica", paste0(file_name, ".xls")),
    #              sheetName = "Generica", row.names = FALSE, showNA = FALSE)
    # 
    # }
    
    texto <- ifelse(input$gener == 1 & !is.null(input$file1), 
                    paste0(texto, "\n----- ", file_name, ".xls sent to Projeto_IPEADATA/ETL/Generica."), 
                    texto)
    
    return(texto)
  })
  
  # Obtendo o nome do arquivo carregado
  # file_name <- reactive({
  #   inFile <- input$file1
  #   
  #   if (is.null(inFile)) {
  #     return(NULL)
  #   }
  #     
  #   return (stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)"))
  # })
  
}

# Create Shiny app ----
shinyApp(ui, server)

## app.R ##
library(shinydashboard)
library(shiny)
library(readxl)
library(magrittr)
library(shinythemes)
library(rJava)
library(xlsxjars)
library(xlsx)
library(stringi)
library(shiny)

function(input, output) {
  
  # Tabela que sera exibida
  output$contents <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    tryCatch(
      {
        
        # ----- Lendo xls
        df <- data.frame(read_excel(input$file1$datapath))
        names(df)[1] <- "VALDATA"
        names(df)[- 1] <- toupper(names(df)[- 1])
        
        # ----- Configurando as colunas para numerico
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
        file_name <- gsub(" ", "_", file_name)
        
        # ----- Ao clicar no botao 'Atualizar'
        if (!is.null(input$file1) & (input$chec == input$gener) & (input$gener > input$limp)) {
          
          # ----- Generica no diretorio
          # write.xlsx(df2,
          #            file.path("", "", "Srjn3", 
          #                      "Area_Corporativa", "Projeto_IPEADATA",
          #                      "ETL", "Generica", paste0(file_name, ".xls")),
          #            sheetName = "Generica", row.names = FALSE, showNA = FALSE)
          
        }
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(df)
    
  }, caption = '',
  options = list(
    searching = FALSE,
    language = list(
      info = 'Mostrando de _START_ até _END_ de _TOTAL_ registros',
      search = 'Pesquisar:',
      emptyTable = 'No data available in table',
      infoEmpty = "Mostrando 0 até 0 de 0 registros",
      infoFiltered = "(Filtrados de _MAX_ registros)",
      infoPostFix = "",
      thousands = ".",
      lengthMenu = "_MENU_ resultados por página",
      loadingRecords = "Carregando...",
      processing = "Processando...",
      zeroRecords = "Nenhum registro encontrado",
      paginate = list(previous = 'Anterior', `next` = 'Próximo', 
                      first = "Primeiro", last = "Último"),
      aria = list(sortAscending = ': Ordenar colunas de forma ascendente',
                  sortDescending = ": Ordenar colunas de forma descendente")
      
    ),
    pageLength = 5
  ))
  
  # Texto informativo dinamico conforme avanco do processo
  output$txtout <- renderText({
    
    inFile <- input$file1
    file_name <- stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)")
    file_name <- gsub(" ", "_", file_name)
    
    if (is.null(input$file1)) {
      texto <- "----- Nenhum arquivo selecionado."
    }
    
    if (!is.null(input$file1) & (input$chec == input$gener) & (input$gener == input$limp)) {
      # texto <- paste0("----- ", file_name, ".xls carregado com sucesso.")
      texto <- NULL
    }
    
    if (!is.null(input$file1) & (input$chec > input$gener) & (input$gener == input$limp)) {
      texto <-  paste0("----- Checando valores de ", file_name, ".xls.")
    }
    
    if (!is.null(input$file1) & (input$chec == input$gener) & (input$gener > input$limp)) {
      texto <- paste0("----- ", file_name, ".xls enviado para a pasta ETL/Generica.")
    }
    
    return(texto)
  })
  
}
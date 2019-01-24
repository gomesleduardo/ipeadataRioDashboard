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

dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  skin = "purple",
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Atualizar", tabName = "Atualizar", icon = icon("arrow-circle-up")),
      menuItem("Atrasos", tabName = "Atrasos", icon = icon("calendar"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Atualizar",
              fluidPage(
                
                theme = shinytheme("simplex"),
                
                # App title ----
                titlePanel("Atualizacao - Ipeadata Macroeconomico (v0.0.1)"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput(inputId = "file1", 
                              label = "Escolha o arquivo .xls para upload",
                              multiple = FALSE,
                              accept = c("text/xls",
                                         "text/comma-separated-values,text/plain",
                                         ".xls"),
                              buttonLabel = "Arquivo...", 
                              placeholder = "Nenhum arquivo selecionado"),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Texto sobre o botao
                    tags$h5("Checar valores"),
                    
                    # Botao de acao
                    actionButton("chec", "Checar", icon = icon("search")),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Texto sobre o botao
                    tags$h5("Atualizar banco de dados"),
                    
                    # Botao de acao
                    actionButton("gener", "Atualizar", class = "btn-primary", icon = icon("arrow-circle-up")),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Texto sobre o botao
                    tags$h5("Limpar cache"),
                    
                    # Botao de acao
                    actionButton("limp", "Limpar", icon = icon("eraser"))
                    
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Data file ----
                    # tableOutput("contents")
                    DT::dataTableOutput("contents"),
                    h4("Avisos"),
                    verbatimTextOutput("txtout")
                  )
                  
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Atrasos",
              h2("Widgets tab content")
      )
    )
  )
)
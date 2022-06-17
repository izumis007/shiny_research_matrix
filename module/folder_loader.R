library(shiny)
library(shinyFiles)
library(tibble)
library(DT)
library(fs)
library(tidyr)
library(dplyr)
library(readr)

folderLoaderUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinyDirButton(ns("directory"), "Select Directory",title = "Select Directory with pdf files."),
      textOutput(ns("selected_folder_path"))
    ),
    fluidRow(
      DT::dataTableOutput(ns("pdftable"))
    )
  )
}

folderLoaderServer <- function(id) {
  moduleServer(
    id,
    function(input,output,session) {
      #choose directory
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)
      
      dir <- reactive({
        parseDirPath(volumes, input$directory)
      })
      
      #when directory with no matrix default content find, then make new one!
      observe({
        req(dir())
        
        tgtrds <- list.files(dir(),"research_matrix_data.rds")
        if(length(tgtrds)==0){
          message("No research_matrix_data.rds found. Create!")
          
          template <- read_csv("matrix_template.csv",col_types = cols(.default = "c"),na = character())
          pdfs <- list.files(dir(),pattern = "pdf$")
          
          basic_template <- tibble(pdfname = pdfs, x = list(template)) |> 
            unnest(c(x))
          
          write_rds(basic_template, str_glue("{dir()}/research_matrix_data.rds"))  
        }
        message("research_matrix_data.rds available! use current one.")
      })
      
      #render selected directory name
      output$selected_folder_path <- renderText({
        dir()
      })
      
      #make table with pdf list
      dat <- reactive({
        tibble(PDF = list.files(dir(),pattern="pdf",full.names = TRUE))
      })
      
      #show pdf files in table with the selected directory
      output$pdftable <- renderDataTable({
        datatable(
          data = dat(),
          selection="single",
          rownames = FALSE,
          options = list(
            dom = "t",
            scrollY = "400px",
            paging = FALSE,
            scrollCollapse = TRUE
          )
        )
      })
      
      #make reactive values for table selection
      selected_filename <- reactive({
        dat() |> 
          slice(input$pdftable_rows_selected) |> 
          pull(PDF)
      })
      
      return(reactive({selected_filename()}))
    }
  )
}

ui <- fluidPage(
  folderLoaderUI("demo")
)

server <- function(input,output,session){
  folderLoaderServer("demo")  
}

shiny::shinyApp(ui,server)
  
  

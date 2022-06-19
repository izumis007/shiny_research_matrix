library(shiny)
library(shinyFiles)
library(tibble)
library(DT)
library(fs)
library(tidyr)
library(dplyr)
library(readr)
source(here::here("module/tag_rds_process_function.R"))

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
        if(file.exists(here::here("prevpath.txt"))){
          prevpath <- read_lines(here::here("prevpath.txt"))
        }else{
          prevpath <- ""
        }
        
        current_selection <- parseDirPath(volumes, input$directory)
        
        if(length(current_selection)==0 & prevpath != ""){
          res <- prevpath
        }else{
          res <- current_selection
        }
        
        return(res)
      })
      
      #when directory with no matrix default content find, then make new one!
      #also update check tag.xlsx and see if any change applied by comparing to rds.
      observe({
        req(dir())
        pdfs <- list.files(dir(),pattern = "pdf$") # pdfs <- list.files(r"(C:\Users\ma060\Desktop\pdfs\)",pattern = "pdf$")
        
        
        tgtrds <- list.files(dir(),"research_matrix_data.rds")
        if(length(tgtrds)==0){
          tag_rds_generate_function(dir())
        }else{
          tag_rds_process_function(dir())
        }
        message("research_matrix_data.rds available! use current one.")
      })
      
      #make txt file for prevuous directory
      observe({
        if(length(dir())==0){
          #do nothing
        }else{
          write_lines(dir(),here::here("prevpath.txt"),append = FALSE)  
        }
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
  
  

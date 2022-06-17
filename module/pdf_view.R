
library(shiny)
library(stringr)

pdfviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("pdfview"))
  )
}

pdfviewServer <- function(id, fullpdfpath) {
  moduleServer(
    id,
    function(input,output,session) {
      
      pdfrootpath <- tryCatch({
        dirname(fullpdfpath)
      }, error=function(e)"")

      pdfpath <- tryCatch({
        basename(fullpdfpath)
      }, error=function(e)"")
      
      if(pdfrootpath != ""){
        addResourcePath(prefix = "pdfsrc", directoryPath = pdfrootpath)
      }
      
      output$pdfview <- renderUI({
        if(pdfrootpath != ""){
          addResourcePath(prefix = "pdfsrc", directoryPath = pdfrootpath)
          tags$iframe(style="height:1000px; width:100%; scrolling=yes",src=str_glue("pdfsrc/{pdfpath}"))  
        }else{
          tags$iframe("Select PDF file!")
        }
        
      })
    }
  )
}

# ui <- fluidPage(pdfviewUI("demo"))
# server <- function(input,output,session){
#   pdfviewServer("demo",r"(C:\Users\ma060\Desktop\pdfs\1.pdf)")
# }
# shinyApp(ui,server)
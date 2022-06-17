
#modules
# *pdf view
#   * folder loader
#   * matrix edittor
# *matrix view
# *setting view
# *language sheet

library(shiny)
library(shinydashboard)
library(tidyr)

source("module/folder_loader.R")
source("module/pdf_view.R")
source("module/vertical_matrix.R")

ui <- dashboardPage(
  header = dashboardHeader(disable = FALSE),
  sidebar = dashboardSidebar(
    fluidPage(
      wellPanel(
        folderLoaderUI("loader") 
      ))
  ),
  body = dashboardBody(
    fluidPage(
      column(width=8, pdfviewUI("pdfview")),
      column(width=4,  
             fluidRow(
               column(width=6, actionButton("save","Save")),
               column(width=6, actionButton("edit","Edit Tags"))
             ), 
             div(style="height: 600pt;overflow-y: auto;",
                 vmatrixUI(id = "matrix",direction = "vertical")))
  ))
)


server <- function(input,output,session){
  selected_pdf <- folderLoaderServer("loader")
  
  observe({
    pdfviewServer("pdfview",selected_pdf())
  })

  #make matrix contents from selected pdf  
  all_matrix <- reactive({
    dir <- dirname(selected_pdf())
    apath <- list.files(dir,pattern="research_matrix_data\\.rds",full.names = TRUE)
    return(read_rds(apath))
  })
  
  selected_matrix <- reactive({
    initial_matrix <- all_matrix() |> 
      filter(pdfname == basename(selected_pdf()))
    
    return(initial_matrix)
  })
  
  altered_matrix <- vmatrixServer(id = "matrix",matrix_contents = selected_matrix)  
  
  
  #save data for research_matrix_data.rds-----
  save_this <- reactive({
    all_matrix() |> 
      anti_join(altered_matrix(), by=c("pdfname","pid","pname","cid","cname")) |> 
      bind_rows(altered_matrix()) |>
      arrange(pdfname, pid, cid)
  })
  
  #save save_this to pdf folder----------------------
  observeEvent(input$save,{
    save_to <- dirname(selected_pdf()) |> str_c("/research_matrix_data.rds")
    write_rds(save_this(),save_to)
  })
}

shinyApp(ui,server)
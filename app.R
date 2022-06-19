
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
  header = dashboardHeader(disable = FALSE, title = "Shiny Research Matrix"),
  sidebar = dashboardSidebar(
    fluidPage(
      wellPanel(
        folderLoaderUI("loader") 
      ))
  ),
  body = dashboardBody(
    fluidPage(
      column(width=9, pdfviewUI("pdfview")),
      column(width=3,  
             fluidRow(
               column(width=6, actionButton("save","Save")),
               column(width=6, actionButton("write_matrix","Write Matrix"))
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
    print("read!")
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
  
  #write out matrix
  observeEvent(input$write_matrix, {
    
    pdfs <- dirname(selected_pdf()) |> 
      list.files(pattern = "pdf$")
    
    dat <- all_matrix()
    
    dat <- dat |> 
      filter(pid != "removed") |>
      filter(cid != "removed") |> 
      filter(pdfname %in% pdfs) |> 
      group_by(pdfname,pid,pname) |> 
      nest() |> 
      mutate(text = map(data, ~{
        adata <- .
        adata |> 
          mutate(txt = if_else(
            cname == ".default", 
            str_c(val,"\n"),
            str_c(" [",cname,"]:", val, "\n")
          )) |> 
          pull(txt) |> 
          str_c(collapse="")
      })) |> 
      ungroup() |> 
      select(pid, pname, pdfname, text)
    
    dat2 <- dat |> 
      unnest(text)
    
    pnameorder <- dat2 |> select(pid,pname) |> 
      arrange(pid) |> distinct() |> 
      pull(pname)
    
    dat3 <- dat2 |> 
      mutate(pname = factor(pname,levels=pnameorder)) |> 
      select(pname,pdfname,text) |> 
      pivot_wider(id_cols = c("pdfname"), names_from = pname, values_from=text)
    
    stylewrap <- openxlsx::createStyle(wrapText = TRUE)
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb,"matrix")
    openxlsx::writeData(wb,"matrix",dat3)
    openxlsx::addStyle(wb,"matrix",style=stylewrap,rows = c(1:nrow(dat3)), cols = c(2:ncol(dat3)), gridExpand=TRUE)
    openxlsx::saveWorkbook(wb, str_c(dirname(selected_pdf()),"/matrix.xlsx"),overwrite = TRUE)
  })
}


shinyApp(ui,server)
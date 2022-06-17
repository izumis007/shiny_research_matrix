library(purrr)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)

#direction = "vertical" or "horizontal"
vmatrixUI <- function(id, direction="vertical") {
  ns <- NS(id)
  tagList(
    uiOutput(ns("mx"))
  )
}

vmatrixServer <- function(id,matrix_contents) {
  moduleServer(
    id,
    function(input,output,session) {
      
      ns <- session$ns
      
      apdf <- reactive({matrix_contents()$pdfname[1]})
      
      #logic for display inputs
      output$mx <- renderUI({
        
        mc_row <- nrow(matrix_contents())
        
        if(mc_row==0){
          ret <- "No group available make at least one"
        }else{
          
          ret <- matrix_contents() |> 
            mutate(id    = str_glue("___{pid}___{pname}_{cid}_{cname}")) |> 
            mutate(input = pmap(list(pname,cid,cname,id,val), ~{
              if(..2=="00"){
                textAreaInput(ns(..4),label = ..1, value = ..5)
              }else{
                textInput(ns(..4),label=..3,value=..5)
              }
            }))
        }  
            
        ret <- ret |> 
          select(pid, cid, input) |> 
          group_by(pid)
        
        #get shiny inputs for parent textarea and make button for add child
        pret <- ret |> 
          filter(cid=="00")
        
        #get shiny inputs for child textinput
        cret <- ret |> filter(cid!="00")
        
        return_ui <- map(pret$pid, ~{
          current_id <- .
          parent_input <- pret |> filter(pid == current_id) |> pull(input)
          child_input  <- cret |> filter(pid == current_id) |> pull(input)
          
          wellPanel(
            fluidRow(parent_input),
            fluidRow(column(offset=1,width=11,child_input))
          )
        })
        
        return(return_ui)
      })
      
      #logic for return manipulated value
      module_returning_value <- reactive({
        mxids <- names(input)[str_detect(names(input),"^___")]
    
        new_mxdat <- tibble(pdfname = apdf(), x = mxids, val = map_chr(mxids,~input[[.]])) |> 
          extract(col=x,
                  into=c("pid","pname","cid","cname"),
                  regex="^___(\\d+)___(.+)_(\\d+)_(.+)$") |> 
          arrange(pid,cid)
      })

      return(reactive({
        module_returning_value()
      }))
    }
  )
}



ui <- fluidPage(
  vmatrixUI("demo"),
  tableOutput("out")
)
server <- function(input,output,session){
  
  matrix_contents <- reactive({
    read_rds(r"(C:\Users\ma060\Desktop\pdfs\research_matrix_data.rds)") |> 
      filter(pdfname == "1.pdf")
  })
  
  newmat <- vmatrixServer("demo",matrix_contents)
  
  output$out <- renderTable({
    temp <<- newmat()
  },sanitize.text.function=identity)
}
shinyApp(ui,server)
editTagUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=6, 
             selectInput(ns("ptag"),"Choose Parent Tag",choices=NULL),
             tags$span(
               actionButton(ns("padd"),"+"),
               actionButton(ns("pmin"),"-")
             )),
      column(width=6, 
             uiOutput(ns("ctag")),
             column(width=2, actionButton(ns("cadd"),"+"))
             )
    )
  )
}

editTagServer <- function(id,path_of_rds) {
  moduleServer(
    id,
    function(input,output,session) {
      ns <- session$ns
      
      the_path <- str_c(path_of_rds,"/research_matrix_data.rds")
      dat <- read_rds(the_path) |> 
        select(pid,pname,cid,cname) |> 
        distinct() |> 
        arrange(pid,cid)
      
      uidata <- reactive({dat})
      
      observe({
        
        uid <- uidata() |> 
          select(pid,pname) |> 
          distinct()
        
        choiceval <- uid$pid
        names(choiceval) <- uid$pname
        
        updateSelectInput(session=session, inputId = "ptag", choices = choiceval)
      })
      
      output$ctag <- renderUI({
        req(input$ptag)
        browser()
        
        uidata() |> 
          filter(pid == input$ptag) |> 
          mutate(shinyinput = )
      })
      
      
})}

ui <- fluidPage(editTagUI("demo"))
server <- function(input,output,session){
  editTagServer("demo",path_of_rds = r"(C:\Users\ma060\Desktop\pdfs)")
}
shinyApp(ui,server)
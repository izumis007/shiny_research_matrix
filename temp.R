library(shiny)
library(purrr)

tUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui"))
  )
}

tServer <- function(id,rv) {
  moduleServer(
    id,
    function(input,output,session) {
      ns <- session$ns
      output$ui <- renderUI({
        
        tagList(
          map(c(1:rv()),~{
            textInput(str_c("id",.),.,.)
          }),
          actionButton(ns("add"),"+")
        )
        
      })
      
      ret <- eventReactive(input$add,{
        ret <- rv()+1
      })
      
      return(reactive(ret()))
      
      
    }
  )
}

ui <- fluidPage(
  numericInput("num","num",value=3),
  tUI("demo")
)
server <- function(input,output,session){
  
  rev <- reactive({input$num})
  
  res <- tServer("demo",rev)
  
  observeEvent(res(),{
    updateNumericInput(session,"num",value=res())
  })
}


shinyApp(ui,server)
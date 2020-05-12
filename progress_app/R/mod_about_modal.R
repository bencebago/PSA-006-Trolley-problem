mod_about_modal_ui <- function(id){
  
  tagList(
    shinyWidgets::actionBttn(inputId = NS(id, "open_about"),
                             label = "Content of the table",
                             style = "simple",
                             color = "succes",
                             size = "lg")
  )
}

mod_about_modal_server <- function(id){
  moduleServer(id, function(input, output, session) {
    modal <- function() {
      
      modalDialog(
        easyClose = TRUE,
        footer = modalButton("Close Modal"),
        size = "l",
        includeMarkdown("www/about.Rmd"))
    }
    
    observeEvent(input$open_about, {
      showModal(modal())})
  })
}
# Load packages
library(shiny)
library(tidyverse)
library(qualtRics)
library(jsonlite)
library(lubridate)
library(DT)
library(here)
library(waiter)
library(shinyWidgets)

# Source scripts
source(here::here("R", "mod_read_data.R"))
source(here::here("R", "mod_summary_table.R"))
source(here::here("R", "moduleServer.R"))
source(here::here("R", "mod_about_modal.R"))

# Define UI
ui <- function() {
  tagList(
    
    use_waiter(),
    fluidPage(
   
   # Application title
   h1("PSA 006 Moral: Lab progress", style = "color: #326F5E; font-weight: 700;", align = "center"),
   fluidRow(
     column(1, offset = 1,
            fluidRow(
            mod_read_data_ui("read")),
              br(),
            fluidRow(
            mod_about_modal_ui("about"))
            ),
     column(8, offset = 1,
            fluidRow(
              mod_summary_table_ui("summary_raw"))),
     column(1)
      )
   )
  )
  }

# Define server logic
server <- function(input, output) {
  
  read_out <- mod_read_data_server("read")
  
  mod_summary_table_server("summary_raw", input_data_raw = read_out$raw)
  
  mod_about_modal_server("about")
}

# Run the application 
shinyApp(ui = ui, server = server)


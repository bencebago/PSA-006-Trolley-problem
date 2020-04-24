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
source("R/mod_read_data.R")
source("R/mod_summary_table.R")
source("R/moduleServer.R")

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
              mod_read_data_ui("read"))),
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
  
  mod_summary_table_server("summary_raw", input_data_raw = read_out$raw, input_data_processed = read_out$processed)
  
}

# Run the application 
shinyApp(ui = ui, server = server)


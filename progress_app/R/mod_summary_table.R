mod_summary_table_ui <- function(id){
  
  tagList(
    h2("The current state of data collection by labs",style = "color: #326F5E;"),
    DT::dataTableOutput(NS(id, "table"))
  )
}

mod_summary_table_server <- function(id, input_data_raw, input_data_processed) {
  stopifnot(is.reactive(input_data_raw))
  stopifnot(is.reactive(input_data_processed))
  
  moduleServer(id, function(input, output, session) {
    
    summary <- reactive({
      
      # data validation
      req(input_data_raw(),
          input_data_processed())
      
      started <- 
        input_data_raw() %>% 
        # Remove all practice runs
        filter(str_detect(str_to_lower(practice), "false")) %>%
        count(lab, sort = TRUE) %>% 
        tibble::as_tibble() %>% 
        rename("Number of participants started" = n)
      
      finished <- 
        input_data_raw() %>% 
        # Remove all practice runs
        filter(str_detect(str_to_lower(practice), "false")) %>%
        # Remove those who didn't finish the questionnaire
        filter(Progress >= 98) %>% 
        count(lab, sort = TRUE) %>% 
        tibble::as_tibble() %>% 
        rename("Number of participants finished" = n)
      
      all_exclusion <- input_data_processed() %>% 
        count(lab, sort = TRUE) %>% 
        tibble::as_tibble() %>% 
        rename("Number of participants after all exclusions" = n)
      
      started %>%
        left_join(., finished, by = "lab") %>% 
        left_join(., all_exclusion, by = "lab")
      })
    
    output$table <- DT::renderDataTable({
      
      DT::datatable(summary(), rownames = FALSE,
                    options = list(initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#b2dcce', 'color': '#4CA88A'});",
        "}")))
      
      })
    
    
  })
}
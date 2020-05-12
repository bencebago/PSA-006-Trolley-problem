mod_summary_table_ui <- function(id){
  
  tagList(
    h2("The current state of data collection by labs",style = "color: #326F5E;"),
    DT::dataTableOutput(NS(id, "table"))
  )
}

mod_summary_table_server <- function(id, input_data_raw) {
  stopifnot(is.reactive(input_data_raw))
  
  moduleServer(id, function(input, output, session) {
    
    # Correct answers for attention check for the tasks
    correct_answers <- read_csv(here::here("www", "correct_answers.csv"))
    
    summary <- reactive({
      
      # Data validation
      req(input_data_raw())
      
      # Data filtering
      started <- 
        input_data_raw() %>% 
        # Remove all practice runs
        filter(str_detect(str_to_lower(practice), "false"))
      
      finished <- 
        started %>% 
        # Remove those who didn't finish the questionnaire
        filter(Progress >= 98)
      
      careless <- 
        finished %>% 
        # Exclude careless responders
        filter_at(vars(careless_1, careless_2), all_vars(. != 1)) %>%
        filter(careless_3 != 2)
        
      confused <- 
        careless %>%  
        # Remove confused participants 
        filter(confusion != 3)
      
      familiarity <- 
        confused %>% 
        # Exclude those with familiarity of the topic
        filter(familiarity <= 3)
      
      technical <- 
        familiarity %>% 
        # Technical problems is not in the master questionnaire!
        filter(technical_problems != 2)
      
      native <- 
        technical %>% 
        # Exclude those who did not fill the questionnaire on their native language
        filter(native_language != 2)
      
      scenario <- 
        native %>%  
        # Remove those who can't tell which scenarios they saw
        left_join(correct_answers, by = "scenario1") %>% 
        filter(trolley_attention == trolley_answer)
      
      started %>%
        count(lab, sort = TRUE, name = "N started") %>% 
        left_join(., count(finished, lab, sort = TRUE, name = "N finished"), by = "lab") %>% 
        left_join(., count(careless, lab, sort = TRUE, name = "N careless"), by = "lab") %>% 
        left_join(., count(confused, lab, sort = TRUE, name = "N confused"), by = "lab") %>% 
        left_join(., count(familiarity, lab, sort = TRUE, name = "N familiarity"), by = "lab") %>% 
        left_join(., count(technical, lab, sort = TRUE, name = "N technical problem"), by = "lab") %>% 
        left_join(., count(native, lab, sort = TRUE, name = "N not native language"), by = "lab") %>% 
        left_join(., count(scenario, lab, sort = TRUE, name = "N scenarios"), by = "lab")
      })
    
    output$table <- DT::renderDataTable({
      
      DT::datatable(summary(), rownames = FALSE,
                    options = list(
                      scrollX = TRUE,
                      pageLength = 7,
                      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#b2dcce', 'color': '#4CA88A'});",
        "}")))
      
      })
    
    
  })
}
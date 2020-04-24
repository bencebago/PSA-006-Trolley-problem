mod_read_data_ui <- function(id){
  
  tagList(
    shinyWidgets::actionBttn(NS(id, "read"), label = "Refresh data", style = "simple", size = "lg", color = "success")
  )
}

mod_read_data_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    w <- Waiter$new(html = tagList(
      waiter::spin_4(),
      br(),
      h4("The dataset is loading! It takes a few seconds!")), color = "#b2dcce")
    
    # This is just a text file with one line, that is the API key.
    qualtrics_api_key <- read_lines("qualtrics_api_key.txt")
    
    # Read survey_id data
    qualtrics_survey_ids <- 
      read_csv(here::here("www/qualtrics_surveys.csv")) %>% 
      filter(survey_name != "PSA006_master")    
    
    # Correct answers for attention check for the tasks
    correct_answers <- read_csv("www/correct_answers.csv")
    
    qualtrics_api_credentials(api_key = qualtrics_api_key,
                              base_url = "https://lapsyde.eu.qualtrics.com")
    
    # Reading data fro
    trolley_raw <- eventReactive(input$read, {
      w$show()
      
      data <- qualtrics_survey_ids %>% 
        mutate(data = map(survey_id ,~fetch_survey( surveyID = .x,
                                                    include_display_order = TRUE,
                                                    force_request = TRUE, 
                                                    label = FALSE,
                                                    convert = FALSE) %>% 
                            mutate(practice = as.character(practice)))) %>% 
        unnest(data)
      
      return(data)
    },
    ignoreNULL = FALSE)
    
    trolley_processed <- reactive({
        
      data <- trolley_raw() %>% 
          # Aggregate randomization variables
          mutate(scenario1 = case_when(FL_24_DO_Footbridgepole == 1 ~ "Pole",
                                       FL_24_DO_Footbridgeswitch == 1 ~ "Switch",
                                       TRUE ~ NA_character_),
                 scenario2 = case_when(FL_22_DO_Standardswitch == 1 ~ "Standardswitch",
                                       FL_22_DO_Standardfootbridge == 1 ~ "Standardfootbridge",
                                       FL_22_DO_Loop == 1 ~ "Loop",
                                       FL_22_DO_Obstaclecollide == 1 ~ "Obstaclecollide",
                                       TRUE ~ NA_character_)) %>% 
          # Remove those who didn't finish the questionnaire
          filter(Progress >= 98) %>% 
          # Remove all practice runs
          filter(str_detect(str_to_lower(practice), "false")) %>% 
          # Exclude careless responders
          filter_at(vars(careless_1, careless_2), all_vars(. != 1)) %>%
          filter(careless_3 != 2) %>%
          # Remove confused participants 
          filter(confusion != 3) %>%
          # Exclude those with familiarity of the topic
          filter(familiarity <= 3) %>% 
          # Technical problems is not in the master questionnaire!
          filter(technical_problems != 2) %>% 
          # Exclude those who did not fill the questionnaire on their native language
          filter(native_language != 2) %>% 
          # Remove those who can't tell which scenarios they saw
          left_join(correct_answers, by = "scenario1") %>% 
          # Use the correct answer descriptions as attention check for the tasks
          # Only those are kept who answered to either of the tasks correctly
          filter(trolley_attention == trolley_answer) %>% 
          select(-trolley_answer, -speedboat_answer) %>% 
          # Remove the answers for a particular lab that has unflagged practice data
          filter(!(lab == "TUR_021" & StartDate < date("2020-04-22")))
      
      w$hide()
      
      return(data)
      })
    
    return(list(
      raw = trolley_raw,
      processed = trolley_processed,
      activate = reactive(input$read)
    ))
  })
}
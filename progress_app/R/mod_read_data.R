mod_read_data_ui <- function(id){
  
  tagList(
    shinyWidgets::actionBttn(NS(id, "read"),
                             label = "Refresh data",
                             style = "simple",
                             size = "lg",
                             color = "success")
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
      read_csv(here::here("www", "qualtrics_surveys.csv")) %>% 
      filter(survey_name != "PSA006_master")    
    
    qualtrics_api_credentials(api_key = qualtrics_api_key,
                              base_url = "https://lapsyde.eu.qualtrics.com")
    
    # Reading data fro
    trolley_raw <- eventReactive(input$read, {
      w$show()
      
      raw <- qualtrics_survey_ids %>% 
        mutate(data = map(survey_id ,~fetch_survey( surveyID = .x,
                                                    include_display_order = TRUE,
                                                    force_request = TRUE, 
                                                    label = FALSE,
                                                    convert = FALSE) %>% 
                            mutate(practice = as.character(practice)))) %>%
        mutate(data = map(data,
                          . %>% 
                            mutate(trolley_3_just = as.character(trolley_3_just),
                                   trolley_4_just = as.character(trolley_4_just),
                                   trolley_5_just = as.character(trolley_5_just),
                                   trolley_6_just = as.character(trolley_6_just)))) %>% 
        unnest(data) %>% 
        # Aggregate randomization variables
        mutate(scenario1 = case_when(FL_24_DO_Footbridgepole == 1 ~ "Pole",
                                     FL_24_DO_Footbridgeswitch == 1 ~ "Switch",
                                     TRUE ~ NA_character_),
               scenario2 = case_when(FL_22_DO_Standardswitch == 1 ~ "Standardswitch",
                                     FL_22_DO_Standardfootbridge == 1 ~ "Standardfootbridge",
                                     FL_22_DO_Loop == 1 ~ "Loop",
                                     FL_22_DO_Obstaclecollide == 1 ~ "Obstaclecollide",
                                     TRUE ~ NA_character_)) %>%
        filter(!(lab == "TUR_021" & StartDate < date("2020-04-22")))
      
      w$hide()
      
      return(raw)
    },
    ignoreNULL = FALSE)
    
    return(list(
      raw = trolley_raw,
      activate = reactive(input$read)
    ))
  })
}
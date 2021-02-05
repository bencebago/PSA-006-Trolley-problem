library(readxl)
library(writexl)
library(tidyverse)

infosheet <- read_xlsx("006 authorship For Filling In.xlsx", sheet = 1)

# names_sep <- 
#   infosheet %>% 
#   mutate(Firstname = str_remove(`Full Name as used for publishing`, " .*$"),
#          `Middle Name` = str_remove(`Full Name as used for publishing`, Firstname),
#          `Middle Name` = str_remove(`Middle Name`, `Last Name`)) %>% 
#   select(`Full Name as used for publishing`, `Last Name`, Firstname, `Middle Name`)

# write_xlsx(names_sep, "names_sep.xlsx")

core_team <- infosheet %>% 
  filter(!is.na(`Order in publication`)) %>% 
  mutate(`Core team` = 1L)

not_core_team <- infosheet %>% 
  filter(is.na(`Order in publication`)) %>% 
  arrange(Surname) %>% 
  mutate(`Order in publication` = 7:245,
         `Core team` = 0L,
         Investigation = TRUE) %>%
  mutate_at(vars(11:14, 16:24), ~ FALSE)
  
infosheet_clean <- bind_rows(core_team, not_core_team)

infosheet_clean <- 
  infosheet_clean %>% 
  arrange(`Order in publication`)

write_xlsx(infosheet_clean, "psa006_infosheet.xlsx")


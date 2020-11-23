library(tidyverse)
library(lubridate)
library(readxl)  
library(tidyxl)
library(zoo)


# trial code on 2014 for now
workbook_2014 <- xlsx_cells("/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/pefa.surv/data/test.xlsx")


# lay date ----------------------------------------------------------------



lay_date <- workbook_2014 %>% 
  filter(!is.na(date) & col %in% 3:6 & row %in% 6:10) 


# hatch date --------------------------------------------------------------


hatch_date <- workbook_2014 %>% 
  filter(!is.na(date) & col %in% 3:6 & row %in% 15:20) 

View(workbook_2014 %>% 
  filter(sheet == "Site8") %>%
  filter(col %in% 9:14 & row %in% 1:30))

# Chick weights -----------------------------------------------------------

sheet_list <- workbook_2014 %>% 
  filter(col %in% 9:14 & row %in% 6:30) %>%
  filter(!is.na(date) | !is.na(numeric)) %>%
  select(sheet:date) %>%
  group_by(sheet) %>%
  group_split()

for (i in 1:length(sheet_list)){
  sheet_list[[i]] <- sheet_list[[i]] %>%
    mutate(date = na.locf(date)) %>%
    filter(numeric < 3000) %>%
    do({year(.$date)<-2014; .})
}




# banding -----------------------------------------------------------------





# Comment -----------------------------------------------------------------




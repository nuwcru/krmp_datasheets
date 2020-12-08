library(tidyverse)
library(lubridate)
library(readxl)  
library(tidyxl)
library(zoo)


# trial code on 2014 for now
workbook_2014 <- xlsx_cells("data/sheet_2014.xlsx")


# lay date ----------------------------------------------------------------



lay_date <- workbook_2014 %>% 
  filter(!is.na(date) & col %in% 3:6 & row %in% 6:10) %>%
  select(site = sheet, laydate = date) %>%
  mutate(site = str_replace(site, " ",  "")) %>%
  mutate(site = str_replace(tolower(site), "site", "")) %>%
  do({year(.$laydate)<-2013; .}) 


lay_date <- lay_date %>% 
  group_by(site)  %>% 
  arrange(laydate, .by_group = TRUE) %>%
  mutate(nestling_id = row_number())
  


# hatch date --------------------------------------------------------------


hatch_date <- workbook_2014 %>% 
  filter(!is.na(date) & col %in% 3:6 & row %in% 15:20) %>%
  select(site = sheet, hatchdate = date) %>%
  mutate(site = str_replace(site, " ",  "")) %>%
  mutate(site = str_replace(tolower(site), "site", "")) %>%
  do({year(.$hatchdate)<-2013; .}) 

hatch_date <- hatch_date %>% 
  group_by(site)  %>% 
  arrange(hatchdate, .by_group = TRUE) %>%
  mutate(nestling_id = row_number())
  
# calculate incubation to check for errors

lay_date %>% left_join(hatch_date, by = c("site", "nestling_id")) %>%
  mutate(incubation_time = as.double(hatchdate - laydate, units = "days")) %>% 
  arrange(desc(incubation_time))



# Chick weights -----------------------------------------------------------
#WIP - eh

sheet_list <- workbook_2014 %>% 
  filter(col %in% 9:14 & row %in% 6:30) %>%
  filter(!is.na(date) | !is.na(numeric)) %>%
  select(sheet:date) %>%
  group_by(sheet) %>%
  group_split()

for (i in 1:length(sheet_list)){
  sheet_list[[i]] <- sheet_list[[i]] %>%
    mutate(date = na.locf(date)) %>%
    filter(!is.na(numeric)) %>%
    filter(numeric < 3000) %>%
    do({year(.$date)<-2014; .}) # some years in the dates are incorrect
}




# banding -----------------------------------------------------------------





# comment -----------------------------------------------------------------

str_clean <- function(string){
  # Lowercase
  temp <- tolower(string)
  # Remove everything that is not a number or letter
  #temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}


# various info to be pulled from comment cell

# this is going to take some creativity...
x <- workbook_2014 %>% 
  filter(col %in% 18 & row %in% 15) %>%
  select(sheet, character) %>%
  filter(!is.na(character))


dates[,1:10] <- c()
for (i in 1:length(test)){
    if (str_detect(test[i],"(\\d+):")){
      dates[i,1] <- paste0(test[i-1], "-", str_replace(test[i], ":", ""))}
    else {dates[i,i] <- paste(test[i])}
    }

    }
  


for (j in which(str_detect(test,"(\\d+):"))){
  print(j)
  
  }

test <- str_clean(x[2,2])
test[which(str_detect(test, "(\\d+):"))[1]]


str_extract_all(x[2,2], "(\\d+)", simplify = FALSE)
str_extract(tolower(x[2,2]),"\\w+:\\s\\d+")

# string length should be indicative of occupancy, but is it reliable
x %>% filter(str_length(character) > 500)
x %>% filter(str_detect(character, "PEFA"))
x %>% filter(str_detect(character, "RLHA"))
x %>% filter(str_detect(character, "nestling") | str_detect(character, "chicks"))
x %>% filter(str_detect(character, "quail"))

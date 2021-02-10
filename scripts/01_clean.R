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
  filter(str_detect(character, "Jul")  & col %in% 3:6 & row %in% 13:19)   %>%
  select(site = sheet, hatchdate = date) %>%
  mutate(site = str_replace(site, " ",  "")) %>%
  mutate(site = str_replace(tolower(site), "site", "")) %>%
  do({year(.$hatchdate)<-2014; .}) 

hatch_date <- hatch_date %>% 
  group_by(site)  %>% 
  arrange(hatchdate, .by_group = TRUE) %>%
  mutate(nestling_id = row_number())

hatch_14 <- hatch_date %>% mutate(col = case_when(
  nestling_id == 1 ~ "b",
  nestling_id == 2 ~ "g",
  nestling_id == 3 ~ "r",
  nestling_id == 4 ~ "bk"
)) %>%
  mutate(ind = paste0(site, col, "14")) %>%
  mutate(yday = yday(hatchdate))

write_csv(hatch_14, "data/hatch_14.csv")

  
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
  filter(col %in% 10 & row %in% 15) %>%
  select(sheet, character) %>%
  filter(!is.na(character)) %>%
  rename("site" = "sheet", "comment" = "character")


x %>% filter(site == "Site35")


supp <- x %>% 
  mutate(comment = tolower(comment)) %>%
  mutate(supplemented = ifelse(str_detect(comment, "supplement|quail|supplemented|quails|quial|quials"), "supp", "control"))


d <- c("quails.", "quails", "supplement", "supplement.")

str_detect(d, "quail|quails|supplement")
View(supp)

dates[,1:10] <- c()
for (i in 1:length(test)){
    if (str_detect(test[i],"(\\d+):")){
      dates[i,1] <- paste0(test[i-1], "-", str_replace(test[i], ":", ""))}
    else {dates[i,i] <- paste(test[i])}
    }
  
test <- str_clean(x[3,2])
test[which(str_detect(test, "(\\d+):"))[1]]


str_extract_all(x[2,2], "(\\d+)", simplify = FALSE)
str_extract(tolower(x[2,2]),"\\w+:\\s\\d+")

# string length should be indicative of occupancy, but is it reliable
x %>% filter(str_length(character) > 500)
x %>% filter(str_detect(character, "PEFA"))
x %>% filter(str_detect(character, "RLHA"))
x %>% filter(str_detect(character, "nestling") | str_detect(character, "chicks"))
x %>% filter(str_detect(character, "quail"))



ˇ

# Find supplemented --------------------------------------------------------

workbook_2016 <- xlsx_cells("data/sheet_2016.xlsx")

x <- workbook_2016 %>%
    # isolates comment box based on number of chars, many ways to do this
  filter(str_length(character) > 40) %>%
  select("site" = sheet, "comment" = character) %>%
  mutate(comment = tolower(comment)) %>%
  mutate(treatment = ifelse(str_detect(comment, "supplement|quail|supplemented|quails|quial|quials"), "supp", "control")) %>%
  mutate(conflict = ifelse(str_detect(comment, "supplement|quail|supplemented|quails|quial|quials") & 
                             str_detect(comment, "control|ctrl"), "yes", "no"))

x %>% filter(treatment == "supp")

# indications that site was both control and supplemented
x %>% filter(conflict == "yes")



# Nest Visit Dates --------------------------------------------------------

# load data from "Find Supplemented section"

library(stringi)

# set up empty lists, and months to iterate through - partial string matching, don't need full month
dates <- c("may", "june", "jul", "aug")
site_list <- list()
interior_list <- list()

for (i in 1:nrow(x)){
  for (t in 1:length(dates)){
    # extract characters 3n on either side of occurences of month[t] in the comment box
  days <- unlist(stri_sub_all(x[i,2], 
                              from = str_locate_all(x[i,2], paste0(dates[t]))[[1]][,1]-3, 
                              to = str_locate_all(x[i,2], paste0(dates[t]))[[1]][,2]+3))
    # extract the numbers (days) extracted in previous line
  days  <- unlist(str_extract_all(days, "(\\d+)", simplify = FALSE))
    # make data frame for month[t]
  interior_list[[t]] <- expand.grid(site = paste0(x[i,"site"]), month = paste0(dates[t]),day = days)
    # once you reach the last month, collapse list of months into the site list
  if (t == length(dates)) {site_list[[i]] <- bind_rows(interior_list)}
  }
}

site_visits <- bind_rows(site_list) %>% filter(day != "0") 

# Double check the order of levels before running this line, change to str_replace to be safe?
levels(site_visits$month) <- c("Aug", "Jul", "Jun", "May")

site_visits_2016 <- site_visits %>%
  mutate(date = lubridate::ymd(paste0("2016-",site_visits$month,"-", site_visits$day))) %>%
  select(-month, -day)



           

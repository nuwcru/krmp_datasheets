library(tidyverse)
library(lubridate)
library(readxl)  
library(tidyxl)
library(zoo)
library(stringi)



# Find supplemented --------------------------------------------------------

workbook_2017 <- xlsx_cells("data/sheet_2017.xlsx")

x <- workbook_2017 %>%
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

# load data from "Find Supplemented" section

# set up empty lists, and months to iterate through - partial string matching, don't need full month
dates <- c("may", "june", "jul", "aug")
site_list <- list()
interior_list <- list()

for (i in 1:nrow(x)){
  for (t in 1:length(dates)){
    # extract characters 3n on either side of occurences of month[t] in the comment box
    from <- str_locate_all(x[i,2], paste0(dates[t]))[[1]][,1]-3
    from[from < 0] <- 0
    to <- str_locate_all(x[i,2], paste0(dates[t]))[[1]][,2]+3
    days <- unlist(stri_sub_all(x[i,2], from = from, to = to))
    
    # extract the numbers (days) extracted in previous line
    days  <- unlist(str_extract_all(days, "(\\d+)", simplify = FALSE))
    
    # make data frame for month[t]
    interior_list[[t]] <- expand.grid(site = paste0(x[i,"site"]), month = paste0(dates[t]),day = days)
    
    # once you reach the last month, collapse list of months into the site list
    if (t == length(dates)) {site_list[[i]] <- bind_rows(interior_list)}
  }
}

site_visits <- bind_rows(site_list) %>% 
  mutate(day = as.numeric(day),
         month = as.factor(month)) %>%
  # some stray numbers in the vicinity of month
  # don't think this should be an issue
  filter(day != 0 & day < 32) 

# Fix months for dates
months <- str_to_title(levels(site_visits$month)) %>%
  str_sub(., 0, 3)
levels(site_visits$month) <- months

site_visits_2017 <- site_visits %>% 
  mutate(date = lubridate::ymd(paste0("2017-",site_visits$month,"-", site_visits$day))) %>%
  select(-month, -day)




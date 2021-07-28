library(tidyverse)

database <- read.csv(file = "~/Documents/phd/RHUL/formulation-vs-active-ingredient-project/initial_search/ActiveSubstances.csv", header = TRUE)

# get rid of introduced gaps and make first row column names
database <- database [-1, ]

for (i in 1:ncol(database)) {
  
  colnames(database) [i] <- as.character(database [1, i])
  
}

database <- database [-1, ]

# select only insecticides and acaricides

ac_in <- c("AC", "IN")

insecticides_acaricides <- tibble()

for (i in 1:length(ac_in)) {
  
  only_insecticides_acaricides <- database [grep(pattern = ac_in [i], x = database$Category), ]
  
  insecticides_acaricides <- rbind(insecticides_acaricides, only_insecticides_acaricides) 
  
}

# remove duplicates introduced by me.
no_duplicates <- insecticides_acaricides %>% unique()

# only approved or pending
search_term_ap_pen <- c("Approved", "Pending")

approved_and_pending <- tibble()

for (i in 1:length(search_term_ap_pen)) {
  
  approved_or_pending <- no_duplicates [no_duplicates$`Status under Reg. (EC) No 1107/2009` == search_term_ap_pen [i], ]
  
  approved_and_pending <- rbind(approved_and_pending, approved_or_pending)
  
}

sorted_by_approval_date <- approved_and_pending[order(as.Date(approved_and_pending$`Date of approval`, format="%d/%m/%Y")),]

sorted_by_approval_date <- sorted_by_approval_date[!duplicated(sorted_by_approval_date$Substance),]

# only after Sulfoxaflor approval date
sorted_by_approval_date$`Date of approval` <- as.Date(sorted_by_approval_date$`Date of approval`, format="%d/%m/%Y")

logical_search_term <- sorted_by_approval_date$`Date of approval` > "2015-08-18"

# sets NA to TRUE
logical_search_term [is.na(logical_search_term)] <- TRUE 

released_after_sulfoxaflor <- sorted_by_approval_date [logical_search_term,]

# remove basic substances
released_after_sulfoxaflor <- released_after_sulfoxaflor [released_after_sulfoxaflor$`Basic substance` == "", ]

# remove biopesticides
released_after_sulfoxaflor <- released_after_sulfoxaflor [-grep(pattern = "strain", x = released_after_sulfoxaflor$Substance), ]

write.csv(released_after_sulfoxaflor, "~/Documents/phd/RHUL/formulation-vs-active-ingredient-project/initial_search/ten_insecticides_to_research.csv", row.names = FALSE)
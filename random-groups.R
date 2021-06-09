
library(googlesheets4)
library(tidyverse)

url <- "https://docs.google.com/spreadsheets/d/1OjEyJrHyLeBR3RzBsb19zKNaT9pzAoGCytroV-k-dDs/"
info <- read_sheet(url, sheet = "Info")

info <- info %>% 
  separate(Emails, c("a", "b"), sep = ", ") %>% 
  pivot_longer(!Name, names_to = "temp", values_to = "Emails") %>% 
  select(!temp) %>% 
  drop_na()

## 30 participants, split into 6 groups of 5

create_sheet <- function() {
  
  participants <- unique(info$Name)  
  id <- rep(paste0("room", 1:6), each = 5)
  index <- sample(id)
  names(index) <- participants
  
  data.frame(
    "Pre-assign Room Name" = index[info$Name],
    "Email Address" = info$Emails
  )
  
}

for (L in LETTERS[1:26]) {
  write_sheet(
    data = create_sheet(), 
    ss = url, 
    sheet = L
  )
}



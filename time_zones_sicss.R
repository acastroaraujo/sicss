
library(googlesheets4)
library(tidyverse)

url <- "https://docs.google.com/spreadsheets/d/1gUsTD_3QtS1uNSbhvrXdeqB5NOlEIlHBa1UcoWu-k9Q/"
df <- read_sheet(url)

#df <- read_rds("locations.rds")
df

set_time <- function(x, tz) {
  x <- as.POSIXct(x)
  out <- as.list(rep(x, length(tz)))
  
  for (i in seq_along(out)) {
    attr(out[[i]], "tzone") <- tz[[i]]
  }
  
  lapply(out, function(x) format(x, "%H:%M"))
  
}

locations <- c(
  "Beijing" = "Asia/Shanghai",
  "Bologna" = "Europe/Rome",
  "Chicago" = "America/Chicago",
  "FGV/DAPP Brazil" = "America/Sao_Paulo",
  "HSE University" = "Europe/Moscow",
  "Helsinki" = "Europe/Helsinki",
  "Hong Kong" = "Asia/Hong_Kong",
  "Howard/Mathematica" = "America/New_York",
  "Istanbul" = "Europe/Istanbul",
  "Law" = "Europe/Amsterdam",
  "Lisbon" = "Europe/Lisbon",
  "London" = "Europe/London",
  "Los Angeles" = "America/Los_Angeles",
  "Montréal" = "America/Toronto",
  "Oxford" = "Europe/London",
  "Princeton" = "America/New_York",
  "Rutgers" = "America/New_York",
  "Stellenbosch" = "Africa/Johannesburg",
  "Taipei, Taiwan (National Chengchi University)" = "Asia/Taipei",
  "Tokyo" = "Asia/Tokyo",
  "Zürich" = "Europe/Zurich"	
)


grid <- full_join(df, enframe(locations, name = "location", value = "tzone"))
write_rds(grid, "locations.rds")

grid %>% 
  as_tibble() %>% 
  select(!geometry) %>%
  mutate(first = set_time("2021/06/01 24:00:00", tzone)) %>% 
  arrange(first) %>%
  select(location, first)


# During Second Week ------------------------------------------------------

# June 21 - June 25

grid







library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)
library(haven)

# read in data
d <- read_excel("data/past-season.xlsx",
  col_names = c("county", "lhd", "agegp", "facility",
                "doses", "date"), skip = 1)

weekly <- d %>% select(doses,date) %>% 
  mutate(week = week(date),
        isowk = isoweek(date),
        weekyr = yearweek(date)) %>%
  group_by(weekyr) %>%
  summarise(ndoses = sum(doses))
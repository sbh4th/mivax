library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)
library(haven)

# read in data
# get county disadvantage index
dd <- read_dta("data/svi-sdi.dta")

# create quintiles of SVI and SDI scores
dq <- dd %>% rename_all(tolower) %>%
  select(county, svi_totpop, svi_2020, 
         sdi_county_population, sdi_2020) %>%
  mutate(sdiq = ntile(sdi_2020, 5),
         sviq = ntile(svi_2020, 5)) 

# check quintile values
dq %>% group_by(sdiq) %>% 
  summarize(msdi = mean(sdi_2020)) %>%
  kbl() %>%
  kable_styling()

dq %>% group_by(sviq) %>% 
  summarize(msvi = mean(svi_2020))%>%
  kbl() %>%
  kable_styling()


# vaccine numerators
vn <- read_excel("data/week-15-administered.xlsx", 
  col_names = c("county", "lhd", "raceeth", "agegp",
    "sex", "facility", "doses", "date"), skip = 1)

# summarize doses by county and race (adults)
vns <- vn %>% select(county, raceeth, agegp, doses) %>%
  # recode race
  mutate(race = recode(raceeth, `Hispanic` = 1,
    `NH American Indian/Alaska Native` = 2,
    `NH Asian/Native Hawaiian/Other Pacific Islands` = 3,
    `NH Black` = 4, `NH Other Race` = 5, `NH White` = 6,
    `Unknown` = 7, .default = NA_real_),
  # recode age
    age = recode(agegp, `6 months through 4 years` = 1,
    `5 through 12 years` = 2, `13 through 17 years` = 3,
    `18 through 24 years` = 4, `25 through 49 years` = 5,
    `50 through 64 years` = 6, `65 years and older` = 7)) %>%
  
  # limit to adults
  filter(age>=4 & race<7) %>% drop_na() %>%
  group_by(county, race) %>%
  summarise(doses = sum(doses))


# vaccine denominators

vd <- read_excel("data/coverage.xlsx",
  col_names = c("season", "county", "lhd", "raceeth",
    "agegp", "sex", "month", "flu_week", "pop",
    "coverage", "date"), skip = 1)

# summarize population by county and race (adults)
vds <- vd %>% 
  filter(season == "2022-23") %>%
  select(county, raceeth, agegp, sex, pop) %>%
  # recode race
  mutate(race = recode(raceeth, `Hispanic` = 1,
    `NH American Indian/Alaska Native` = 2,
    `NH Asian/Native Hawaiian/Other Pacific Islands` = 3,
    `NH Black` = 4, `NH Other Race` = 5, `NH White` = 6,
    `Unknown` = 7, .default = NA_real_),
  # recode age
    age = recode(agegp, `6 months through 4 years` = 1,
    `5 through 12 years` = 2, `13 through 17 years` = 3,
    `18 through 24 years` = 4, `25 through 49 years` = 5,
    `50 through 64 years` = 6, `65 years and older` = 7)) %>%
  
  # first need to get mean pop by month and sex
  group_by(county,race,age,sex) %>%
  summarise(mp = mean(pop, na.rm=TRUE)) %>%
  ungroup() %>%
  
  # limit to adults
  filter(age>=4 & race<7) %>% drop_na() %>%
  group_by(county, race) %>%
  summarise(pop = sum(mp))
  

# combine vaccine doses and population by county and race
vax <- vns %>% 
  left_join(vds, by = join_by(county, race)) %>%

# now combine with county deprivation indices
  inner_join(dq, by = join_by(county)) %>%
  mutate(racef = recode_factor(race, `1` = "Hispanic",
    `2` = "NH AI/AN", `3` = "NH API", 
    `4` = "NH Black", `5` = "NH Other", 
    `6` = "NH White"))

# now collapse to calculate vaccine rates
# by race-ethnicity and deprivation

t <- vax %>% group_by(sviq, racef) %>%
  summarise(doses = sum(doses), 
            pop = sum(pop)) %>%
  mutate(rate = doses / pop * 100)

vax %>% group_by(sdiq, race) %>%
  summarise(doses = sum(doses), 
            pop = sum(pop)) %>%
  mutate(rate = doses / pop * 100) %>%
  filter(race!=5) %>%
  ggplot(aes(x = sdiq, y = rate, group=race, color=race)) + geom_line() 

library(kableExtra)

t1 <- vax %>% filter(race!=5) %>%
  group_by(racef,sviq) %>%
  summarise(doses = sum(doses), 
            pop = sum(pop)) %>%
  mutate(rate = doses / pop * 100) %>%
  select(racef,sviq,rate) %>%
  
  pivot_wider(names_from = sviq, names_prefix = "SVI",
              values_from = rate)

t1 %>%
  kbl(digits=1, escape = FALSE,
      col.names = c("", "Q1", "Q2", "Q3", "Q4", "Q5")) %>%
  add_header_above(c(" " = 1, "SVI Quintile (1=lowest)" = 5)) %>%
  kable_classic(html_font = "Helvetica", full_width = F) %>%
  footnote(general = "SVI quintiles based on unweighted distribution across counties.")

t2 <- vax %>% filter(race!=5) %>%
  group_by(racef,sdiq) %>%
  summarise(doses = sum(doses), 
            pop = sum(pop)) %>%
  mutate(rate = doses / pop * 100) %>%
  select(racef,sdiq,rate) %>%
  
  pivot_wider(names_from = sdiq, names_prefix = "SDI",
              values_from = rate)

t2 %>%
  kbl(digits=1, escape = FALSE,
      col.names = c("", "Q1", "Q2", "Q3", "Q4", "Q5")) %>%
  add_header_above(c(" " = 1, "SDI Quintile (1=lowest)" = 5)) %>%
  kable_classic(html_font = "Helvetica", full_width = F) %>%
  footnote(general = "SDI quintiles based on unweighted distribution across counties.")




%>%
  mutate(rate = doses )



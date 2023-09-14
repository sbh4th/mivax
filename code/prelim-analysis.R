#  program:  prelim-analysis.R
#  task:     preliminary analysis tables for vaccination
#  input:    svi-sdi.dta, coverage.xlsx, week-15-administered.xlsx 
#  output:   none
#  project:  mivax
#  author:   sam harper \ 2023-09-14

##### 0 #####
##### load libraries
library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)
library(haven)
library(kableExtra)
library(gglorenz)

##### 1 #####
##### generate county disadvantage index

# read in county data
dd <- read_dta(here("data", "svi-sdi.dta"))

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


##### 2 #####
##### generate vaccine rates

# vaccine numerators
vn <- read_excel(here("data", "week-15-administered.xlsx"), 
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

vd <- read_excel(here("data", "coverage.xlsx"),
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
  

##### 3 #####
##### merge vaccine and county data

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


##### 4 #####
##### generate initial tables of vaccine rates
##### by race-ethnicity and disadvantage quintiles

# vaccine rates by SVI quintile
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


# vaccine rates by SDI quintile

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



f1 <- vn %>% select(county, doses) %>%
  group_by(county) %>%
  summarise(tdoses = sum(doses)) %>%
  right_join(dq, by = join_by(county)) %>%
  mutate(svi_rate = tdoses / svi_totpop * 100,
         sdi_rate = tdoses / sdi_county_population * 100)


svi_label <- "Least vaccinated 50%\nof counties are 42% of\noverall vaccination rate"

f1 %>%
  arrange(svi_rate) %>%
  mutate(cpc = rank(svi_rate)/length(svi_rate)*100,
         cpv = cumsum(svi_rate)/sum(svi_rate)*100) %>%
  ggplot(aes(x = cpc, y = cpv, colour="coverage")) + geom_line() +
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 100,
    colour = "equality"), linetype = 'dashed') +
  coord_cartesian(expand=FALSE) +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_color_manual(values = c("#377eb8", "black")) +
  theme_bw() +
  geom_curve(aes(x = 53, y = 32, xend = 50, yend = 40),
    curvature = -0.3, arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text_npc(aes(npcx = 0.9, npcy = 0.2, label = svi_label),
                color = "#377eb8") +
  labs(x = "Cumulative Percentage of Counties, Ranked by Vaccine Coverage",
    y = "Cumulative Percentage of Vaccine Coverage",
    title = "Lorenz Curve for Relative Inequality by SVI")


   ggplot(aes(svi_rate)) +
    stat_lorenz(desc = TRUE) +
    coord_fixed() +
    geom_abline(linetype = "dashed") +
    theme_minimal() +
    # scale_x_percent() +
    # scale_y_percent() +
    # theme_ipsum_rc() +
    labs(x = "Cumulative Percentage of Counties, Ranked by Vaccine Coverage",
         y = "Cumulative Percentage of Vaccine Coverage",
         title = "Inequality Among Counties")



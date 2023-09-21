#  program:  prelim-analysis.R
#  task:     preliminary analysis tables for vaccination
#  input:    svi-sdi.dta, coverage.xlsx, week-15-administered.xlsx 
#  output:   none
#  project:  mivax
#  author:   sam harper \ 2023-09-19

##### 0 #####
##### load libraries
library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)
library(haven)
library(kableExtra)
library(ggpp)
library(marginaleffects)

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
  group_by(county, age, race) %>%
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
  group_by(county, age, race) %>%
  summarise(pop = sum(mp))
  

##### 3 #####
##### merge vaccine and county data

# combine vaccine doses and population by county and race
vax <- vns %>% 
  left_join(vds, by = join_by(county, age, race)) %>%

# now combine with county deprivation indices
  inner_join(dq, by = join_by(county)) %>%
# create race as factor
  mutate(racef = recode_factor(race, `1` = "Hispanic",
    `2` = "NH AI/AN", `3` = "NH API", 
    `4` = "NH Black", `5` = "NH Other", 
    `6` = "NH White"),
# create age as factor
  agef = recode_factor(age, `4` = "18-24 years",
    `5` = "25-49 years", `6` = "50-64 years",
    `7` = "65+ years"))

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

t1_test <- vax %>% filter(race!=5) %>%
  group_by(racef, agef, sviq) %>%
  summarise(doses = sum(doses),
            pop = sum(pop)) %>%
  mutate(pop100 = pop / 100,
         rate = doses / pop * 100,
         lnrate = log(rate),
         sviqf = as.factor(sviq))

library(RStata)
options("RStata.StataVersion" = 16)
options("RStata.StataPath"= '/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')

s_me <- '
qui poisson doses i.racef##i.sviqf, exp(pop100) cformat(%4.3f)
margins racef#sviqf, predict(ir)
qui poisson doses i.racef##i.sviqf i.agef, exp(pop100) robust
margins racef#sviqf, predict(ir)
'
stata(s_me, data.in=t1_test)

# model ignoring age
t1m <- glm(doses ~ racef * sviqf + offset(log(pop100)), 
            data = t1_test, family="poisson")

# model predictions (marginal predictions)
t1mp <- avg_predictions(t1m, newdata = datagrid(racef=unique, sviqf=unique, pop100=1), type ="response")

# reshape for table
t1mp %>% 
  select(racef, sviqf, estimate) %>%
  pivot_wider(names_from = sviqf, names_prefix = "SVI",
              values_from = estimate) %>%
  kbl(digits=1, escape = FALSE,
      caption = "Crude vaccination rates",
      col.names = c("", "Q1", "Q2", "Q3", "Q4", "Q5")) %>%
  add_header_above(c(" " = 1, "SVI Quintile (1=lowest)" = 5)) %>%
  kable_classic(html_font = "Helvetica", full_width = F) %>%
  footnote(general = "SVI quintiles based on unweighted distribution across counties.")
  
t1m2 <- glm(doses ~ racef * sviqf + as.factor(agef) + 
  offset(log(pop100)), data = t1_test, family="poisson")

# model predictions (marginal predictions)
t1mp2 <- predictions(t1m2, by = c("racef", "sviqf"), 
  newdata = datagrid(racef=unique, 
    sviqf=unique, agef=unique, pop100=1), 
  type ="response")

# reshape for table
t1mp2 %>% 
  select(racef, sviqf, estimate) %>%
  pivot_wider(names_from = sviqf, names_prefix = "SVI",
              values_from = estimate) %>%
  kbl(digits=1, escape = FALSE,
      caption = "Age-adjusted vaccination rates",
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


##### 5 #####
##### Lorenz and Concentration curves

f1 <- vn %>% select(county, doses) %>%
  group_by(county) %>%
  summarise(tdoses = sum(doses)) %>%
  right_join(dq, by = join_by(county)) %>%
  mutate(svi_rate = tdoses / svi_totpop * 100,
         sdi_rate = tdoses / sdi_county_population * 100)


f1_label <- "Least vaccinated 50%\nof counties account for\n42% of the overall\nvaccination rate"

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
  theme_bw() + theme(legend.position = "none") +
  geom_curve(aes(x = 53, y = 32, xend = 50, yend = 40),
    curvature = -0.3, arrow = arrow(length = unit(0.02, "npc"))) +
  annotate("text", label = f1_label, x = 54, y = 30, 
    size = 5, colour = "#377eb8", hjust=0) +
  labs(x = "Cumulative Percentage of Counties, Ranked by Vaccine Coverage",
       y = "Cumulative Percentage of Vaccine Coverage",
       title = "Lorenz Curve for Relative Inequality by SVI")



f2_label <- "Least advantaged 50%\nof counties account for\nnearly 50% of the overall\nvaccination rate"

f1 %>%
  arrange(desc(svi_2020)) %>%
  mutate(disadv = 1 - svi_2020,
         cpc = rank(disadv)/length(disadv)*100,
         cpv = cumsum(svi_rate)/sum(svi_rate)*100) %>%
  ggplot(aes(x = cpc, y = cpv, colour="coverage")) + geom_line() +
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 100,
                   colour = "equality"), linetype = 'dashed') +
  coord_cartesian(expand=FALSE) +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_color_manual(values = c("#377eb8", "black")) +
  theme_bw() + theme(legend.position = "none") +
  geom_curve(aes(x = 53, y = 32, xend = 50, yend = 40),
    curvature = -0.3, arrow = arrow(length = unit(0.02, "npc"))) +
  annotate("text", label = f2_label, x = 54, y = 30, 
           size = 5, colour = "#377eb8", hjust=0) +
  labs(x = "Cumulative Percentage of Counties, Ranked by SVI",
       y = "Cumulative Percentage of Vaccine Coverage",
       title = "Concentration Curve for Relative Inequality by SVI")

##### 6 #####
##### Slope and Relative Index of Inequality
t2 <- f1 %>%
  arrange(desc(svi_2020)) %>%
  mutate(disadv = 1 - svi_2020,
  ppop = 1/length(disadv),
  ridit = (cumsum(ppop) - 0.5 * ppop)/sum(ppop)) 

t2 %>%
  ggplot(aes(x = ridit, y = svi_rate)) +
    geom_point() + 
    geom_smooth(method=lm , color="#377eb8", se=T) +
  labs(x = "Position in cumulative distribution of SVI",
       y = "Vaccination rate") +
  theme_bw()

t2m <- lm(svi_rate ~ ridit, data = t2)
sii <- avg_comparisons(t2m, comparison = function(hi, lo) 
         hi - lo, vcov = "HC2")
rii <- avg_comparisons(t2m, comparison = function(hi, lo) 
         hi / lo, vcov = "HC2")

sii %>% bind_rows(rii) %>%
  add_column(measure = c("SII", "RII")) %>%
  select(measure, estimate, conf.low, conf.high) %>%
  kbl(digits = 2, escape = F,
      col.names = c("Measure", "Estimate", "95% LL", "95% UL")) %>%
  add_header_above(c(" " = 1, 
    "Slope and Relative Index of Inequality" = 3)) %>%
  kable_classic(html_font = "Helvetica", full_width = F) %>%
  footnote(general = "Standard errors adjusted for heteroskedasticity")
  
  







  

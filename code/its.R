library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)
library(haven)
library(cowplot)
library(forecast)
library(fpp2)
library(tsModel)
library(modelr)
library(tsModel)
library(modelsummary)
library(kableExtra)


# read in data
d <- read_excel("data/past-season.xlsx",
  col_names = c("county", "lhd", "agegp", "facility",
                "doses", "date"), skip = 1)


# convert to weekly counts of vaccine doses
weekly <- d %>% select(doses,date) %>%
  # limit to two full years
  filter(date>="2020-01-01" & date<="2021-12-31") %>% 
  mutate(week = week(date),
        weekyr = yearweek(date),
        month = month(date),
        event = make_yearweek(
          year = 2021L, week = 1L,
          week_start = getOption("lubridate.week.start", 1)
        ),
        tsevent = weekyr - event,
        post = if_else(tsevent>=0,1,0),
        time = as.integer(weekyr - min(weekyr))) %>%
  group_by(weekyr) %>%
  summarise(ndoses = sum(doses),
            tsevent = mean(tsevent),
            post = mean(post),
            time = mean(time),
            week = mean(week),
            month = mean(month)) %>%
  mutate(fyear = year(weekyr),
         fseason = if_else(week < 27, 
           paste(fyear-1, fyear, sep = "-"),
           paste(fyear, fyear + 1, sep = "-")))

# simple plot
weekly %>% ggplot(aes(x = weekyr, y=ndoses)) +
  geom_point() + geom_line() + theme_bw() +
  scale_y_continuous(labels = ~ format(.x, scientific = FALSE)) +
  labs(x = "Year and Week", y = "Number of doses")

# Poisson model with harmonic terms
# note no offset as not provide in dataset
ma <- glm(ndoses ~ post + time + harmonic(week,2,52.25), 
              family=poisson, data=weekly)

check_overdispersion(ma)

modelsummary(list("Conventional SE" = ma,
                  "Robust SE" = ma),
             vcov = list("iid", "HC3"),
             gof_omit = ".*")

# plot with predictions vs. actual
weekly %>%
  add_predictions(ma, type = "response") %>%
  # mutate(pdoses = exp(pred)) %>%
  ggplot(aes(x = weekyr, y = ndoses)) +
  geom_point() + geom_line() + theme_bw() +
  geom_line(aes(x = weekyr, y = pred), 
            color = "#377eb8") +
  scale_y_continuous(labels = ~ format(.x, scientific = FALSE)) +
  labs(x = "Year and Week", y = "Number of doses") +
  ggtitle("Observed and model predicted counts")


## simulated data for same 2-year period
tib <- tibble(
  week = rep(seq(from = 1, to = 52, by =1), 2),
  year = rep(2020:2021, each = 52),
  time = seq(from = 1, to=length(week), by = 1),
  post = if_else(year >= 2021, 1, 0),
  tsince = if_else(post==1, time - 52, 0),
  u_x = rnorm(length(week)),
  # estimate = ifelse(post == 1, time - min(time), 0),
  pop100 = 90000 + (900000 * 0.00025 * time)
  ) %>%
  mutate(
    lambda = 9 + (0.00 * time) + (0 * post) +
    (0 * post * tsince) + 
      -1.9 * sin(2 * pi * week / 52.25) +
      0.38 * sin(2 * pi * 2 * week / 52.25) +
      2.65 * cos(2 * pi * week / 52.25) +
      -1.28 * cos(2 * pi * 2 * week / 52.25) +
      0.25 * u_x,
    lambda_o = exp((lambda - 11.5) + log(pop100)),
    doses = rpois(length(week), exp(lambda)),
    doses2 = rpois(length(week), lambda_o),
    rate = doses / pop100)

ms <-glm(doses ~ post + time + harmonic(week, 2, 52.25), 
  family = poisson, data=tib)

check_overdispersion(ms)

# comparison of count models for actual vs.
# simulated data
modelsummary(list("Observed" = ma, "Simulated" = ms,
  "Observed" = ma, "Simulated" = ms),
  vcov = list("iid", "iid", "HC3", "HC3"),
  gof_omit = ".*", output = "kableExtra") %>%
  add_header_above(c(" " = 1, "Conventional SE" = 2, 
    "Robust SE" = 2))


# model including offset
mso <- glm(doses2 ~ post + time + 
  harmonic(week, 2, 52.25) + offset(log(pop100)), 
  family = poisson, data=tib)



res_sim <- residuals(test_model, type="deviance")

# compare real vs. simulate models
cm <- 

tib %>% # filter(time<105) %>%
  mutate(yw = make_yearweek(
    year = year, week= week),
    pop100 = 1) %>%
      add_predictions(mso, type = "response") %>%
  ggplot(aes(x = yw, y = rate)) +
  geom_point() + geom_line() + theme_bw() +  
  geom_line(aes(x = yw, y = pred), 
            color = "#377eb8") +
  labs(x = "Year and Week", y = "Vaccinated (%)") +
  ggtitle("Simulated and model predicted rates")

# comparison of observed and simulated data
weekly %>% select(time,ndoses) %>%
  rename(doses = ndoses) %>%
  mutate(type = "Observed") %>%
  bind_rows(tib %>% 
              select(time, doses) %>%
              mutate(type = "Simulated")) %>%
  ggplot(aes(x = time, y = doses, 
             color = type, group = type)) +
    geom_point() + geom_line() + theme_bw() +
    scale_color_manual("Data", 
      values = c("#377eb8", "#e41a1c"))


iseed  = 20230923
nrep <- 10  

set.seed(iseed)

# create function to make fake dataset
make_data <- function(b1 = 0, 
              b2 = 0, b3 = 0) {
  
  tibble(
    week = rep(seq(from = 1, to = 52, by =1), 6),
    year = rep(2017:2022, each = 52),
    time = seq(from = 1, to=length(week), by = 1),
    post = if_else(time > 196, 1, 0),
    tsince = if_else(post==1, time - 196, 0),
    post_tsince = post * tsince,
    u_x = rnorm(length(week)),
    pop100 = 90000 + (900000 * 0.00005 * time)
    ) %>%
  mutate(
    lambda = 9 + (b1 * time) + (b2 * post) +
    (b3 * post_tsince) + 
      -1.9 * sin(2 * pi * week / 52.25) +
      0.38 * sin(2 * pi * 2 * week / 52.25) +
      2.65 * cos(2 * pi * week / 52.25) +
      -1.28 * cos(2 * pi * 2 * week / 52.25) +
      0.25 * u_x,
    lambda_o = exp((lambda - 11.5) + log(pop100)),
    doses = rpois(length(week), exp(lambda)),
    doses2 = rpois(length(week), lambda_o),
    rate = doses / pop100)
}

# make dataset
data <- make_data()

# simple plot of time series
data %>% ggplot(aes(x = time, y = rate)) +
  geom_point() + geom_line() + 
  geom_vline(xintercept = 196, 
   linetype = "dashed", color = "gray60") +
  labs(y = "Weekly vaccination rate (%)",
       x = "Weeks since 2017") +
  geom_smooth(aes(x = time, y = rate), 
    data=subset(data, post==0), method="lm",
    color = "#377eb8") +
  geom_smooth(aes(x = time, y = rate), 
    data=subset(data, post==1), method="lm",
    color = "#e41a1c") +
  theme_bw()

# function to run ITS model

run_ITS = function(...) {
  
  # resimulate the data
  data <- make_data()
  
  # estimate the model
  mod <- glm(doses2 ~ post + time + post_tsince +
    harmonic(week, 2, 52.25) + offset(log(pop100)), 
    family = quasipoisson, data=data)
  
  # grab the results for post
  coef_results[i] <- coef(model)[2]
  sig_results[i] <- tidy(model)$p.value[2] <= .05
  
}

coef_results <- c()
sig_results <- c()

for (i in 1:2000) {
  # Have to re-create the data EVERY TIME or it will just be the same data over and over
  data <- make_data()
  
  # Run the analysis
  mod <- glm(doses2 ~ post + time + post_tsince +
    harmonic(week, 2, 52.25) + offset(log(pop100)), 
    family = quasipoisson, data=data)
  
  # Get the results
  coef_results[i] <- coef(mod)[2]
  sig_results[i] <- tidy(mod)$p.value[2] <= .05
}

data %>% 
  mutate(yw = make_yearweek(
    year = year, week= week),
    pop100 = 1) %>%
  add_predictions(mod, type = "response") %>%
  ggplot(aes(x = yw, y = rate)) +
  geom_point() + geom_line() + theme_bw() +  
  geom_line(aes(x = yw, y = pred), 
            color = "#377eb8") +
  labs(x = "Year and Week", y = "Vaccinated (%)") +
  ggtitle("Simulated and model predicted rates") 


# Geoms for base plot
ggplot_its_base <- function(data, n_years) {
  ggplot(data, aes(x = months_elapsed)) +
    ylab('Counts (n)') +
    xlab('Month/Seasons elapsed') +
    scale_x_continuous(breaks = seq(1, n_years*12, by = 6),
                       labels = function(x) {paste0('Season ', ceiling(x/12), ' \n(', data$months[x],')')}) + # Start at Sept
    theme_minimal() + 
    theme(legend.position = 'top', 
          legend.title = element_blank(), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(vjust = 5))
  
}

# Additional geoms for predictions
ggplot_its_base <- function(data) {
  ggplot(data, aes(x = time)) +
    ylab('Counts (n)') +
    xlab('Weeks elapsed') +
    scale_x_continuous(breaks = seq(1, 157, by = 36)) +
    theme_minimal() + 
    theme(legend.position = 'top', 
          legend.title = element_blank(), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(vjust = 5))
  
}

ggplot_its_pred <- function(plot) {
  list(
    geom_ribbon(aes(ymin = `Q5`, ymax = `Q95`), fill = 'grey90'),
    geom_point(aes(y = Estimate, color = 'Predictions')),
    geom_line(aes(y = Estimate, color = 'Predictions')),
    geom_point(aes(y = ndoses, color = 'Actual')),
    geom_segment(aes(xend = time, y = ndoses, yend  = Estimate), 
                 linetype = 'dotted', color = 'grey20'),
    scale_color_manual(values = c('Predictions' = 'steelblue2', 'Actual' = 'forestgreen'))
  )
}



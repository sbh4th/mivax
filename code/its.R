library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)
library(haven)
library(cowplot)
library(fpp2)
library(tsModel)
library(modelr)
library(tsModel)

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
            month = mean(month))

library(RStata)
options("RStata.StataVersion" = 16)
options("RStata.StataPath"= '/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')

model1 <- glm(ndoses ~ post + time, family=poisson, 
              data=weekly)
summary(model1)

model2 <- glm(ndoses ~ post + time, family=quasipoisson, 
              data=weekly)
summary(model2)

weekly %>% ggplot(aes(x = weekyr, y=ndoses)) +
  geom_line()

library(forecast)

components <- decompose(ts(weekly$ndoses, frequency=52))
plot(components)


res2 <- residuals(model2, type="deviance")

ggtsdisplay(res2)

pattern <- decompose(ts(res2, frequency=52))
plot(pattern)

model3 <- glm(ndoses ~ post + time + harmonic(week,6,52), 
              family=quasipoisson, data=weekly)
summary(model3)

res3 <- residuals(model3, type="deviance")
ggtsdisplay(res3)

grid <- data.frame(x = seq(0, 1, length = length(weekly)))
grid %>% add_predictions(m1)

t <- weekly %>% # filter(time>25 & time<130) %>%
  add_predictions(model5) %>%
  mutate(pdoses = exp(pred)) %>%
  ggplot(aes(x = weekyr, y = ndoses)) +
  geom_point() +
  geom_line(aes(x = weekyr, y = pdoses)) 
+
  scale_x_continuous(breaks = seq(from = 0, to = 50, by=10))

model4 <- glm(ndoses ~ post + time + harmonic(week,3,52), 
              family=quasipoisson, data=weekly)
summary(model4)

model5 <- glm(ndoses ~ post + time + harmonic(week,2,52.25), 
              family=poisson, data=weekly)
summary(model5)

res4 <- residuals(model4, type="deviance")
ggtsdisplay(res4)

library(brms)
library(cmdstanr)

model_br <- brm(ndoses ~ time + post + s(week),
                data = weekly,
                family = poisson(),
                sample_prior = 'yes',
                chains=4, cores=4,
                backend = 'cmdstanr')

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

plotB <- as.data.frame(predict(model_br, probs = c(0.05, 0.95))) |> 
  cbind(weekly) |>
  ggplot(aes(x = time)) +
  ggplot_its_pred()


## simulated data
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

test_model <-glm(doses ~ post + time + harmonic(week, 2, 52.25), 
  family = poisson, data=tib)

test_model_o <- glm(doses2 ~ post + time + harmonic(week, 2, 52.25) + offset(log(pop100)), family = poisson, data=tib)

test_model_r <- glm(rate ~ post + time + harmonic(week, 2, 52.25), family = poisson, data=tib)

res_sim <- residuals(test_model, type="deviance")

# compare real vs. simulate models
cm <- 

tib %>% # filter(time<105) %>%
  mutate(yw = make_yearweek(
    year = year, week= week),
    pop100 = 1) %>%
      add_predictions(test_model_o, type = "response") %>%
  # mutate(pdoses = exp(pred)) %>%
  ggplot(aes(x = yw, y = pred)) +
  geom_point() +
  geom_line(aes(x = yw, y = pred))

weekly %>% select(time,ndoses) %>%
  rename(doses = ndoses) %>%
  mutate(type = "Observed") %>%
  bind_rows(tib %>% 
              select(time, doses) %>%
              mutate(type = "Simulated")) %>%
  ggplot(aes(x = time, y = doses, 
             color = type, group = type)) +
    geom_point() + geom_line() + theme_bw() +
    scale_color_manual("Data", values = c("#377eb8", "#e41a1c"))


iseed  = 20201221
nrep <- 100  
true_mu <- 1
set.seed(iseed)

# create function to make fake dataset
make_data <- function(b1 = 0, 
              b2 = 0, b3 = 0) {
  
  tibble(
    week = rep(seq(from = 1, to = 52, by =1), 6),
    year = rep(2017:2022, each = 52),
    time = seq(from = 1, to=length(week), by = 1),
    post = if_else(year >= 2021, 1, 0),
    tsince = if_else(post==1, time - 52, 0),
    u_x = rnorm(length(week)),
    pop100 = 90000 + (900000 * 0.0005 * time)
    ) %>%
  mutate(
    lambda = 9 + (b1 * time) + (b2 * post) +
    (b3 * post * tsince) + 
      -1.9 * sin(2 * pi * week / 52.25) +
      0.38 * sin(2 * pi * 2 * week / 52.25) +
      2.65 * cos(2 * pi * week / 52.25) +
      -1.28 * cos(2 * pi * 2 * week / 52.25) +
      0.25 * u_x + log(pop100),
    doses = rpois(length(week), exp(lambda)))
}

# make dataset
data <- make_data()

# simple plot of time series
data %>% ggplot(aes(x = time, y = doses)) +
  geom_point() + geom_line() + theme_bw()

# function to run ITS model

run_ITS = function(...) {
  
  # resimulate the data
  data <- make_data()
  
  # estimate the model
  mod <- glm(doses)
  
}

  



library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)
library(haven)
library(cowplot)
library(performance)
library(tsModel)
library(modelr)
library(modelsummary)
library(kableExtra)
library(sandwich)
library(lmtest)


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

# compare real vs. simulated models
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
data <- make_data(b1 = 0, b2 = 0, b3 = 0)

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



# simulate results for whole population

coef_results <- c()
sig_results <- c()

for (i in 1:2000) {
  # Have to re-create the data EVERY TIME or it will just be the same data over and over
  data <- make_data(b1=0.01,b2=0.3,b3=0)
  
  # Run the analysis
  mod <- glm(doses2 ~ time + post + post_tsince +
    harmonic(week, 2, 52.25) + offset(log(pop100)), 
    family = poisson, data=data)
  
  # Get the results
  # use robust SEs
  tmod <- tidy(coeftest(mod, vcov = vcovHAC))
  coef_results[i] <- tmod$estimate[3]
  sig_results[i] <- tmod$p.value[3] <= .05
}

mean(sig_results)

results_tib <- tibble(coef = coef_results,
                         sig = sig_results)

ggplot(results_tib, aes(x = coef)) + 
  geom_density() +   theme_bw() + 
  labs(x = 'Coefficient', y = 'Density') +
  ggtitle("Distribution of estimated coefficients across 500 simulations when `post` = 0.3")


# Create function to test different effect sizes
post_power <- function(effect) {
  # empty object to hold power values
  sig_results <- c()
  
  for (i in 1:500) {
  # re-create simulated data
  data <- make_data(b1=0,b2=effect,b3=0)
  
    # Run the analysis
  mod <- glm(doses2 ~ time + post + post_tsince +
    harmonic(week, 2, 52.25) + offset(log(pop100)), 
    family = poisson, data=data)
  
  # Get the results
  # use robust SEs
  tmod <- tidy(coeftest(mod, vcov = vcovHC))

  sig_results[i] <- tmod$p.value[3] <= .05
    
  }
  sig_results %>%
    mean() %>%
    return()
}

# Now try different effect sizes
power_levels <- c()

effects <- c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)

for (i in 1:7) {
  power_levels[i] <- post_power(effects[i])
}

# Where do we cross 80%?
power_results <- tibble(effect = effects,
                        power = power_levels)
kbl(power_results) %>%
  kable_styling()

data <- make_data(b2 = 0.3)

mod <- glm(doses2 ~ post + time + post_tsince +
    harmonic(week, 2, 52.25) + offset(log(pop100)), 
    family = poisson, data=data)

p1 <- data %>% 
  mutate(pop100 = 1) %>%
  add_predictions(mod, type = "response") 

p1 %>%
  ggplot(aes(x = time, y = rate)) +
  geom_point(color = "gray60") + 
  geom_line(color = "gray60") + theme_bw() +
  geom_line(aes(x = time, y = pred), 
    data=subset(p1, post==0), color = "#377eb8") +
  geom_smooth(aes(x = time, y = pred), 
    data=subset(p1, post==0), color = "#377eb8",
    method = 'lm', se = FALSE) +
  geom_line(aes(x = time, y = pred), 
    data=subset(p1, post==1), color = "#e41a1c") +
  geom_smooth(aes(x = time, y = pred), 
    data=subset(p1, post==1), color = "#e41a1c",
    method = 'lm', se = FALSE) +
  labs(x = "Year and Week", y = "Vaccinated (%)") +
  ggtitle("Simulated and model predicted rates") 

# get marginal estimates for post and
# make contrast

# marginal predictions for post
mp <- predictions(mod, newdata = datagrid(post=c(0,1), 
  pop100=1), type = "response", vcov = "HAC")

mp %>% select(post, estimate, std.error,
  conf.low, conf.high) %>%
  kbl(digits = 5) %>% kable_styling()

# now the contrast between predictions
mc <- avg_slopes(mod, newdata = datagrid(post=c(0,1), 
  pop100=1), type = "response", vcov = "HC3")

mc %>%
  filter(term=="post") %>%
  select(term, estimate, std.error, 
         conf.low, conf.high) %>%
  kbl(digits = 5) %>% kable_styling()


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

# create function to make fake dataset
make_datab <- function(b1 = 0, 
              b2 = 0, b3 = 0) {
  
  tibble(
    week = rep(seq(from = 1, to = 52, by =1), 6),
    year = rep(2017:2022, each = 52),
    time = seq(from = 1, to=length(week), by = 1),
    post = if_else(time > 196, 1, 0),
    tsince = if_else(post==1, time - 196, 0),
    post_tsince = post * tsince,
    u_x = rnorm(length(week)),
    pop100 = 14000  + (14000  * 0.00005 * time)
    ) %>%
  mutate(
    lambda = 6.5 + (b1 * time) + (b2 * post) +
    (b3 * post_tsince) + 
      -1.9 * sin(2 * pi * week / 52.25) +
      0.38 * sin(2 * pi * 2 * week / 52.25) +
      2.65 * cos(2 * pi * week / 52.25) +
      -1.28 * cos(2 * pi * 2 * week / 52.25) +
      0.3 * u_x,
    lambda_o = exp((lambda - 9.5) + log(pop100)),
    doses = rpois(length(week), exp(lambda)),
    doses2 = rpois(length(week), lambda_o),
    rate = doses / pop100)
}

datab <- make_datab()

msob <- glm(doses2 ~ time + post + 
  harmonic(week, 2, 52.25) + offset(log(pop100)), 
  family = poisson, data=datab)

datab %>% # filter(time<105) %>%
  mutate(yw = make_yearweek(
    year = year, week= week),
    pop100 = 1) %>%
      add_predictions(msob, type = "response") %>%
  ggplot(aes(x = yw, y = rate)) +
  geom_point() + geom_line() + theme_bw() +  
  geom_line(aes(x = yw, y = pred), 
            color = "#377eb8") +
  labs(x = "Year and Week", y = "Vaccinated (%)") +
  ggtitle("Simulated and model predicted rates")

coef_resultsb <- c()
sig_resultsb <- c()

for (i in 1:2000) {
  # Have to re-create the data EVERY TIME or it will just be the same data over and over
  datab <- make_datab(b1=0.0,b2=0.3,b3=0)
  
  # Run the analysis
  modb <- glm(doses2 ~ time + post + post_tsince +
    harmonic(week, 2, 52.25) + offset(log(pop100)), 
    family = poisson, data=datab)
  
  # Get the results
  # use robust SEs
  tmodb <- tidy(coeftest(modb, vcov = vcovHAC))
  coef_resultsb[i] <- tmodb$estimate[3]
  sig_resultsb[i] <- tmodb$p.value[3] <= .05
}

mean(sig_resultsb)

results_tib <- tibble(coef = coef_results,
                         sig = sig_results)


# Create function to test different effect sizes
post_powerb <- function(effect) {
  # empty object to hold power values
  sig_resultsb <- c()
  
  for (i in 1:500) {
  # re-create simulated data
  datab <- make_datab(b1=0,b2=effect,b3=0)
  
    # Run the analysis
  modb <- glm(doses2 ~ time + post + post_tsince +
    harmonic(week, 2, 52.25) + offset(log(pop100)), 
    family = poisson, data=datab)
  
  # Get the results
  # use robust SEs
  tmodb <- tidy(coeftest(modb, vcov = vcovHC))

  sig_resultsb[i] <- tmodb$p.value[3] <= .05
    
  }
  sig_resultsb %>%
    mean() %>%
    return()
}

# Now try different effect sizes
power_levelsb <- c()

effects <- c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)

for (i in 1:7) {
  power_levelsb[i] <- post_powerb(effects[i])
}

# Where do we cross 80%?
power_resultsb <- tibble(effect = effects,
                        power = power_levelsb)
kbl(power_resultsb) %>%
  kable_styling()







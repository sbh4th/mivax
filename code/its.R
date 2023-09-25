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

weekly <- d %>% select(doses,date) %>%
  filter(date>="2020-01-01" & date<="2021-12-31") %>% 
  mutate(week = week(date),
        weekyr = yearweek(date),
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
            week = mean(week))

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
              family=quasipoisson, data=weekly)
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
  weeks = rep(seq(from = 1, to = 52, by =1), 2),
  year = rep(2020:2021, each = 52),
  time = seq(from = 1, to=length(weeks), by = 1),
  post = if_else(year >= 2021, 1, 0),
  tsince = if_else(post==1, time - 52, 0),
  # estimate = ifelse(post == 1, time - min(time), 0),
  pop100 = 90000 + (900000 * 0.0005 * time)
  ) %>%
  mutate(
    lambda = 9.33 + (0.00 * time) + (0 * post) +
    (0 * post * tsince) + 
      -1.9 * sin(2 * pi * weeks / 52.25) +
      0.38 * sin(2 * pi * 2 * weeks / 52.25) +
      2.65 * cos(2 * pi * weeks / 52.25) +
      -1.28 * cos(2 * pi * 2 * weeks / 52.25) +
      dnorm(length(weeks), mean = 0, sd = 100),
    doses = rpois(length(weeks), exp(lambda)))

test_model <-glm(doses ~ post + time + harmonic(weeks, 2, 52.25), 
  family = quasipoisson, data=tib)
res_sim <- residuals(test_model, type="deviance")

tib %>% # filter(time<105) %>%
  mutate(yw = make_yearweek(
    year = year, week= weeks)) %>%
      add_predictions(test_model) %>%
  mutate(pdoses = exp(pred)) %>%
  ggplot(aes(x = yw, y = doses)) +
  geom_point() +
  geom_line(aes(x = yw, y = pdoses))


library(tscount)
ITSC.single.group = function(nsmp=18, bet.inter, bet.time, bet.x1, bet.timex1, gam, time, time.intrv1, y0=0, mu0=3){
  # nsmp: the sample size (the number of time points)
  # pchi.14: the upper quantile of chi-square (df)
  # time: a vector of time (start.time:final.time)
  # time.intrv1: indicator for onset of the intervention in time.
  # Regression coefficients:
  # bet.inter: intercept coefficient
  # bet.x1: the coefficient for the binary indicator for the second phase of the study
  # bet.time: the coefficient for time
  # bet.timex1: the coefficient for the interaction of x1 and time
  # gam: the coefficient 𝛾
  # mu0: the coefficient 𝜇
  pchi.14 = qchisq(0.95, 2)
  pchi.24 = qchisq(0.95, 1)
  bet = c(bet.inter, bet.time, bet.x1, bet.timex1 )
  x1 = c(rep(0,time.intrv1), rep(1,nsmp-time.intrv1))
  logtime = time
  logtime1 = time-time.intrv1
  #--------- generate data ---------#
  x.t= model.matrix( ∼ logtime + x1 + logtime1:x1-1 )
  eta = apply(cbind(1,x.t), 1, function(s){sum(s*bet)})
  mu.lag = mu0
  y = rep(NA, nsmp+1)
  y[1] = y0
  for (i in 2:(nsmp+1)){
    e.lag = gam*log(y[i-1]+1)
    mu.lag = exp(eta[i-1] + e.lag)
    y[i] = rpois(1,mu.lag) # or rnbinom
  }
  
  harmonic <- function(x, nfreq, period, intercept = FALSE) {
    stopifnot(nfreq > 0)
    pi <- base::pi  ## Just in case someone has redefined pi!
    x <- as.numeric(x)
    
    k <- seq(1, nfreq) * 2 * pi / period
    M <- outer(x, k)
    sinM <- apply(M, 2, sin)
    cosM <- apply(M, 2, cos)
    if(!intercept) 
      cbind(sinM, cosM)
    else
      cbind(1, sinM, cosM)
  }
  
  A∗cos(2∗pi∗f∗t)+B∗sin(2∗pi∗f∗t)


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

t <- weekly %>% add_predictions(model4) %>%
  mutate(pdoses = exp(pred)) 
%>%
  ggplot(aes(x = weekyr, y = ndoses)) +
  geom_point() +
  geom_line(aes(x = weekyr, y = pdoses))

model4 <- glm(ndoses ~ post + time + harmonic(week,3,52), 
              family=quasipoisson, data=weekly)
summary(model4)

res4 <- residuals(model4, type="deviance")

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
  weeks = rep(seq(from = 1, to = 52, by =1), 6),
  year = rep(2017:2022, each = 52),
  time = seq(from = 1, to=312, by = 1),
  post = if_else(year >= 2020, 1, 0),
  tsince = if_else(post==1, time - 157, 0),
  pop100 = 90000 + (900000 * 0.0005 * time)
  ) %>%
  mutate(
    lambda = 10 + (0.02 * time) + (0.2 * post) +
    (0 * post * tsince),
    doses = rpois(312, exp(lambda)))


library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)
library(haven)
library(cowplot)
library(fpp2)
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

grid <- data.frame(x = seq(0, 1, length = length(weekly)))
grid %>% add_predictions(m1)

weekly %>% add_predictions(model3) %>%
  ggplot(aes(x = weekyr, y = pred)) +
  geom_line() 
+ 
  # geom_line(aes(x = weekyr, y = pred))


library(its2es)
data <- Israel_mortality
form <- as.formula("percent ~ time")
intervention_start_ind <- which(data$Year==2020 & data$Month>2| data$Year==2021)[1]
form <- as.formula("monthly_total ~ time")
fit <- its_poisson_fourier(data=data,form=form, 
  offset_name = "monthly_est", time_name = "time",
  intervention_start_ind=intervention_start_ind, 
  over_dispersion=TRUE, freq=12,
  impact_model = "full",counterfactual = TRUE, keep_significant_fourier=FALSE)


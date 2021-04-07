################### #
# Attach packages ----
################### #

library(tidyverse)
library(tidymodels)
library(readxl)

#################### #
# Exercise 1 - Import data ----
#################### #

houses_raw <- read_excel("temp/houses.xlsx")

#################### #
# Exercise 2 - Transform data ----
#################### #

houses <- houses_raw %>%
  select(sqm, expense, tot_price, lat, lng, kommune_name) %>%
  mutate(kommune_name = fct_lump_n(kommune_name, 220),
         log_sqm      = log(sqm))

#################### #
# Exercise 3 - Split data ----
#################### #

set.seed(42)

split <- initial_split(houses, prop = 3/4)
train <- training(split)
test  <- testing(split)

#################### #
# Exercise 4 - Fit a linear model  ----
#################### #

linear_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(log(tot_price) ~ ., data = train)

# View summary of fit
summary(linear_model$fit)

#################### #
# Exercise 5 - Use model for predictions  ----
#################### #

linear_model_preds <- 
  predict(linear_model, test) %>% 
  bind_cols(test) %>% 
  mutate(.pred = exp(.pred)) %>% 
  rename(estimate     = .pred, 
         truth        = tot_price) %>% 
  mutate(abs_dev      = abs(truth - estimate),
         abs_dev_perc = abs_dev/truth) %>% 
  select(estimate, truth, everything()) # See comment below

# Line 64, `select(estimate, truth, everything())` is a trick to reorder the columns such that `estimate` and `truth` appears
# as the first two columns in the dataset. This was not requested of the students in the exercise.

#################### #
# Exercise 6 - Evaluate model  ----
#################### #

mape(data = linear_model_preds, truth = truth, estimate = estimate)

##################################### #
# Excerise 7 - Plot results ----
##################################### #

linear_model_preds %>%
  ggplot(aes(x = abs_dev_perc)) +
  geom_histogram(fill = "dodgerblue3", color = "white") +
  labs(x = "Prosentvis feil",
       y = "Antall observasjoner") +
  scale_x_continuous(limits = c(0,5), labels = scales::percent) +   
  theme_minimal()

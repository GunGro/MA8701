################### #
# Attach packages ----
################### #

library(tidyverse)
library(tidymodels)
library(readxl)

#################### #
# Import the updated `houses` dataset ----
#################### #

houses_raw <- read_excel("temp/houses.xlsx")

#################### #
# Select only the columns of interest ----
#################### #

houses <- houses_raw %>%
  select(id, sqm, expense, tot_price)

################### #
# Split data using the rsample package ----
# Set seed in order to make the analysis reproducible and same initial conditions
################### #

set.seed(42)

split <- initial_split(houses, prop = 3/4)
train_raw <- training(split)
test_raw  <- testing(split)

train <- train_raw %>% 
  select(-id)

test <- test_raw %>% 
  select(-id)

################### #
# Specify and fit a linear model ----
################### #

# Specify the linear model and fit it according to the specified formula
model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(tot_price ~ ., data = train)
  
################### #
# View summary of fit regression line ----
################### #

options(scipen = 999) # scientific notation sensitivity
summary(model$fit)

################### #
# Use the linear model to make predictions on test data ----
################### #

model_preds <- 
  predict(model, test) %>% 
  bind_cols(test_raw) %>% 
  rename(estimate     = .pred, 
         truth        = tot_price) %>% 
  mutate(abs_dev      = abs(truth - estimate),
         abs_dev_perc = abs_dev/truth)

#################### #
# Use the package yardstick to get evaluation of the model ----
#################### #

mape(data = model_preds, truth = truth, estimate = estimate)

##################################### #
# Plot predictions ----
##################################### #

model_preds %>% 
  ggplot(aes(x = estimate/1000000, y = truth/1000000)) +
  geom_point(color = "dodgerblue3", alpha = 0.4) +
  labs(x = "Predikert pris [MNOK]",
       y = "Faktisk pris [MNOK]") +
  theme_minimal()

model_preds %>%
  ggplot(aes(x = abs_dev_perc)) +
  geom_histogram(fill = "dodgerblue3", color = "white") +
  labs(x = "Prosentvis feil",
       y = "Antall") +
  scale_x_continuous(limits = c(0,5), labels = scales::percent) +
  theme_minimal()


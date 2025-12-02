### load necessary packages ----
library(plumber)
library(tidyverse)
library(tidymodels)

set.seed(1993)

### read in diabetes data ----
dm_data <- read_csv('diabetes_binary_health_indicators_BRFSS2015.csv')

### change relevant columns to factor type ---
dm_data <- dm_data |>
  mutate(Diabetes = factor(Diabetes_binary, levels = c(0, 1), labels = c('no', 'yes')),
         Smoker = factor(Smoker, levels = c(0, 1), labels = c('no', 'yes')),
         PhysActivity = factor(PhysActivity, levels = c(0,1), labels = c('no', 'yes')),
         Fruits = factor(Fruits, levels = c(0,1), labels = c('no', 'yes')),
         Veggies = factor(Veggies, levels = c(0,1), labels = c('no', 'yes')),
         HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0,1), labels = c('no', 'yes')),
         DiffWalk = factor(DiffWalk, levels = c(0,1), labels = c('no', 'yes')),
         Sex = factor(Sex, levels = c(0,1), labels = c('female', 'male'))
  )

### create recipe ---
dm_rec <- recipe(Diabetes ~ BMI + Smoker + PhysActivity + Fruits + Veggies + HvyAlcoholConsump + DiffWalk + Sex, data = dm_data) |>
  step_dummy(all_factor(), -Diabetes)

### create model ---
### note we can set mtry to 3 without tuning because we found this to be the optimal parameter on our modeling page ---
rf_mod <- rand_forest(mtry = 3, trees = 100) |>
  set_engine('ranger') |>
  set_mode('classification')

### create workflow ---
rf_wkfl <- workflow() |>
  add_recipe(dm_rec) |>
  add_model(rf_mod)

### fit model ---
rf_fit <- rf_wkfl |>
  fit(data = dm_data) 

### predict ---
predict(rf_fit, new_data = dm_data) |> summary()

### create confusion matrix ---
conf_mat(data = dm_data |> mutate(estimate = predict(rf_fit, dm_data) |> pull()),
         truth = Diabetes,
         estimate = estimate)
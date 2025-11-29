#Author: Brian Rabe
#Date: 11/29/2025
#Purpose: Fit our random forest model with tuned mtry parameter to the entire data set and define an API. 
#Collaborators: No direct collaborators, but Dr. Justin Post's ST558 course materials were used heavily in creating the materials in this file. 

### load necessary packages ----
library(plumber)
library(tidyverse)
library(tidymodels)

### read in diabetes data ----
dm_data <- read_csv('../diabetes_binary_health_indicators_BRFSS2015.csv')

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

###Now we start defining the API ----

#* @apiTitle Diabetes Prediction
#* @apiDescription This API takes a set of health data predictors and predicts whether the individual has diabetes. 

#* Make a diabetes prediction 
#* @param bmi What is the person's BMI?
#* @param smoker Has the person smoked 100 or more cigarettes in their lifetime? 
#* @param activity Has the person done physical activity in the past 30 days?
#* @param fruits Does the person consume fruit one or more times per day?
#* @param veggies Does the person consume vegetables one or more times per day?
#* @param alcohol Does the person consume 14 or more alcoholic drinks per week (if male) or 7 or more drinks per week (if female)?
#* @param diff_walk Does the person have serious difficulty walking or climbing stairs
#* @param sex What is the person's sex?
#* @get /pred
function(bmi = mean(dm_data$BMI), 
         smoker = dm_data$Smoker |> summary() |> which.max() |> names(),
         activity = dm_data$PhysActivity |> summary() |> which.max() |> names(),
         fruits = dm_data$Fruits |> summary() |> which.max() |> names(),
         veggies = dm_data$Veggies |> summary() |> which.max() |> names(),
         alcohol = dm_data$HvyAlcoholConsump |> summary() |> which.max() |> names(),
         diff_walk = dm_data$DiffWalk |> summary() |> which.max() |> names(),
         sex = dm_data$Sex |> summary() |> which.max() |> names()) {
  
    ### define new data frame for input values with same column names as data the model was trained on ---
  df <- data.frame('BMI' = as.double(bmi),
                   'Smoker' = smoker,
                   'PhysActivity' = activity,
                   'Fruits' = fruits,
                   'Veggies' = veggies,
                   'HvyAlcoholConsump' = alcohol,
                   'DiffWalk' = diff_walk,
                   'Sex' = sex)
  
  ### predict presence or absence of diabetes ----
  predict(rf_fit, df)
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}

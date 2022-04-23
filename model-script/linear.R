if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, janitor, themis, ROSE, vip)

dat <- read_csv("data/heart_2020_cleaned.csv") |>
  clean_names()

dat <- dat |>
  mutate(heart_disease = as_factor(heart_disease),
         smoking = if_else(smoking == "Yes", 1, 0),
         alcohol_drinking = if_else(alcohol_drinking == "Yes", 1, 0),
         stroke = if_else(stroke == "Yes", 1, 0),
         diff_walking = if_else(diff_walking == "Yes", 1, 0),
         sex = as_factor(sex),
         age_category = factor(age_category,
                               levels = c("18-24", "25-29", "30-34", "35-39",
                                          "40-44", "45-49", "50-54", "55-59",
                                          "60-64", "65-69", "70-74", "75-79",
                                          "80 or older")),
         race = as_factor(race),
         diabetic = as_factor(diabetic),
         physical_activity = if_else(physical_activity == "Yes", 1, 0),
         gen_health = factor(gen_health,
                             levels = c("Poor", "Fair", "Good", "Very good", "Excellent")),
         asthma = if_else(asthma == "Yes", 1, 0),
         kidney_disease = if_else(skin_cancer == "Yes", 1, 0),
         skin_cancer = if_else(skin_cancer == "Yes", 1, 0))


tidymodels_prefer()

set.seed(2022)
heart_split <- initial_split(data = dat, prop = 0.8, strata = heart_disease)
heart_train <- training(heart_split)
heart_test <- testing(heart_split)

set.seed(2023)
heart_folds <- vfold_cv(heart_train, v = 10, strata = heart_disease)

heart_recipe <- recipe(heart_disease ~ ., data = heart_train) |>
  step_dummy(all_nominal_predictors(), one_hot=TRUE) |>
  step_zv(all_predictors()) |> # precautionary step to remove variables only containing a single value
  step_rose(heart_disease)

prep(heart_recipe)

lr_model <- logistic_reg()

lr_wf <- workflow(heart_recipe, lr_model)

doParallel::registerDoParallel()

heart_metrics <- metric_set(recall, precision, j_index, roc_auc)
set.seed(2180)
lr_rose_res <- fit_resamples(
  lr_wf, 
  resamples = heart_folds, 
  metrics = heart_metrics
)

lr_fit <- lr_wf |>
  last_fit(heart_split)

collect_metrics(lr_fit)
collect_predictions(lr_fit) |>
  conf_mat(heart_disease, .pred_class)
collect_predictions(lr_fit) |>
  recall(heart_disease, .pred_class, event_level="second")
collect_predictions(lr_fit) |>
  precision(heart_disease, .pred_class, event_level="second")
collect_predictions(lr_fit) |>
  j_index(heart_disease, .pred_class, event_level="second")
collect_predictions(lr_fit) |>
  pr_auc(heart_disease, .pred_Yes, event_level="second")
collect_predictions(lr_fit) |>
  kap(heart_disease, .pred_class, event_level="second")
collect_predictions(lr_fit) |>
  roc_curve(heart_disease, .pred_No) |>
  write_csv("data/linear_roc.csv")
collect_predictions(lr_fit) |>
  pr_curve(heart_disease, .pred_Yes, event_level="second") |>
  write_csv("data/linear_prc.csv")

extract_workflow(lr_fit) |>
  extract_fit_parsnip() |>
  vip::vi() |>
  knitr::kable()
extract_workflow(lr_fit) |>
  extract_fit_parsnip() |>
  vip(num_features = 10, geom = "point") +
  theme_minimal()

collect_predictions(lr_fit) |>
  roc_curve(heart_disease, .pred_No) |>
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(alpha = 0.8, size = 1, color = "royalblue") +
  coord_equal() +
  labs(color = NULL)

save.image("data/linear_model.RData")
load("data/linear_model.RData")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, janitor, themis, ROSE, parsnip, discrim)

dat <- read_csv("data/heart_2020_cleaned.csv") |>
  clean_names()

dat <- dat |>
  mutate(heart_disease = factor(heart_disease),
         smoking = as_factor(smoking),
         alcohol_drinking = as_factor(alcohol_drinking),
         stroke = as_factor(stroke),
         diff_walking = as_factor(diff_walking),
         sex = as_factor(sex),
         age_category = factor(age_category,
                               levels = c("18-24", "25-29", "30-34", "35-39",
                                          "40-44", "45-49", "50-54", "55-59",
                                          "60-64", "65-69", "70-74", "75-79",
                                          "80 or older")),
         race = as_factor(race),
         diabetic = as_factor(diabetic),
         physical_activity = as_factor(physical_activity),
         gen_health = factor(gen_health,
                             levels = c("Poor", "Fair", "Good", "Very good", "Excellent")),
         asthma = as_factor(asthma),
         kidney_disease = as_factor(kidney_disease),
         skin_cancer = as_factor(skin_cancer))


tidymodels_prefer()

set.seed(2022)
heart_split <- initial_split(data = dat, prop = 0.8, strata = heart_disease)
heart_train <- training(heart_split)
heart_test <- testing(heart_split)

set.seed(2023)
heart_folds <- vfold_cv(heart_train, v = 10, strata = heart_disease)

heart_recipe <- recipe(heart_disease ~ ., data = heart_train) |>
  step_zv(all_predictors()) |> # precautionary step to remove variables only containing a single value
  step_rose(heart_disease)

prep(heart_recipe)

nb_model <- naive_Bayes()

nb_wf <- workflow(heart_recipe, nb_model)

doParallel::registerDoParallel()

heart_metrics <- metric_set(recall, precision, j_index, roc_auc)
set.seed(2180)
nb_rose_res <- fit_resamples(
  nb_wf, 
  resamples = heart_folds, 
  metrics = heart_metrics
)

nb_fit <- nb_wf |>
  last_fit(heart_split)

collect_metrics(nb_fit)
collect_predictions(nb_fit) |>
  conf_mat(heart_disease, .pred_class)
collect_predictions(nb_fit) |>
  recall(heart_disease, .pred_class, event_level="second")
collect_predictions(nb_fit) |>
  precision(heart_disease, .pred_class, event_level="second")
collect_predictions(nb_fit) |>
  j_index(heart_disease, .pred_class, event_level="second")
collect_predictions(nb_fit) |>
  pr_auc(heart_disease, .pred_Yes, event_level="second")
collect_predictions(nb_fit) |>
  kap(heart_disease, .pred_class, event_level="second")
collect_predictions(nb_fit) |>
  roc_curve(heart_disease, .pred_No) |>
  write_csv("data/nb_roc.csv")
collect_predictions(nb_fit) |>
  pr_curve(heart_disease, .pred_Yes, event_level="second") |>
  write_csv("data/nb_prc.csv")

collect_predictions(nb_fit) |>
  roc_curve(heart_disease, .pred_No) |>
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(alpha = 0.8, size = 1, color = "royalblue") +
  coord_equal() +
  labs(color = NULL)

save.image("data/naive_bayes.RData")
load("data/naive_bayes.RData")

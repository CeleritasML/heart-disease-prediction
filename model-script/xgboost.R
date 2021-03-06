if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, janitor,
               skimr, tictoc, vip)

font_add_google(name = "Roboto Mono", family = "Roboto Mono")

dat <- read_csv("data/heart_2020_cleaned.csv") |>
  clean_names()

# skim(dat)

dat <- dat |>
  mutate(heart_disease = as_factor(heart_disease),
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
heart_metrics <- metric_set(accuracy, roc_auc, mn_log_loss)

set.seed(2023)
heart_folds <- vfold_cv(heart_train, v = 10, strata = heart_disease)

# heart_train |>
#   ggplot(aes(x = bmi, y = physical_health, color = heart_disease)) +
#   geom_point(alpha = 0.5, size = 0.5) +
#   labs(x = "BMI", y = "Physical Health") +
#   theme_minimal()

# heart_train |>
#   ggplot(aes(x = age_category, fill = heart_disease)) +
#   geom_bar(alpha = 0.5, position = "identity") +
#   labs(x = "Age Category", fill = NULL) +
#   theme_minimal()

heart_recipe <- recipe(heart_disease ~ ., data = heart_train) |>
  step_dummy(all_nominal_predictors(), one_hot=FALSE) |>
  step_zv(all_predictors()) # precautionary step to remove variables only containing a single value

prep(heart_recipe)

# Tunable xgboost model with early stopping

yes_no_ratio <- 27373/292422

stopping_spec <- boost_tree(
  trees = tune(), mtry = tune(), learn_rate = tune(), stop_iter = tune()
) |>
  set_engine("xgboost", validation = 0.1,
             scale_pos_weight = yes_no_ratio) |>
  set_mode("classification")

stopping_grid <- grid_latin_hypercube(
  trees(range = c(25, 200)),
  mtry(range = c(2L, 10L)),
  learn_rate(range = c(-5, -1)),
  stop_iter(range = c(10L, 50L)),
  size = 10
)

# Workflow

early_stop_wf <- workflow(heart_recipe, stopping_spec)

doParallel::registerDoParallel()

set.seed(1234)
tic("total")
tic("tune grid")
stopping_rs <- tune_grid(
  early_stop_wf,
  heart_folds,
  grid = stopping_grid,
  metrics = heart_metrics
)
toc()
toc()

# approximately 1000 seconds

# autoplot(stopping_rs) +
#   geom_line()

show_best(stopping_rs, metric = "roc_auc")

stopping_fit <- early_stop_wf |>
  finalize_workflow(select_best(stopping_rs, "roc_auc")) |>
  last_fit(heart_split)

collect_metrics(stopping_fit)

# extract_workflow(stopping_fit) |>
#   extract_fit_parsnip() |>
#   vip(num_features = 15, geom = "point")

collect_predictions(stopping_fit) |>
  conf_mat(heart_disease, .pred_class)

collect_predictions(stopping_fit) |>
  recall(heart_disease, .pred_class, event_level = "second")
collect_predictions(stopping_fit) |>
  precision(heart_disease, .pred_class, event_level = "second")
collect_predictions(stopping_fit) |>
  j_index(heart_disease, .pred_class, event_level = "second")
collect_predictions(stopping_fit) |>
  pr_auc(heart_disease, .pred_Yes, event_level = "second")

collect_predictions(stopping_fit) |>
  kap(heart_disease, .pred_class)

collect_predictions(stopping_fit) |>
  roc_curve(heart_disease, .pred_No) |>
  write_csv("data/xgboost_roc.csv")

collect_predictions(stopping_fit) |>
  pr_curve(heart_disease, .pred_Yes, event_level = "second") |>
  write_csv("data/xgboost_prc.csv")

collect_predictions(stopping_fit) |>
  roc_curve(heart_disease, .pred_No) |>
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(alpha = 0.8, size = 1, color = "grey") +
  coord_equal() +
  labs(color = NULL)

# collect_predictions(stopping_fit) |>
#   conf_mat(heart_disease, .pred_class) |>
#   autoplot()

save.image("data/xgboost_w_early_stopping.RData")
load("data/xgboost_w_early_stopping.RData")

# Set ggplot theme
theme_set(theme_minimal(base_family = "Roboto Mono"))
theme_update(
  # panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
  # plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
  panel.border = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(linetype = "dashed"),
  panel.grid.minor = element_blank(),
  panel.grid = element_line(color = "#b4aea9"),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  axis.title.x = element_text(size = 13, colour = "#495057", face = "bold"),
  axis.title.y = element_text(size = 13, colour = "#495057",  margin = margin(r = 10), face = "bold"),
  axis.line = element_line(colour = "grey50"),
  legend.title = element_text(size = 13, face = "bold", colour = "#495057"),
  legend.text = element_text(size = 10, color = "#495057"), 
  plot.title = element_text(hjust = 0.5, size = 15, face = "bold", color = "#2a475e"),
  plot.caption = element_text(family = "Special Elite", size = 10, color = "grey70", face = "bold",
                              hjust = .5, margin = margin(5, 0, 20, 0)),
  plot.margin = margin(10, 25, 10, 25),
)

extract_workflow(stopping_fit) |>
  extract_fit_parsnip() |>
  vip(num_features = 10, geom = "point") +
  labs(
    title = "Feature Importance of xgboost model"
  )
ggsave("vip-xgboost.png", width = 8, height = 4)

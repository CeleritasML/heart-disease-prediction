# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, janitor,
               skimr, tictoc, vip, tidyverse, ggtext, 
               sysfonts)

######################
## set figure style ##
######################

font_add_google(name = "Roboto Mono", family = "Roboto Mono")

theme_set(
  theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.background = element_rect(fill = "#F9EFE6", color = "#F9EFE6"),
      legend.key = element_rect(fill = "#F9EFE6", color = "#F9EFE6"),
      legend.title = element_text(size = 7, color = "#3B372E"),
      legend.text = element_text(size = 7, color = "#3B372E"),
      plot.background = element_rect(fill = "#F9EFE6", color = "#F9EFE6"),
      panel.background = element_rect(fill = "#F9EFE6", color = "#F9EFE6"),
      text = element_text(
        family = "Roboto Mono",
        color = "#3B372E"
      ),
      axis.title.y = element_text(vjust = 0.2, face = "bold"),
      axis.title.x = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(),
      axis.text.y = element_text(angle = 30),
      plot.title = element_markdown(
        size = 18, hjust = 0.5,
        family = "Roboto Slab"
      ),
      plot.subtitle = element_markdown(
        hjust = 0.5,
        family = "Roboto Slab"
      ),
      plot.caption = element_markdown(
        size = 10,
        family = "Roboto Slab",
        hjust = 0
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
)

###################
## data cleaning ##
###################

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

heart_cv_split <- initial_split(data = heart_train, prop = 0.1, strata = heart_disease)
heart_cv <- training(heart_cv_split)

set.seed(2023)
heart_folds <- vfold_cv(heart_cv, v = 10, strata = heart_disease)

#################
## build model ##
#################

# precautionary step to remove variables only containing a single value
heart_recipe <- recipe(heart_disease ~ ., data = heart_train) |>
  step_zv(all_predictors()) 

prep(heart_recipe)

# Tunable tree model with early stopping

stopping_spec <- rand_forest(
  trees = tune(), mtry = tune()) |>
  set_engine("randomForest", validation = 0.2, classwt=c(27373, 292422)) |>
  set_mode("classification")

stopping_grid <- grid_latin_hypercube(
  trees(range = c(50, 500)),
  mtry(range = c(2L, 10L)),
  size = 5
)

# Workflow

early_stop_wf <- workflow(heart_recipe, stopping_spec)

doParallel::registerDoParallel()

heart_metrics <- metric_set(recall, precision, j_index, roc_auc)

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

autoplot(stopping_rs) +
  geom_line() 


show_best(stopping_rs, metric = "recall")

stopping_fit <- early_stop_wf |>
  finalize_workflow(select_best(stopping_rs, "recall")) |>
  last_fit(heart_split)

collect_metrics(stopping_fit)
collect_predictions(stopping_fit) |>
  conf_mat(heart_disease, .pred_class)
collect_predictions(stopping_fit) |>
  recall(heart_disease, .pred_class, event_level="second")
collect_predictions(stopping_fit) |>
  precision(heart_disease, .pred_class, event_level="second")
collect_predictions(stopping_fit) |>
  j_index(heart_disease, .pred_class, event_level="second")
collect_predictions(stopping_fit) |>
  pr_auc(heart_disease, .pred_Yes, event_level="second")
collect_predictions(stopping_fit) |>
  kap(heart_disease, .pred_class, event_level="second")
collect_predictions(stopping_fit) |>
  roc_curve(heart_disease, .pred_No) |>
  write_csv("data/rf_roc.csv")
collect_predictions(stopping_fit) |>
  pr_curve(heart_disease, .pred_Yes, event_level="second") |>
  write_csv("data/rf_prc.csv")

extract_workflow(stopping_fit) |>
  extract_fit_parsnip() |>
  vip(num_features = 17, geom = "point") 

collect_predictions(stopping_fit) |>
  roc_curve(heart_disease, .pred_No) |>
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(alpha = 0.8, size = 1, color = "royalblue") +
  coord_equal() +
  labs(color = NULL)

collect_predictions(stopping_fit) |>
  conf_mat(heart_disease, .pred_class) |>
  autoplot()

save.image("data/tree_early_stopping.RData")
#load("data/xgboost_w_early_stopping.RData")
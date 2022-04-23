if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

font_add_google(name = "Roboto Mono", family = "Roboto Mono")

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

lda_df <- read_csv("data/lda_roc.csv") |>
  mutate(method="lda")
xgboost_df <- read_csv("data/xgboost_roc.csv") |>
  mutate(method="xgboost")
nb_df <- read_csv("data/nb_roc.csv") |>
  mutate(method="nb")
linear_df <- read_csv("data/linear_roc.csv") |>
  mutate(method="linear")
rf_df <- read_csv("data/rf_roc.csv") |>
  mutate(method="rf")
dat2 <- bind_rows(lda_df, xgboost_df, nb_df, linear_df, rf_df)

dat2 |>
  ggplot(aes(1 - specificity, sensitivity, color = method)) +
  geom_abline(lty = 2, color = "gray80", size = 1) +
  geom_path(alpha = 0.8, size = 0.8) +
  coord_equal() +
  labs(color = NULL,
       title = "ROC-AUC Comparison")

ggsave("roc_auc.png", width = 8, height = 8)

lda_prc <- read_csv("data/lda_prc.csv") |>
  mutate(method="lda")
xgboost_prc <- read_csv("data/xgboost_prc.csv") |>
  mutate(method="xgboost")
nb_prc <- read_csv("data/nb_prc.csv") |>
  mutate(method="nb")
linear_prc <- read_csv("data/linear_prc.csv") |>
  mutate(method="linear")
rf_prc <- read_csv("data/rf_prc.csv") |>
  mutate(method="rf")
dat3 <- bind_rows(lda_prc, xgboost_prc, nb_prc, linear_prc, rf_prc)

dat3 |>
  ggplot(aes(recall, precision, color = method)) +
  geom_path(alpha = 0.8, size = 0.8) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(color = NULL,
       title = "Precision-Recall Curve Comparison")

ggsave("prc.png", width = 8, height = 8)



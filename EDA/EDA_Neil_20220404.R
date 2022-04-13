#---------- Load the Packages ----------#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, janitor,
               skimr, tictoc, vip)
library(cowplot)
library(showtext)
library(ggstatsplot)


#---------- Read and Clean the Data#
dat <- read_csv("heart_2020_cleaned.csv") |>
  clean_names()

dat <- dat |>
  mutate(heart_disease = as_factor(heart_disease),
         smoking = as_factor(smoking),
         alcohol_drinking = as_factor(alcohol_drinking),
         stroke = as_factor(stroke),
         diff_walking = as_factor(diff_walking),
         sex = as_factor(sex),
         diabetic = ifelse(dat$diabetic %in% c("Yes", "Yes (during pregnancy)"), "Yes", "No"),
         diabetic = factor(diabetic, levels=c("No", "Yes")),
         age_category = ifelse(age_category=="80 or older", "80+", age_category),
         age_category = factor(age_category,
                               levels = c("18-24", "25-29", "30-34", "35-39",
                                          "40-44", "45-49", "50-54", "55-59",
                                          "60-64", "65-69", "70-74", "75-79",
                                          "80+")),
         race = as_factor(race),
         physical_activity = as_factor(physical_activity),
         gen_health = factor(gen_health,
                             levels = c("Poor", "Fair", "Good", "Very good", "Excellent")),
         asthma = as_factor(asthma),
         kidney_disease = as_factor(kidney_disease),
         skin_cancer = as_factor(skin_cancer))


#---------- Set the theme of ggplot ----------#
showtext_auto()

# Add fonts from Google.
font_add_google("Roboto Mono", "Roboto Mono")
font_add_google("Open Sans", "Open Sans")
font_add_google("Special Elite", "Special Elite")

# Set ggplot theme
theme_set(theme_minimal(base_family = "Roboto Mono"))
theme_update(
  panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
  plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
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

# Turn on showtext
showtext_auto()


#---------- Error Plot ----------#
dat$response <- ifelse(dat$heart_disease=="Yes", 1, 0)

se <- function(x) sqrt((mean(x)*(1-mean(x)))/ length(x))
dat_summary = dat %>% 
  group_by(gen_health, stroke, diabetic) %>%
  summarise(mean = mean(response), se = se(response))

diabetic.labs <- c("Diabetic: No", "Diabetic: Yes")
names(diabetic.labs) <- c("No", "Yes")
errorplot <- ggplot(dat_summary, aes(x = gen_health, y = mean, group = stroke, color = stroke)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .4,
                position = position_dodge(0.1)) + 
  scale_color_manual(values = c("#6078A8", "#C7B0C1")) + 
  facet_wrap(~diabetic, labeller = labeller(diabetic=diabetic.labs)) + 
  labs(title = "Error Plot of Heart Disease in Different Health Conditions", 
       x = "General Health", 
       y = "Proportion of Heart Disease") + 
  guides(color = guide_legend(title = "Stroke")) + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, margin = margin(t = 12)), 
        plot.title.position = "plot",
        strip.text.x = element_text(size = 10, color = "#495057"))
  
errorplot
ggsave("error.png", errorplot)



#---------- Violin Plot ----------#

dat$heart_gender <- paste(dat$heart_disease, dat$sex, sep = "-")

violinplot <- ggbetweenstats(data = dat, x = heart_gender, y = bmi, type = "p", 
                             xlab = "Heart Disease - Gender", ylab = "BMI",
                             point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.5), 
                                       alpha = 0.05, size = 1, stroke = 0), 
                             pairwise.comparisons = TRUE,
                             pairwise.display = "s")

violinplot <- violinplot + 
  labs(title = "Distribution of BMI across Heart Disease and Gender") + 
  theme(
    text = element_text(size = 8, color = "#495057"),
    plot.title = element_text(size = 15, face = "bold", color = "#2a475e", hjust = 0.5),
    plot.subtitle = element_text(size = 10, face = "bold", color="#1b2838", hjust = 0.5),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 13), 
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.border = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.margin = margin(5, 10, 5, 10))

violinplot
ggsave("violin.png", violinplot)


#---------- Lollipop Plot ----------#
dat$count = 1
dat_race_avg <- dat %>%
  group_by(age_category, race) %>%
  summarise(avg = mean(response), 
            count = sum(count)) %>%
  mutate(epi = row_number()) %>%
  mutate(epi_mod = epi + 15*(as.numeric(age_category)) -6)

dat_race_avg <- dat_race_avg %>%
  group_by(age_category) %>%
  mutate(mid = median(epi_mod)) %>%
  inner_join(dat %>%
               group_by(age_category) %>%
               summarise(avg_all = mean(response)), 
             by = 'age_category')

dat_race_line <- dat_race_avg %>%
  group_by(age_category) %>%
  summarise(start_x = min(epi_mod)-5,
            end_x = max(epi_mod)+5,
            y = avg_all) %>%
  pivot_longer(
    cols = c(start_x, end_x), 
    names_to = "type", 
    values_to = "x"
  ) %>% 
  mutate(
    x_group = if_else(type == "start_x", x + .1, x - .1),
    x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
    x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
  )


# First, horizontal lines that are used as scale reference. 
# They are added first to ensure they stay in the background.
p_lollipop <- 
  ggplot(data = dat_race_avg, aes(epi_mod, avg)) 
#+geom_hline(data = tibble(y = 0:0.3), aes(yintercept = y), color = "grey82", size = .5)

# Add vertical segments. 
# These represent the deviation of episode's rating from the mean rating of 
# the season they appeared.
p_lollipop <- 
  p_lollipop + 
  geom_segment(aes(xend = epi_mod, yend = avg_all, 
                   color = age_category, color = after_scale(colorspace::lighten(color, .2))))

# Add lines and dots.
# These represent the mean rating per season. 
# The dots mark each episode's rating, with its size given by the number of votes.
p_lollipop <- 
  p_lollipop + 
  geom_line(data = dat_race_line, aes(x, y), color = "grey40") + 
  geom_line(data = dat_race_line, aes(x_group, y, color = age_category, 
                                      color = after_scale(colorspace::darken(color, .2))),
            size = 2.5) +
  geom_point(aes(size = count, color = age_category)) 


# Add labels on top.
# They indicate the season and free us from using a legend.
p_lollipop <- 
  p_lollipop + 
  geom_label(aes(mid, 0.35,
                 label = glue::glue("{age_category}"),
                 color = age_category, 
                 color = after_scale(colorspace::darken(color, .2))),
             fill = NA, family = "Special Elite", fontface = "bold",
             label.padding = unit(.2, "lines"), label.r = unit(.25, "lines"), label.size = .5) 

# Scale and labels customization.
# Override default colors with a much better looking palette.

p_lollipop <- 
  p_lollipop + 
  scale_x_continuous(expand = c(.015, .015)) +
  scale_color_manual(
    values = c("#486090", "#D7BFA6", "#6078A8", "#9CCCCC", "#7890A8", 
               "#C7B0C1", "#B5C9C9", "#90A8C0", "#A8A890", "#486090", 
               "#D7BFA6", "#6078A8", "#9CCCCC"),
    guide = FALSE) +
  scale_size_binned(name = "Number of People", range = c(.3, 3)) +
  labs(title = "Proportion of Heart Disease in Different Ages",
       x = NULL, y = "Proportion of Heart Disease") + 
  guides(size = guide_bins(show.limits = TRUE,
                           direction = "horizontal",
                           title.position = "top",
                           title.hjust = .4)) +
  theme(legend.position = c(0.9, 0.1),
        legend.key.width = unit(2, "lines"),
        legend.title = element_text(size = 10),
        legend.title.align = -1.5, 
        legend.text = element_text(size = 8),
        axis.text.x = element_blank(), 
        strip.text.x = element_text(size = 10, color = "#495057")) 
    
p_lollipop
ggsave("lollipop.png", p_lollipop)

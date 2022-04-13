# heart-disease-prediction

- [Data](https://www.kaggle.com/datasets/kamilpytlak/personal-key-indicators-of-heart-disease)
- [FigJam](https://www.figma.com/file/SbhnqENmxPH2YNJcKHz6ji/heart%E2%9D%A4%EF%B8%8F-512?node-id=0%3A1)
- [Todo List](https://github.com/CeleritasML/heart-disease-prediction/projects/1)

## Style

### Neil
```{r}
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext
)

font_add_google(name = "Roboto Mono", family = "Roboto Mono")

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
```

### Rui
```{r}
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
```

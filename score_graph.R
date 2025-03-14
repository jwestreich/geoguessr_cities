library(dplyr)
library(ggplot2)

city<-"DC"

if(city=="DC"){
  max_dist=73000
}
if(city=="NYC"){
  max_dist=130000
}

df <- data.frame(distance = 0:max_dist) %>%
  mutate(score = round(5000 * exp(-10 * distance / max_dist), 0)) %>%
  mutate(highlights = ifelse(
    score %in% c(100, 500, 1000, 2000, 3000, 4000, 5000),
    score,
    NA
  )) %>%
  group_by(highlights) %>%
  mutate(max_distance = max(distance)) %>%
  ungroup() %>%
  mutate(max_distance = ifelse(is.na(highlights), NA, max_distance)) %>%
  mutate(highlights = ifelse(max_distance == distance, highlights, NA)) %>%
  select(-max_distance)%>%
  mutate(dist_miles=distance/5280)

ggplot(df, aes(x = dist_miles)) +
  geom_line(aes(y = score), color = "#5F9EA0", size = 1) +
  geom_point(aes(y = highlights), color = "#5F9EA0", shape = 18, size = 4) +
  geom_segment(aes(
    x = dist_miles, xend = 0,
    y = highlights, yend = highlights
  ), linetype = "dotted", color = "black", size = 1) +
  geom_text(
    aes(
      x = dist_miles,
      y = highlights,
      label = ifelse(!is.na(highlights), sprintf("%.1f miles", round(dist_miles, 1)), NA)
    ),
    hjust = -0.3,
    vjust = -0.2,
    color = "black",
    size = 4
  ) +
  {
    if (city == "NYC") {
      scale_x_continuous(
        limits = c(0, 25),
        breaks = seq(0, 25, by = 5),
        labels = scales::comma
      )
    } else if (city == "DC") {
      scale_x_continuous(
        limits = c(0, 15),
        breaks = seq(0, 15, by = 5),
        labels = scales::comma
      )
    }
  } +
  scale_y_continuous(
    limits = c(0, 5000),
    breaks = c(100, 500, 1000, 2000, 3000, 4000, 5000),
    labels = scales::comma
  ) +
  labs(
    title = "Score by Distance",
    x = "Distance (miles)",
    y = "Score"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "black", size=14),
    axis.title.y = element_text(color = "black", size=14),
    axis.text.x = element_text(color = "black", size=12),
    axis.text.y = element_text(color = "black", size=12),
    axis.line.y = element_line()
  )
ggsave("geoguesser_score_DC.png", width = 5, height = 5, units = "in")

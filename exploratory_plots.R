library(tidyverse)
library(ggrepel)
library(lubridate)

# Load and clean all data
source("access_merge.R")
source("ama_filter.R")

cbsa_pop <- read_csv("data/tract_qcbsa_2015.csv") %>%
  group_by(qcbsa) %>%
  summarize(pop = sum(tract_pop, na.rm = T)) %>%
  mutate(rural = between(`qcbsa`, 0, 2)) %>%
  group_by(rural) %>%
  summarize(pop = sum(pop))

# Plot 1: Population Pyramid
ama_filtered %>%
  mutate(
    age = cut(
      year(now()) - yob, 
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = c(paste(
        seq(0, 95, by = 5),
        seq(0 + 5 - 1, 100 - 1, by = 5),
        sep = "-"),
        paste(100, "+", sep = "")),
      right = FALSE),
    rural = between(`2013_qcbsa`, 0, 2)
    ) %>%
  group_by(age, rural) %>%
  summarize(doc_count = n()) %>%
  filter(!is.na(rural)) %>%
  left_join(cbsa_pop, by = "rural") %>%
  mutate(
    docs_per_100k = (doc_count / pop) * 1e5,
    docs_per_100k = ifelse(rural == "TRUE", docs_per_100k, -docs_per_100k),
    rural = ifelse(rural == "TRUE", "Rural", "Urban")
    ) %>%
  ggplot() +
    geom_bar(aes(x = age, y = docs_per_100k, fill = rural), stat = "identity") +
    scale_y_continuous(labels = abs, limits = c(-25, 25)) +
    scale_fill_brewer(name = "Type", palette = "Set1") +
    coord_flip() +
    theme_bw() +
    labs(
      title = "Rural Doctors Are Getting Older And Rarer",
      subtitle = paste("New doctors avoid moving to rural areas,",
                       "leaving older doctors to pick up the slack"),
      caption = "Source: American Medical Association Master File",
      y = "Doctors Per 100k People",
      x = "Doctor Age"
    ) + 
    theme(
      plot.title = element_text(size = 14),
      plot.subtitle = element_text(color = "grey35", margin = margin(b = 7), size = 12),
      plot.caption = element_text(color = "grey35"),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 7)),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11),
      axis.text.y = element_text(margin = margin(r = 5)),
      axis.text.x = element_text(margin = margin(t = 5)),
      legend.justification = "top",
      legend.position = c(.93, .96)
      ) +
    ggsave("p1_pop_pyramid.pdf", width = 8, height = 6)

# Plot 2: Arrows
ama_filtered %>%
  group_by(med_school_id) %>%
  summarize(
    avg_score = mean(med_mcat_score, na.rm = T),
    prac_qcbsa = mean(`2013_qcbsa`, na.rm = T),
    birth_qcbsa = mean(birth_qcbsa, na.rm = T)
    ) %>% 
  mutate(
    direction = ifelse(sign(birth_qcbsa - prac_qcbsa) == -1, "Urban", "Rural"),
    avg_score = avg_score + rnorm(avg_score, sd = 1)
    ) %>%
  filter(!is.na(direction)) %>%
  ggplot() +
  geom_segment(aes(
    x = birth_qcbsa,
    xend = prac_qcbsa,
    y = avg_score,
    yend = avg_score,
    color = direction),
    arrow = arrow(length = unit(0.15, "cm"))
    ) +
  scale_x_continuous(
    limits = c(2, 9),
    breaks = seq(2, 9, by = 1),
    labels = c("", "More Rural", rep("", 4), "More Urban", "")
    ) +
  scale_y_continuous(limits = c(500, 520), breaks = seq(500, 520, by = 2)) +
  scale_color_brewer(
    name = "Direction",
    palette = "Set1"
    ) +
  labs(
    title = "Where Medical Schools Send Their Graduates",
    subtitle = "Arrows describe the change in rurality from student birthplace to place of practice",
    x = "Average Student Location Rurality",
    y = "Average School MCAT Score",
    caption = "Source: American Medical Association Master File"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(color = "grey35", margin = margin(b = 7), size = 12),
    plot.caption = element_text(color = "grey35"),
    axis.title.x = element_text(margin = margin(t = 5)),
    axis.title.y = element_text(margin = margin(r = 7)),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(size = 12, margin = margin(t = 5)),
    legend.justification = "top",
    legend.position = c(.93, .97)
  ) +
  ggsave("p2_arrows.pdf", width = 8, height = 6)

  
# Plot 3
ama_filtered %>%
  filter(between(yot, 1940, 2020)) %>%
  mutate(
    GP = ifelse(
      primary_specialty %in% c("GP", "FMP", "FPG", "GPM", "PD"),
      "Primary Care",
      "Specialist")
    ) %>% 
  group_by(yot, GP) %>%
  summarize(doc_count = n()) %>%
  ggplot() +
    geom_line(aes(x = yot, y = doc_count, color = factor(GP)), size = 1.5) +
    scale_color_brewer(palette = "Set1", name = "Type") +
    scale_x_continuous(
      breaks = seq(1940, 2020, by = 10)
    ) +
  labs(
    title = "More Doctors Are Becoming Specialists",
    subtitle = "As tuition costs rise, more doctors are choosing to become specialists over GPs",
    x = "Year of Training",
    y = "Number of Doctors",
    caption = "Source: American Medical Association Master File"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(color = "grey35", margin = margin(b = 7), size = 12),
    plot.caption = element_text(color = "grey35"),
    axis.title.x = element_text(margin = margin(t = 7)),
    axis.title.y = element_text(margin = margin(r = 7)),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    # axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(size = 12, margin = margin(t = 5)),
    legend.justification = "top",
    legend.position = c(.2, .97),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  ) +
  guides(colour = guide_legend(nrow = 1)) +
  ggsave("p3_specialists.pdf", width = 8, height = 6)




library(readxl)
library(tidyverse)

Glucose_pH_rawdata <- read_excel("Glucose_pH_rawdata_FB2+4.xlsx")



ggplot(Glucose_pH_rawdata %>% filter(!is.na(`Glucose_corr_[g/L]`)), 
       aes(x = Hour, y = `Glucose_corr_[g/L]`, color = Condition, shape = Replicate)) +
  geom_point(size = 1, na.rm = TRUE)+
  geom_line(size = 1, na.rm = TRUE) +
  theme_bw()+
  theme(
    plot.title = element_text(size=14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal", 
    legend.title = element_text(size = 12, face = "bold", hjust = 1),
    legend.text = element_text(size = 12),
    legend.box = "horizontal",
    legend.box.just = "center"
  )+
  labs(
    title = "Glucose Concentration",
    x = "Time [Hours]",
    y = "Glucose [g/L]")



# Calculate the mean and standard error for each condition
summary_data <- Glucose_pH_rawdata %>%
  filter(Condition %in% c("A", "B", "C", "D", "E", "F", "G")) %>%
  group_by(Condition, Hour) %>%
  summarise(
    mean_glucose = mean(`Glucose_corr_[g/L]`, na.rm = TRUE),
    se_glucose = sd(`Glucose_corr_[g/L]`, na.rm = TRUE) / sqrt(sum(!is.na(`Glucose_corr_[g/L]`)))
  ) %>%
  filter(!is.nan(mean_glucose))  # Remove rows with NaN mean_glucose


# Plot the summarized data with color coding based on condition
ggplot(summary_data, aes(x = Hour, y = mean_glucose, color = Condition)) +
  geom_point(size = 1) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = mean_glucose - se_glucose, ymax = mean_glucose + se_glucose), width = 1, size = 1) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12, face = "bold", hjust = 1),
    legend.text = element_text(size = 12),
    legend.box = "horizontal",
    legend.box.just = "center"
  ) +
  labs(
    title = "Glucose Concentration",
    x = "Time [Hours]",
    y = "Glucose [g/L]"
  )+
  scale_color_manual(
    values = c(
      "A" = "#ee3377",
      "B" = "#56b4e9",
      "C" = "#009e73",
      "D" = "#cc79a7",
      "E" = "#ee7733",
      "F" = "#0072b2",
      "G" = "#ffd800"
    ),
    name = "Feeding Strategy",
  guide = guide_legend(nrow = 1))
  

# separate plots ----------------------------------------------------------

high_glc <- summary_data %>%
  filter(Condition %in% c("A", "B"))

high_glc <- ggplot(high_glc, aes(x = Hour, y = mean_glucose, color = Condition)) +
  geom_point(size = 0.7) +
  geom_line(size = 0.5) +
  geom_errorbar(aes(ymin = mean_glucose - se_glucose, ymax = mean_glucose + se_glucose), width = 3, size = 0.5) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 8, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.y.right = element_text(size = 8),
    axis.text.y.right = element_text(size = 8, color = "black"),
    legend.position = "bottom",
    # legend.justification = c(0.5, 0.5),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  labs(
    title = "Strategy A and B",
    x = "Time [Hours]",
    y = "Glucose [g/L]"
  )+
  scale_color_manual(
    values = c(
      "A" = "#ee3377",
      "B" = "#56b4e9",
      "C" = "#009e73",
      "D" = "#cc79a7",
      "E" = "#ee7733",
      "F" = "#0072b2",
      "G" = "#ffd800"
    ),
    name = "Feeding Strategy",
    guide = guide_legend(nrow = 1))+
  scale_x_continuous(limits = c(0, 270), breaks = seq(24, 270, 48))+
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1))

ggsave("plots/glucose_high.png",
       units = c("cm"),
       height = 10,
       width = 7,
       bg = "white",
       dpi = 600)


med_glc <- summary_data %>%
  filter(Condition %in% c("G", "C", "D"))

med_glc <- med_glc %>%
  filter(!(Hour %in% c("72", "73", "96")))

med_glc <- ggplot(med_glc, aes(x = Hour, y = mean_glucose, color = Condition)) +
  geom_point(size = 0.7) +
  geom_line(size = 0.5) +
  geom_errorbar(aes(ymin = mean_glucose - se_glucose, ymax = mean_glucose + se_glucose), width = 3, size = 0.5) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 8, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.y.right = element_text(size = 8),
    axis.text.y.right = element_text(size = 8, color = "black"),
    legend.position = "bottom",
    # legend.justification = c(0.5, 0.5),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  labs(
    title = "Strategy G, C and D",
    x = "Time [Hours]",
    y = "Glucose [g/L]"
  )+
  scale_color_manual(
    values = c(
      "A" = "#ee3377",
      "B" = "#56b4e9",
      "C" = "#009e73",
      "D" = "#cc79a7",
      "E" = "#ee7733",
      "F" = "#0072b2",
      "G" = "#ffd800"
    ),
    name = "Feeding Strategy",
    guide = guide_legend(nrow = 1))+
  scale_x_continuous(limits = c(0, 270), breaks = seq(24, 270, 48))+
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1))

ggsave("plots/glucose_med.png",
       units = c("cm"),
       height = 10,
       width = 7,
       bg = "white",
       dpi = 600)


low_glc <- summary_data %>%
  filter(Condition %in% c("E", "F"))

low_glc <- med_low %>%
  filter(!(Hour %in% c("72", "73", "96")))

low_glc <- ggplot(low_glc, aes(x = Hour, y = mean_glucose, color = Condition)) +
  geom_point(aes(group = Condition), size = 0.7, position = position_dodge(width = 2)) +
  geom_line(aes(group = Condition), size = 0.5, position = position_dodge(width = 2)) +
  geom_errorbar(aes(ymin = mean_glucose - se_glucose, ymax = mean_glucose + se_glucose), width = 3, size = 0.5) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 8, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.y.right = element_text(size = 8),
    axis.text.y.right = element_text(size = 8, color = "black"),
    legend.position = "bottom",
    # legend.justification = c(0.5, 0.5),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  labs(
    title = "Strategy E and F",
    x = "Time [Hours]",
    y = "Glucose [g/L]"
  )+
  scale_color_manual(
    values = c(
      "A" = "#ee3377",
      "B" = "#56b4e9",
      "C" = "#009e73",
      "D" = "#cc79a7",
      "E" = "#ee7733",
      "F" = "#0072b2",
      "G" = "#ffd800"
    ),
    name = "Feeding Strategy",
    guide = guide_legend(nrow = 1))+
  scale_x_continuous(limits = c(-1, 271), breaks = seq(24, 270, 48))+
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1))

ggsave("plots/glucose_low.png",
       units = c("cm"),
       height = 10,
       width = 7,
       bg = "white",
       dpi = 600)




ggarrange(high_glc, med_glc, low_glc, labels = c("(a)", "(b)", "(c)"),
          common.legend = FALSE, legend = "bottom")

ggsave("plots/glucose_arranged.png",
       units = c("cm"),
       height = 10,
       width = 21,
       bg = "white",
       dpi = 600)

 # pH ----------------------------------------------------------------------



### ph

# Calculate the mean and standard error for each condition
sum_data_ph <- Glucose_pH_rawdata %>%
  filter(Condition %in% c("A", "B", "C", "D", "E", "F")) %>%
  group_by(Condition, Hour) %>%
  summarise(
    mean_pH = mean(pH, na.rm = TRUE),
    se_pH = sd(pH, na.rm = TRUE) / sqrt(sum(!is.na(pH))),
    .groups = 'drop'  # This ensures that the grouping is dropped after summarizing
  ) %>%
  filter(!is.nan(mean_pH)) 

# Plot the summarized data with color coding based on condition
ggplot(sum_data_ph, aes(x = Hour, y = mean_pH, color = Condition)) +
  geom_point(size = 1) +
  geom_line(size = 1) +
  # geom_errorbar(aes(ymin = mean_glucose - se_glucose, ymax = mean_glucose + se_glucose), width = 1, size = 1) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12, face = "bold", hjust = 1),
    legend.text = element_text(size = 12),
    legend.box = "horizontal",
    legend.box.just = "center"
  ) +
  labs(
    title = "pH by Condition",
    x = "Time [Hours]",
    y = "pH"
  )+
  facet_wrap(~Condition)


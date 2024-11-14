
#20241112

library(tidyverse)
library(readr)
library(ggpubr)

setwd("R:/AG/Borth/Proj/Shared/DigiTherapeutX/TPP_tube_runs/Project_Feeding_Strategies")


# dir.create("plots", showWarnings = FALSE)


Vicell_data_fb2 <- read_delim("Vicell_data_fb2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% select (-ID, -TP_old) %>% mutate(Batch = "FB2")
Vicell_data_fb4 <- read_csv("Vicell_data_fb4.csv") %>% select (-...1, -ID) %>% mutate(Batch = "FB4")


df <- rbind(Vicell_data_fb2, Vicell_data_fb4)

df$ID <- paste(df$Date, df$Batch,df$Condition, df$TP, df$Replicate,sep = "_")

write.csv(df, "20241112_vicell_sum.csv")

#color scheme
Batch <- c(
  "FB2" = "#C3D878", 
  "FB4" = "#58A787"
  )

Condition_colors <- c(
  "A" = "#ee3377",
  "B" = "#56b4e9",
  "C" = "#009e73",
  "D" = "#cc79a7",
  "E" = "#ee7733",
  "F" = "#0072b2",
  "G" = "#ffd800")

scale_factor <- 0.5


# plot individual replicates ----------------------------------------------


Con_A <- filter(df, Condition == "A")

ggplot(Con_A, aes(x = Hours, y = Total_VCD, color = Batch, shape = Replicate)) +
  geom_line(size = 1)+
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
    title = "Con A Replicates",
    x = "Time [Hours]",
    y = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]")
  ) +
  scale_x_continuous(
    breaks = seq(0, 288, 24),
    limits = c(0, 288)) +
  scale_color_manual(values = Batch, name = "Replicates")


ggsave("Con_A_VCD_.png",
       units = c("cm"),
       height = 20,
       width = 40,
       bg = "white",
       dpi = 600)


ggplot(Con_A, aes(x = Hours, y = Viability, color = Batch, shape = Replicate)) +
  geom_line(size = 1)+
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
    title = "Con A Replicates",
    x = "Time [Hours]",
    y = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]")
  ) +
  scale_x_continuous(
    breaks = seq(0, 288, 24),
    limits = c(0, 288)) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    limits = c(0, 100)) +
  scale_color_manual(values = Batch, name = "Replicates")

ggsave("Con_A_VIAB_.png",
       units = c("cm"),
       height = 20,
       width = 40,
       bg = "white",
       dpi = 600)



Con_B <- filter(df, Condition == "B")

ggplot(Con_B, aes(x = Hours, y = Total_VCD, color = Batch, shape = Replicate)) +
  geom_line(size = 1)+
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
    title = "Con B Replicates",
    x = "Time [Hours]",
    y = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]")
  ) +
  scale_x_continuous(
    breaks = seq(0, 288, 24),
    limits = c(0, 288)) +
  scale_color_manual(values = Batch, name = "Replicates")


ggsave("Con_B_VCD_.png",
       units = c("cm"),
       height = 20,
       width = 40,
       bg = "white",
       dpi = 600)


ggplot(Con_B, aes(x = Hours, y = Viability, color = Batch, shape = Replicate)) +
  geom_line(size = 1)+
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
    title = "Con B Replicates",
    x = "Time [Hours]",
    y = "Viability [%]") +
  scale_x_continuous(
    breaks = seq(0, 288, 24),
    limits = c(0, 288)) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    limits = c(0, 100)) +
  scale_color_manual(values = Batch, name = "Replicates")

ggsave("Con_B_VIAB_.png",
       units = c("cm"),
       height = 20,
       width = 40,
       bg = "white",
       dpi = 600)



Con_C <- filter(df, Condition == "C")

ggplot(Con_C, aes(x = Hours, y = Total_VCD, color = Batch, shape = Replicate)) +
  geom_line(size = 1)+
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
    title = "Con C Replicates",
    x = "Time [Hours]",
    y = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]")
  ) +
  scale_x_continuous(
    breaks = seq(0, 288, 24),
    limits = c(0, 288)) +
  scale_color_manual(values = Batch, name = "Replicates")


ggsave("Con_C_VCD_.png",
       units = c("cm"),
       height = 20,
       width = 40,
       bg = "white",
       dpi = 600)


ggplot(Con_C, aes(x = Hours, y = Viability, color = Batch, shape = Replicate)) +
  geom_line(size = 1)+
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
    title = "Con C Replicates",
    x = "Time [Hours]",
    y = "Viability [%]") +
  scale_x_continuous(
    breaks = seq(0, 288, 24),
    limits = c(0, 288)) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    limits = c(0, 100)) +
  scale_color_manual(values = Batch, name = "Replicates")

ggsave("Con_C_VIAB_.png",
       units = c("cm"),
       height = 20,
       width = 40,
       bg = "white",
       dpi = 600)


# average data ------------------------------------------------------------

# calculate average data
avg_df <- df %>%
  group_by(Condition, TP) %>%
  summarise(
    mean_vcd = mean(Total_VCD, na.rm = TRUE),
    se_vcd = sd(Total_VCD, na.rm = TRUE) / sqrt(n()),
    mean_hours = mean(Hours, na.rm = TRUE),
    se_hours = sd(Hours, na.rm = TRUE) / sqrt(n()), 
    mean_dia = mean(Average_diameter, na.rm = TRUE),
    se_dia = sd(Average_diameter, na.rm = TRUE) / sqrt(n()),
    mean_via = mean(Viability, na.rm = TRUE),
    se_via = sd(Viability, na.rm = TRUE) / sqrt(n())) 


avg_df <- avg_df %>%
  filter(!(TP == "TP10" & Condition %in% c("C", "G")))

# vcd + viab
avg_df %>%
  ggplot(aes(x = mean_hours)) +
  geom_line(aes(y = mean_vcd, color = Condition, linetype = "VCD"), linewidth = 1, na.rm = TRUE) + 
  geom_point(aes(y = mean_vcd, color = Condition), size = 2, na.rm = TRUE) +
  geom_errorbar(aes(ymin = mean_vcd - se_vcd, ymax = mean_vcd + se_vcd, color = Condition, linetype = "VCD"), width = 3, na.rm = TRUE) +
  geom_point(aes(y = mean_via * scale_factor, color = Condition), size = 2, na.rm = TRUE) +
  geom_line(aes(y = mean_via * scale_factor, color = Condition, linetype = "Viability"), linewidth = 1.5, na.rm = TRUE) + 
  geom_errorbar(aes(ymin = (mean_via - se_via) * scale_factor, ymax = (mean_via + se_via) * scale_factor, color = Condition, linetype = "Viability"), width = 3, na.rm = TRUE) +
  scale_y_continuous(
    name = "Viable Cell Density [x10^6 vc/mL]",
    sec.axis = sec_axis(~./scale_factor, name = "Viability [%]")
  ) +
  labs(
    x = "Time [Hours]",
    y = "Viable Cell Density [x" ~ 10^6 ~ "vc/mL]",
    title = "VCD & Viability"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.y.right = element_text(size = 14),
    axis.text.y.right = element_text(size = 14, color = "black"),
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
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
    name = "Feeding Strategy") +
  scale_linetype_manual(
    values = c("VCD" = "solid", "Viability" = "dotted"),
    name = "Type",
    labels = c("VCD" = "VCD", "Viability" = "Viability")
  ) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  scale_x_continuous(limits = c(0, 288), breaks = seq(0, 288, 24))


ggsave("plots/VCD_VIAB_averaged_.png",
       units = c("cm"),
       height = 20,
       width = 40,
       bg = "white",
       dpi = 600)



#individual vcd and viab

# vcd
vcd <- avg_df %>%
  ggplot(aes(x = mean_hours, y = mean_vcd, color = Condition)) +
  geom_line(linewidth = 0.5)+ 
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = mean_vcd - se_vcd, ymax = mean_vcd + se_vcd, color = Condition), width = 3, na.rm = TRUE) +
  labs(
    x = "Time [Hours]",
    y = "Viable Cell Density [x" ~ 10^6 ~ "vc/mL]",
    title = "Viable Cell Density"
  ) +
  theme_bw() +
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
    guide = guide_legend(nrow = 1)) +
  scale_x_continuous(limits = c(0, 270), breaks = seq(0, 270, 24))

plot(vcd)

ggsave("plots/VCD_averaged.png",
       units = c("cm"),
       height = 10,
       width = 10,
       bg = "white",
       dpi = 600)

# vcd
via <- avg_df %>%
  ggplot(aes(x = mean_hours, y = mean_via, color = Condition)) +
  geom_line(linewidth = 0.5)+ 
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = mean_via - se_via, ymax = mean_via + se_via, color = Condition), width = 3, na.rm = TRUE) +
  labs(
    x = "Time [Hours]",
    y = "Viability [%]",
    title = "Viability"
  ) +
  theme_bw() +
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
    guide = guide_legend(nrow = 1)) +
  scale_x_continuous(limits = c(0, 270), breaks = seq(0, 270, 24))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10))


plot(via)


ggsave("plots/VIA_averaged.png",
       units = c("cm"),
       height = 10,
       width = 10,
       bg = "white",
       dpi = 600)


ggarrange(vcd, via, labels = c("(a)", "(b)"),
          common.legend = TRUE, legend = "bottom")

ggsave("plots/VCD_VIA_arranged.png",
       units = c("cm"),
       height = 10,
       width = 20,
       bg = "white",
       dpi = 600)


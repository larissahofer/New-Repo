

library(tidyverse)
library(readr)

setwd("R:/AG/Borth/Proj/Shared/DigiTherapeutX/TPP_tube_runs/Project_Feeding_Strategies")


# dir.create("plots", showWarnings = FALSE)


Vicell_data_fb2 <- read_csv("Vicell_data_fb2.csv") %>% select (-...1, -ID) %>% mutate(Batch = "FB2")
Vicell_data_fb4 <- read_csv("Vicell_data_fb4.csv") %>% select (-...1, -ID) %>% mutate(Batch = "FB4")


df <- rbind(Vicell_data_fb2, Vicell_data_fb4)

df$ID <- paste(df$Date, df$Batch,df$Condition, df$TP, df$Replicate,sep = "_")

Batch <- c("FB2" = "#C3D878", "FB4" = "#58A787")


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

###########################################################

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

####################################################

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


# earlier runs ------------------------------------------------------------




df <- Sum_BR_TempShift %>%
  mutate(Group = ifelse(Experiment %in% c("E05", "E07", "E08"), "Trial Run 2", "Trial Run 3"))



grouped_data %>%
  ggplot(aes(x=Hours, y=Total_VCD, color=Experiment))+
  geom_point()+
  #facet_wrap(~E)+
  geom_line()+
  labs(x="Time [Hours]", y="Viable Cells / mL", title = "Viable cells / mL over time", subtitle = "BR23 runs: E05-E08")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme_bw()

Runs <- c("Trial Run 2" = "#ee3377", "Trial Run 3" = "#77722E")



# Define color mapping for the Groups
Runs <- c("Run2" = "#D8537D", "Run3" = "#4C8F8B")

Sum_BR_TempShift %>%
  filter(Group != "Run4+5") %>%  # Exclude "Run4+5"
  na.omit() %>%
  ggplot(aes(x = Hours/24, y = Total_VCD, color = Group)) +
  #geom_point(size = 1.5, na.rm = TRUE) +
  geom_line(linewidth = 1, na.rm = TRUE, aes(group = interaction(Group, Experiment))) +  # Use Group for line grouping
  labs(
    title = "Viable Cell Density",
    x = "Time [d]",
    y = "Viable Cell Density [x" ~ 10^6 ~ "vc/mL]"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = Runs, name = "Group") +  # Apply custom colors for Groups
  scale_x_continuous(limits = c(1, 14), breaks = seq(1, 14, 1))+
  scale_y_continuous(limits = c(0, 30), breaks = seq(1, 30, 5))




Sum_BR_TempShift %>%
  filter(Group != "Run4+5") %>%  # Exclude "Run4+5"
  na.omit() %>%
  ggplot(aes(x = Hours/24, y = Viability, color = Group)) +
  #geom_point(size = 1.5, na.rm = TRUE) +
  geom_line(linewidth = 1, na.rm = TRUE, aes(group = interaction(Group, Experiment))) +  # Use Group for line grouping
  labs(
    title = "Viability",
    x = "Time [d]",
    y = "Viability [%]"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = Runs, name = "Group") +  # Apply custom colors for Groups
  scale_x_continuous(limits = c(1, 14), breaks = seq(1, 14, 1))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10))




# tom plot ----------------------------------------------------------------

setwd("R:/AG/Borth/Proj/Shared/DigiTherapeutX/BR_Fed_Batch_Runs/20240419_Char_Run_Sum")

vicell <- read.csv("20240505_CharRun_Sum_ViCell.csv") 


cqa_sampling <- c(120, 144, 168, 192, 216, 288, 336)


ggplot(vicell) +
  geom_point(aes(x = Hours, y = Total_VCD, color = Experiment, shape = "VCD"), size = 3) +
  geom_point(aes(x = Hours, y = Viability / 2, color = Experiment, shape = "Viability"), size = 3) +
  geom_vline(xintercept = 146, linetype = "dashed", color = "red", size = 1.5) +  # Direct linetype usage
  geom_vline(xintercept = cqa_sampling, linetype = "dashed", color = "#018FC7", size = 1.5) +  # Same here
  scale_shape_manual(
    name = "Type",
    values = c("VCD" = 16, "Viability" = 17)
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, color = "black"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.box = "horizontal",
    legend.box.just = "center"
  ) +
  labs(
    x = "Time [Hours]",
    y = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]")
  ) +
  scale_color_manual(
    values = c(
      "E13" = "#b35806",
      "E15" = "#e08214",
      "E17" = "#fdb863",
      "E19" = "#fee0b6",
      "E14" = "#542788",
      "E16" = "#8073ac",
      "E18" = "#b2abd2",
      "E20" = "#d8daeb"
    ),
    name = "Experiment"
  ) +
  scale_linetype_manual(
    values = c(
      "Temp. shift" = "dashed", 
      "CQA Sampling" = "dashed"
    ),
    name = "Event"
  ) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    linetype = guide_legend(order = 3)
  ) +
  scale_x_continuous(
    breaks = seq(0, 384, 24),
    limits = c(0, 384)
  ) +
  scale_y_continuous(
    name = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]"),
    limits = c(0, 50),
    breaks = seq(0, 50, 5),
    sec.axis = sec_axis(~ . * 2, name = "Viability [%]")
  )


ggsave("VCD_VIAB.png",
       units = c("cm"),
       height = 20,
       width = 40,
       bg = "white",
       dpi = 600)


# charrun viab vcd --------------------------------------------------------

setwd("R:/AG/Borth/Proj/Shared/DigiTherapeutX/BR_Fed_Batch_Runs/20240419_Char_Run_Sum/plenary meeting 10_2024")

vicell <- read.csv("20240505_CharRun_Sum_ViCell.csv") 

ggplot(vicell) +
  geom_point(aes(x = Hours, y = Total_VCD, color = Experiment, shape = "VCD"), size = 3) +
  geom_point(aes(x = Hours, y = Viability / 2, color = Experiment, shape = "Viability"), size = 3) +
  geom_vline(aes(xintercept = 146, linetype = "Temp. shift"), color = "#58A787", size = 1.5) +
  scale_shape_manual(
    name = "Type",
    values = c("VCD" = 16, "Viability" = 17)
  ) +
  theme_minimal()+
  theme(
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
    x = "Time [Hours]",
    y = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]")
  ) +
  scale_color_manual(
    values = c(
      "E13" = "#b35806",
      "E15" = "#e08214",
      "E17" = "#fdb863",
      "E19" = "#fee0b6",
      "E14" = "#542788",
      "E16" = "#8073ac",
      "E18" = "#b2abd2",
      "E20" = "#d8daeb"
    ),
    name = "Constant \n \n Temperature Shifted"
  ) +
  scale_linetype_manual(
    values = c(
      "Temp. shift" = "dashed", 
      "CQA Sampling" = "dashed"
    ),
    name = "Event"
  ) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    linetype = guide_legend(order = 3)
  ) +
  scale_x_continuous(
    breaks = seq(0, 384, 24),
    limits = c(0, 384)
  ) +
  scale_y_continuous(
    name = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]"),
    limits = c(0, 50),
    breaks = seq(0, 50, 5),
    sec.axis = sec_axis(~ . * 2, name = "Viability [%]")
  )


ggsave("plots/VCD_VIAB.png", 
       units = "cm", 
       height = 15, 
       width = 25, 
       bg = "white", 
       dpi = 600)


# Char Run 1 vs Char Run 2

vicell <- vicell %>%
  mutate(Condition = ifelse(Experiment %in% c("E14", "E16", "E18", "E20"), "Temp. shifted", "Constant"))



vicell_CT <- vicell %>%
  filter(Condition == "Constant")



ggplot(vicell_CT) +
  geom_point(aes(x = Hours, y = Total_VCD, color = Group, shape = "VCD"), size = 3) +
  geom_point(aes(x = Hours, y = Viability / 2, color = Group, shape = "Viability"), size = 3) +
  scale_shape_manual(
    name = "Type",
    values = c("VCD" = 16, "Viability" = 17)
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.box = "horizontal",
    legend.box.just = "center"
  ) +
  labs(
    title = "Constant Temperature Condition",
        x = "Time [Hours]",
    y = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]")
  ) +
  scale_color_manual(
    values = c(
      "Char Run 1" = "#D8537D",
      "Char Run 2" = "#EECA76"
    ),
    name = "Group"
  ) +
  scale_linetype_manual(
    values = c(
      "Temp. shift" = "dashed"),
    name = "Event"
  ) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    linetype = guide_legend(order = 3)
  ) +
  scale_x_continuous(
    breaks = seq(0, 384, 24),
    limits = c(0, 384)
  ) +
  scale_y_continuous(
    name = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]"),
    limits = c(0, 50),
    breaks = seq(0, 50, 5),
    sec.axis = sec_axis(~ . * 2, name = "Viability [%]")
  )


ggsave("plots/VCD_VIAB_CT.png", 
       units = "cm", 
       height = 15, 
       width = 20, 
       bg = "white", 
       dpi = 600)


vicell_TS <- vicell %>%
  filter(Condition == "Temp. shifted")

ggplot(vicell_TS) +
  geom_point(aes(x = Hours, y = Total_VCD, color = Group, shape = "VCD"), size = 3) +
  geom_point(aes(x = Hours, y = Viability / 2, color = Group, shape = "Viability"), size = 3) +
  geom_vline(aes(xintercept = 146, linetype = "Temp. shift"), color = "#58A787", size = 1.5) +
  scale_shape_manual(
    name = "Type",
    values = c("VCD" = 16, "Viability" = 17)
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.box = "horizontal",
    legend.box.just = "center"
  ) +
  labs(
    title = "Temperature Shifted Condition",
    x = "Time [Hours]",
    y = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]")
  ) +
  scale_color_manual(
    values = c(
      "Char Run 1" = "#D8537D",
      "Char Run 2" = "#EECA76"
    ),
    name = "Group"
  ) +
  scale_linetype_manual(
    values = c(
      "Temp. shift" = "dashed"),
    name = "Event"
  ) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    linetype = guide_legend(order = 3)
  ) +
  scale_x_continuous(
    breaks = seq(0, 384, 24),
    limits = c(0, 384)
  ) +
  scale_y_continuous(
    name = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]"),
    limits = c(0, 50),
    breaks = seq(0, 50, 5),
    sec.axis = sec_axis(~ . * 2, name = "Viability [%]")
  )


ggsave("plots/VCD_VIAB_TS.png", 
       units = "cm", 
       height = 15, 
       width = 20, 
       bg = "white", 
       dpi = 600)


# ln growth 


ggplot(vicell, aes(x = Hours, y = log(Total_VCD), color = Experiment)) +
  geom_line(linewidth = 1) +
  theme_minimal()+
  theme(
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
    x = "Time [Hours]",
    y = expression("ln (Viable Cell Density [x" ~ 10^6 ~ "cells/mL])")
  ) +
  scale_color_manual(
    values = c(
      "E13" = "#b35806",
      "E15" = "#e08214",
      "E17" = "#fdb863",
      "E19" = "#fee0b6",
      "E14" = "#542788",
      "E16" = "#8073ac",
      "E18" = "#b2abd2",
      "E20" = "#d8daeb"
    ),
    name = "Constant \n \n Temperature Shifted"
  ) +
  guides(
    color = guide_legend(order = 1),
  ) +
  scale_x_continuous(
    breaks = seq(0, 384, 24),
    limits = c(0, 384)
  )

ggsave("plots/VCD_VIAB_ln.png", 
       units = "cm", 
       height = 15, 
       width = 25, 
       bg = "white", 
       dpi = 600)


# LDH smoothing -----------------------------------------------------------
setwd("R:/AG/Borth/Proj/Shared/DigiTherapeutX/BR_Fed_Batch_Runs/20240419_Char_Run_Sum/cedex")



#create groups 
df <- LDH_remeasure%>%
  mutate(Condition = ifelse(Experiment %in% c("E14", "E16", "E18", "E20"), "Temp. shifted", "Constant"))

df <- LDH_timecourse_loess

library(tidyverse)

ggplot(df, aes(x = Hours, y = LDH))+
  geom_line(linewidth = 1)+
  theme_minimal()+
  labs(
    x = "Time [h]",
    y = "LDH [U/L]")+
  facet_wrap(~ Experiment)



# geom_smooth plot: loess
ggplot(df, aes(x = Hours, y = LDH, color = Experiment)) +
  geom_point()+
  stat_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1) +  # Apply LOESS smoothing
  theme_minimal() +
  labs(
    x = "Time [h]",
    y = "LDH [U/L]")


df <- LDH_timecourse_loess

# Initialize the Predicted_LDH column
df$Predicted_LDH <- NA  

# Loop through each unique Experiment
for (exp in unique(df$Experiment)) {
  # Subset the data for the specific Experiment, removing rows with NA in LDH
  subset_data <- na.omit(df[df$Experiment == exp, c("Hours", "LDH")])
  
  # Check if there are enough valid data points for fitting
  if (nrow(subset_data) > 0) {
    # Fit the LOESS model
    loess_fit <- loess(LDH ~ Hours, data = subset_data, span = 0.3)
    
    # Predict using the fitted model for the current experiment
    predicted_LDH <- predict(loess_fit, newdata = data.frame(Hours = df$Hours[df$Experiment == exp]))
    
    # Store predictions in the original dataframe
    df$Predicted_LDH[df$Experiment == exp] <- predicted_LDH
  } else {
    cat("No valid data for Experiment:", exp, "\n")
  }
}


ggplot(df, aes(x = Hours, y = LDH, color = Experiment)) +
  geom_point()+
  geom_line()+
  theme_minimal() +
  labs(
    x = "Time [h]",
    y = "LDH [U/L]")



write.csv(df, "LDH_predicted.csv", row.names = FALSE)



# Titer smoothing ---------------------------------------------------------

df <- read.csv("Titer_Measurements_outliers_removed.csv")

library(tidyverse)

# native plot
ggplot(na.omit(df),
       aes(x = Hours, y = Titer/1000, color = Experiment))+
  geom_line(linewidth = 1.5)+
  scale_color_manual(
  values = c(
    "E13" = "#b35806",
    "E15" = "#e08214",
    "E17" = "#fdb863",
    "E19" = "#fee0b6",
    "E14" = "#542788",
    "E16" = "#8073ac",
    "E18" = "#b2abd2",
    "E20" = "#d8daeb"
  ),
  name = "Experiment")+
  theme_minimal()+
  labs(
    x = "Time [h]",
    y = "Titer [g/L]")+
  theme(
    axis.title.x = element_text(size = 16, color = "black"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.box = "horizontal",
    legend.box.just = "center")


ggsave("plots/Titer_timecourse.png", 
       units = c("cm"),
       height = 15,
       width = 15,
       bg = "white",
       dpi = 600)

# geom_smooth plot: loess
ggplot(na.omit(df), aes(x = Hours, y = Titer/1000, color = Experiment)) +
  #geom_point() +
  stat_smooth(method = "loess", span = 0.4, se = FALSE, linewidth = 1.5) +  # Apply LOESS smoothing
  scale_color_manual(
    values = c(
      "E13" = "#b35806",
      "E15" = "#e08214",
      "E17" = "#fdb863",
      "E19" = "#fee0b6",
      "E14" = "#542788",
      "E16" = "#8073ac",
      "E18" = "#b2abd2",
      "E20" = "#d8daeb"
    ),
    name = "Experiment")+
  theme_minimal()+
  labs(
    x = "Time [h]",
    y = "Titer [g/L]")+
  theme(
    axis.title.x = element_text(size = 16, color = "black"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.box = "horizontal",
    legend.box.just = "center")


ggsave("plots/Titer_timecourse_loess.png", 
       units = c("cm"),
       height = 15,
       width = 15,
       bg = "white",
       dpi = 600)


df <- Titer_Measurements_outliers_removed
# Initialize the Predicted_Titer column
df$Predicted_Titer <- NA  

# Loop through each unique Experiment
for (exp in unique(df$Experiment)) {
  # Subset the data for the specific Experiment, removing rows with NA in LDH
  subset_data <- na.omit(df[df$Experiment == exp, c("Hours", "Titer")])
  
  # Check if there are enough valid data points for fitting
  if (nrow(subset_data) > 0) {
    # Fit the LOESS model
    loess_fit <- loess(Titer ~ Hours, data = subset_data, span = 0.4)
    
    # Predict using the fitted model for the current experiment
    Predicted_Titer <- predict(loess_fit, newdata = data.frame(Hours = df$Hours[df$Experiment == exp]))
    
    # Store predictions in the original dataframe
    df$Predicted_Titer[df$Experiment == exp] <- Predicted_Titer
  } else {
    cat("No valid data for Experiment:", exp, "\n")
  }
}


ggplot(df, aes(x = Hours, y = Predicted_Titer, color = Experiment)) +
  geom_point()+
  geom_line()+
  theme_minimal() +
  labs(
    x = "Time [h]",
    y = "Titer [µg/mL]")

write.csv(df, "Titer_predicted.csv", row.names = FALSE)



# Timecourse AA / Meta ----------------------------------------------------

# Define colors for conditions and experiments
condition_colors <- c("Constant" = "#D94801",
                      "Temp. shifted" = "#6A51A3")

experiment_colors <- c(
  "E13" = "#b35806",
  "E14" = "#542788",
  "E15" = "#e08214",
  "E16" = "#8073ac",
  "E17" = "#fdb863",
  "E18" = "#b2abd2",
  "E19" = "#fee0b6",
  "E20" = "#d8daeb"
)

shape_values <- c(
  "E13" = 15,
  "E14" = 15,
  "E15" = 19,
  "E16" = 19,
  "E17" = 17,
  "E18" = 17,
  "E19" = 18,
  "E20" = 18
)

# Filter out specific values from AA_meta
filtered_data <- conversed_long %>%
  filter(!AA_meta %in% c("LDH", "GLC", "LAC", "NH3", "Titer"))

filtered_avg_data <- avg_data %>%
  filter(!AA_meta %in% c("LDH", "GLC", "LAC", "NH3", "Titer"))

# Create the plot using the filtered data
ggplot() +  # Start ggplot without data
  geom_point(data = filtered_data,
             aes(x = Hours, y = Concentration_mM, color = Experiment, shape = Experiment),
             size = 1) +
  geom_line(data = filtered_avg_data,
            aes(x = mean_hours, y = mean_conc, group = Condition, color = Condition),
            linetype = "solid", size = 1) +
  scale_color_manual(values = c(experiment_colors, condition_colors)) +  # Combine both color scales
  scale_shape_manual(values = shape_values) +  # Assign unique shapes
  labs(
    x = "Time [Hours]",
    y = "Concentration mM/L]",
    shape = "Experiment",
    color = "Condition"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.box = "horizontal",
    legend.box.just = "center"
  ) +
  facet_wrap(~ AA_meta, scales = "free")


# Meta

# Filter out specific values from AA_meta
filtered_meta <- conversed_long %>%
  filter(AA_meta %in% c("LDH", "GLC", "LAC", "NH3"))

filtered_avg_meta <- avg_data %>%
  filter(AA_meta %in% c("LDH", "GLC", "LAC", "NH3"))


AA <- Rates_DCW2 %>%
  filter(!AA_meta %in% c("LDH", "GLC", "LAC", "NH3", "Titer"))

write.csv(AA, "AA_rates.csv")



# radar plots -------------------------------------------------------------

conversed_long <- AA_rates %>%
  pivot_wider(
    names_from = AA_meta,
    values_from = Rate_mM.gDCW.h
  ) %>%
  select(-...1, -Rate_pmol.cell.h, -SE_pmol.cell.h, -DCW.pg, -SD_DCW.pg, -SD_mM.gDCW.h, -Growth_rate.h, -Growth_rate_SE)

conversed_long <- AA_rates %>%
  pivot_longer(
    cols = Rate_mM.gDCW.h,  # Specify the columns to pivot
    names_to = "AA_meta",
    values_to = "Rate_mM.gDCW.h"
  )


AA_rates <- AA_rates %>%
  select(-...1)
  
  
conversed_long <- AA_rates %>%
    pivot_wider(names_from = AA_meta, values_from = Rate_mM.gDCW.h)


# Transcriptomics samples -------------------------------------------------

setwd("R:/AG/Borth/Proj/Shared/DigiTherapeutX/BR_Fed_Batch_Runs/20240419_Char_Run_Sum")

vicell <- read.csv("20240505_CharRun_Sum_ViCell.csv") 


ggplot(vicell) +
  geom_point(aes(x = Hours, y = Total_VCD, color = Experiment, shape = "VCD"), size = 3) +
  geom_point(aes(x = Hours, y = Viability / 2, color = Experiment, shape = "Viability"), size = 3) +
  geom_vline(aes(xintercept = 146, linetype = "Temp. shift"), color = "#58A787", size = 1.5) +
  geom_vline(aes(xintercept = 72, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 76, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 96, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 120, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 124, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 144, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 148, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 152, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 168, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 192, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 216, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 220, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 240, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 264, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 268, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 288, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  geom_vline(aes(xintercept = 312, linetype = "TRA Sampling"), color = "#D8537D", size = 1.5) +
  scale_shape_manual(
    name = "Type",
    values = c("VCD" = 16, "Viability" = 17)
  ) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 16, color = "black"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.box = "horizontal",
    legend.box.just = "center"
  ) +
  labs(
    x = "Time [Hours]",
    y = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]")
  ) +
  scale_color_manual(
    values = c(
      "E13" = "#b35806",
      "E15" = "#e08214",
      "E17" = "#fdb863",
      "E19" = "#fee0b6",
      "E14" = "#542788",
      "E16" = "#8073ac",
      "E18" = "#b2abd2",
      "E20" = "#d8daeb"
    ),
    name = "Experiment"
  ) +
  scale_linetype_manual(
    values = c(
      "Temp. shift" = "dashed", 
      "TRA Sampling" = "dashed"
    ),
    name = "Event"
  ) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    linetype = guide_legend(order = 3)
  ) +
  scale_x_continuous(
    breaks = seq(0, 384, 24),
    limits = c(0, 384)
  ) +
  scale_y_continuous(
    name = expression("Viable Cell Density [x" ~ 10^6 ~ "cells/mL]"),
    limits = c(0, 50),
    breaks = seq(0, 50, 5),
    sec.axis = sec_axis(~ . * 2, name = "Viability [%]")
  )


ggsave("VCD_VIAB_TRA.png",
       units = c("cm"),
       height = 20,
       width = 40,
       bg = "white",
       dpi = 600)


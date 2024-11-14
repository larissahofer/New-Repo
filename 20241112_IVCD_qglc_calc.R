
# required packages -------------------------------------------------------
require(tidyverse)
library(tidyverse)


# setwd and path ----------------------------------------------------------
setwd("R:/AG/Borth/Proj/Shared/DigiTherapeutX/TPP_tube_runs/Project_Feeding_Strategies")


# read csv rawdata
df <- read.csv("20241112_vicell_sum.csv")
df_avg <- read.csv("Vicell_avg.csv")

df <- df %>%
  filter(!(TP == "TP1_2"))

df <- df %>%
  filter(!(TP == "TP1_3"))


# calculate integral viable cell density (IVCD) ---------------------------
IVCD <- df %>%
  group_by(Condition, Replicate) %>%
  arrange(Hours, .by_group = TRUE) %>%
  mutate(
    delta_t = Hours - lag(Hours),  # calculate delta_t as the difference between consecutive TP
    delta_t = ifelse(is.na(delta_t), 0, delta_t),  # set the first delta_t in each group to 0
    VCD_t2 = Total_VCD,  # current VCD (VCD_t2)
    VCD_t1 = lag(Total_VCD),  # previous VCD (VCD_t1)
    IVCD = ifelse(is.na(VCD_t1), 0, 0.5 * (VCD_t1 + VCD_t2) * delta_t)  # set IVCD to 0 if VCD_t1 is NA (first TP)
  ) %>%
  mutate(IVCD_sum = cumsum(IVCD)) %>%  # calculate the cumulative sum of IVCD
  select(-VCD_t1, -VCD_t2)

write.csv(IVCD, "20241112_IVCD_FB2+FB4_individual.csv")

#calculate it for average
IVCD_avg <- df_avg %>%
  group_by(Cell_Line) %>%
  arrange(TP, .by_group = TRUE) %>%
  mutate(
    delta_t = Avg_Hours - lag(Avg_Hours),  # calculate delta_t as the difference between consecutive TP
    delta_t = ifelse(is.na(delta_t), 0, delta_t),  # set the first delta_t in each group to 0
    VCD_t2 = Avg_VCD,  # current VCD (VCD_t2)
    VCD_t1 = lag(Avg_VCD),  # previous VCD (VCD_t1)
    IVCD = ifelse(is.na(VCD_t1), 0, 0.5 * (VCD_t1 + VCD_t2) * delta_t)  # set IVCD to 0 if VCD_t1 is NA (first TP)
  ) %>%
  mutate(IVCD_sum = cumsum(IVCD)) %>%  # calculate the cumulative sum of IVCD
  select(-VCD_t1, -VCD_t2) %>%
  ungroup()

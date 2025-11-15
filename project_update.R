#
# Jace Royer
# FS25 / DS1030
# Final Project
#

# Data from Baseball Reference:
# https://www.baseball-reference.com/
# and Baseball Savant:
# https://baseballsavant.mlb.com/

library(dplyr)
library(ggplot2)
library(stringr)

# List of all players who appeared as a hitter in Major League Baseball
# from baseball reference
hitters_bbref <- read.csv("project/data/2025_reg_season_hitters_bbref.csv")
hitters_savant <- read.csv("project/data/2025_reg_season_hitters_savant.csv")

# Filter data so that there are only hitters who have taken more than 100 plate
# appearances, and also ensure that only unique hitters are kept.
# On Baseball-Reference, hitters' handedness is kept inside their name,
# so here we'll parse their name and create a new variable for handedness
hitters_clean <- hitters_bbref %>%
  filter(PA > 502) %>%
  distinct(Player, .keep_all = TRUE) %>%
  mutate(Handedness = case_when(
    str_sub(Player, -1) == "*" ~ "L",
    str_sub(Player, -1) == "#" ~ "S",
    TRUE ~ "R"
  )) %>%
  mutate(BBrate = BB / PA) %>%
  mutate(Krate = SO / PA) %>%
  mutate(HRrate = HR / PA) %>%
  mutate(TTOrate = BBrate + Krate + HRrate)

dataframe <- cbind(hitters_clean$WAR, hitters_clean$OPS, hitters_clean$PA, hitters_clean$TTOrate)
colnames(dataframe) <- c("WAR", "OPS", "PA", "TT0rate")

pairs(dataframe)
cor(dataframe)
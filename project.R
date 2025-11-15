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

ggplot(data = hitters_clean, aes(x = HR, y = BA, color = Handedness)) +
  geom_point() +
  scale_color_manual(values = c("L" = "red", "R" = "blue", "S" = "purple"))
#  scale_color_manual(values = c("AL" = "red", "NL" = "blue", "2LG" = "purple"))

summary(hitters_clean$BA)

ggplot(tibble(x = hitters_clean), aes(x = x$BA)) +
  geom_histogram(aes(y = after_stat(density)), fill = "skyblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hitters_clean$BA), sd = sd(hitters_clean$BA)), color = "red", linetype = "dashed") +
  theme_minimal()

ggplot(tibble(x = hitters_savant), aes(x = x$xbadiff)) +
  geom_histogram(aes(y = after_stat(density)), fill = "skyblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hitters_savant$xbadiff), sd = sd(hitters_savant$xbadiff)), color = "red", linetype = "dashed") +
  theme_minimal()

ggplot(hitters_clean, aes(sample = WAR)) +
  stat_qq() +
  stat_qq_line()

summary(hitters_clean$TTOrate)


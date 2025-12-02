#################################
# DS1030 / FS25
# Final Project: "Modeling the modern era"
# Jace Royer
#################################

# Prepare and clean data before use

library(dplyr)
library(ggplot2)
library(stringr)

hitters_bbref <- read.csv(
  "project/data/2025_reg_season_hitters_bbref.csv",
  fileEncoding = "UTF-8"
)

calculate_WRC <- function(H, HR, BB, HBP, IBB, SB, CS, TB, AB) {
  ((H - HR) + (0.7 * BB) + (0.7 * HBP) - (0.7 * IBB) + (0.9 * SB) - (0.45 * CS)) * (TB / AB)
}

# Include only qualified hitters (3.1 PA/game);
# include only distinct player entries (on BBref, it creates a new row
# each time a player appears for a new team, so take the first instance
# of the player and discard the individual team entries);
# create a new variable for handedness (requires parsing the player name,
# as that is where handedness is stored);
# introduce new variables of interest based on pre-existing data:
# - "three true outcome rate" (walks, strikeouts, and home runs per PA),
# - "ISO" (isolated power; emphasises XBH)
# - "wRC" (weighted runs created; weighs each outcome and its ability to produce runs)
# - "BABIP" (batting average on balls in play; excludes strikeouts and home runs);
# and sorts by plate appearances, descending
hitters_clean <- hitters_bbref %>%
  filter(PA > 502) %>%
  distinct(Player, .keep_all = TRUE) %>%
  mutate(Handedness = case_when(
    str_sub(Player, -1) == "*" ~ "L",
    str_sub(Player, -1) == "#" ~ "S",
    TRUE ~ "R"
  )) %>%
  mutate(TTOrate = (BB / PA) + (SO / PA) + (HR / PA)) %>%
  mutate(ISO = SLG - BA) %>%
  mutate(wRC = calculate_WRC(H, HR, BB, HBP, IBB, SB, CS, TB, AB)) %>%
  mutate(BABIP = (H - HR) / (AB - SO - HR + SF)) %>%
  arrange(desc(PA))

# Also sort Statcast data by PA, descending, so that we can
# use it in conjunction with BBref data.
hitters_statcast <- hitters_statcast %>% arrange(desc(pa))

#################################
# Variables of interest
#################################

WAR <- hitters_clean$WAR

OBP <- hitters_clean$OBP
PA <- hitters_clean$PA
ISO <- hitters_clean$ISO
Handedness <- as.factor(hitters_clean$Handedness)

#################################
# Model 1
# Linear multi-variable regression model
#################################

model_1 <- lm(WAR ~ OBP + PA + ISO + Handedness)
y_hat_1 <- predict(model_1)
error_1 <- WAR - y_hat_1
summary(model_1)

df_1 <- data.frame(
  x = error_1,
  y = y_hat_1,
  labels = hitters_clean$Player,
  handedness = hitters_clean$Handedness
)

ggplot(df_1, aes(x = x, y = y, color = handedness)) +
  geom_point() +
  scale_color_manual(values = c("L" = "red", "R" = "blue", "S" = "purple")) +
  labs(
    x = "Residual",
    y = "Predicted WAR",
    title = "Model 1",
    subtitle = "OBP, PA, ISO, & Handedness"
  ) +
  scale_y_continuous(
    limits = c(-1, 10),
    breaks = seq(0, 10, 2.5),
    minor_breaks = seq(-1, 10, 0.5)
  ) +
  geom_text(
    aes(label = labels),
    vjust = -0.5,
    hjust = 0.5
  )

#################################
# Model 2
# Quadratic model using OPS
#################################

OPS_sq <- OPS ^ 2

model_2 <- lm(WAR ~ OPS + OPS_sq)
y_hat_2 <- predict(model_2)
error_2 <- WAR - y_hat_2
summary(model_2)

df_2 <- data.frame(
  x = error_2,
  y = y_hat_2,
  labels = hitters_clean$Player
)

ggplot(df_2, aes(x = x, y = y)) +
  geom_point() +
  labs(
    x = "Residual",
    y = "Predicted WAR",
    title = "Model 2",
    subtitle = "Quadratic OPS"
  ) +
  scale_y_continuous(
    limits = c(-1, 10),
    breaks = seq(0, 10, 2.5),
    minor_breaks = seq(-1, 10, 0.5)
  ) +
  geom_text(
    aes(label = labels),
    vjust = -0.5,
    hjust = 0.5
  )

#################################
# Model 3
# wRC-component-based model
#################################

# Model 3 has its own variables of interest, so I'll go ahead and define them
# here, as opposed to next to the other variables I had defined.

H <- hitters_clean$H
HR <- hitters_clean$HR
BB <- hitters_clean$BB
HBP <- hitters_clean$HBP
IBB <- hitters_clean$IBB
SB <- hitters_clean$SB
CS <- hitters_clean$CS
TB <- hitters_clean$TB
AB <- hitters_clean$AB

model_3 <- lm(WAR ~ H + HR + BB + HBP + IBB + SB + CS + TB + AB)
y_hat_3 <- predict(model_3)
error_3 <- WAR - y_hat_3
summary(model_3)

df_3 <- data.frame(
  x = error_3,
  y = y_hat_3,
  labels = hitters_clean$Player,
  EV_avg = hitters_statcast$exit_velocity_avg,
  HH_pct = hitters_statcast$hard_hit_percent,
  barrel_rate = hitters_statcast$barrel_batted_rate,
  TTOrate = hitters_clean$TTOrate
)

stat_to_investigate <- df_3$EV_avg

ggplot(df_3, aes(x = x, y = y, color = stat_to_investigate)) +
  geom_point() +
  scale_color_gradient2(
    midpoint = median(stat_to_investigate),
    low = "blue",
    mid = "#36393e",
    high = "red"
  ) +
  labs(
    x = "Residual",
    y = "Predicted WAR",
    title = "Model 3",
    subtitle = "wRC Components"
  ) +
  scale_y_continuous(
    limits = c(-1, 10),
    breaks = seq(0, 10, 2.5),
    minor_breaks = seq(-1, 10, 0.5)
  ) +
  geom_text(
    aes(label = labels),
    vjust = -0.5,
    hjust = 0.5
  )
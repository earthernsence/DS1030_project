# Prepare and clean data before use

library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)

hitters_bbref <- read.csv("project/data/2025_reg_season_hitters_bbref.csv")
hitters_savant <- read.csv("project/data/2025_reg_season_hitters_savant.csv")
hitters_statcast <- read.csv("project/data/2025_reg_season_hitters_savant_statcast.csv")
hitters_expected <- read.csv("project/data/2025_reg_season_hitters_savant_expected_stats.csv")

calculate_WRC <- function(H, HR, BB, HBP, IBB, SB, CS, TB, AB) {
  ((H - HR) + (0.7 * BB) + (0.7 * HBP) - (0.7 * IBB) + (0.9 * SB) - (0.45 * CS)) * (TB / AB)
}

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
  mutate(TTOrate = BBrate + Krate + HRrate) %>%
  mutate(ISO = (X2B + (2 * X3B) + (3 * HR)) / AB) %>%
  mutate(wRC = calculate_WRC(H, HR, BB, HBP, IBB, SB, CS, TB, AB)) %>%
  mutate(BABIP = (H - HR) / (AB - SO - HR + SF))

#################################
# Linear multi-variable regression model
#################################

model_1 <- lm(hitters_clean$WAR ~ hitters_clean$OPS + hitters_clean$PA + hitters_clean$ISO + hitters_clean$Handedness)
y_hat_1 <- predict(model_1)
error_1 <- hitters_clean$WAR - y_hat_1
plot(error_1, y_hat_1)
summary(model_1)

#################################
# Quadratic model using OPS
#################################

stat_2 <- hitters_clean$OPS
stat_2_sq <- stat_2 ^ 2

model_2 <- lm(hitters_clean$WAR ~ stat_2 + stat_2_sq)
y_hat_2 <- predict(model_2)
error_2 <- hitters_clean$WAR - y_hat_2
plot(error_2, y_hat_2)
summary(model_2)

#################################
# Logarithmic model using BABIP
#################################

model_5 <- lm(hitters_clean$WAR ~ hitters_clean$BABIP + log(hitters_clean$BABIP))
y_hat_5 <- predict(model_5)
error_5 <- hitters_clean$WAR - y_hat_5
plot(error_5, y_hat_5)
summary(model_5)

#################################
# Statcast expected statistics model
#################################

# model_3 <- lm(hitters_clean$WAR ~ hitters_expected$xwobacon + hitters_expected$xiso)
# y_hat_3 <- predict(model_3)
# error_3 <- hitters_clean$WAR - y_hat_3
# plot(error_3, y_hat_3)
# summary(model_3)

#################################
# Statcast batted ball model
#################################

# model_4 <- lm(hitters_clean$WAR ~
#                 hitters_statcast$exit_velocity_avg +
#                   hitters_statcast$launch_angle_avg +
#                   hitters_savant$sweet_spot_percent
# )
# y_hat_4 <- predict(model_4)
# error_4 <- hitters_clean$WAR - y_hat_4
# plot(error_4, y_hat_4)
# summary(model_4)

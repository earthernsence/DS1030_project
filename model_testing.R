# Prepare and clean data before use

library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)

hitters_bbref <- read.csv("project/data/2025_reg_season_hitters_bbref.csv", fileEncoding = "UTF-8")
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

hitters_statcast <- hitters_statcast %>% arrange(desc(pa))

#################################
# Variables of interest
#################################

WAR <- hitters_clean$WAR
OBP <- hitters_clean$OBP
OPS <- hitters_clean$OPS
PA <- hitters_clean$PA
ISO <- hitters_clean$ISO
BABIP <- hitters_clean$BABIP
TTOrate <- hitters_clean$TTOrate
TB <- hitters_clean$TB
RBI <- hitters_clean$RBI
Handedness <- as.factor(hitters_clean$Handedness)

#################################
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
  labs(x = "Residual",
       y = "Predicted WAR",
       title = "Model 1",
       subtitle = "OBP, PA, ISO, & Handedness"
  ) +
  scale_y_continuous(
    limits = c(-1, 10),
    breaks = seq(0, 10, 2.5),
    minor_breaks = seq(-1, 10, 0.5)
  ) +
  geom_text(aes(label = labels),
            vjust = -0.5,
            hjust = 0.5)

#################################
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
  labs(x = "Residual",
       y = "Predicted WAR",
       title = "Model 2",
       subtitle = "Quadratic OPS"
  ) +
  scale_y_continuous(
    limits = c(-1, 10),
    breaks = seq(0, 10, 2.5),
    minor_breaks = seq(-1, 10, 0.5)
  ) +
  geom_text(aes(label = labels),
            vjust = -0.5,
            hjust = 0.5)

#################################
# wRC-component-based model
#################################

H <- hitters_clean$H
HR <- hitters_clean$HR
BB <- hitters_clean$BB
HBP <- hitters_clean$HBP
IBB <- hitters_clean$IBB
SB <- hitters_clean$SB
CS <- hitters_clean$CS
TB <- hitters_clean$TB
AB <- hitters_clean$AB

model_6 <- lm(WAR ~ H + HR + BB + HBP + IBB + SB + CS + TB + AB)
y_hat_6 <- predict(model_6)
error_6 <- WAR - y_hat_6
summary(model_6)

df_3 <- data.frame(
  x = error_6,
  y = y_hat_6,
  labels = hitters_clean$Player,
  EV_avg = hitters_statcast$exit_velocity_avg,
  HH_pct = hitters_statcast$hard_hit_percent,
  barrel_rate = hitters_statcast$barrel_batted_rate,
  TTOrate = hitters_clean$TTOrate
)

ggplot(df_3, aes(x = x, y = y, color = TTOrate)) +
  geom_point() +
  scale_color_gradient2(midpoint = median(df_3$TTOrate), low = "blue", mid = "#36393e", high = "red") +
  labs(x = "Residual",
       y = "Predicted WAR",
       title = "Model 3",
       subtitle = "wRC Components"
  ) +
  scale_y_continuous(
    limits = c(-1, 10),
    breaks = seq(0, 10, 2.5),
    minor_breaks = seq(-1, 10, 0.5)
  ) +
  geom_text(aes(label = labels),
            vjust = -0.5,
            hjust = 0.5)

#################################
# wRC comparison model (for model 3)
#################################

# wRC <- hitters_clean$wRC

# wrc_model <- lm(WAR ~ wRC)
# y_hat_wrc <- predict(wrc_model)
# error_wrc <- WAR - y_hat_wrc
# summary(wrc_model)
#
# df_wrc <- data.frame(
#   x = error_wrc,
#   y = y_hat_wrc,
#   labels = hitters_clean$Player
# )
#
# ggplot(df_wrc, aes(x = x, y = y)) +
#   geom_point() +
#   labs(x = "Residual",
#        y = "Predicted WAR",
#        title = "Model 3: Comparison",
#        subtitle = "wRC-based model"
#   ) +
#   scale_y_continuous(
#     limits = c(-1, 10),
#     breaks = seq(0, 10, 2.5),
#     minor_breaks = seq(0, 10, 0.5)
#   ) +
#   geom_text(aes(label = labels),
#             vjust = -0.5,
#             hjust = 0.5)

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
#                   hitters_savant$sweet_spot_percent +
#                   hitters_statcast$avg_swing_speed
# )
# y_hat_4 <- predict(model_4)
# error_4 <- hitters_clean$WAR - y_hat_4
# plot(error_4, y_hat_4)
# summary(model_4)

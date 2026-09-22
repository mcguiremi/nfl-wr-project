# =============================================================================
# NFL Wide Receiver Combine Analysis
#
# Question: Do NFL Scouting Combine metrics predict a wide receiver's career
# success (measured as Approximate Value per season)?
#
# Data:
#   - data/sportsref_download.xlsx  : career stats for WRs drafted 2000+
#                                      (Pro Football Reference / Stathead)
#   - data/NFL_Combine_Since_2000.csv: combine testing results, 2000-2025
#                                      (Kaggle: isaaksipple/nfl-combine-2000-2025)
# =============================================================================

library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)

# ---- 1. Load data -----------------------------------------------------------

career_data <- read_excel("data/sportsref_download.xlsx")
combine_data <- read_csv("data/NFL_Combine_Since_2000.csv")

# ---- 2. Filter to wide receivers --------------------------------------------

combine_wr <- combine_data %>%
  filter(Position == "WR")

career_wr <- career_data %>%
  filter(Pos == "WR", From >= 2000)

# ---- 3. Standardize player names for joining --------------------------------
# Player names differ slightly between sources (periods, suffixes like Jr./II/III),
# so normalize both sides before joining on name + draft year.

clean_names <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\.", "") %>%
    str_replace_all(" jr", "") %>%
    str_replace_all(" iii", "") %>%
    str_replace_all(" ii", "") %>%
    str_squish()
}

career_wr$Player <- clean_names(career_wr$Player)
combine_wr$Player <- clean_names(combine_wr$Player)

# ---- 4. Merge career outcomes with combine testing data ---------------------

merged_data <- inner_join(
  career_wr, combine_wr,
  by = c("Player", "From" = "Year")
) %>%
  distinct() %>%
  rename(
    Career_AV      = `AV...3`,
    Forty          = `40-yd Dash`,
    Vertical_Jump  = `Vertical Jump`,
    Bench_Press    = `Bench Press`,
    Broad_Jump     = `Broad Jump`,
    Cone_Drill     = `3-Cone Drill`,
    Shuttle        = `20-yd Shuttle`,
    Draft_Team     = Team.y
  ) %>%
  select(-Team.x, -Rk, -Position, -AV...9) %>%
  mutate(
    Seasons       = To - From + 1,
    AV_per_season = Career_AV / Seasons
  )

combine_metrics <- c("Forty", "Vertical_Jump", "Bench_Press",
                      "Broad_Jump", "Cone_Drill", "Shuttle")

merged_data[combine_metrics] <- lapply(merged_data[combine_metrics], as.numeric)

cat("Merged dataset:", nrow(merged_data), "wide receivers\n")

# ---- 5. Visualize each combine metric vs. career performance ----------------

combine_untimed_metrics <- c("Vertical_Jump", "Bench_Press", "Broad_Jump")
combine_timed_metrics   <- c("Forty", "Cone_Drill", "Shuttle")

dir.create("plots", showWarnings = FALSE)

for (metric in combine_untimed_metrics) {
  p <- ggplot(merged_data, aes(x = .data[[metric]], y = AV_per_season)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = paste(metric, "vs AV Per Season"), x = metric, y = "AV Per Season")
  ggsave(filename = paste0("plots/", metric, "_vs_AV.png"), plot = p, width = 8, height = 6)
}

for (metric in combine_timed_metrics) {
  # Lower (faster) times are better, so the x-axis is reversed for readability
  p <- ggplot(merged_data, aes(x = .data[[metric]], y = AV_per_season)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_x_reverse() +
    labs(title = paste(metric, "vs AV Per Season"), x = metric, y = "AV Per Season")
  ggsave(filename = paste0("plots/", metric, "_vs_AV.png"), plot = p, width = 8, height = 6)
}

# ---- 6. Regression models ----------------------------------------------------

# Full model: all six combine metrics together
full_model <- lm(
  AV_per_season ~ Forty + Vertical_Jump + Bench_Press + Broad_Jump + Cone_Drill + Shuttle,
  data = merged_data
)
cat("\n===== Full model (all combine metrics) =====\n")
print(summary(full_model))

# Speed-only model: the three timed drills
speed_model <- lm(
  AV_per_season ~ Forty + Shuttle + Cone_Drill,
  data = merged_data
)
cat("\n===== Speed model (40, shuttle, cone) =====\n")
print(summary(speed_model))

# Explosion-only model: vertical + broad jump
explosion_model <- lm(
  AV_per_season ~ Vertical_Jump + Broad_Jump,
  data = merged_data
)
cat("\n===== Explosion model (vertical + broad jump) =====\n")
print(summary(explosion_model))

# ---- 7. Combined explosion score ---------------------------------------------

merged_data <- merged_data %>%
  mutate(Explosion_Score = Vertical_Jump + Broad_Jump)

explosion_plot <- ggplot(merged_data, aes(x = Explosion_Score, y = AV_per_season)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(
    title = "Explosion Score (Vertical + Broad Jump) vs AV Per Season",
    x = "Explosion Score",
    y = "AV Per Season"
  )
ggsave("plots/Explosion_Score_vs_AV.png", plot = explosion_plot, width = 8, height = 6)

cat("\nDone. Plots saved to plots/\n")

# ============================================
# Trajectory Analysis (using best parameters)
# ============================================

library(tidyverse)

# ============================================
# 1. Read data
# ============================================

output_dir <- "results/agent_comparison"
all_results <- read_csv(file.path(output_dir, "agent_results_raw.csv"), show_col_types = FALSE)

# Filter Lc only
all_results_lc <- all_results %>%
  filter(type == "Lc") %>%
  mutate(
    RT_total = RT_1 + RT_2 + RT_3 + RT_4
  )

# ============================================
# 2. Get best parameters for each strategy
# ============================================

best_params <- all_results_lc %>%
  filter(strategy != "baseline") %>%
  group_by(strategy) %>%
  summarise(
    best_thres_schema = thres_schema[which.max(performance)],
    best_thres_item_final = thres_item_final[which.max(performance)],
    best_performance = max(performance, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Best parameters ===\n")
print(best_params)

# ============================================
# 3. Filter data to only best parameter combinations
# ============================================

best_data <- all_results_lc %>%
  inner_join(best_params, by = "strategy") %>%
  filter(
    thres_schema == best_thres_schema,
    thres_item_final == best_thres_item_final
  ) %>%
  select(-best_thres_schema, -best_thres_item_final, -best_performance)

cat(sprintf("\nBest data rows: %d\n", nrow(best_data)))

# Check how many rounds per strategy
best_data %>%
  group_by(strategy) %>%
  summarise(
    n_rounds = n_distinct(Round),
    min_round = min(Round),
    max_round = max(Round),
    .groups = "drop"
  ) %>%
  print()

# ============================================
# 4. Trajectory by round (using best parameters)
# ============================================

# Performance trajectory
perf_trajectory <- best_data %>%
  group_by(strategy, Round) %>%
  summarise(
    mean_performance = mean(performance, na.rm = TRUE),
    se_performance = sd(performance, na.rm = TRUE) / sqrt(n()),
    n_trials = n(),
    .groups = "drop"
  )

p_perf_traj <- ggplot(perf_trajectory, aes(x = Round, y = mean_performance, color = strategy)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_performance - se_performance,
                  ymax = mean_performance + se_performance,
                  fill = strategy), alpha = 0.2, color = NA) +
  labs(x = "Round", y = "Mean performance", 
       title = "Performance trajectory (best parameters)",
       subtitle = paste0("Bonus-oriented: schema=", best_params$best_thres_schema[1], 
                         ", final=", best_params$best_thres_item_final[1],
                         " | Payoff-oriented: schema=", best_params$best_thres_schema[2], 
                         ", final=", best_params$best_thres_item_final[2]),
       color = "Strategy", fill = "Strategy") +
  theme_bw(base_size = 12) +
  theme(legend.position = "top")

# AC trajectory
ac_trajectory <- best_data %>%
  group_by(strategy, Round) %>%
  summarise(
    mean_AC = mean(AC, na.rm = TRUE),
    se_AC = sd(AC, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p_ac_traj <- ggplot(ac_trajectory, aes(x = Round, y = mean_AC, color = strategy)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_AC - se_AC,
                  ymax = mean_AC + se_AC,
                  fill = strategy), alpha = 0.2, color = NA) +
  labs(x = "Round", y = "Mean accuracy", 
       title = "Accuracy trajectory (best parameters)",
       color = "Strategy", fill = "Strategy") +
  theme_bw(base_size = 12) +
  theme(legend.position = "top")

# RT trajectory
rt_trajectory <- best_data %>%
  group_by(strategy, Round) %>%
  summarise(
    mean_RT = mean(RT_total, na.rm = TRUE),
    se_RT = sd(RT_total, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p_rt_traj <- ggplot(rt_trajectory, aes(x = Round, y = mean_RT, color = strategy)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_RT - se_RT,
                  ymax = mean_RT + se_RT,
                  fill = strategy), alpha = 0.2, color = NA) +
  labs(x = "Round", y = "Mean reaction time (s)", 
       title = "Reaction time trajectory (best parameters)",
       color = "Strategy", fill = "Strategy") +
  theme_bw(base_size = 12) +
  theme(legend.position = "top")

# ============================================
# 5. Statistical test: Strategy × Round interaction
# ============================================

# Check if the gap changes over time
library(lme4)
library(lmerTest)

mixed_model_perf <- lmer(performance ~ Round * strategy + (1 | rep), data = best_data)
mixed_model_ac <- lmer(AC ~ Round * strategy + (1 | rep), data = best_data)
mixed_model_rt <- lmer(RT_total ~ Round * strategy + (1 | rep), data = best_data)

cat("\n=== Mixed model: Performance ===\n")
print(summary(mixed_model_perf))

cat("\n=== Mixed model: AC ===\n")
print(summary(mixed_model_ac))

cat("\n=== Mixed model: RT ===\n")
print(summary(mixed_model_rt))

# Extract p-value for interaction
perf_interaction_p <- coef(summary(mixed_model_perf))["Round:strategypayoff_oriented", "Pr(>|t|)"]
ac_interaction_p <- coef(summary(mixed_model_ac))["Round:strategypayoff_oriented", "Pr(>|t|)"]
rt_interaction_p <- coef(summary(mixed_model_rt))["Round:strategypayoff_oriented", "Pr(>|t|)"]

cat("\n=== Interaction p-values ===\n")
cat(sprintf("Performance Round×Strategy: p = %.4f\n", perf_interaction_p))
cat(sprintf("AC Round×Strategy: p = %.4f\n", ac_interaction_p))
cat(sprintf("RT Round×Strategy: p = %.4f\n", rt_interaction_p))

# ============================================
# 6. Save outputs
# ============================================

# Create figures directory
fig_dir <- file.path(output_dir, "trajectory_figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Save plots
ggsave(file.path(fig_dir, "performance_trajectory_best.png"), p_perf_traj, width = 8, height = 5)
ggsave(file.path(fig_dir, "ac_trajectory_best.png"), p_ac_traj, width = 8, height = 5)
ggsave(file.path(fig_dir, "rt_trajectory_best.png"), p_rt_traj, width = 8, height = 5)

# Save trajectory data
write_csv(perf_trajectory, file.path(fig_dir, "performance_trajectory_data.csv"))
write_csv(ac_trajectory, file.path(fig_dir, "ac_trajectory_data.csv"))
write_csv(rt_trajectory, file.path(fig_dir, "rt_trajectory_data.csv"))

# Save mixed model results
sink(file.path(fig_dir, "mixed_model_results.txt"))

cat("=== Mixed Model: Performance ~ Round * Strategy ===\n")
print(summary(mixed_model_perf))

cat("\n=== Mixed Model: AC ~ Round * Strategy ===\n")
print(summary(mixed_model_ac))

cat("\n=== Mixed Model: RT ~ Round * Strategy ===\n")
print(summary(mixed_model_rt))

cat("\n=== Interaction p-values ===\n")
cat(sprintf("Performance: p = %.4f\n", perf_interaction_p))
cat(sprintf("AC: p = %.4f\n", ac_interaction_p))
cat(sprintf("RT: p = %.4f\n", rt_interaction_p))

sink()

cat("\n=== All done! Trajectory analysis saved to:", fig_dir, "===\n")
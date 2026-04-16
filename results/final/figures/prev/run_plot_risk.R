# ============================================
# Complete Visualization Script
# ============================================

library(tidyverse)
library(ggpubr)
library(patchwork)

# ============================================
# 1. Read data
# ============================================

# Set your results directory
results_dir <- "results/final"

# Read the three levels of data
all_results <- read_csv(file.path(results_dir, "all_results.csv"), show_col_types = FALSE)
run_summary <- read_csv(file.path(results_dir, "run_summary.csv"), show_col_types = FALSE)
final_summary <- read_csv(file.path(results_dir, "final_summary.csv"), show_col_types = FALSE)

cat("Data loaded successfully!\n")
cat(sprintf("  all_results: %d rows\n", nrow(all_results)))
cat(sprintf("  run_summary: %d rows\n", nrow(run_summary)))
cat(sprintf("  final_summary: %d rows\n", nrow(final_summary)))

# ============================================
# 2. Define color palette
# ============================================

agent_colors <- c(
  "baseline" = "#6B7280",   # grey
  "bonus" = "#3B82F6",      # blue
  "payoff" = "#EF4444"      # red
)

risk_colors <- c(
  "Lc" = "#10B981",         # green
  "Hc" = "#F59E0B"          # orange
)

# ============================================
# 3. Bar plot: Performance by agent and risk
# ============================================

p_perf_bar <- final_summary %>%
  ggplot(aes(x = risk, y = perf_mean, fill = agent)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = perf_mean - perf_se, ymax = perf_mean + perf_se),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.3) +
  scale_fill_manual(values = agent_colors, name = "Agent") +
  labs(x = "Risk condition", y = "Mean performance", 
       title = "Performance by agent and risk condition") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")

# ============================================
# 4. Bar plot: AC by agent and risk
# ============================================

p_ac_bar <- final_summary %>%
  ggplot(aes(x = risk, y = AC_mean, fill = agent)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = AC_mean - AC_se, ymax = AC_mean + AC_se),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.3) +
  scale_fill_manual(values = agent_colors, name = "Agent") +
  labs(x = "Risk condition", y = "Mean accuracy", 
       title = "Accuracy by agent and risk condition") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")

# ============================================
# 5. Bar plot: RT by agent and risk
# ============================================

p_rt_bar <- final_summary %>%
  ggplot(aes(x = risk, y = RT_mean, fill = agent)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = RT_mean - RT_se, ymax = RT_mean + RT_se),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.3) +
  scale_fill_manual(values = agent_colors, name = "Agent") +
  labs(x = "Risk condition", y = "Mean reaction time (s)", 
       title = "Reaction time by agent and risk condition") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")

# ============================================
# 6. Line plot: Performance trajectory over rounds
# ============================================

# Calculate trajectory
trajectory <- all_results %>%
  mutate(total_RT = RT_1 + RT_2 + RT_3 + RT_4) %>%
  group_by(agent, risk, Round) %>%
  summarise(
    mean_performance = mean(performance, na.rm = TRUE),
    se_performance = sd(performance, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p_trajectory <- trajectory %>%
  ggplot(aes(x = Round, y = mean_performance, color = agent, fill = agent)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = mean_performance - se_performance,
                  ymax = mean_performance + se_performance),
              alpha = 0.2, color = NA) +
  facet_wrap(~ risk, ncol = 2) +
  scale_color_manual(values = agent_colors, name = "Agent") +
  scale_fill_manual(values = agent_colors, name = "Agent") +
  labs(x = "Round", y = "Mean performance", 
       title = "Performance trajectory over rounds") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")

# ============================================
# 7. Box plot: Distribution of performance across runs
# ============================================

p_box <- run_summary %>%
  ggplot(aes(x = agent, y = performance, fill = risk)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = risk_colors, name = "Risk") +
  labs(x = "Agent", y = "Performance per run", 
       title = "Distribution of performance across runs") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")

# ============================================
# 8. Scatter plot: AC vs RT (speed-accuracy tradeoff)
# ============================================

p_scatter <- run_summary %>%
  ggplot(aes(x = RT, y = AC, color = agent, shape = risk)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = agent_colors, name = "Agent") +
  labs(x = "Reaction time (s)", y = "Accuracy", 
       title = "Speed-accuracy tradeoff by agent and risk",
       shape = "Risk") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")

# ============================================
# 9. Heatmap: Performance across parameters (if available)
# ============================================

# This requires param_summary from your previous analysis
# If you have param_summary, uncomment:

# param_summary <- read_csv("results/agent_comparison/param_summary_Lc.csv", show_col_types = FALSE)
# 
# p_heatmap <- param_summary %>%
#   filter(strategy != "baseline") %>%
#   ggplot(aes(x = thres_schema, y = thres_item_final, fill = mean_performance)) +
#   geom_tile() +
#   facet_wrap(~ strategy) +
#   scale_fill_gradient(low = "white", high = "darkblue") +
#   labs(x = "Schema threshold", y = "Item threshold", 
#        title = "Performance landscape") +
#   theme_bw(base_size = 11)

# ============================================
# 10. Combined figure (for paper)
# ============================================

# Combine bar plots into one figure
combined_bars <- (p_perf_bar + p_ac_bar) / p_rt_bar +
  plot_annotation(
    title = "Agent comparison across risk conditions",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
  )

# ============================================
# 11. Statistical annotations
# ============================================

# Add significance stars to bar plots
# You can run t-tests and add annotations manually

# Example: t-test between bonus and payoff in Lc
bonus_vs_payoff_Lc <- run_summary %>%
  filter(agent %in% c("bonus", "payoff"), risk == "Lc")

t_test_result <- t.test(performance ~ agent, data = bonus_vs_payoff_Lc)
p_val <- t_test_result$p.value

# Add significance annotation to p_perf_bar
p_perf_bar_sig <- p_perf_bar +
  annotate("text", x = 1.5, y = max(final_summary$perf_mean) + 5,
           label = sprintf("p = %.3f", p_val), size = 3)

# ============================================
# 12. Save all plots
# ============================================

# Create figures directory
fig_dir <- file.path(results_dir, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Save individual plots
ggsave(file.path(fig_dir, "01_performance_bar.png"), p_perf_bar, width = 5, height = 4)
ggsave(file.path(fig_dir, "02_AC_bar.png"), p_ac_bar, width = 5, height = 4)
ggsave(file.path(fig_dir, "03_RT_bar.png"), p_rt_bar, width = 5, height = 4)
ggsave(file.path(fig_dir, "04_trajectory.png"), p_trajectory, width = 8, height = 4)
ggsave(file.path(fig_dir, "05_boxplot.png"), p_box, width = 6, height = 5)
ggsave(file.path(fig_dir, "06_scatter.png"), p_scatter, width = 6, height = 5)
ggsave(file.path(fig_dir, "07_combined_bars.png"), combined_bars, width = 10, height = 8)

# Save R data
saveRDS(list(
  final_summary = final_summary,
  run_summary = run_summary,
  trajectory = trajectory,
  p_perf_bar = p_perf_bar,
  p_ac_bar = p_ac_bar,
  p_rt_bar = p_rt_bar,
  p_trajectory = p_trajectory,
  p_box = p_box,
  p_scatter = p_scatter
), file = file.path(fig_dir, "plots_data.rds"))

cat("\n✅ All plots saved to:", fig_dir, "\n")

# ============================================
# 13. Display plots (if running interactively)
# ============================================

if (interactive()) {
  print(p_perf_bar)
  print(p_ac_bar)
  print(p_rt_bar)
  print(p_trajectory)
  print(p_box)
  print(p_scatter)
}
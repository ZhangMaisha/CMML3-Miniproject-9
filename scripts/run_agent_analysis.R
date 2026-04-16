# ============================================
# Agent Comparison Analysis (Lc only)
# ============================================

library(tidyverse)
library(ggpubr)
library(patchwork)

# ============================================
# 1. Read the pre-computed results
# ============================================

output_dir <- "results/agent_comparison"
all_results <- read_csv(file.path(output_dir, "agent_results_raw.csv"), show_col_types = FALSE)

cat(sprintf("Loaded %d rows of data\n", nrow(all_results)))

# ============================================
# 2. Filter Lc only
# ============================================

all_results_lc <- all_results %>%
  filter(type == "Lc") %>%
  mutate(
    RT_total = RT_1 + RT_2 + RT_3 + RT_4
  )

cat(sprintf("Lc rows retained: %d\n", nrow(all_results_lc)))

# ============================================
# 3. Rep-level summary (for boxplots)
# ============================================

rep_summary <- all_results_lc %>%
  group_by(strategy, thres_schema, thres_item_final, rep) %>%
  summarise(
    mean_AC = mean(AC, na.rm = TRUE),
    mean_RT = mean(RT_total, na.rm = TRUE),
    mean_performance = mean(performance, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================
# 4. Parameter-combination-level summary
# ============================================

param_summary <- rep_summary %>%
  group_by(strategy, thres_schema, thres_item_final) %>%
  summarise(
    n_reps = n(),
    mean_AC = mean(mean_AC, na.rm = TRUE),
    se_AC = sd(mean_AC, na.rm = TRUE) / sqrt(n()),
    mean_RT = mean(mean_RT, na.rm = TRUE),
    se_RT = sd(mean_RT, na.rm = TRUE) / sqrt(n()),
    mean_performance = mean(mean_performance, na.rm = TRUE),
    se_performance = sd(mean_performance, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# ============================================
# 5. Get baseline values
# ============================================

baseline_performance <- param_summary %>%
  filter(strategy == "baseline") %>%
  pull(mean_performance) %>%
  mean()

baseline_AC <- param_summary %>%
  filter(strategy == "baseline") %>%
  pull(mean_AC) %>%
  mean()

baseline_RT <- param_summary %>%
  filter(strategy == "baseline") %>%
  pull(mean_RT) %>%
  mean()

# ============================================
# 6. Statistical tests
# ============================================

test_data_perf <- param_summary %>%
  filter(strategy %in% c("bonus_oriented", "payoff_oriented"))

t_test_perf <- t.test(mean_performance ~ strategy, data = test_data_perf)
p_perf <- t_test_perf$p.value
p_label_perf <- ifelse(p_perf < 0.001, "p < 0.001", 
                       ifelse(p_perf < 0.01, "p < 0.01",
                              ifelse(p_perf < 0.05, "p < 0.05",
                                     sprintf("p = %.3f", p_perf))))

# AC t-test
t_test_ac <- t.test(mean_AC ~ strategy, data = test_data_perf)
p_ac <- t_test_ac$p.value
p_label_ac <- ifelse(p_ac < 0.001, "p < 0.001", 
                     ifelse(p_ac < 0.01, "p < 0.01",
                            ifelse(p_ac < 0.05, "p < 0.05",
                                   sprintf("p = %.3f", p_ac))))

# RT t-test
t_test_rt <- t.test(mean_RT ~ strategy, data = test_data_perf)
p_rt <- t_test_rt$p.value
p_label_rt <- ifelse(p_rt < 0.001, "p < 0.001", 
                     ifelse(p_rt < 0.01, "p < 0.01",
                            ifelse(p_rt < 0.05, "p < 0.05",
                                   sprintf("p = %.3f", p_rt))))

# ============================================
# 7. Boxplot: Performance by agent
# ============================================

boxplot_data_perf <- param_summary %>%
  filter(strategy != "baseline") %>%
  select(strategy, mean_performance)

agent_colors <- c("bonus_oriented" = "#3B82F6", "payoff_oriented" = "#EF4444")

# Get y-axis limits for annotation positioning
y_max_perf <- max(boxplot_data_perf$mean_performance, baseline_performance)
y_min_perf <- min(boxplot_data_perf$mean_performance, baseline_performance)
y_range_perf <- y_max_perf - y_min_perf

p_perf_box <- ggplot(boxplot_data_perf, aes(x = strategy, y = mean_performance, fill = strategy)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1.5, width = 0.6) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.5, color = "black") +
  geom_hline(yintercept = baseline_performance, linetype = "dashed", 
             color = "#6B7280", linewidth = 0.8) +
  annotate("text", x = 1.5, y = baseline_performance - y_range_perf * 0.08, 
           label = paste("Baseline:", round(baseline_performance, 1)), 
           size = 3, color = "#6B7280") +
  annotate("text", x = 1.5, y = y_max_perf + y_range_perf * 0.05, 
           label = p_label_perf, size = 4, fontface = "bold") +
  scale_fill_manual(values = agent_colors) +
  labs(x = "Strategy", y = "Mean performance (across parameter combinations)",
       title = "Performance comparison: Bonus-oriented vs Payoff-oriented",
       subtitle = paste("Lc condition only | Each point = one parameter combination")) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "#6B7280", size = 10)
  )

# ============================================
# 8. Boxplot: AC by agent
# ============================================

boxplot_data_ac <- param_summary %>%
  filter(strategy != "baseline") %>%
  select(strategy, mean_AC)

y_max_ac <- max(boxplot_data_ac$mean_AC, baseline_AC)
y_min_ac <- min(boxplot_data_ac$mean_AC, baseline_AC)
y_range_ac <- y_max_ac - y_min_ac

p_ac_box <- ggplot(boxplot_data_ac, aes(x = strategy, y = mean_AC, fill = strategy)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1.5, width = 0.6) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.5, color = "black") +
  geom_hline(yintercept = baseline_AC, linetype = "dashed", 
             color = "#6B7280", linewidth = 0.8) +
  annotate("text", x = 1.5, y = baseline_AC - y_range_ac * 0.08, 
           label = paste("Baseline:", round(baseline_AC, 3)), 
           size = 3, color = "#6B7280") +
  annotate("text", x = 1.5, y = y_max_ac + y_range_ac * 0.05, 
           label = p_label_ac, size = 4, fontface = "bold") +
  scale_fill_manual(values = agent_colors) +
  labs(x = "Strategy", y = "Mean accuracy (AC)",
       title = "Accuracy comparison: Bonus-oriented vs Payoff-oriented",
       subtitle = "Lc condition only | Each point = one parameter combination") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "#6B7280", size = 10)
  )

# ============================================
# 9. Boxplot: RT by agent
# ============================================

boxplot_data_rt <- param_summary %>%
  filter(strategy != "baseline") %>%
  select(strategy, mean_RT)

y_max_rt <- max(boxplot_data_rt$mean_RT, baseline_RT)
y_min_rt <- min(boxplot_data_rt$mean_RT, baseline_RT)
y_range_rt <- y_max_rt - y_min_rt

p_rt_box <- ggplot(boxplot_data_rt, aes(x = strategy, y = mean_RT, fill = strategy)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1.5, width = 0.6) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.5, color = "black") +
  geom_hline(yintercept = baseline_RT, linetype = "dashed", 
             color = "#6B7280", linewidth = 0.8) +
  annotate("text", x = 1.5, y = baseline_RT + y_range_rt * 0.08, 
           label = paste("Baseline:", round(baseline_RT, 1)), 
           size = 3, color = "#6B7280") +
  annotate("text", x = 1.5, y = y_max_rt + y_range_rt * 0.05, 
           label = p_label_rt, size = 4, fontface = "bold") +
  scale_fill_manual(values = agent_colors) +
  labs(x = "Strategy", y = "Mean reaction time (s)",
       title = "Reaction time comparison: Bonus-oriented vs Payoff-oriented",
       subtitle = "Lc condition only | Each point = one parameter combination") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "#6B7280", size = 10)
  )

# ============================================
# 10. Combined figure
# ============================================

combined_plot <- (p_perf_box | p_ac_box | p_rt_box) +
  plot_annotation(
    title = "Agent comparison under low-risk condition",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
  )

# ============================================
# 11. Save outputs
# ============================================

# Create figures directory
fig_dir <- file.path(output_dir, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Save individual plots
ggsave(file.path(fig_dir, "performance_boxplot_Lc.png"), p_perf_box, width = 5, height = 5)
ggsave(file.path(fig_dir, "ac_boxplot_Lc.png"), p_ac_box, width = 5, height = 5)
ggsave(file.path(fig_dir, "rt_boxplot_Lc.png"), p_rt_box, width = 5, height = 5)

# Save combined figure
ggsave(file.path(fig_dir, "combined_agent_comparison_Lc.png"), combined_plot, width = 15, height = 5)

# Save statistical results
sink(file.path(output_dir, "statistical_tests_Lc.txt"))

cat("=== T-test: Bonus-oriented vs Payoff-oriented (parameter level, Lc only) ===\n")

cat("\nPerformance:\n")
print(t_test_perf)
cat("\nBaseline performance:", round(baseline_performance, 2), "\n")

cat("\nAC:\n")
print(t_test_ac)
cat("\nBaseline AC:", round(baseline_AC, 3), "\n")

cat("\nRT:\n")
print(t_test_rt)
cat("\nBaseline RT:", round(baseline_RT, 1), "\n")

sink()

cat("\n=== All done! Results saved to:", output_dir, "===\n")
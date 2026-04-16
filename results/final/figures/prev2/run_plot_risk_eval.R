library(tidyverse)
library(patchwork)

results_dir <- "results/final"
fig_dir <- file.path(results_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

run_summary <- read_csv(file.path(results_dir, "run_summary.csv"), show_col_types = FALSE)
all_results <- read_csv(file.path(results_dir, "all_results.csv"), show_col_types = FALSE)

agent_levels <- c("baseline", "bonus", "payoff")
risk_levels <- c("Lc", "Hc")
model_levels <- c("original", "adapted")

run_summary <- run_summary %>%
  mutate(
    agent = factor(agent, levels = agent_levels),
    risk = factor(risk, levels = risk_levels),
    model = factor(model, levels = model_levels)
  )

all_results <- all_results %>%
  mutate(
    agent = factor(agent, levels = agent_levels),
    risk = factor(risk, levels = risk_levels),
    model = factor(model, levels = model_levels),
    total_RT = RT_1 + RT_2 + RT_3 + RT_4
  )

agent_colors <- c(
  "baseline" = "#6B7280",
  "bonus" = "#2563EB",
  "payoff" = "#DC2626"
)

model_colors <- c(
  "original" = "#111827",
  "adapted" = "#059669"
)

# 1) Overall comparison (Performance / AC / RT)
overall <- run_summary %>%
  group_by(model, agent, risk) %>%
  summarise(
    AC_mean = mean(AC, na.rm = TRUE),
    AC_se = sd(AC, na.rm = TRUE) / sqrt(n()),
    perf_mean = mean(performance, na.rm = TRUE),
    perf_se = sd(performance, na.rm = TRUE) / sqrt(n()),
    RT_mean = mean(RT, na.rm = TRUE),
    RT_se = sd(RT, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p_perf <- ggplot(overall, aes(x = model, y = perf_mean, fill = agent)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = perf_mean - perf_se, ymax = perf_mean + perf_se),
    position = position_dodge(0.75), width = 0.2, linewidth = 0.3
  ) +
  facet_wrap(~ risk) +
  scale_fill_manual(values = agent_colors) +
  labs(title = "Performance", x = "Model", y = "Mean performance") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")

p_ac <- ggplot(overall, aes(x = model, y = AC_mean, fill = agent)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = AC_mean - AC_se, ymax = AC_mean + AC_se),
    position = position_dodge(0.75), width = 0.2, linewidth = 0.3
  ) +
  facet_wrap(~ risk) +
  scale_fill_manual(values = agent_colors) +
  labs(title = "Accuracy (AC)", x = "Model", y = "Mean AC") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")

p_rt <- ggplot(overall, aes(x = model, y = RT_mean, fill = agent)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = RT_mean - RT_se, ymax = RT_mean + RT_se),
    position = position_dodge(0.75), width = 0.2, linewidth = 0.3
  ) +
  facet_wrap(~ risk) +
  scale_fill_manual(values = agent_colors, name = "Agent") +
  labs(title = "Reaction Time", x = "Model", y = "Mean RT") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

fig_overall <- (p_perf / p_ac / p_rt) +
  plot_annotation(title = "Overall Model Comparison by Risk and Agent")

ggsave(
  file.path(fig_dir, "11_overall_model_comparison.png"),
  fig_overall, width = 10, height = 12, dpi = 300
)

# 2) Delta plot (adapted - original), paired by run
delta <- run_summary %>%
  select(model, agent, risk, run, AC, performance, RT) %>%
  pivot_wider(names_from = model, values_from = c(AC, performance, RT)) %>%
  mutate(
    delta_AC = AC_adapted - AC_original,
    delta_performance = performance_adapted - performance_original,
    delta_RT = RT_adapted - RT_original
  ) %>%
  select(agent, risk, run, delta_AC, delta_performance, delta_RT) %>%
  pivot_longer(
    cols = starts_with("delta_"),
    names_to = "metric",
    values_to = "delta"
  ) %>%
  mutate(metric = recode(
    metric,
    delta_AC = "Delta AC",
    delta_performance = "Delta Performance",
    delta_RT = "Delta RT"
  ))

delta_summary <- delta %>%
  group_by(agent, risk, metric) %>%
  summarise(
    mean_delta = mean(delta, na.rm = TRUE),
    se_delta = sd(delta, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

fig_delta <- ggplot(delta_summary, aes(x = agent, y = mean_delta, fill = agent)) +
  geom_col(width = 0.65) +
  geom_errorbar(
    aes(ymin = mean_delta - se_delta, ymax = mean_delta + se_delta),
    width = 0.2, linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.3) +
  facet_grid(metric ~ risk, scales = "free_y") +
  scale_fill_manual(values = agent_colors) +
  labs(
    title = "Adaptation Effect: adapted - original",
    x = "Agent",
    y = "Mean delta"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")

ggsave(file.path(fig_dir, "12_adaptation_delta.png"), fig_delta, width = 10, height = 8, dpi = 300)

# 3) Run-level distribution
fig_box <- ggplot(run_summary, aes(x = agent, y = performance, fill = model)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.7, position = position_dodge(width = 0.75)) +
  facet_wrap(~ risk) +
  scale_fill_manual(values = model_colors) +
  labs(
    title = "Run-level Performance Distribution",
    x = "Agent",
    y = "Performance"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "13_run_level_performance_box.png"), fig_box, width = 9, height = 5, dpi = 300)

# 4) Speed-performance tradeoff
fig_tradeoff <- ggplot(run_summary, aes(x = RT, y = performance, color = model, shape = risk)) +
  geom_point(alpha = 0.75, size = 2) +
  facet_wrap(~ agent) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Speed-Performance Tradeoff",
    x = "Mean RT per run",
    y = "Mean performance per run"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "14_speed_performance_tradeoff.png"), fig_tradeoff, width = 10, height = 4, dpi = 300)

# 5) Round trajectory by model
trajectory <- all_results %>%
  group_by(model, agent, risk, Round) %>%
  summarise(
    mean_performance = mean(performance, na.rm = TRUE),
    se_performance = sd(performance, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

fig_traj <- ggplot(trajectory, aes(x = Round, y = mean_performance, color = model, fill = model)) +
  geom_line(linewidth = 0.9) +
  geom_ribbon(
    aes(ymin = mean_performance - se_performance, ymax = mean_performance + se_performance),
    alpha = 0.18, color = NA
  ) +
  facet_grid(risk ~ agent) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  labs(
    title = "Performance Trajectory Across Rounds",
    x = "Round",
    y = "Mean performance"
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "15_performance_trajectory_model.png"), fig_traj, width = 12, height = 6, dpi = 300)

saveRDS(
  list(
    overall = overall,
    delta = delta,
    delta_summary = delta_summary,
    trajectory = trajectory
  ),
  file = file.path(fig_dir, "risk_eval_data.rds")
)

cat("Saved evaluation figures to:", fig_dir, "\n")

library(tidyverse)
library(patchwork)

results_dir <- "../results/final_v2_bestpair_rep50"
fig_dir <- file.path(results_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

all_results <- read_csv(file.path(results_dir, "all_results.csv"), show_col_types = FALSE)

run_summary <- all_results %>%
  mutate(total_RT = RT_1 + RT_2 + RT_3 + RT_4) %>%
  group_by(model, strategy, combo_id, thres_schema, thres_item_final, risk, run) %>%
  summarise(
    AC = mean(AC, na.rm = TRUE),
    performance = mean(performance, na.rm = TRUE),
    RT = mean(total_RT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    model = factor(model, levels = c("original", "adapted")),
    risk = factor(risk, levels = c("Lc", "Hc")),
    strategy = factor(strategy, levels = c("bonus_oriented", "payoff_oriented"))
  )

write_csv(run_summary, file.path(results_dir, "run_summary.csv"))
write_csv(run_summary, file.path(fig_dir, "bestpair_selected_run_summary.csv"))

bestpair_summary <- run_summary %>%
  group_by(model, strategy, risk) %>%
  summarise(
    n = n(),
    perf_mean = mean(performance, na.rm = TRUE),
    perf_se = sd(performance, na.rm = TRUE) / sqrt(n()),
    AC_mean = mean(AC, na.rm = TRUE),
    AC_se = sd(AC, na.rm = TRUE) / sqrt(n()),
    RT_mean = mean(RT, na.rm = TRUE),
    RT_se = sd(RT, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

write_csv(bestpair_summary, file.path(fig_dir, "bestpair_summary.csv"))

gap_by_run <- run_summary %>%
  select(model, risk, run, strategy, performance, AC, RT) %>%
  pivot_wider(names_from = strategy, values_from = c(performance, AC, RT)) %>%
  mutate(
    gap_performance = performance_bonus_oriented - performance_payoff_oriented,
    gap_AC = AC_bonus_oriented - AC_payoff_oriented,
    gap_RT = RT_bonus_oriented - RT_payoff_oriented
  )

gap_summary <- gap_by_run %>%
  pivot_longer(starts_with("gap_"), names_to = "metric", values_to = "gap") %>%
  mutate(metric = recode(
    metric,
    gap_performance = "Performance (bonus - payoff)",
    gap_AC = "AC (bonus - payoff)",
    gap_RT = "RT (bonus - payoff)"
  )) %>%
  group_by(model, risk, metric) %>%
  summarise(
    mean_gap = mean(gap, na.rm = TRUE),
    se_gap = sd(gap, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

did_tbl <- gap_summary %>%
  select(model, risk, metric, mean_gap) %>%
  pivot_wider(names_from = risk, values_from = mean_gap) %>%
  mutate(DoD_Hc_minus_Lc = Hc - Lc)

write_csv(gap_summary, file.path(fig_dir, "bestpair_gap_summary.csv"))
write_csv(did_tbl, file.path(fig_dir, "bestpair_did.csv"))

fit_interaction <- function(df, y) {
  fit <- lm(as.formula(paste0(y, " ~ strategy * risk")), data = df)
  co <- summary(fit)$coefficients
  idx <- grep("^strategypayoff_oriented:riskHc$", rownames(co))
  if (length(idx) == 0) {
    return(tibble(metric = y, estimate = NA_real_, p_value = NA_real_))
  }
  tibble(
    metric = y,
    estimate = unname(co[idx, "Estimate"]),
    p_value = unname(co[idx, "Pr(>|t|)"])
  )
}

interaction_stats <- bind_rows(
  lapply(levels(run_summary$model), function(m) {
    d <- run_summary %>% filter(model == m)
    bind_rows(
      fit_interaction(d, "performance"),
      fit_interaction(d, "AC"),
      fit_interaction(d, "RT")
    ) %>% mutate(model = m, .before = 1)
  })
)

write_csv(interaction_stats, file.path(fig_dir, "bestpair_interaction_stats.csv"))

# Unified palette (match earlier blue-orange style)
model_colors <- c("original" = "#4C78A8", "adapted" = "#F58518")
strategy_colors <- c("bonus_oriented" = "#4C78A8", "payoff_oriented" = "#F58518")

# 1) Overall bars
p_perf <- ggplot(bestpair_summary, aes(x = model, y = perf_mean, fill = strategy)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = perf_mean - perf_se, ymax = perf_mean + perf_se),
    position = position_dodge(0.75), width = 0.2, linewidth = 0.3
  ) +
  facet_wrap(~ risk) +
  scale_fill_manual(values = strategy_colors) +
  theme_bw(base_size = 11) +
  labs(title = "Performance", x = "Model", y = "Mean performance")

p_ac <- ggplot(bestpair_summary, aes(x = model, y = AC_mean, fill = strategy)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = AC_mean - AC_se, ymax = AC_mean + AC_se),
    position = position_dodge(0.75), width = 0.2, linewidth = 0.3
  ) +
  facet_wrap(~ risk) +
  scale_fill_manual(values = strategy_colors) +
  theme_bw(base_size = 11) +
  labs(title = "AC", x = "Model", y = "Mean AC")

p_rt <- ggplot(bestpair_summary, aes(x = model, y = RT_mean, fill = strategy)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = RT_mean - RT_se, ymax = RT_mean + RT_se),
    position = position_dodge(0.75), width = 0.2, linewidth = 0.3
  ) +
  facet_wrap(~ risk) +
  scale_fill_manual(values = strategy_colors) +
  theme_bw(base_size = 11) +
  labs(title = "RT", x = "Model", y = "Mean RT")

fig_overall <- (p_perf / p_ac / p_rt) +
  plot_annotation(title = "Best-pair (rep50) Comparison")

ggsave(file.path(fig_dir, "31_bestpair_overall_rep50.png"), fig_overall, width = 10, height = 12, dpi = 300)

# 2) Interaction lines
l_perf <- ggplot(bestpair_summary, aes(x = risk, y = perf_mean, color = strategy, group = strategy)) +
  geom_point(size = 2) + geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = perf_mean - perf_se, ymax = perf_mean + perf_se), width = 0.12, linewidth = 0.3) +
  scale_color_manual(values = strategy_colors) +
  facet_wrap(~ model) + theme_bw(base_size = 11) +
  labs(title = "Performance", x = "Risk", y = "Mean performance")

l_ac <- ggplot(bestpair_summary, aes(x = risk, y = AC_mean, color = strategy, group = strategy)) +
  geom_point(size = 2) + geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = AC_mean - AC_se, ymax = AC_mean + AC_se), width = 0.12, linewidth = 0.3) +
  scale_color_manual(values = strategy_colors) +
  facet_wrap(~ model) + theme_bw(base_size = 11) +
  labs(title = "AC", x = "Risk", y = "Mean AC")

l_rt <- ggplot(bestpair_summary, aes(x = risk, y = RT_mean, color = strategy, group = strategy)) +
  geom_point(size = 2) + geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = RT_mean - RT_se, ymax = RT_mean + RT_se), width = 0.12, linewidth = 0.3) +
  scale_color_manual(values = strategy_colors) +
  facet_wrap(~ model) + theme_bw(base_size = 11) +
  labs(title = "RT", x = "Risk", y = "Mean RT")

fig_lines <- (l_perf / l_ac / l_rt) +
  plot_annotation(title = "Best-pair (rep50) Strategy x Risk Interaction")

ggsave(file.path(fig_dir, "32_bestpair_interaction_lines_rep50.png"), fig_lines, width = 10, height = 12, dpi = 300)

# 3) DoD bars
fig_did <- ggplot(did_tbl, aes(x = metric, y = DoD_Hc_minus_Lc, fill = model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  scale_fill_manual(values = model_colors) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(
    title = "Best-pair (rep50) DoD",
    subtitle = "[(bonus - payoff) in Hc] - [(bonus - payoff) in Lc]",
    x = "Metric",
    y = "DoD (Hc - Lc)",
    fill = "Model"
  )

ggsave(file.path(fig_dir, "33_bestpair_did_rep50.png"), fig_did, width = 9, height = 5, dpi = 300)

saveRDS(
  list(
    bestpair_summary = bestpair_summary,
    gap_summary = gap_summary,
    did_tbl = did_tbl,
    interaction_stats = interaction_stats
  ),
  file = file.path(fig_dir, "bestpair_analysis_data_rep50.rds")
)

cat("Best-pair rep50 analysis done. Outputs saved to:", fig_dir, "\n")

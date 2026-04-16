library(tidyverse)
library(patchwork)

results_dir <- "results/final_v2_combo19"
fig_dir <- file.path(results_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

all_file <- file.path(results_dir, "all_results.csv")
if (!file.exists(all_file)) {
  stop("Missing file: ", all_file)
}

# Expected schema from v2 simulation output
expected_cols <- c(
  "Subject", "Round", "Phase", "afterbreak", "Schema", "Schema_RT", "Schema_OB", "Schema_AS",
  "Cho_1", "Cho_2", "Cho_3", "Cho_4",
  "RT_1", "RT_2", "RT_3", "RT_4",
  "OB_1", "OB_2", "OB_3", "OB_4",
  "AS_1", "AS_2", "AS_3", "AS_4",
  "schema_payoff", "AC", "performance", "payoff", "breakR", "type",
  "dwellmean_1", "dwellmean_2", "dwellmean_3", "dwellmean_4",
  "strategy", "combo_id", "thres_schema", "thres_item_final",
  "model", "risk", "run", "attempt"
)

read_all_results <- function(path) {
  # Try normal read first
  d1 <- suppressWarnings(read_csv(path, show_col_types = FALSE))
  if (all(c("RT_1", "RT_2", "RT_3", "RT_4", "model", "risk", "strategy") %in% names(d1))) {
    return(d1)
  }
  # Fallback for append-without-header file
  d2 <- read_csv(path, col_names = FALSE, show_col_types = FALSE)
  if (ncol(d2) != length(expected_cols)) {
    stop("Unexpected column count in all_results.csv: ", ncol(d2))
  }
  names(d2) <- expected_cols
  d2
}

all_results <- read_all_results(all_file) %>%
  mutate(
    strategy = factor(strategy, levels = c("baseline", "bonus_oriented", "payoff_oriented")),
    model = factor(model, levels = c("original", "adapted")),
    risk = factor(risk, levels = c("Lc", "Hc")),
    total_RT = RT_1 + RT_2 + RT_3 + RT_4
  )

# 1) Rebuild summaries
run_summary <- all_results %>%
  group_by(model, strategy, combo_id, thres_schema, thres_item_final, risk, run) %>%
  summarise(
    AC = mean(AC, na.rm = TRUE),
    performance = mean(performance, na.rm = TRUE),
    RT = mean(total_RT, na.rm = TRUE),
    .groups = "drop"
  )

combo_summary <- run_summary %>%
  group_by(model, strategy, combo_id, thres_schema, thres_item_final, risk) %>%
  summarise(
    AC_mean = mean(AC, na.rm = TRUE),
    AC_se = sd(AC, na.rm = TRUE) / sqrt(n()),
    perf_mean = mean(performance, na.rm = TRUE),
    perf_se = sd(performance, na.rm = TRUE) / sqrt(n()),
    RT_mean = mean(RT, na.rm = TRUE),
    RT_se = sd(RT, na.rm = TRUE) / sqrt(n()),
    n_runs = n(),
    .groups = "drop"
  )

strategy_summary <- run_summary %>%
  group_by(model, strategy, risk, run) %>%
  summarise(
    AC = mean(AC, na.rm = TRUE),
    performance = mean(performance, na.rm = TRUE),
    RT = mean(RT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(model, strategy, risk) %>%
  summarise(
    AC_mean = mean(AC, na.rm = TRUE),
    AC_se = sd(AC, na.rm = TRUE) / sqrt(n()),
    perf_mean = mean(performance, na.rm = TRUE),
    perf_se = sd(performance, na.rm = TRUE) / sqrt(n()),
    RT_mean = mean(RT, na.rm = TRUE),
    RT_se = sd(RT, na.rm = TRUE) / sqrt(n()),
    n_runs = n(),
    .groups = "drop"
  )

write_csv(run_summary, file.path(results_dir, "run_summary.csv"))
write_csv(combo_summary, file.path(results_dir, "combo_summary.csv"))
write_csv(strategy_summary, file.path(results_dir, "strategy_summary.csv"))

# 2) Strategy-risk analysis: bonus vs payoff
bp <- run_summary %>%
  filter(strategy %in% c("bonus_oriented", "payoff_oriented")) %>%
  mutate(strategy = factor(strategy, levels = c("payoff_oriented", "bonus_oriented")))

bp_summary <- bp %>%
  group_by(model, risk, strategy) %>%
  summarise(
    AC_mean = mean(AC, na.rm = TRUE),
    AC_se = sd(AC, na.rm = TRUE) / sqrt(n()),
    perf_mean = mean(performance, na.rm = TRUE),
    perf_se = sd(performance, na.rm = TRUE) / sqrt(n()),
    RT_mean = mean(RT, na.rm = TRUE),
    RT_se = sd(RT, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

gap_by_run <- bp %>%
  group_by(model, risk, run, strategy) %>%
  summarise(
    AC = mean(AC, na.rm = TRUE),
    performance = mean(performance, na.rm = TRUE),
    RT = mean(RT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = strategy, values_from = c(AC, performance, RT)) %>%
  mutate(
    gap_AC = AC_bonus_oriented - AC_payoff_oriented,
    gap_performance = performance_bonus_oriented - performance_payoff_oriented,
    gap_RT = RT_bonus_oriented - RT_payoff_oriented
  )

gap_summary <- gap_by_run %>%
  pivot_longer(starts_with("gap_"), names_to = "metric", values_to = "gap") %>%
  mutate(metric = recode(
    metric,
    gap_AC = "AC (bonus - payoff)",
    gap_performance = "Performance (bonus - payoff)",
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

fit_interaction <- function(df, y) {
  f <- as.formula(paste0(y, " ~ strategy * risk"))
  fit <- lm(f, data = df)
  co <- summary(fit)$coefficients
  idx <- grep("^strategybonus_oriented:riskHc$", rownames(co))
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
  lapply(levels(bp$model), function(m) {
    d <- bp %>% filter(model == m)
    bind_rows(
      fit_interaction(d, "performance"),
      fit_interaction(d, "AC"),
      fit_interaction(d, "RT")
    ) %>% mutate(model = m, .before = 1)
  })
)

write_csv(bp_summary, file.path(fig_dir, "v2_strategy_risk_summary.csv"))
write_csv(gap_summary, file.path(fig_dir, "v2_strategy_risk_gap_summary.csv"))
write_csv(did_tbl, file.path(fig_dir, "v2_strategy_risk_did.csv"))
write_csv(interaction_stats, file.path(fig_dir, "v2_strategy_risk_interaction_stats.csv"))

# 3) Plots
p_perf <- ggplot(bp_summary, aes(x = risk, y = perf_mean, color = strategy, group = strategy)) +
  geom_point(size = 2) + geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = perf_mean - perf_se, ymax = perf_mean + perf_se), width = 0.12, linewidth = 0.3) +
  facet_wrap(~ model) + theme_bw(base_size = 11) +
  labs(title = "Performance", x = "Risk", y = "Mean performance", color = "Strategy")

p_ac <- ggplot(bp_summary, aes(x = risk, y = AC_mean, color = strategy, group = strategy)) +
  geom_point(size = 2) + geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = AC_mean - AC_se, ymax = AC_mean + AC_se), width = 0.12, linewidth = 0.3) +
  facet_wrap(~ model) + theme_bw(base_size = 11) +
  labs(title = "AC", x = "Risk", y = "Mean AC", color = "Strategy")

p_rt <- ggplot(bp_summary, aes(x = risk, y = RT_mean, color = strategy, group = strategy)) +
  geom_point(size = 2) + geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = RT_mean - RT_se, ymax = RT_mean + RT_se), width = 0.12, linewidth = 0.3) +
  facet_wrap(~ model) + theme_bw(base_size = 11) +
  labs(title = "RT", x = "Risk", y = "Mean RT", color = "Strategy")

fig_interaction <- (p_perf / p_ac / p_rt) + plot_annotation(title = "V2: Strategy x Risk Interaction")
ggsave(file.path(fig_dir, "21_v2_strategy_risk_interaction.png"), fig_interaction, width = 10, height = 12, dpi = 300)

fig_gap <- ggplot(gap_summary, aes(x = risk, y = mean_gap, fill = risk)) +
  geom_col(width = 0.62) +
  geom_errorbar(aes(ymin = mean_gap - se_gap, ymax = mean_gap + se_gap), width = 0.2, linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  facet_grid(metric ~ model, scales = "free_y") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none") +
  labs(title = "V2: Strategy Gap by Risk", subtitle = "Gap = bonus - payoff", x = "Risk", y = "Mean gap")
ggsave(file.path(fig_dir, "22_v2_strategy_gap_by_risk.png"), fig_gap, width = 10, height = 8, dpi = 300)

# Delta heatmap in 2 strategy regions (adapted - original) for performance
perf_delta <- combo_summary %>%
  select(model, strategy, risk, thres_schema, thres_item_final, perf_mean) %>%
  pivot_wider(names_from = model, values_from = perf_mean) %>%
  mutate(delta_perf = adapted - original)

fig_heat <- perf_delta %>%
  filter(strategy %in% c("bonus_oriented", "payoff_oriented")) %>%
  ggplot(aes(x = thres_schema, y = thres_item_final, fill = delta_perf)) +
  geom_tile(color = "white") +
  facet_grid(strategy ~ risk) +
  scale_fill_gradient2(low = "#B91C1C", mid = "white", high = "#065F46", midpoint = 0) +
  theme_bw(base_size = 11) +
  labs(title = "V2: Adaptation Effect Heatmap", x = "thres_schema", y = "thres_item_final", fill = "Delta perf")
ggsave(file.path(fig_dir, "23_v2_delta_performance_heatmap.png"), fig_heat, width = 10, height = 6, dpi = 300)

saveRDS(
  list(
    run_summary = run_summary,
    combo_summary = combo_summary,
    strategy_summary = strategy_summary,
    gap_summary = gap_summary,
    did_tbl = did_tbl,
    interaction_stats = interaction_stats
  ),
  file = file.path(fig_dir, "v2_postprocess_data.rds")
)

cat("V2 postprocess done. Outputs saved to:", results_dir, "and", fig_dir, "\n")

library(tidyverse)
library(patchwork)

results_dir <- "results/final"
fig_dir <- file.path(results_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

run_summary <- read_csv(file.path(results_dir, "run_summary.csv"), show_col_types = FALSE)

# Focus on the task-relevant strategy contrast: bonus vs payoff
dat <- run_summary %>%
  filter(agent %in% c("bonus", "payoff")) %>%
  mutate(
    strategy = factor(agent, levels = c("payoff", "bonus")),
    risk = factor(risk, levels = c("Lc", "Hc")),
    model = factor(model, levels = c("original", "adapted"))
  ) %>%
  select(model, risk, run, strategy, performance, AC, RT)

if (nrow(dat) == 0) {
  stop("No bonus/payoff rows found in results/final/run_summary.csv")
}

summary_tbl <- dat %>%
  group_by(model, risk, strategy) %>%
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

# Strategy gap within each run: bonus - payoff
gap_by_run <- dat %>%
  select(model, risk, run, strategy, performance, AC, RT) %>%
  pivot_wider(names_from = strategy, values_from = c(performance, AC, RT)) %>%
  mutate(
    gap_performance = performance_bonus - performance_payoff,
    gap_AC = AC_bonus - AC_payoff,
    gap_RT = RT_bonus - RT_payoff
  ) %>%
  select(model, risk, run, starts_with("gap_"))

gap_summary <- gap_by_run %>%
  pivot_longer(
    cols = starts_with("gap_"),
    names_to = "metric",
    values_to = "gap"
  ) %>%
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

# Difference-in-differences by metric and model:
# DoD = (bonus - payoff)_Hc - (bonus - payoff)_Lc
did_tbl <- gap_summary %>%
  select(model, risk, metric, mean_gap) %>%
  pivot_wider(names_from = risk, values_from = mean_gap) %>%
  mutate(DoD_Hc_minus_Lc = Hc - Lc)

# Linear models for interaction: metric ~ strategy * risk, run-level
fit_interaction <- function(df, y) {
  formula <- as.formula(paste0(y, " ~ strategy * risk"))
  fit <- lm(formula, data = df)
  co <- summary(fit)$coefficients
  rn <- rownames(co)
  idx <- grep("^strategybonus:riskHc$", rn)
  if (length(idx) == 0) {
    return(tibble(
      metric = y,
      term = "strategybonus:riskHc",
      estimate = NA_real_,
      std_error = NA_real_,
      t_value = NA_real_,
      p_value = NA_real_
    ))
  }
  tibble(
    metric = y,
    term = rn[idx],
    estimate = unname(co[idx, "Estimate"]),
    std_error = unname(co[idx, "Std. Error"]),
    t_value = unname(co[idx, "t value"]),
    p_value = unname(co[idx, "Pr(>|t|)"])
  )
}

interaction_stats <- bind_rows(
  lapply(levels(dat$model), function(m) {
    d <- dat %>% filter(model == m)
    bind_rows(
      fit_interaction(d, "performance"),
      fit_interaction(d, "AC"),
      fit_interaction(d, "RT")
    ) %>% mutate(model = m, .before = 1)
  })
)

# Plot 1: interaction lines (strategy x risk), faceted by model
p_perf <- ggplot(summary_tbl, aes(x = risk, y = perf_mean, color = strategy, group = strategy)) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = perf_mean - perf_se, ymax = perf_mean + perf_se), width = 0.12, linewidth = 0.3) +
  facet_wrap(~ model) +
  labs(title = "Performance", x = "Risk", y = "Mean performance", color = "Strategy") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

p_ac <- ggplot(summary_tbl, aes(x = risk, y = AC_mean, color = strategy, group = strategy)) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = AC_mean - AC_se, ymax = AC_mean + AC_se), width = 0.12, linewidth = 0.3) +
  facet_wrap(~ model) +
  labs(title = "Accuracy (AC)", x = "Risk", y = "Mean AC", color = "Strategy") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

p_rt <- ggplot(summary_tbl, aes(x = risk, y = RT_mean, color = strategy, group = strategy)) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = RT_mean - RT_se, ymax = RT_mean + RT_se), width = 0.12, linewidth = 0.3) +
  facet_wrap(~ model) +
  labs(title = "Reaction Time", x = "Risk", y = "Mean RT", color = "Strategy") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

fig_interaction <- (p_perf / p_ac / p_rt) +
  plot_annotation(title = "Strategy Difference Under Risk (bonus vs payoff)")

ggsave(file.path(fig_dir, "16_strategy_risk_interaction.png"), fig_interaction, width = 10, height = 12, dpi = 300)

# Plot 2: strategy gap (bonus - payoff) by risk
fig_gap <- ggplot(gap_summary, aes(x = risk, y = mean_gap, fill = risk)) +
  geom_col(width = 0.62) +
  geom_errorbar(aes(ymin = mean_gap - se_gap, ymax = mean_gap + se_gap), width = 0.2, linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  facet_grid(metric ~ model, scales = "free_y") +
  labs(
    title = "Strategy Gap by Risk",
    subtitle = "Gap = bonus - payoff",
    x = "Risk",
    y = "Mean gap"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")

ggsave(file.path(fig_dir, "17_strategy_gap_by_risk.png"), fig_gap, width = 10, height = 8, dpi = 300)

# Plot 3: Difference-in-differences
fig_did <- ggplot(did_tbl, aes(x = metric, y = DoD_Hc_minus_Lc, fill = model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  labs(
    title = "Difference-in-Differences",
    subtitle = "[(bonus - payoff) in Hc] - [(bonus - payoff) in Lc]",
    x = "Metric",
    y = "DoD (Hc - Lc)",
    fill = "Model"
  ) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(file.path(fig_dir, "18_strategy_did.png"), fig_did, width = 9, height = 5, dpi = 300)

write_csv(summary_tbl, file.path(fig_dir, "strategy_risk_summary.csv"))
write_csv(gap_summary, file.path(fig_dir, "strategy_risk_gap_summary.csv"))
write_csv(did_tbl, file.path(fig_dir, "strategy_risk_did.csv"))
write_csv(interaction_stats, file.path(fig_dir, "strategy_risk_interaction_stats.csv"))

saveRDS(
  list(
    summary_tbl = summary_tbl,
    gap_summary = gap_summary,
    did_tbl = did_tbl,
    interaction_stats = interaction_stats
  ),
  file = file.path(fig_dir, "strategy_risk_analysis_data.rds")
)

cat("Saved strategy-risk analysis outputs to:", fig_dir, "\n")

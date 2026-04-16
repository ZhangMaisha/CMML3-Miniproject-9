library(tidyverse)
library(patchwork)

results_dir <- "../results/final"
fig_dir <- file.path(results_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

run_summary <- read_csv(file.path(results_dir, "run_summary.csv"), show_col_types = FALSE) %>%
  mutate(
    model = factor(model, levels = c("original", "adapted")),
    risk = factor(risk, levels = c("Lc", "Hc")),
    strategy = factor(strategy, levels = c("baseline", "bonus_oriented", "payoff_oriented"))
  )

# =========================================================
# A) Region-level (bonus vs payoff) interaction and DoD
# =========================================================
bp <- run_summary %>%
  filter(strategy %in% c("bonus_oriented", "payoff_oriented")) %>%
  mutate(strategy = factor(strategy, levels = c("payoff_oriented", "bonus_oriented")))

# Average over combos within each strategy for each run
bp_run <- bp %>%
  group_by(model, risk, run, strategy) %>%
  summarise(
    performance = mean(performance, na.rm = TRUE),
    AC = mean(AC, na.rm = TRUE),
    RT = mean(RT, na.rm = TRUE),
    .groups = "drop"
  )

bp_summary <- bp_run %>%
  group_by(model, risk, strategy) %>%
  summarise(
    perf_mean = mean(performance, na.rm = TRUE),
    perf_se = sd(performance, na.rm = TRUE) / sqrt(n()),
    AC_mean = mean(AC, na.rm = TRUE),
    AC_se = sd(AC, na.rm = TRUE) / sqrt(n()),
    RT_mean = mean(RT, na.rm = TRUE),
    RT_se = sd(RT, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

gap_by_run <- bp_run %>%
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

# DoD SE from run-level paired contrast
did_se_tbl <- gap_by_run %>%
  select(model, run, risk, gap_performance, gap_AC, gap_RT) %>%
  pivot_wider(names_from = risk, values_from = c(gap_performance, gap_AC, gap_RT)) %>%
  mutate(
    dod_performance = gap_performance_Hc - gap_performance_Lc,
    dod_AC = gap_AC_Hc - gap_AC_Lc,
    dod_RT = gap_RT_Hc - gap_RT_Lc
  ) %>%
  pivot_longer(starts_with("dod_"), names_to = "metric_key", values_to = "dod") %>%
  mutate(metric = recode(
    metric_key,
    dod_performance = "Performance (bonus - payoff)",
    dod_AC = "AC (bonus - payoff)",
    dod_RT = "RT (bonus - payoff)"
  )) %>%
  group_by(model, metric) %>%
  summarise(DoD_se = sd(dod, na.rm = TRUE) / sqrt(n()), .groups = "drop")

did_tbl <- did_tbl %>% left_join(did_se_tbl, by = c("model", "metric"))

fit_interaction <- function(df, y) {
  fit <- lm(as.formula(paste0(y, " ~ strategy * risk")), data = df)
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
  lapply(levels(bp_run$model), function(m) {
    d <- bp_run %>% filter(model == m)
    bind_rows(
      fit_interaction(d, "performance"),
      fit_interaction(d, "AC"),
      fit_interaction(d, "RT")
    ) %>% mutate(model = m, .before = 1)
  })
)

# Plots: keep only key outputs for concise reporting
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

fig_interaction <- (p_perf / p_ac / p_rt) +
  plot_annotation(
    title = "Strategy x Risk Interaction",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )
ggsave(file.path(fig_dir, "24_v2_strategy_risk_interaction_detailed.png"), fig_interaction, width = 10, height = 12, dpi = 300)

fig_did <- ggplot(did_tbl, aes(x = metric, y = DoD_Hc_minus_Lc, fill = model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_errorbar(
    aes(ymin = DoD_Hc_minus_Lc - DoD_se, ymax = DoD_Hc_minus_Lc + DoD_se),
    position = position_dodge(width = 0.7), width = 0.2, linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  scale_fill_manual(values = c("original" = "#4C78A8", "adapted" = "#F58518")) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1, size = 12),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Difference-in-Differences of Strategy Gap",
    subtitle = "[(bonus - payoff) in Hc] - [(bonus - payoff) in Lc]",
    x = "Metric",
    y = "DoD (Hc - Lc)",
    fill = "Model"
  )
ggsave(file.path(fig_dir, "26_v2_strategy_did_detailed.png"), fig_did, width = 9, height = 5, dpi = 300)

# =========================================================
# B) Combo-level heterogeneity (can effects cancel out?)
# =========================================================
combo_lvl <- run_summary %>%
  filter(strategy %in% c("bonus_oriented", "payoff_oriented")) %>%
  group_by(model, strategy, combo_id, thres_schema, thres_item_final, risk) %>%
  summarise(
    perf_mean = mean(performance, na.rm = TRUE),
    AC_mean = mean(AC, na.rm = TRUE),
    RT_mean = mean(RT, na.rm = TRUE),
    .groups = "drop"
  )

# Risk effect within each combo: (Hc - Lc)
combo_risk_effect <- combo_lvl %>%
  pivot_wider(names_from = risk, values_from = c(perf_mean, AC_mean, RT_mean)) %>%
  mutate(
    risk_effect_perf = perf_mean_Hc - perf_mean_Lc,
    risk_effect_AC = AC_mean_Hc - AC_mean_Lc,
    risk_effect_RT = RT_mean_Hc - RT_mean_Lc
  )

# Adaptation effect on risk effect by combo:
# [risk_effect(adapted)] - [risk_effect(original)]
combo_dod <- combo_risk_effect %>%
  select(model, strategy, combo_id, thres_schema, thres_item_final,
         risk_effect_perf, risk_effect_AC, risk_effect_RT) %>%
  pivot_wider(names_from = model, values_from = c(risk_effect_perf, risk_effect_AC, risk_effect_RT)) %>%
  mutate(
    dod_perf = risk_effect_perf_adapted - risk_effect_perf_original,
    dod_AC = risk_effect_AC_adapted - risk_effect_AC_original,
    dod_RT = risk_effect_RT_adapted - risk_effect_RT_original
  )

# Heatmap: combo-level performance DoD (cell size follows parameter step)
combo_dod_plot <- combo_dod %>%
  mutate(
    x_half = 2.5,
    y_half = if_else(strategy == "bonus_oriented", 2.5, 1.25),
    xmin = thres_schema - x_half,
    xmax = thres_schema + x_half,
    ymin = thres_item_final - y_half,
    ymax = thres_item_final + y_half
  )

strategy_labels <- combo_dod_plot %>%
  group_by(strategy) %>%
  summarise(
    label_x = mean(thres_schema, na.rm = TRUE),
    label_y = max(ymax, na.rm = TRUE) + 0.8,
    .groups = "drop"
  ) %>%
  mutate(label = if_else(strategy == "bonus_oriented", "bonus_oriented", "payoff_oriented"))

fig_combo_heat <- combo_dod_plot %>%
  ggplot(aes(fill = dod_perf)) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "white") +
  geom_text(
    data = strategy_labels,
    aes(x = label_x, y = label_y, label = label),
    inherit.aes = FALSE,
    size = 3.2,
    fontface = "bold"
  ) +
  scale_fill_gradient2(low = "#4C78A8", mid = "#F7F7F7", high = "#F58518", midpoint = 0) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  coord_fixed(ratio = 1) +
  labs(
    title = "Combo-level DoD Heatmap (Performance)",
    subtitle = "DoD = [Hc-Lc]_adapted - [Hc-Lc]_original",
    x = "thres_schema",
    y = "thres_item_final",
    fill = "DoD perf"
  )
ggsave(file.path(fig_dir, "27_v2_combo_dod_heatmap_perf.png"), fig_combo_heat, width = 9, height = 4.8, dpi = 300)

# =========================================================
# C) Threshold-interaction regression (explicit test)
# =========================================================
extract_coef <- function(fit, metric_name) {
  co <- summary(fit)$coefficients
  tibble(
    term = rownames(co),
    estimate = co[, "Estimate"],
    std_error = co[, "Std. Error"],
    t_value = co[, "t value"],
    p_value = co[, "Pr(>|t|)"]
  ) %>%
    mutate(metric = metric_name, .before = 1)
}

bp_reg <- bp %>%
  mutate(
    strategy = relevel(strategy, ref = "payoff_oriented"),
    risk = relevel(risk, ref = "Lc"),
    model = relevel(model, ref = "original")
  )

fit_perf <- lm(
  performance ~ strategy * risk * model +
    risk * thres_schema + risk * thres_item_final +
    model:risk:thres_schema + model:risk:thres_item_final,
  data = bp_reg
)
fit_ac <- lm(
  AC ~ strategy * risk * model +
    risk * thres_schema + risk * thres_item_final +
    model:risk:thres_schema + model:risk:thres_item_final,
  data = bp_reg
)
fit_rt <- lm(
  RT ~ strategy * risk * model +
    risk * thres_schema + risk * thres_item_final +
    model:risk:thres_schema + model:risk:thres_item_final,
  data = bp_reg
)

reg_terms <- bind_rows(
  extract_coef(fit_perf, "performance"),
  extract_coef(fit_ac, "AC"),
  extract_coef(fit_rt, "RT")
)

target_terms <- reg_terms %>%
  filter(str_detect(term, "riskHc:thres_schema|riskHc:thres_item_final|strategybonus_oriented:riskHc|modeladapted:riskHc"))

write_csv(did_tbl, file.path(fig_dir, "v2_detailed_did.csv"))
write_csv(interaction_stats, file.path(fig_dir, "v2_detailed_interaction_stats.csv"))
write_csv(target_terms, file.path(fig_dir, "v2_detailed_regression_terms_target.csv"))

saveRDS(
  list(
    did_tbl = did_tbl,
    interaction_stats = interaction_stats,
    target_terms = target_terms
  ),
  file = file.path(fig_dir, "v2_detailed_analysis_data.rds")
)

cat("Detailed analysis completed. Saved to:", fig_dir, "\n")

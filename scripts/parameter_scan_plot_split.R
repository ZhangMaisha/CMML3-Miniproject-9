library(tidyverse)

#========================
# 1. Set results directory
#========================
results_dir <- "../results/param_scan_20260412"

#========================
# 2. Read and summarize all parameter scan results
#========================
make_summary_table <- function(results_dir) {
  csv_files <- list.files(results_dir, pattern = "_scan\\.csv$", full.names = TRUE)
  summary_all <- list()

  for (file in csv_files) {
    df <- read_csv(file, show_col_types = FALSE)

    # Extract parameter name, e.g., "Phi_scan.csv" -> "Phi"
    param_name <- gsub("_scan\\.csv$", "", basename(file))
    param_col <- paste0("param_", param_name)

    # Calculate RT_total row-wise (more robust)
    df <- df %>%
      mutate(
        RT_total = rowSums(across(c(RT_1, RT_2, RT_3, RT_4)), na.rm = TRUE)
      )

    # First level: average within each rep
    rep_summary <- df %>%
      group_by(type, rep, param_value = .data[[param_col]]) %>%
      summarise(
        rep_mean_AC = mean(AC, na.rm = TRUE),
        rep_mean_performance = mean(performance, na.rm = TRUE),
        rep_mean_RT = mean(RT_total, na.rm = TRUE),
        .groups = "drop"
      )

    # Second level: compute mean and SE across reps
    final_summary <- rep_summary %>%
      group_by(type, param_value) %>%
      summarise(
        mean_AC = mean(rep_mean_AC, na.rm = TRUE),
        se_AC = ifelse(
          sum(!is.na(rep_mean_AC)) > 1,
          sd(rep_mean_AC, na.rm = TRUE) / sqrt(sum(!is.na(rep_mean_AC))),
          NA_real_
        ),
        mean_performance = mean(rep_mean_performance, na.rm = TRUE),
        se_performance = ifelse(
          sum(!is.na(rep_mean_performance)) > 1,
          sd(rep_mean_performance, na.rm = TRUE) / sqrt(sum(!is.na(rep_mean_performance))),
          NA_real_
        ),
        mean_RT = mean(rep_mean_RT, na.rm = TRUE),
        se_RT = ifelse(
          sum(!is.na(rep_mean_RT)) > 1,
          sd(rep_mean_RT, na.rm = TRUE) / sqrt(sum(!is.na(rep_mean_RT))),
          NA_real_
        ),
        n_reps = n(),
        .groups = "drop"
      ) %>%
      mutate(parameter = param_name)

    summary_all[[param_name]] <- final_summary
  }

  bind_rows(summary_all)
}

summary_df <- make_summary_table(results_dir) %>%
  mutate(
    parameter = factor(
      parameter,
      levels = c("w", "Phi", "thres_item_final", "thres_schema", "theta_shift")
    )
  )

#========================
# 3. Get raw rep-level data for boxplots
#========================
make_rep_data <- function(results_dir) {
  csv_files <- list.files(results_dir, pattern = "_scan\\.csv$", full.names = TRUE)
  rep_data_all <- list()

  for (file in csv_files) {
    df <- read_csv(file, show_col_types = FALSE)
    param_name <- gsub("_scan\\.csv$", "", basename(file))
    param_col <- paste0("param_", param_name)

    df <- df %>%
      mutate(
        RT_total = rowSums(across(c(RT_1, RT_2, RT_3, RT_4)), na.rm = TRUE),
        param_value = .data[[param_col]],
        parameter = param_name
      ) %>%
      group_by(type, rep, param_value, parameter) %>%
      summarise(
        AC = mean(AC, na.rm = TRUE),
        performance = mean(performance, na.rm = TRUE),
        RT = mean(RT_total, na.rm = TRUE),
        .groups = "drop"
      )

    rep_data_all[[param_name]] <- df
  }

  bind_rows(rep_data_all)
}

rep_df <- make_rep_data(results_dir) %>%
  mutate(
    parameter = factor(
      parameter,
      levels = c("w", "Phi", "thres_item_final", "thres_schema", "theta_shift")
    )
  )

#========================
# 4. Plot functions (same style, with parameter subset)
#========================
plot_box_line_ac <- function(data, rep_data, params, facet_ncol = 3, title_suffix = "") {
  line_data <- data %>%
    filter(parameter %in% params) %>%
    group_by(parameter, param_value) %>%
    summarise(mean_AC = mean(mean_AC, na.rm = TRUE), .groups = "drop")

  box_data <- rep_data %>%
    filter(parameter %in% params) %>%
    select(parameter, param_value, AC)

  if (length(params) == 1) {
    return(
      ggplot() +
        geom_boxplot(
          data = box_data,
          aes(x = factor(param_value, levels = sort(unique(param_value))), y = AC, group = param_value),
          fill = "grey80", alpha = 0.5, outlier.size = 0.5, width = 0.6
        ) +
        geom_line(
          data = line_data,
          aes(x = factor(param_value, levels = sort(unique(param_value))), y = mean_AC, group = 1),
          color = "#4C78A8", linewidth = 1.2
        ) +
        geom_point(
          data = line_data,
          aes(x = factor(param_value, levels = sort(unique(param_value))), y = mean_AC),
          color = "#4C78A8", size = 2.5
        ) +
        labs(
          x = "Parameter value",
          y = "Mean AC",
          title = paste0("AC vs ", title_suffix)
        ) +
        theme_bw(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    )
  }

  ggplot() +
    geom_boxplot(
      data = box_data,
      aes(x = as.numeric(factor(param_value)), y = AC, group = param_value),
      fill = "grey80", alpha = 0.5, outlier.size = 0.5, width = 0.6
    ) +
    geom_line(
      data = line_data,
      aes(x = as.numeric(factor(param_value)), y = mean_AC),
      color = "#4C78A8", linewidth = 1.2
    ) +
    geom_point(
      data = line_data,
      aes(x = as.numeric(factor(param_value)), y = mean_AC),
      color = "#4C78A8", size = 2.5
    ) +
    scale_x_continuous(
      breaks = seq_along(unique(box_data$param_value)),
      labels = sort(unique(box_data$param_value))
    ) +
    facet_wrap(~ parameter, scales = "free_x", ncol = facet_ncol) +
    labs(
      x = "Parameter value",
      y = "Mean AC",
      title = paste0("AC vs ", title_suffix)
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_box_line_rt <- function(data, rep_data, params, facet_ncol = 3, title_suffix = "") {
  line_data <- data %>%
    filter(parameter %in% params) %>%
    group_by(parameter, param_value) %>%
    summarise(mean_RT = mean(mean_RT, na.rm = TRUE), .groups = "drop")

  box_data <- rep_data %>%
    filter(parameter %in% params) %>%
    select(parameter, param_value, RT)

  if (length(params) == 1) {
    return(
      ggplot() +
        geom_boxplot(
          data = box_data,
          aes(x = factor(param_value, levels = sort(unique(param_value))), y = RT, group = param_value),
          fill = "grey80", alpha = 0.5, outlier.size = 0.5, width = 0.6
        ) +
        geom_line(
          data = line_data,
          aes(x = factor(param_value, levels = sort(unique(param_value))), y = mean_RT, group = 1),
          color = "#F58518", linewidth = 1.2
        ) +
        geom_point(
          data = line_data,
          aes(x = factor(param_value, levels = sort(unique(param_value))), y = mean_RT),
          color = "#F58518", size = 2.5
        ) +
        labs(
          x = "Parameter value",
          y = "Mean RT (s)",
          title = paste0("RT vs ", title_suffix)
        ) +
        theme_bw(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    )
  }

  ggplot() +
    geom_boxplot(
      data = box_data,
      aes(x = as.numeric(factor(param_value)), y = RT, group = param_value),
      fill = "grey80", alpha = 0.5, outlier.size = 0.5, width = 0.6
    ) +
    geom_line(
      data = line_data,
      aes(x = as.numeric(factor(param_value)), y = mean_RT),
      color = "#F58518", linewidth = 1.2
    ) +
    geom_point(
      data = line_data,
      aes(x = as.numeric(factor(param_value)), y = mean_RT),
      color = "#F58518", size = 2.5
    ) +
    scale_x_continuous(
      breaks = seq_along(unique(box_data$param_value)),
      labels = sort(unique(box_data$param_value))
    ) +
    facet_wrap(~ parameter, scales = "free_x", ncol = facet_ncol) +
    labs(
      x = "Parameter value",
      y = "Mean RT (s)",
      title = paste0("RT vs ", title_suffix)
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_box_line_perf <- function(data, rep_data, params, facet_ncol = 3, title_suffix = "") {
  line_data <- data %>%
    filter(parameter %in% params) %>%
    group_by(parameter, param_value, type) %>%
    summarise(mean_perf = mean(mean_performance, na.rm = TRUE), .groups = "drop")

  box_data <- rep_data %>%
    filter(parameter %in% params) %>%
    select(parameter, param_value, type, performance)

  if (length(params) == 1) {
    return(
      ggplot() +
        geom_boxplot(
          data = box_data,
          aes(
            x = factor(param_value, levels = sort(unique(param_value))),
            y = performance,
            group = interaction(param_value, type),
            fill = type
          ),
          alpha = 0.3, outlier.size = 0.5, width = 0.6,
          position = position_dodge(0.8)
        ) +
        geom_line(
          data = line_data,
          aes(
            x = factor(param_value, levels = sort(unique(param_value))),
            y = mean_perf, color = type, group = type
          ),
          linewidth = 1.2
        ) +
        geom_point(
          data = line_data,
          aes(
            x = factor(param_value, levels = sort(unique(param_value))),
            y = mean_perf, color = type
          ),
          size = 2.5
        ) +
        scale_fill_manual(values = c("Lc" = "#38A464", "Hc" = "#F60507"), name = "Risk") +
        scale_color_manual(values = c("Lc" = "#38A464", "Hc" = "#F60507"), name = "Risk") +
        labs(
          x = "Parameter value",
          y = "Mean performance",
          title = paste0("Performance vs ", title_suffix)
        ) +
        theme_bw(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    )
  }

  ggplot() +
    geom_boxplot(
      data = box_data,
      aes(
        x = as.numeric(factor(param_value)),
        y = performance,
        group = interaction(param_value, type),
        fill = type
      ),
      alpha = 0.3, outlier.size = 0.5, width = 0.6,
      position = position_dodge(0.8)
    ) +
    geom_line(
      data = line_data,
      aes(x = as.numeric(factor(param_value)), y = mean_perf, color = type, group = type),
      linewidth = 1.2
    ) +
    geom_point(
      data = line_data,
      aes(x = as.numeric(factor(param_value)), y = mean_perf, color = type),
      size = 2.5
    ) +
    scale_x_continuous(
      breaks = seq_along(unique(box_data$param_value)),
      labels = sort(unique(box_data$param_value))
    ) +
    scale_fill_manual(values = c("Lc" = "#38A464", "Hc" = "#F60507"), name = "Risk") +
    scale_color_manual(values = c("Lc" = "#38A464", "Hc" = "#F60507"), name = "Risk") +
    facet_wrap(~ parameter, scales = "free_x", ncol = facet_ncol) +
    labs(
      x = "Parameter value",
      y = "Mean performance",
      title = paste0("Performance vs ", title_suffix)
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#========================
# 5. Split figure groups
#========================
params_other <- c("w", "Phi", "theta_shift")

# Threshold plots: split into 6 separate figures (2 thresholds x 3 metrics)
p_ac_thres_item_final <- plot_box_line_ac(summary_df, rep_df, "thres_item_final", facet_ncol = 1, title_suffix = " | thres_item_final")
p_ac_thres_schema <- plot_box_line_ac(summary_df, rep_df, "thres_schema", facet_ncol = 1, title_suffix = " | thres_schema")

p_rt_thres_item_final <- plot_box_line_rt(summary_df, rep_df, "thres_item_final", facet_ncol = 1, title_suffix = " | thres_item_final")
p_rt_thres_schema <- plot_box_line_rt(summary_df, rep_df, "thres_schema", facet_ncol = 1, title_suffix = " | thres_schema")

p_perf_thres_item_final <- plot_box_line_perf(summary_df, rep_df, "thres_item_final", facet_ncol = 1, title_suffix = " | thres_item_final")
p_perf_thres_schema <- plot_box_line_perf(summary_df, rep_df, "thres_schema", facet_ncol = 1, title_suffix = " | thres_schema")

# Other-parameter plots: split (no facet_wrap)
p_ac_w <- plot_box_line_ac(summary_df, rep_df, "w", facet_ncol = 1, title_suffix = "w")
p_ac_phi <- plot_box_line_ac(summary_df, rep_df, "Phi", facet_ncol = 1, title_suffix = "phi")
p_ac_theta_shift <- plot_box_line_ac(summary_df, rep_df, "theta_shift", facet_ncol = 1, title_suffix = "theta_shift")
p_ac_w <- p_ac_w + labs(title = "Effects of parameter changes on AC | w")
p_ac_phi <- p_ac_phi + labs(title = "Effects of parameter changes on AC | phi")
p_ac_theta_shift <- p_ac_theta_shift + labs(title = "Effects of parameter changes on AC | theta_shift")

p_rt_w <- plot_box_line_rt(summary_df, rep_df, "w", facet_ncol = 1, title_suffix = "w")
p_rt_phi <- plot_box_line_rt(summary_df, rep_df, "Phi", facet_ncol = 1, title_suffix = "phi")
p_rt_theta_shift <- plot_box_line_rt(summary_df, rep_df, "theta_shift", facet_ncol = 1, title_suffix = "theta_shift")
p_rt_w <- p_rt_w + labs(title = "Effects of parameter changes on RT | w")
p_rt_phi <- p_rt_phi + labs(title = "Effects of parameter changes on RT | phi")
p_rt_theta_shift <- p_rt_theta_shift + labs(title = "Effects of parameter changes on RT | theta_shift")

p_perf_w <- plot_box_line_perf(summary_df, rep_df, "w", facet_ncol = 1, title_suffix = "w")
p_perf_phi <- plot_box_line_perf(summary_df, rep_df, "Phi", facet_ncol = 1, title_suffix = "phi")
p_perf_theta_shift <- plot_box_line_perf(summary_df, rep_df, "theta_shift", facet_ncol = 1, title_suffix = "theta_shift")
p_perf_w <- p_perf_w + labs(title = "Effects of parameter changes on performance | w")
p_perf_phi <- p_perf_phi + labs(title = "Effects of parameter changes on performance | phi")
p_perf_theta_shift <- p_perf_theta_shift + labs(title = "Effects of parameter changes on performance | theta_shift")

print(p_ac_thres_item_final)
print(p_ac_thres_schema)
print(p_ac_w)
print(p_ac_phi)
print(p_ac_theta_shift)
print(p_rt_thres_item_final)
print(p_rt_thres_schema)
print(p_rt_w)
print(p_rt_phi)
print(p_rt_theta_shift)
print(p_perf_thres_item_final)
print(p_perf_thres_schema)
print(p_perf_w)
print(p_perf_phi)
print(p_perf_theta_shift)

#========================
# 6. Save figures
#========================
fig_dir <- "../results/param_scan_20260412/figures_split"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

ggsave(file.path(fig_dir, "AC_box_line_thres_item_final.png"), p_ac_thres_item_final, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "AC_box_line_thres_schema.png"), p_ac_thres_schema, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "AC_box_line_w.png"), p_ac_w, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "AC_box_line_phi.png"), p_ac_phi, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "AC_box_line_theta_shift.png"), p_ac_theta_shift, width = 7, height = 5, dpi = 300)

ggsave(file.path(fig_dir, "RT_box_line_thres_item_final.png"), p_rt_thres_item_final, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "RT_box_line_thres_schema.png"), p_rt_thres_schema, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "RT_box_line_w.png"), p_rt_w, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "RT_box_line_phi.png"), p_rt_phi, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "RT_box_line_theta_shift.png"), p_rt_theta_shift, width = 7, height = 5, dpi = 300)

ggsave(file.path(fig_dir, "Performance_box_line_thres_item_final.png"), p_perf_thres_item_final, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "Performance_box_line_thres_schema.png"), p_perf_thres_schema, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "Performance_box_line_w.png"), p_perf_w, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "Performance_box_line_phi.png"), p_perf_phi, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "Performance_box_line_theta_shift.png"), p_perf_theta_shift, width = 7, height = 5, dpi = 300)

cat("✅ Split figures saved to:", fig_dir, "\n")

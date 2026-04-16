library(tidyverse)

#========================
# 1. Set results directory
#========================
results_dir <- "results/param_scan_20260412"

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
    param_col  <- paste0("param_", param_name)
    
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
        se_AC = ifelse(sum(!is.na(rep_mean_AC)) > 1,
                       sd(rep_mean_AC, na.rm = TRUE) / sqrt(sum(!is.na(rep_mean_AC))),
                       NA_real_),
        
        mean_performance = mean(rep_mean_performance, na.rm = TRUE),
        se_performance = ifelse(sum(!is.na(rep_mean_performance)) > 1,
                                sd(rep_mean_performance, na.rm = TRUE) / sqrt(sum(!is.na(rep_mean_performance))),
                                NA_real_),
        
        mean_RT = mean(rep_mean_RT, na.rm = TRUE),
        se_RT = ifelse(sum(!is.na(rep_mean_RT)) > 1,
                       sd(rep_mean_RT, na.rm = TRUE) / sqrt(sum(!is.na(rep_mean_RT))),
                       NA_real_),
        
        n_reps = n(),
        .groups = "drop"
      ) %>%
      mutate(parameter = param_name)
    
    summary_all[[param_name]] <- final_summary
  }
  
  bind_rows(summary_all)
}

summary_df <- make_summary_table(results_dir)

#========================
# 3. Set factor levels for consistent ordering
#========================
summary_df <- summary_df %>%
  mutate(
    parameter = factor(parameter,
                       levels = c("w", "Phi", "thres_item_final", "thres_schema", "theta_shift"))
  )

#========================
# 4. Get raw rep-level data for boxplots
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

rep_df <- make_rep_data(results_dir)

#========================
# 5. Boxplot + Line plot function (AC)
#========================
plot_box_line_ac <- function(data, rep_data) {
  
  # Line data (means across reps)
  line_data <- data %>%
    group_by(parameter, param_value) %>%
    summarise(mean_AC = mean(mean_AC, na.rm = TRUE), .groups = "drop")
  
  # Boxplot data (raw rep-level values)
  box_data <- rep_data %>%
    select(parameter, param_value, AC)
  
  ggplot() +
    # Boxplot
    geom_boxplot(data = box_data, 
                 aes(x = as.numeric(factor(param_value)), 
                     y = AC, 
                     group = param_value),
                 fill = "grey80", alpha = 0.5, outlier.size = 0.5, width = 0.6) +
    # Line
    geom_line(data = line_data,
              aes(x = as.numeric(factor(param_value)), y = mean_AC),
              color = "steelblue", linewidth = 1.2) +
    # Points on line
    geom_point(data = line_data,
               aes(x = as.numeric(factor(param_value)), y = mean_AC),
               color = "steelblue", size = 2.5) +
    # Custom x-axis labels
    scale_x_continuous(
      breaks = seq_along(unique(box_data$param_value)),
      labels = sort(unique(box_data$param_value))
    ) +
    facet_wrap(~ parameter, scales = "free_x", ncol = 3) +
    labs(x = "Parameter value", y = "Mean AC",
         title = "Effects of parameter changes on AC (boxplot + line)") +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#========================
# 6. Boxplot + Line plot function (RT)
#========================
plot_box_line_rt <- function(data, rep_data) {
  
  line_data <- data %>%
    group_by(parameter, param_value) %>%
    summarise(mean_RT = mean(mean_RT, na.rm = TRUE), .groups = "drop")
  
  box_data <- rep_data %>%
    select(parameter, param_value, RT)
  
  ggplot() +
    geom_boxplot(data = box_data, 
                 aes(x = as.numeric(factor(param_value)), 
                     y = RT, 
                     group = param_value),
                 fill = "grey80", alpha = 0.5, outlier.size = 0.5, width = 0.6) +
    geom_line(data = line_data,
              aes(x = as.numeric(factor(param_value)), y = mean_RT),
              color = "darkorange", linewidth = 1.2) +
    geom_point(data = line_data,
               aes(x = as.numeric(factor(param_value)), y = mean_RT),
               color = "darkorange", size = 2.5) +
    scale_x_continuous(
      breaks = seq_along(unique(box_data$param_value)),
      labels = sort(unique(box_data$param_value))
    ) +
    facet_wrap(~ parameter, scales = "free_x", ncol = 3) +
    labs(x = "Parameter value", y = "Mean RT (s)",
         title = "Effects of parameter changes on RT (boxplot + line)") +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#========================
# 7. Boxplot + Line plot function (Performance, split by condition)
#========================
plot_box_line_perf <- function(data, rep_data) {
  
  line_data <- data %>%
    group_by(parameter, param_value, type) %>%
    summarise(mean_perf = mean(mean_performance, na.rm = TRUE), .groups = "drop")
  
  box_data <- rep_data %>%
    select(parameter, param_value, type, performance)
  
  # Get unique param values for x-axis labeling
  param_levels <- box_data %>%
    group_by(parameter) %>%
    summarise(values = list(sort(unique(param_value))), .groups = "drop")
  
  ggplot() +
    geom_boxplot(data = box_data, 
                 aes(x = as.numeric(factor(param_value)), 
                     y = performance, 
                     group = interaction(param_value, type),
                     fill = type),
                 alpha = 0.3, outlier.size = 0.5, width = 0.6,
                 position = position_dodge(0.8)) +
    geom_line(data = line_data,
              aes(x = as.numeric(factor(param_value)), y = mean_perf, 
                  color = type, group = type),
              linewidth = 1.2) +
    geom_point(data = line_data,
               aes(x = as.numeric(factor(param_value)), y = mean_perf, 
                   color = type),
               size = 2.5) +
    scale_x_continuous(
      breaks = seq_along(unique(box_data$param_value)),
      labels = sort(unique(box_data$param_value))
    ) +
    scale_fill_manual(values = c("Lc" = "#10B981", "Hc" = "#EF4444"), 
                      name = "Risk") +
    scale_color_manual(values = c("Lc" = "#10B981", "Hc" = "#EF4444"),
                       name = "Risk") +
    facet_wrap(~ parameter, scales = "free_x", ncol = 3) +
    labs(x = "Parameter value", y = "Mean performance",
         title = "Effects of parameter changes on performance (boxplot + line)") +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#========================
# 8. Generate plots
#========================

p_ac_box <- plot_box_line_ac(summary_df, rep_df)
p_rt_box <- plot_box_line_rt(summary_df, rep_df)
p_perf_box <- plot_box_line_perf(summary_df, rep_df)

# Display
print(p_ac_box)
print(p_rt_box)
print(p_perf_box)

#========================
# 9. Save figures
#========================

# Create figures directory
fig_dir <- "results/param_scan_20260412/figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

ggsave(file.path(fig_dir, "AC_box_line.png"), p_ac_box, width = 12, height = 7, dpi = 300)
ggsave(file.path(fig_dir, "RT_box_line.png"), p_rt_box, width = 12, height = 7, dpi = 300)
ggsave(file.path(fig_dir, "Performance_box_line.png"), p_perf_box, width = 12, height = 7, dpi = 300)

cat("✅ Figures saved to:", fig_dir, "\n")
library(tidyverse)

source("model_functions_modified_risk_v2.R")

set.seed(123)

# =========================
# 1. Baseline parameters
# =========================
baseline_params <- list(
  a_schema = 0.2,
  h_schema = 1000,
  Beta_N = 0.2,
  Beta_Var = 0.3,
  a_generic = 0.1,
  h_generic = 1500,
  Beta_gN = 0.1,
  Beta_gVar = 0.2,
  w = 0.5,
  Phi = 20,
  decay_speed = 0.999,
  decay_speed_thres = 0.999,
  thres_item_inter = 6,
  thres_item_final = 15,
  thres_schema = 30,
  theta_shift = 3,
  timevar = 0.0001,
  modeltimestep = 0.061
)

# =========================
# 2. 19 parameter combinations
# =========================
bonus_combinations <- expand.grid(
  thres_schema = c(40, 45, 50),
  thres_item_final = c(40, 45, 50),
  strategy = "bonus_oriented",
  stringsAsFactors = FALSE
)

payoff_combinations <- expand.grid(
  thres_schema = c(15, 20, 25),
  thres_item_final = c(10, 12.5, 15),
  strategy = "payoff_oriented",
  stringsAsFactors = FALSE
)

baseline_row <- data.frame(
  thres_schema = 30,
  thres_item_final = 15,
  strategy = "baseline",
  stringsAsFactors = FALSE
)

all_combinations <- bind_rows(bonus_combinations, payoff_combinations, baseline_row) %>%
  mutate(combo_id = row_number())

cat(sprintf("Total parameter combinations: %d\n", nrow(all_combinations)))

# =========================
# 3. Build Param.df
# =========================
make_param_df <- function(thres_schema_val, thres_item_final_val) {
  p <- baseline_params
  p$thres_schema <- thres_schema_val
  p$thres_item_final <- thres_item_final_val
  
  tibble(
    Subject = 1,
    a_schema = p$a_schema,
    h_schema = p$h_schema,
    Beta_N = p$Beta_N,
    Beta_Var = p$Beta_Var,
    a_generic = p$a_generic,
    h_generic = p$h_generic,
    Beta_gN = p$Beta_gN,
    Beta_gVar = p$Beta_gVar,
    w = p$w,
    Phi = p$Phi,
    decay_speed = p$decay_speed,
    decay_speed_thres = p$decay_speed_thres,
    thres_item_inter = p$thres_item_inter,
    thres_item_final = p$thres_item_final,
    thres_schema = p$thres_schema,
    theta_shift = p$theta_shift,
    timevar = p$timevar,
    modeltimestep = p$modeltimestep
  )
}

# =========================
# 4. Single simulation with retry
# =========================
run_one <- function(param_df, strategy, combo_id, risk_type, adapted_flag, run_id, max_retries = 3) {
  
  set.seed(1000 + run_id * 100 + combo_id * 10 + as.numeric(as.factor(risk_type)) * 10000)
  
  for (attempt in 1:max_retries) {
    res <- tryCatch({
      simulation(
        Param.df = param_df,
        type = risk_type,
        exp_type = "painting",
        save = FALSE,
        sim.mode = "whole",
        scale.confi.init = FALSE,
        use_risk_adaptation = adapted_flag,
        risk_low_multiplier = 0.9
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(res) && nrow(res$allresult_processed) > 0) {
      df <- res$allresult_processed %>%
        mutate(
          strategy = strategy,
          combo_id = combo_id,
          thres_schema = param_df$thres_schema[1],
          thres_item_final = param_df$thres_item_final[1],
          model = ifelse(adapted_flag, "adapted", "original"),
          risk = risk_type,
          run = run_id,
          attempt = attempt
        )
      return(list(success = TRUE, data = df, attempts = attempt))
    }
  }
  
  return(list(success = FALSE, data = NULL, attempts = max_retries))
}

# =========================
# 5. Main loop with save on the fly
# =========================
n_sims <- 10
types <- c("Lc", "Hc")

output_dir <- "../results/final_v2_combo19"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(output_dir, "all_results.csv")
timeout_log <- file.path(output_dir, "timeout_log.txt")

# Initialize timeout counter
timeout_count <- 0

# Check existing progress
completed_keys <- c()
if (file.exists(output_file)) {
  existing <- read_csv(output_file, show_col_types = FALSE)
  completed_keys <- paste(existing$combo_id, existing$risk, existing$model, existing$run)
  cat(sprintf("Found existing data with %d rows\n", nrow(existing)))
}

total_runs <- nrow(all_combinations) * length(types) * 2 * n_sims
run_count <- 0

for (i in seq_len(nrow(all_combinations))) {
  this_combo <- all_combinations[i, ]
  param_df <- make_param_df(this_combo$thres_schema, this_combo$thres_item_final)
  
  for (risk_type in types) {
    for (adapt_flag in c(FALSE, TRUE)) {
      for (rep_id in seq_len(n_sims)) {
        run_count <- run_count + 1
        
        key <- paste(this_combo$combo_id, risk_type, ifelse(adapt_flag, "adapted", "original"), rep_id)
        
        if (key %in% completed_keys) {
          cat(sprintf("[%d/%d] SKIP: combo=%d risk=%s model=%s rep=%d\n",
                      run_count, total_runs, this_combo$combo_id, risk_type, 
                      ifelse(adapt_flag, "adapted", "original"), rep_id))
          next
        }
        
        cat(sprintf("[%d/%d] RUN: combo=%d (%s) schema=%.1f final=%.1f risk=%s model=%s rep=%d\n",
                    run_count, total_runs, this_combo$combo_id, this_combo$strategy,
                    this_combo$thres_schema, this_combo$thres_item_final,
                    risk_type, ifelse(adapt_flag, "adapted", "original"), rep_id))
        
        result <- run_one(
          param_df = param_df,
          strategy = this_combo$strategy,
          combo_id = this_combo$combo_id,
          risk_type = risk_type,
          adapted_flag = adapt_flag,
          run_id = rep_id,
          max_retries = 3
        )
        
        if (result$success) {
          write_csv(result$data, output_file, append = TRUE)
          cat(sprintf("  SUCCESS (attempts=%d)\n", result$attempts))
        } else {
          timeout_count <- timeout_count + 1
          cat(sprintf("  TIMEOUT (after %d attempts)\n", result$attempts))
          write(sprintf("combo=%d,risk=%s,model=%s,rep=%d", 
                        this_combo$combo_id, risk_type, 
                        ifelse(adapt_flag, "adapted", "original"), rep_id),
                file = timeout_log, append = TRUE)
        }
      }
    }
  }
}

cat("\n========== COMPLETED ==========\n")
cat(sprintf("Total timeout count: %d\n", timeout_count))
cat(sprintf("Results saved to: %s\n", output_file))
cat(sprintf("Timeout log saved to: %s\n", timeout_log))

# =========================
# 6. Load and summarize results
# =========================
all_results <- read_csv(output_file, show_col_types = FALSE)

run_summary <- all_results %>%
  mutate(total_RT = RT_1 + RT_2 + RT_3 + RT_4) %>%
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

print(strategy_summary)

# Save summaries
write_csv(run_summary, file.path(output_dir, "run_summary.csv"))
write_csv(combo_summary, file.path(output_dir, "combo_summary.csv"))
write_csv(strategy_summary, file.path(output_dir, "strategy_summary.csv"))

cat("\nAll summaries saved to:", output_dir, "\n")
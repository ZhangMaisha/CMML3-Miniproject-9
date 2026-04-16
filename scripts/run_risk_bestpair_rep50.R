library(tidyverse)

source("model_functions_modified.R")

set.seed(123)

# =========================
# 1) Baseline parameters
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
# 2) Best-pair combinations only
# =========================
bestpair_combinations <- bind_rows(
  tibble(
    combo_id = 1L,
    strategy = "bonus_oriented",
    thres_schema = 50,
    thres_item_final = 45
  ),
  tibble(
    combo_id = 2L,
    strategy = "payoff_oriented",
    thres_schema = 25,
    thres_item_final = 10
  )
)

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

run_one <- function(param_df, strategy, combo_id, risk_type, adapted_flag, run_id, max_retries = 3) {
  set.seed(1000 + run_id * 100 + combo_id * 10 + as.numeric(as.factor(risk_type)) * 10000)

  for (attempt in seq_len(max_retries)) {
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
      NULL
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

  list(success = FALSE, data = NULL, attempts = max_retries)
}

# =========================
# 3) Main loop (resume-safe)
# =========================
n_sims <- 50
types <- c("Lc", "Hc")
model_flags <- c(FALSE, TRUE) # original, adapted

output_dir <- "../results/final_bestpair"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(output_dir, "all_results.csv")
run_summary_file <- file.path(output_dir, "run_summary.csv")
timeout_log <- file.path(output_dir, "timeout_log.txt")

completed_keys <- character(0)
if (file.exists(output_file)) {
  existing <- read_csv(output_file, show_col_types = FALSE)
  completed_keys <- existing %>%
    distinct(combo_id, risk, model, run) %>%
    transmute(key = paste(combo_id, risk, model, run)) %>%
    pull(key)
  cat(sprintf("Found existing results: %d unique run keys\n", length(completed_keys)))
}

total_runs <- nrow(bestpair_combinations) * length(types) * length(model_flags) * n_sims
run_count <- 0L
timeout_count <- 0L

for (i in seq_len(nrow(bestpair_combinations))) {
  this_combo <- bestpair_combinations[i, ]
  param_df <- make_param_df(this_combo$thres_schema, this_combo$thres_item_final)

  for (risk_type in types) {
    for (adapt_flag in model_flags) {
      model_name <- ifelse(adapt_flag, "adapted", "original")
      for (rep_id in seq_len(n_sims)) {
        run_count <- run_count + 1L

        key <- paste(this_combo$combo_id, risk_type, model_name, rep_id)
        if (key %in% completed_keys) {
          cat(sprintf("[%d/%d] SKIP combo=%d strategy=%s risk=%s model=%s rep=%d\n",
                      run_count, total_runs, this_combo$combo_id, this_combo$strategy,
                      risk_type, model_name, rep_id))
          next
        }

        cat(sprintf("[%d/%d] RUN  combo=%d strategy=%s schema=%.1f final=%.1f risk=%s model=%s rep=%d\n",
                    run_count, total_runs, this_combo$combo_id, this_combo$strategy,
                    this_combo$thres_schema, this_combo$thres_item_final,
                    risk_type, model_name, rep_id))

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
          write_csv(result$data, output_file, append = file.exists(output_file))
          cat(sprintf("  SUCCESS attempts=%d\n", result$attempts))
        } else {
          timeout_count <- timeout_count + 1L
          cat(sprintf("  TIMEOUT after %d attempts\n", result$attempts))
          write(
            sprintf("combo=%d,strategy=%s,risk=%s,model=%s,rep=%d",
                    this_combo$combo_id, this_combo$strategy, risk_type, model_name, rep_id),
            file = timeout_log,
            append = TRUE
          )
        }
      }
    }
  }
}

cat("\n========== RUN FINISHED ==========\n")
cat(sprintf("Timeout count: %d\n", timeout_count))
cat(sprintf("Raw output: %s\n", output_file))

if (file.exists(output_file)) {
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

  write_csv(run_summary, run_summary_file)
  cat(sprintf("Run summary saved: %s\n", run_summary_file))
}

# ============================================
# Agent Comparison: Bonus-oriented vs Payoff-oriented
# ============================================

library(tidyverse)
source("model_functions_modified.R")

# ============================================
# 1. Baseline parameters
# ============================================

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

# ============================================
# 2. Define parameter combinations
# ============================================

# Bonus-oriented (high thresholds)
bonus_combinations <- expand.grid(
  thres_schema = c(40, 45, 50),
  thres_item_final = c(40, 45, 50),
  strategy = "bonus_oriented",
  stringsAsFactors = FALSE
)

# Payoff-oriented (low thresholds)
payoff_combinations <- expand.grid(
  thres_schema = c(15, 20, 25),
  thres_item_final = c(10, 12.5, 15),
  strategy = "payoff_oriented",
  stringsAsFactors = FALSE
)

# Baseline
baseline_row <- data.frame(
  thres_schema = 30,
  thres_item_final = 15,
  strategy = "baseline",
  stringsAsFactors = FALSE
)

# Combine all
all_combinations <- bind_rows(bonus_combinations, payoff_combinations, baseline_row)

cat(sprintf("Total parameter combinations: %d\n", nrow(all_combinations)))
print(all_combinations)

# ============================================
# 3. Run single simulation
# ============================================

run_single <- function(params, type, rep_id) {
  
  set.seed(1000 + rep_id * 100 + as.numeric(as.factor(type)) * 10000)
  
  Param.df <- data.frame(
    Subject = 1,
    a_schema = params$a_schema,
    h_schema = params$h_schema,
    Beta_N = params$Beta_N,
    Beta_Var = params$Beta_Var,
    a_generic = params$a_generic,
    h_generic = params$h_generic,
    Beta_gN = params$Beta_gN,
    Beta_gVar = params$Beta_gVar,
    w = params$w,
    Phi = params$Phi,
    decay_speed = params$decay_speed,
    decay_speed_thres = params$decay_speed_thres,
    thres_item_inter = params$thres_item_inter,
    thres_item_final = params$thres_item_final,
    thres_schema = params$thres_schema,
    theta_shift = params$theta_shift,
    timevar = params$timevar,
    modeltimestep = params$modeltimestep
  )
  
  res <- simulation(
    Param.df = Param.df,
    type = type,
    exp_type = "painting",
    save = FALSE,
    sim.mode = "whole",
    scale.confi.init = FALSE
  )
  
  allresult <- res$allresult_processed
  allresult$strategy <- params$strategy
  allresult$thres_schema <- params$thres_schema
  allresult$thres_item_final <- params$thres_item_final
  allresult$type <- type
  allresult$rep <- rep_id
  
  return(allresult)
}

# ============================================
# 4. Run all simulations
# ============================================

n_reps <- 10
types <- c("Lc", "Hc")

results <- list()
total_runs <- nrow(all_combinations) * length(types) * n_reps
run_count <- 0

start_time <- Sys.time()

for (i in 1:nrow(all_combinations)) {
  for (type in types) {
    for (rep in 1:n_reps) {
      
      run_count <- run_count + 1
      current_params <- baseline_params
      current_params$thres_schema <- all_combinations$thres_schema[i]
      current_params$thres_item_final <- all_combinations$thres_item_final[i]
      current_params$strategy <- all_combinations$strategy[i]
      
      cat(sprintf("[%d/%d] %s, %s, rep=%d\n", 
                  run_count, total_runs,
                  current_params$strategy, type, rep))
      
      res <- tryCatch({
        run_single(current_params, type, rep)
      }, error = function(e) {
        cat("  Error:", e$message, "\n")
        return(NULL)
      })
      
      if (!is.null(res)) {
        results[[length(results) + 1]] <- res
      }
    }
  }
}

end_time <- Sys.time()
cat(sprintf("\nCompleted in %.2f minutes\n", difftime(end_time, start_time, units = "mins")))

# ============================================
# 5. Save results
# ============================================

all_results <- bind_rows(results)

# Create output directory
output_dir <- "results/agent_comparison"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save raw data
write_csv(all_results, file.path(output_dir, "agent_results_raw.csv"))


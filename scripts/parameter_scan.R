# ============================================
# Parameter Scan Script (Single-parameter testing)
# ============================================

library(tidyverse)
source("model_functions_modified.R")

# ============================================
# 1. Define baseline parameters
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
# 2. Define parameters to test
# ============================================

test_parameters <- list(
  w = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9),
  Phi = c(1, 5, 10, 20, 40),
  thres_item_final = c(10, 15, 20, 30, 40, 50),
  thres_schema = c(10, 20, 25, 30, 35, 40, 50),
  theta_shift = c(1, 2, 3, 5, 7, 10)
)

# ============================================
# 3. Function for a single simulation run
# ============================================

run_single <- function(params, type, rep_id, save_path = NULL) {
  
  # Ensure reproducibility across repeated simulations
  set.seed(1000 + rep_id)
  
  # Construct parameter dataframe for the model
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
  
  # Run simulation (no file saving, results returned directly)
  res <- simulation(
    Param.df = Param.df,
    type = type,
    exp_type = "painting",
    save = FALSE,
    savepath = save_path,
    sim.mode = "whole",
    scale.confi.init = FALSE
  )
  
  # Extract results and attach metadata
  allresult <- res$allresult_processed
  allresult$param_w <- params$w
  allresult$param_Phi <- params$Phi
  allresult$param_thres_item_final <- params$thres_item_final
  allresult$param_thres_schema <- params$thres_schema
  allresult$param_theta_shift <- params$theta_shift
  allresult$type <- type
  allresult$rep <- rep_id
  
  return(allresult)
}

# ============================================
# 4. Test all values of a single parameter
# ============================================

test_one_parameter <- function(param_name, param_values, 
                               n_reps = 10, 
                               types = c("Lc", "Hc"),
                               output_dir = "results/param_scan") {
  
  # Create output directory if it does not exist
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Store results
  all_results <- list()
  
  for (val in param_values) {
    for (type in types) {
      for (rep in 1:n_reps) {
        
        cat(sprintf("Running: %s = %s, type = %s, rep = %d\n", 
                    param_name, val, type, rep))
        
        # Copy baseline parameters and modify the target parameter
        current_params <- baseline_params
        current_params[[param_name]] <- val
        
        # IMPORTANT: thres_item_inter stays FIXED at 6, no proportional adjustment
        # (Do nothing here - inter remains at baseline value 6)
        
        # Run simulation with error handling
        result <- tryCatch({
          run_single(current_params, type, rep,
                     save_path = file.path(output_dir, "temp"))
        }, error = function(e) {
          cat("  Error:", e$message, "\n")
          return(NULL)
        })
        
        if (!is.null(result)) {
          all_results[[length(all_results) + 1]] <- result
        }
      }
    }
  }
  
  # Combine results and save to CSV
  if (length(all_results) > 0) {
    combined <- do.call(rbind, all_results)
    
    output_file <- file.path(output_dir, paste0(param_name, "_scan.csv"))
    write_csv(combined, output_file)
    cat(sprintf("\nSaved: %s\n", output_file))
    
    return(combined)
  } else {
    warning("No results collected for parameter: ", param_name)
    return(NULL)
  }
}

# ============================================
# 5. Run parameter scans
# ============================================

# Create main output directory
main_output_dir <- "results/param_scan_20260412"
if (!dir.exists(main_output_dir)) dir.create(main_output_dir, recursive = TRUE)

# Record start time
start_time <- Sys.time()

# Run parameter scans
results_list <- list()

# Test w
results_list$w <- test_one_parameter(
  "w", 
  c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9),
  n_reps = 10,
  types = c("Lc", "Hc"),
  output_dir = main_output_dir
)

# Test Phi
results_list$Phi <- test_one_parameter(
  "Phi", 
  c(1, 5, 10, 20, 40),
  n_reps = 10,
  types = c("Lc", "Hc"),
  output_dir = main_output_dir
)

# Test thres_item_final
results_list$thres_item_final <- test_one_parameter(
  "thres_item_final", 
  c(10, 15, 20, 30, 40, 50),
  n_reps = 10,
  types = c("Lc", "Hc"),
  output_dir = main_output_dir
)

# Test thres_schema
results_list$thres_schema <- test_one_parameter(
  "thres_schema", 
  c(10, 20, 25, 30, 35, 40, 50),
  n_reps = 10,
  types = c("Lc", "Hc"),
  output_dir = main_output_dir
)

# Test theta_shift
results_list$theta_shift <- test_one_parameter(
  "theta_shift", 
  c(1, 2, 3, 5, 7, 10),
  n_reps = 10,
  types = c("Lc", "Hc"),
  output_dir = main_output_dir
)

# Compute total runtime
end_time <- Sys.time()
cat("\n========== All simulations completed ==========\n")
cat(sprintf("Total runtime: %.2f minutes\n", difftime(end_time, start_time, units = "mins")))

# ============================================
# 6. Summary analysis (optional)
# ============================================

summarize_results <- function(results_dir) {
  
  csv_files <- list.files(results_dir, pattern = "_scan.csv", full.names = TRUE)
  
  summary_list <- list()
  
  for (file in csv_files) {
    df <- read_csv(file, show_col_types = FALSE)
    
    # Identify parameter name
    param_name <- gsub("_scan.csv", "", basename(file))
    
    # First average within each repetition, then across repetitions
    summary <- df %>%
      group_by(rep, !!sym(paste0("param_", param_name)), type) %>%
      summarise(
        rep_mean_AC = mean(AC, na.rm = TRUE),
        rep_mean_performance = mean(performance, na.rm = TRUE),
        rep_mean_schema_payoff = mean(schema_payoff, na.rm = TRUE),
        rep_mean_RT_total = mean(RT_1 + RT_2 + RT_3 + RT_4, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(!!sym(paste0("param_", param_name)), type) %>%
      summarise(
        mean_AC = mean(rep_mean_AC, na.rm = TRUE),
        se_AC = sd(rep_mean_AC, na.rm = TRUE) / sqrt(sum(!is.na(rep_mean_AC))),
        
        mean_performance = mean(rep_mean_performance, na.rm = TRUE),
        se_performance = sd(rep_mean_performance, na.rm = TRUE) / sqrt(sum(!is.na(rep_mean_performance))),
        
        mean_schema_payoff = mean(rep_mean_schema_payoff, na.rm = TRUE),
        se_schema_payoff = sd(rep_mean_schema_payoff, na.rm = TRUE) / sqrt(sum(!is.na(rep_mean_schema_payoff))),
        
        mean_RT_total = mean(rep_mean_RT_total, na.rm = TRUE),
        se_RT_total = sd(rep_mean_RT_total, na.rm = TRUE) / sqrt(sum(!is.na(rep_mean_RT_total))),
        
        n_reps = sum(!is.na(rep_mean_AC)),
        .groups = "drop"
      )
    
    summary_list[[param_name]] <- summary
  }
  
  return(summary_list)
}

summary_results <- summarize_results(main_output_dir)

# Print summary tables
for (param in names(summary_results)) {
  cat("\n========== ", param, " ==========\n")
  print(summary_results[[param]])
}

# Save summary results
saveRDS(summary_results, file.path(main_output_dir, "summary_results.rds"))
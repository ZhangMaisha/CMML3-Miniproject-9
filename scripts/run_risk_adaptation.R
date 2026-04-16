library(tidyverse)

source("model_functions_modified_risk_v2.R")

set.seed(123)

# =========================
# 1. 基础参数
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
# 2. 构建 agent 参数
# =========================
make_param_df <- function(override = list()) {
  p <- modifyList(baseline_params, override)
  
  tibble(
    Subject = 1,  # 单次模拟
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

agents <- list(
  baseline = make_param_df(),
  bonus = make_param_df(list(
    thres_schema = 50,
    thres_item_final = 45
  )),
  payoff = make_param_df(list(
    thres_schema = 25,
    thres_item_final = 10
  ))
)

# =========================
# 3. 单次运行函数
# =========================
run_one <- function(param_df, agent_name, risk_type, adapted_flag, run_id) {
  
  res <- simulation(
    Param.df = param_df,
    type = risk_type,
    exp_type = "painting",
    save = FALSE,
    sim.mode = "whole",
    scale.confi.init = FALSE,
    use_risk_adaptation = adapted_flag,
    risk_low_multiplier = 0.9
  )
  
  df <- res$allresult_processed %>%
    mutate(
      agent = agent_name,
      model = ifelse(adapted_flag, "adapted", "original"),
      risk = risk_type,
      run = run_id
    )
  
  return(df)
}

# =========================
# 4. 重复模拟（核心）
# =========================
n_sims <- 20

all_results <- list()

counter <- 1

for (agent_name in names(agents)) {
  for (risk_type in c("Lc", "Hc")) {
    for (adapt_flag in c(FALSE, TRUE)) {
      for (i in 1:n_sims) {
        
        cat("Running:", agent_name, risk_type,
            ifelse(adapt_flag, "adapted", "original"),
            "rep", i, "\n")
        
        tmp <- run_one(
          param_df = agents[[agent_name]],
          agent_name = agent_name,
          risk_type = risk_type,
          adapted_flag = adapt_flag,
          run_id = i
        )
        
        all_results[[counter]] <- tmp
        counter <- counter + 1
      }
    }
  }
}

all_results <- bind_rows(all_results)

# =========================
# 5. 汇总（按 run）
# =========================
run_summary <- all_results %>%
  mutate(total_RT = RT_1 + RT_2 + RT_3 + RT_4) %>%
  group_by(model, agent, risk, run) %>%
  summarise(
    AC = mean(AC, na.rm = TRUE),
    performance = mean(performance, na.rm = TRUE),
    RT = mean(total_RT, na.rm = TRUE),
    .groups = "drop"
  )

# =========================
# 6. 最终均值 + SE
# =========================
final_summary <- run_summary %>%
  group_by(model, agent, risk) %>%
  summarise(
    AC_mean = mean(AC),
    AC_se = sd(AC)/sqrt(n()),
    perf_mean = mean(performance),
    perf_se = sd(performance)/sqrt(n()),
    RT_mean = mean(RT),
    RT_se = sd(RT)/sqrt(n()),
    .groups = "drop"
  )

print(final_summary)

# =========================
# 7. 保存
# =========================
dir.create("results/final", recursive = TRUE, showWarnings = FALSE)

write_csv(all_results, "results/final/all_results.csv")
write_csv(run_summary, "results/final/run_summary.csv")
write_csv(final_summary, "results/final/final_summary.csv")

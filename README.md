# CMML3 Miniproject 9

Computational model-based analysis of schema-based learning and decision-making (Task 1 focus).

## Project Structure

- `scripts/`: all R scripts and model functions
- `scripts/data/`: input schema files used by simulations
- `results/`: generated outputs
  - `results/param_scan/`: parameter scan outputs
  - `results/agent_comparison/`: bonus/payoff strategy comparison outputs
  - `results/final/`: full-space risk-adaptation detailed analysis outputs (Fig 24/26/27)
  - `results/final_bestpair/`: best-pair rep50 outputs (Fig 31/32/33)

## Environment

R packages used:

- `tidyverse`
- `ggplot2`
- `patchwork`

Install if needed:

```r
install.packages(c("tidyverse", "ggplot2", "patchwork"))
```

## Scripts and Usage

Run from project root (`miniproject9/`):

### 1) Parameter scan

- `scripts/parameter_scan.R`
  - Runs scan on key parameters (`w`, `Phi`, `thres_item_final`, `thres_schema`, `theta_shift`)
  - Writes raw scan tables into `results/param_scan/`

- `scripts/parameter_scan_plot_split.R`
  - Reads scan outputs and generates split figures in `results/param_scan/figures_split/`
  - Includes threshold figures and 3-panel combined figures for `w/Phi/theta_shift`

Run:

```bash
Rscript scripts/parameter_scan.R
Rscript scripts/parameter_scan_plot_split.R
```

### 2) Agent comparison (bonus vs payoff)

- `scripts/agent_compare.R`
  - Generates simulation data and saves `agent_results_raw.csv`

- `scripts/run_agent_analysis.R`
  - Produces boxplots and t-test summary (`statistical_tests_Lc.txt`)
  - Main figures saved in `results/agent_comparison/figures/`

Run:

```bash
Rscript scripts/agent_compare.R
Rscript scripts/run_agent_analysis.R
```

### 3) Risk-adaptation analysis (full parameter space)

- `scripts/run_risk_adaptation_detailed_analysis.R`
  - Uses `results/final/run_summary.csv`
  - Produces key outputs: `24_v2_strategy_risk_interaction_detailed.png`,
    `26_v2_strategy_did_detailed.png`, `27_v2_combo_dod_heatmap_perf.png`

Run:

```bash
Rscript scripts/run_risk_adaptation_detailed_analysis.R
```

### 4) Best-pair rep50 analysis

- `scripts/run_risk_bestpair_rep50.R`
  - Runs best-pair simulations (`original` + `adapted`, `Lc/Hc`, rep=50)
  - Saves to `results/final_bestpair/all_results.csv` and `run_summary.csv`

- `scripts/run_risk_bestpair_analysis.R`
  - Analyzes best-pair outputs and generates:
    - `31_bestpair_overall_rep50.png`
    - `32_bestpair_interaction_lines_rep50.png`
    - `33_bestpair_did_rep50.png`

Run:

```bash
Rscript scripts/run_risk_bestpair_rep50.R
Rscript scripts/run_risk_bestpair_analysis.R
```

## Core Model Files

- `scripts/model_functions_original.R`: instructor-provided baseline model
- `scripts/model_functions_modified.R`: working model used in this project (includes risk-modulation switch used by adapted runs)

## Notes

- Existing files in `results/` may include outputs from previous runs; rerunning scripts will overwrite/update corresponding outputs.
- This repo currently focuses on Task 1 analyses.

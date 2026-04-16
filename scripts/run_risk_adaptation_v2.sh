#!/bin/bash

# ============================================
# Run risk adaptation v2 (19 combos) with sleep prevention
# ============================================

PROJECT_DIR="$HOME/Desktop/CMML3/ICA1/scripts"
LOG_DIR="$PROJECT_DIR/logs"
SCRIPT_NAME="run_risk_adaptation_v2.R"
LOG_FILE="$LOG_DIR/risk_adaptation_v2.log"
PID_FILE="$LOG_DIR/risk_adaptation_v2.pid"

# 1. Ensure project directory exists
if [ ! -d "$PROJECT_DIR" ]; then
  echo "Project directory not found: $PROJECT_DIR"
  exit 1
fi

cd "$PROJECT_DIR" || exit 1

# 2. Ensure target script exists
if [ ! -f "$SCRIPT_NAME" ]; then
  echo "Target script not found: $PROJECT_DIR/$SCRIPT_NAME"
  exit 1
fi

# 3. Prepare log directory
mkdir -p "$LOG_DIR"

# 4. Log start
{
  echo "=========================================="
  echo "Starting risk adaptation v2 run at: $(date)"
  echo "Script: $SCRIPT_NAME"
  echo "=========================================="
} | tee -a "$LOG_FILE"

# 5. Launch R script with caffeinate to prevent sleep
nohup caffeinate -i Rscript "$SCRIPT_NAME" >> "$LOG_FILE" 2>&1 &
RISK_PID=$!
echo "$RISK_PID" > "$PID_FILE"

{
  echo "Launched risk adaptation v2 job with PID: $RISK_PID"
  echo "Logs: $LOG_FILE"
  echo "=========================================="
  echo "Monitor: tail -f \"$LOG_FILE\""
  echo "Stop:    kill $RISK_PID"
  echo "=========================================="
} | tee -a "$LOG_FILE"

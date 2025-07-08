#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# (.Keys[0]):
JQ_QUERY=$(cat <<EOF )
.ResultsByTime
  | map(
    (
      [{
        date: .TimePeriod.Start,
        total: (.Groups | map(.Metrics.BlendedCost.Amount | tonumber) | add)
      }]
      + (.Groups | map({(.Keys[0]): .Metrics.BlendedCost.Amount}))
    )
    | add
  )
EOF

aws ce get-cost-and-usage \
  --time-period Start=$(date +%Y-%m-%d -d "- 6 days"),End=$(date +%Y-%m-%d -d "+ 1 day") \
  --granularity DAILY \
  --metrics BlendedCost \
  --group-by Type=DIMENSION,Key=SERVICE | jq "$JQ_QUERY"

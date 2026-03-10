#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

regions=(us-east-1 us-east-2 us-west-1 us-west-2 ap-east-1)

for region in "${regions[@]}"; do
  # Collect instance IDs that are not terminated
  mapfile -t ids < <(
    aws ec2 describe-instances \
      --region "$region" \
      --query 'Reservations[].Instances[?State.Name!=`terminated`].InstanceId' \
      --output text
  )

  if ((${#ids[@]} == 0)); then
    echo "[$region] No instances to terminate."
    continue
  fi

  echo "[$region] Terminating: ${ids[*]}"
  aws ec2 terminate-instances --region "$region" --instance-ids "${ids[@]}" > /dev/null
done

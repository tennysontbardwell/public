#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

aws ec2 terminate-instances --instance-ids \
    $(aws ec2 describe-instances | jq -r '.Reservations[].Instances[] | select(.State.Name != "terminated") | .InstanceId') \
    | vd

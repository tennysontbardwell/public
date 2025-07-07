#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

get_instances() {
    local jq_query='
    [
        .Reservations[].Instances[]
        | select(.State.Name != "terminated")
        | {
            "InstanceId": .InstanceId,
            "InstanceType": .InstanceType,
            "State": .State.Name,
            "PublicIp": .PublicIpAddress,
            "LaunchTime": .LaunchTime
        }
    ]'

    aws ec2 describe-instances | jq "$jq_query" | qsv json | xan view
}

export -f get_instances
watch -n 60 bash -c "get_instances"

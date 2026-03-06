#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

get_instances() {
    local jq_query='
        .Reservations[].Instances[]
        | select(.State.Name != "terminated")
        | {
            "InstanceId": .InstanceId,
            "InstanceType": .InstanceType,
            "State": .State.Name,
            "PublicIp": .PublicIpAddress,
            "LaunchTime": .LaunchTime
        }
    '

    parallel --group 'aws ec2 describe-instances --region {}' \
        ::: us-{east,west}-{1,2} ap-east-1 \
        | jq "$jq_query" -c | jq -cs | qsv json | xan view
}

export -f get_instances
watch -n 10 bash -c "get_instances"

#!/bin/bash

show_usage() {
    echo "Usage: $0 [-b BROKER]"
    echo "  -b BROKER  broker_ip:broker_port"
    exit 1
}

if [ $# -eq 0 ]; then
    show_usage
fi

broker_option=""

while getopts "b:" opt; do
  case $opt in
    b)
      broker_option="$OPTARG"
      ;;
    \?)
      show_usage
      ;;
  esac
done

if [[ $OPTIND -le $# ]]; then
    echo "This script doesn't take positional arguments." >&2
    show_usage
fi

topics=$(docker exec controller kafka-topics.sh --list --bootstrap-server $broker_option)

IFS=$'\n' read -rd '' -a topic_array <<<"$topics"

# Iterate through the array of topics
for topic in "${topic_array[@]}"; do
    echo "Deleting $topic topic."
    docker exec controller kafka-topics.sh --bootstrap-server 10.10.142.81:9092 --delete --topic "$topic"
    echo "Deleted topic $topic."
done

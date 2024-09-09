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

docker exec controller kafka-topics.sh --list --bootstrap-server $broker_option
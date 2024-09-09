#!/bin/bash

show_usage() {
    echo "Usage: $0 [-b BROKER] topic0 topic1 topic2 ..."
    echo "  -b BROKER  broker_ip:broker_port"
    echo "  topic0, topic1, topic2, ...  List of topics"
    exit 1
}

if [ $# -eq 0 ]; then
    show_usage
fi

broker_option=""
topics=()

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

shift $((OPTIND - 1))

if [ $# -eq 0 ]; then
    echo "Error: You should provide at list one topic."
    show_usage
fi

topics=("$@")

for topic in "${topics[@]}"; do
    existing_topic=$(docker exec controller kafka-topics.sh --bootstrap-server $broker_option --describe --topic "$topic" 2>/dev/null)
    if [[ $existing_topic == *"Topic '$topic' does not exist"* ]]; then
        echo "Topic $topic doesn't exist."
        echo "Creating $topic topic."
        docker exec controller kafka-topics.sh --create --bootstrap-server $broker_option --replication-factor 2 --partitions 1 --topic "$topic"
    else
        echo "Topic $topic already exists."
    fi
done

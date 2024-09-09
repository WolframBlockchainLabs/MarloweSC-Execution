# Apache Kafka with environment variables using openjdk:8-alpine as base image

This is an approximation with environment variables of what the Kafka cluster will be.

First build the `kafka-img` image:

```
docker compose -f docker-compose-build.yaml build
```

To run the kafka-cluster, execute the following command:

```
docker compose -f docker-compose-up.yaml up --detach
```

To see the containers:

```
docker ps
```

Yo should see a similar output to the following:

```
CONTAINER ID   IMAGE       COMMAND                  CREATED          STATUS          PORTS                                           NAMES
664d80c24c47   kafka-img   "/bin/sh"                13 seconds ago   Up 9 seconds                                                    controller
4f91f96efd77   kafka-img   "entrypoint.sh kafka…"   14 seconds ago   Up 10 seconds   0.0.0.0:9093->9092/tcp, :::9093->9092/tcp       broker2
a04f2358d64c   kafka-img   "entrypoint.sh kafka…"   15 seconds ago   Up 11 seconds   0.0.0.0:9092->9092/tcp, :::9092->9092/tcp       broker1
fef87e7ac6f1   kafka-img   "entrypoint.sh zooke…"   16 seconds ago   Up 12 seconds   0.0.0.0:2181->2181/tcp, :::2181->2181/tcp       zookeeper
```

To create a topic named `test`:

```
docker exec controller kafka-topics.sh --create --bootstrap-server <broker_ip>:9092 --replication-factor 2 --partitions 1 --topic test
```

To check if the topics were created:

```
docker exec --interactive --tty controller kafka-topics.sh --list --bootstrap-server <broker_ip>:9092
```

To write messages (from terminal):

```
docker exec -it controller kafka-console-producer.sh --broker-list <broker_ip>:9093 --topic test
```

To see the incoming data (in another terminal):

```
docker exec --interactive --tty controller kafka-console-consumer.sh --bootstrap-server <broker_ip>:9092 --topic test --from-beginning
```

If you want to delete the topic `test`:

```
docker exec controller kafka-topics.sh --bootstrap-server <zookeeper_ip>:9092 --delete --topic test
```

To stop the containers:

```
docker compose -f docker-compose-up.yaml stop
```

To delete the containers:

```
docker compose -f docker-compose-up.yaml down
```

To delete the containers including the base image:

```
docker compose -f docker-compose-up.yaml down --rmi all
```

Observation:

Only use the terminal utilities for testing purposes. Not simultaneously with your application.
On the other hand, it is requested to avoid topic names with hyphens or any other symbols. For some reason it is giving errors when using names with those characters.



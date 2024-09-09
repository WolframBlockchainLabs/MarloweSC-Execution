FROM openjdk:8-alpine

ENV SHELL=/bin/bash

ENV KAFKA_PATH=/opt/wolfram/kafka

ENV PATH="${KAFKA_PATH}/bin:${PATH}"

RUN apk update

RUN apk upgrade

RUN apk add --no-cache bash

RUN mkdir -p ${KAFKA_PATH}

ADD https://archive.apache.org/dist/kafka/3.2.0/kafka_2.13-3.2.0.tgz ${KAFKA_PATH}

RUN tar xzvf ${KAFKA_PATH}/kafka_2.13-3.2.0.tgz -C ${KAFKA_PATH}/ --strip-components 1

RUN rm ${KAFKA_PATH}/kafka_2.13-3.2.0.tgz

COPY ./entrypoint.sh /usr/local/bin/

RUN chmod +x /usr/local/bin/entrypoint.sh

RUN addgroup -S kafkag 

RUN adduser -S kafka -G kafkag

RUN chown -R kafka:kafkag ${KAFKA_PATH}

RUN chown kafka:kafkag /usr/local/bin/entrypoint.sh

RUN mkdir /zookeeper-logs

RUN chown -R kafka:kafkag /zookeeper-logs

RUN mkdir /broker-logs

RUN chown -R kafka:kafkag /broker-logs

USER kafka

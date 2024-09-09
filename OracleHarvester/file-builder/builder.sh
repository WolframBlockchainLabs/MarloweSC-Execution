#!/usr/bin/bash
docker build . -t herodotus-img
docker run -it --name herodotus \
    -v /home/angelm/Delphos:/usr/src/app/Delphos \
    -v ./logs:/usr/src/app/logs \
    -v /var/run/docker.sock:/var/run/docker.sock \
    --network cardano-oracle_kafka-network herodotus-img

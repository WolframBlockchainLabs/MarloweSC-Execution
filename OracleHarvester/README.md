# Commands

To build images:

```bash
docker compose -f docker-compose-build.yaml build
```

To start containers:

```bash
docker compose -f docker-compose-up.yaml up -d
```

To execute listener:

```bash
docker exec listener sh -c "node ./dist/tx-listener.js"
```

To execute filter:

```bash
docker exec filter sh -c "node ./dist/filter.js"
```

To execute sink:

```bash
docker exec filter sh -c "node ./dist/sink.js"
```

To stop containers and remove images:

```bash
docker compose -f docker-compose-up.yaml down --rmi all
```

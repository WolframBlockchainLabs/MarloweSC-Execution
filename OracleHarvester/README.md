# Starting Services

## Listener, Filter and Sync

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

## File Builder and Transaction Builder


### File builder
#### Prerequisites
* Docker container witk `marlowe-cli`
* [Marlowe Runtime Server](https://docs.demeter.run/ports/marlowe)
* MongoDb instance
* Wolfram Price Feed Infrastructure instance

After declaring URI's and credentials in a .env file type:

```bash
npm install
npm run build
cd build
node app.js
```


### Transaction Builder
In a system with access to WolframScript, create a `Cron Job` with the following code:
```Bash
wolframKernel= ##Path to wolframscript
txBuilderDir= ## Path to "tx-builder" directory

* * * * * $localStorageDir/txBuilder.sh $txBuilderDir  $wolframKernel  >> /dev/null 2>&1
```
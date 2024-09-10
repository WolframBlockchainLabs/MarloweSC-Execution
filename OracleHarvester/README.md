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
* Docker container with `marlowe-cli`
* [Marlowe Runtime Server](https://docs.demeter.run/ports/marlowe)
* MongoDb instance
* Wolfram Price Feed Infrastructure instance

Declare env variables in a .env file
```bash
NETWORK = "Testnet"

MARLOW_URL = your_marlowe_runtime_server_demeter_instance
API_KEY = your_marlowe_runtime_server_demeter_API_key

MONGODB_URI = your_mongo_URI

LOCAL_STORAGE_PATH = your_local_storage_for_files

CONTAINER_NAME = "marlowe-starter-kit-notebook-1"

ROOT_DIR = your_local_directory_to_call_marlowe-cli

CCDB_BASEURL = your_CCDB_URI
```
After declaring URI's and credentials type:

```bash
npm install
npm run build
cd build
node app.js
```


### Transaction Builder
Create a file called `config.json` and declare the following variables

```json
{
    "MONGO_URI": your_mongo_URI,
    "MONGO_PORT": your_mongo_port,
    "DATABASE_NAME":your_mongo_database,
    "COLLECTION_NAME":your_mongo_collection,

    "LOCAL_STORAGE_PATH": your_local_storage_for_files,

    "ORACLE_ADDRESS": your_oracle_address
}
```
Additionally, create a `Credentials.wls` file and instantiate there the Private and Public keys for the oracle. Both must be Wolfram Language Objects. For reference check [here](https://reference.wolfram.com/language/ref/BlockchainKeyEncode.html).


In a system with access to WolframScript, create a `Cron Job` with the following code:
```Bash
wolframKernel= ##Path to wolframscript
txBuilderDir= ## Path to "tx-builder" directory

* * * * * $localStorageDir/txBuilder.sh $txBuilderDir  $wolframKernel  >> /dev/null 2>&1
```
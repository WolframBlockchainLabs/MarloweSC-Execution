# Transactions listener

To install dependencies:

```bash
npm install
```

To build source code:

```bash
npm run build
```

To run listener:

```bash
node ./dist/tx-listener.js
```

## Using Docker

To build docker image:

```bash
docker build -t listener-img .
```

To start the container:

```bash
docker run --name listener listener-img
```

To run listener from docker container:

```bash
docker exec listener sh -c "node ./dist/tx-listener.js"
```
# Transactions filter

To install dependencies:

```bash
npm install
```

To build source code:

```bash
npm run build
```

To run filter:

```bash
node ./dist/filter.js
```

To run sink:

```bash
node ./dist/sink.js
```

## Using Docker

To build docker image:

```bash
docker build -t filter-img .
```

To start the container:

```bash
docker run --name filter filter-img
```

To run filter from docker container:

```bash
docker exec listener sh -c "node ./dist/filter.js"
```

To run sink from docker container:

```bash
docker exec listener sh -c "node ./dist/sink.js"
```
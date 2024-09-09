FROM node:20.13.1-alpine AS builder

WORKDIR /usr/src/app

COPY package.json ./

RUN npm install

COPY ./src ./src

COPY ./.babelrc ./.babelrc 

RUN npm run build

FROM node:20.13.1-alpine

RUN apk add curl

WORKDIR /usr/src/app

COPY --from=builder /usr/src/app/dist ./dist

COPY ./.env ./.env

COPY ./marloweStatics ./marloweStatics

RUN addgroup app && adduser -S -G app app

RUN chown -R app:app /usr/src/app

COPY package.json ./

RUN npm install

COPY ./entrypoint.sh ./entrypoint.sh

ENTRYPOINT ["/bin/sh", "entrypoint.sh"]

CMD ["sh"]
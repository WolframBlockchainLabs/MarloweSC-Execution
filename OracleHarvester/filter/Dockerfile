FROM node:20.13.1-alpine as builder

WORKDIR /usr/src/app

COPY package.json ./

RUN npm install

COPY ./src ./src

COPY ./.babelrc ./.babelrc 

RUN npm run build

FROM node:20.13.1-alpine

WORKDIR /usr/src/app

COPY --from=builder /usr/src/app/dist ./dist

COPY ./.env ./.env

RUN addgroup app && adduser -S -G app app

RUN chown -R app:app /usr/src/app

COPY package.json ./

RUN npm install

COPY ./entrypoint.sh ./entrypoint.sh

ENTRYPOINT ["/bin/sh", "entrypoint.sh"]

CMD ["sh"]
FROM node:8.8.1-slim

RUN yarn global add elm

WORKDIR /app
COPY . /app

RUN elm-make --output=elm.js src/Main.elm

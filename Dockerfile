FROM node:8.8.1-slim

RUN yarn global add elm

WORKDIR /app
COPY . /app

RUN sed -i.bak 's/False/True/' src/Request/Helpers.elm

RUN elm-make --yes --output=elm.js src/Main.elm

FROM nginx:alpine

COPY --from=0 /app/index.html /usr/share/nginx/html/index.html
COPY --from=0 /app/elm.js /usr/share/nginx/html/elm.js
COPY --from=0 /app/assets/sprite.svg /usr/share/nginx/html/assets/sprite.svg

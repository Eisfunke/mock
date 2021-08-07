FROM debian:latest

ENV LANG C.UTF-8
COPY mock-web /usr/bin

CMD mock-web

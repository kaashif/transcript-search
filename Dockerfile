FROM ubuntu:18.04

MAINTAINER Kaashif Hymabaccus "kaashif@kaashif.co.uk"

RUN apt-get update && \
    apt-get install -y gunicorn3 python3-flask python3-psycopg2

COPY ./web /web
WORKDIR /web

EXPOSE 8000
ENV PYTHONBUFFERED=0
ENTRYPOINT [ "gunicorn3" ]
CMD [ "-b", "0.0.0.0:8000", "app:app" ]

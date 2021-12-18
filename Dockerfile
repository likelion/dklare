FROM ubuntu:20.04 as build

LABEL maintainer "likelion@gmail.com"

ENV DEBIAN_FRONTEND=noninteractive

ARG PUBLIC_HOST=localhost
ARG PUBLIC_PORT=3020

ADD config-available /opt/dklare/config-available/
ADD lib /opt/dklare/lib/
ADD rdf /opt/dklare/rdf/

COPY settings.db /opt/dklare/
COPY dklare.ttl /opt/dklare/
COPY run.pl /opt/dklare/

WORKDIR /opt/dklare

RUN apt-get update && \
    apt-get -yq --no-install-recommends install tzdata software-properties-common && \
    ln -fs /usr/share/zoneinfo/Europe/Stockholm /etc/localtime && \
    dpkg-reconfigure tzdata && \
    apt-add-repository ppa:swi-prolog/devel && \
    apt-get update && \
    apt-get -yq --no-install-recommends install swi-prolog && \
    apt-get -yq remove --purge software-properties-common && \
    apt-get -yq autoremove && \
    rm -rf /var/lib/apt/lists/* && \
    sed -i 's|%PUBLIC_HOST%|'$PUBLIC_HOST'|g' settings.db && \
    sed -i 's/%PUBLIC_PORT%/'$PUBLIC_PORT'/g' settings.db

VOLUME /opt/dklare/knowledge
VOLUME /opt/dklare/RDF-store

EXPOSE ${PUBLIC_PORT}

CMD ["swipl","run.pl"]

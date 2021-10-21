FROM ubuntu:20.04 as build

LABEL maintainer "likelion@gmail.com"

ARG PUBLIC_HOST=localhost
ARG PUBLIC_PORT=3020

ADD config-available /opt/dklare/config-available/
ADD lib /opt/dklare/lib/
ADD rdf /opt/dklare/rdf/

COPY settings.db /opt/dklare/
COPY run.pl /opt/dklare/

WORKDIR /opt/dklare

RUN apt-get update && \
    DEBIAN_FRONTEND="noninteractive" \
    apt-get -yq --no-install-recommends install tzdata software-properties-common && \
    apt-add-repository ppa:swi-prolog/devel && \
    apt-get update && \
    apt-get -yq --no-install-recommends install swi-prolog && \
    apt-get -y remove --purge software-properties-common && \
    apt-get autoremove -y && \
    rm -rf /var/lib/apt/lists/* && \
    sed -i 's|%PUBLIC_HOST%|'$PUBLIC_HOST'|g' settings.db && \
    sed -i 's/%PUBLIC_PORT%/'$PUBLIC_PORT'/g' settings.db

EXPOSE ${PUBLIC_PORT}

CMD ["swipl","run.pl"]

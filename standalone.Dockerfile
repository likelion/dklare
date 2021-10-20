FROM ubuntu:20.04 as build

LABEL maintainer "likelion@gmail.com"

ARG PUBLIC_PORT=3020

ADD config-available /opt/dklare/config-available/
ADD lib /opt/dklare/lib/
ADD rdf /opt/dklare/rdf/

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
    sed -i 's|port(3020)|port('$PUBLIC_PORT')|g' run.pl

EXPOSE ${PUBLIC_PORT}

CMD ["swipl","run.pl"]

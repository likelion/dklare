FROM ubuntu:20.04 as build

LABEL maintainer "likelion@gmail.com"

ENV DEBIAN_FRONTEND=noninteractive

ARG PUBLIC_HOST=localhost
ARG PUBLIC_PORT=3020

ADD config-available /opt/server/cpack/dklare/config-available/
ADD lib /opt/server/cpack/dklare/lib/
ADD rdf /opt/server/cpack/dklare/rdf/

COPY *.db /opt/server/
COPY dklare.ttl /opt/server/

WORKDIR /opt

RUN apt-get update && \
    apt-get -yq --no-install-recommends install tzdata software-properties-common git && \
    ln -fs /usr/share/zoneinfo/Europe/Stockholm /etc/localtime && \
    dpkg-reconfigure tzdata && \
    apt-add-repository ppa:swi-prolog/devel && \
    apt-get update && \
    apt-get -yq --no-install-recommends install swi-prolog && \
    git clone -b 'V3.1.1' --depth 1 https://github.com/ClioPatria/ClioPatria.git && \
    cd server && \
    apt-get -yq remove --purge software-properties-common && \
    apt-get -yq autoremove && \
    rm -rf /var/lib/apt/lists/* && \
    sh ../ClioPatria/configure && \
    sed -i 's|%PUBLIC_HOST%|'$PUBLIC_HOST'|g' settings.db && \
    sed -i 's/%PUBLIC_PORT%/'$PUBLIC_PORT'/g' settings.db && \
    swipl run.pl --after_load='cpack_configure(dklare), halt'

WORKDIR /opt/server

VOLUME /opt/server/knowledge
VOLUME /opt/server/RDF-store

EXPOSE ${PUBLIC_PORT}

CMD ["swipl","run.pl"]

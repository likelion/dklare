FROM ubuntu:20.04 as build

LABEL maintainer "likelion@gmail.com"

ARG PUBLIC_HOST=localhost
ARG PUBLIC_PORT=3020

ADD config-available /opt/server/cpack/dklare/config-available/
ADD lib /opt/server/cpack/dklare/lib/
ADD rdf /opt/server/cpack/dklare/rdf/

COPY *.db /opt/server/

WORKDIR /opt

RUN apt-get update && \
    DEBIAN_FRONTEND="noninteractive" \
    apt-get -yq --no-install-recommends install tzdata software-properties-common git && \
    apt-add-repository ppa:swi-prolog/devel && \
    apt-get update && \
    apt-get -yq --no-install-recommends install swi-prolog && \
    git clone -b 'V3.1.1' --depth 1 https://github.com/ClioPatria/ClioPatria.git && \
    cd server && \
    apt-get -y remove --purge software-properties-common && \
    apt-get autoremove -y && \
    rm -rf /var/lib/apt/lists/* && \
    sh ../ClioPatria/configure && \
    sed -i 's|%PUBLIC_HOST%|'$PUBLIC_HOST'|g' settings.db && \
    sed -i 's/%PUBLIC_PORT%/'$PUBLIC_PORT'/g' settings.db && \
    swipl run.pl --after_load='cpack_configure(dklare), halt'

WORKDIR /opt/server

EXPOSE ${PUBLIC_PORT}

CMD ["swipl","run.pl"]

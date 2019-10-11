# Use phusion/baseimage as base image. To make your builds reproducible, make
# sure you lock down to a specific version, not to `latest`!
# See https://github.com/phusion/baseimage-docker/blob/master/Changelog.md for
# a list of version numbers.
FROM ubuntu:16.04
COPY files/keyboard /etc/default/keyboard
RUN apt-get update && apt-get install --no-install-recommends -y prover9 ladr4-apps z3 openjdk-8-jdk-headless openjdk-8-jre-headless git maven ant openssh-server && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN useradd -ms /bin/bash lsw
COPY docker-reasoners/vampire /usr/local/bin/
RUN chmod a+x /usr/local/bin/vampire
USER lsw
WORKDIR /home/lsw
RUN mkdir /home/lsw/repos/
# https://stackoverflow.com/questions/36996046/how-to-prevent-dockerfile-caching-git-clone
ADD https://api.github.com/repos/alanruttenberg/abcl/git/refs/heads/stage abcl-version.json
RUN cd repos && git clone --branch stage --depth 1 https://github.com/alanruttenberg/abcl.git 
COPY files/dot-emacs  /home/lsw/.emacs
COPY files/dot-abclrc  /home/lsw/.abclrc
COPY files/dot-Xresources /home/lsw/.Xresources
COPY files/slime-init.el /home/lsw/emacs/slime-init.el
COPY files/font-indent.el /home/lsw/emacs/font-indent.el
RUN cd /home/lsw/repos/abcl && ant abcl-aio.jar
ADD https://api.github.com/repos/alanruttenberg/lsw2/git/refs/heads/owlapiv4 lsw2-version.json
RUN cd repos && git clone --branch owlapiv4 --depth 1 https://github.com/alanruttenberg/lsw2.git 
ADD https://api.github.com/repos/alanruttenberg/slime/git/refs/heads/beta slime-version.json
RUN cd repos && git clone --branch beta --depth 1 https://github.com/alanruttenberg/slime.git
USER lsw
RUN /home/lsw/repos/lsw2/bin/lsw
USER 0
RUN apt-get remove -y --auto-remove git ant
RUN rm -rf /usr/local/share/doc
USER lsw
WORKDIR /home/lsw/repos/lsw2/owl2/lib
RUN rm -rf  Chainsaw-1.0-SNAPSHOT.jar  pelletcli/  Konclude*  factpp-native-1.6.4/  prefuse/  LICENSE-README/  jfact/  sparqldl-api1.0.0/  jfact-1.2.3.jar  telemetry-1.0.0.jar  org.semanticweb.hermit-1.3.8.413.jar  owlapi-4.2.6-dependencies/  uncommons-maths-1.2.2.jar  elk-owlapi-standalone-0.5.0-SNAPSHOT-bin.jar  owlapi-distribution-4.2.6.jar  explanation/  owlapitools/ 
WORKDIR /home/lsw/repos/lsw2/lib
RUN rm abcl-aio.jar
RUN cp /home/lsw/repos/abcl/dist/abcl-aio.jar .
RUN rm -rf /home/lsw/repos/abcl/dist
RUN rm -rf /home/lsw/repos/abcl/build
RUN rm -rf /home/lsw/repos/lsw2/slime-alan
RUN rm -rf /home/lsw/repos/lsw2/protege
RUN rm -rf /home/lsw/repos/lsw2/virtual-machine
RUN rm -rf /home/lsw/repos/*/.git
ENV PATH="/home/lsw/repos/lsw2/bin:${PATH}"
# compile on first run
RUN lsw --eval '(cl-user::quit)'
# compile swank
RUN abcl -- --load /home/lsw/repos/slime/swank-loader.lisp --eval '(swank-loader::load-swank)'
WORKDIR /home/lsw/repos/lsw2/owl2/bin
ENTRYPOINT lsw --load /home/lsw/repos/slime/swank-loader.lisp --eval '(swank-loader::load-swank)'
# FROM openjdk:7u131-jdk-alpine
# RUN apk update
# RUN apk add bash
# RUN apk add maven
# RUN adduser -D -S -s /bin/bash -h /home/lsw lsw
# COPY --from=0 /home/lsw /home/lsw
# RUN chown -R lsw /home/lsw
# USER lsw

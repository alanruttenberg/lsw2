# Use phusion/baseimage as base image. To make your builds reproducible, make
# sure you lock down to a specific version, not to `latest`!
# See https://github.com/phusion/baseimage-docker/blob/master/Changelog.md for
# a list of version numbers.
FROM ubuntu:16.04

COPY files/keyboard /etc/default/keyboard
RUN apt-get update && apt-get install --no-install-recommends -y prover9 z3 openjdk-8-jdk git maven ant openssh-server && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN useradd -ms /bin/bash lsw
USER lsw
COPY docker-reasoners/vampire /usr/local/bin/
RUN chmod a+x /usr/local/bin/vampire
WORKDIR /home/lsw
RUN mkdir /home/lsw/repos/
RUN cd repos && git clone --depth 1 https://github.com/alanruttenberg/lsw2.git 
RUN cd repos && git clone --branch stage --depth 1 https://github.com/alanruttenberg/abcl.git 
RUN cd repos && git clone --branch beta --depth 1 https://github.com/alanruttenberg/slime.git
COPY files/dot-emacs  /home/lsw/.emacs
COPY files/dot-abclrc  /home/lsw/.abclrc
COPY files/dot-Xresources /home/lsw/.Xresources
COPY files/slime-init.el /home/lsw/emacs/slime-init.el
COPY files/font-indent.el /home/lsw/emacs/font-indent.el
COPY files/mvn-bootstrap.sh /tmp/mvn-bootstrap.sh
RUN sh /tmp/mvn-bootstrap.sh 
RUN cd /home/lsw/repos/abcl && ant abcl-aio.jar
RUN /home/lsw/repos/lsw2/bin/lsw

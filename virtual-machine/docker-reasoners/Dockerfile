# Use phusion/baseimage as base image. To make your builds reproducible, make
# sure you lock down to a specific version, not to `latest`!
# See https://github.com/phusion/baseimage-docker/blob/master/Changelog.md for
# a list of version numbers.
FROM ubuntu:16.04

RUN apt-get update && apt-get install --no-install-recommends -y prover9 z3 && apt-get clean && rm -rf /var/lib/apt/lists/*
COPY vampire /usr/local/bin/
RUN chmod a+x /usr/local/bin/vampire

# Clean up APT when done.
# RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
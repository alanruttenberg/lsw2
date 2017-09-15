FROM ubuntu:16.04

# ca-certificates because https://github.com/nodesource/distributions/issues/42
# z3 to bring any libraries that are needed, primarily gmp

RUN apt-get update && apt-get install --no-install-recommends -y\
   python \
   make\
   g++\
   git\
   z3\
   ca-certificates\
   curl\
   tar\
   && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/Z3Prover/z3.git
RUN cd z3 && python scripts/mk_make.py
# PREFIX -> dist for make install, link static
RUN cd z3/build && cat config.mk | sed s/^LINK_FLAGS=/LINK_FLAGS=--static/ | sed s,PREFIX=/usr,PREFIX=dist, > config2.mk &&  mv config2.mk config.mk
RUN cd z3/build && make -j4 install 

RUN curl -L http://www.cs.unm.edu/~mccune/prover9/download/LADR-2009-11A.tar.gz -o LADR-2009-11A.tar.gz
RUN tar xf LADR-2009-11A.tar.gz
WORKDIR LADR-2009-11A
# to address https://stackoverflow.com/questions/11336477/gcc-will-not-properly-include-math-h
RUN cd provers.src && cat Makefile | sed "s/..\\/ladr\\/libladr\\.a/..\\/ladr\\/libladr\.a -lm/" > Makefile
run XFLAGS=--static make all

FROM alpine
COPY --from=0 /z3/build/dist/bin/z3 /usr/local/bin/
COPY vampire /usr/local/bin/
COPY --from=0 /LADR-2009-11A/bin/prover9 /usr/local/bin/
COPY --from=0 /LADR-2009-11A/bin/mace4 /usr/local/bin/
COPY --from=0 /LADR-2009-11A/bin/prooftrans /usr/local/bin/
COPY --from=0 /LADR-2009-11A/bin/interpformat /usr/local/bin/
COPY --from=0 /LADR-2009-11A/bin/ladr_to_tptp /usr/local/bin/
RUN chmod a+x /usr/local/bin/




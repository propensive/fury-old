# Use an Ubuntu image with Java 8, and update apt
FROM openjdk:8
RUN (apt-get -qq update > /dev/null && apt-get -qq install make > /dev/null)

# Set up Git credentials
RUN (git config --global user.email 'fury@propensive.com' && git config --global user.name 'Fury Test')

# Install GraalVM
ENV GRAAL_VERSION "1.0.0-rc11"
RUN sh -c "cd /opt && curl -s -L https://github.com/oracle/graal/releases/download/vm-1.0.0-rc11/graalvm-ce-${GRAAL_VERSION}-linux-amd64.tar.gz | tar zxf -"
ENV GRAAL_HOME "/opt/graalvm-ce-${GRAAL_VERSION}"
RUN apt-get -qq install gcc libz-dev > /dev/null

# Install IPFS
RUN sh -c "cd /opt && curl -s -L https://dist.ipfs.io/go-ipfs/v0.4.22/go-ipfs_v0.4.22_linux-amd64.tar.gz | tar zxf - go-ipfs/ipfs"

# Set up mirror for Maven Central
RUN mkdir -p /root/.config/coursier/
COPY etc/ci-mirror.properties /root/.config/coursier/mirror.properties
RUN mkdir -p /home/bash_user/.config/coursier/
COPY etc/ci-mirror.properties /home/bash_user/.config/coursier/mirror.properties

# Set up build directory
RUN mkdir -p /build /build/bootstrap
RUN ln -s /opt/scala-2.12.8 /build/bootstrap/scala
ENV PATH="/opt/scala-2.12.8/bin:/usr/local/openjdk-8/bin:/root/.bloop:/opt/go-ipfs:${PATH}"

COPY Makefile /build/Makefile
COPY etc /build/etc

ENV COURSIER_CACHE="/var/cache/coursier"
RUN groupadd coursier
RUN mkdir -p "${COURSIER_CACHE}"

# Build a local version of Fury
COPY .version /build/.version
COPY build.fury /build/build.fury
COPY src /build/src
RUN (cd /build && make clean && make dist/install.sh || cat /build/.fury/logs/*)

# Clean up build
RUN mv /build/dist/install.sh /install.sh

RUN chmod -R 777 "${COURSIER_CACHE}"
RUN chgrp -R coursier "${COURSIER_CACHE}"

# Install Fury via multiple shells
COPY etc/testshell.sh /testshell.sh
RUN /testshell.sh bash
RUN /testshell.sh zsh 
RUN /testshell.sh fish

# Quick tests of native nailgun client (enabled when GCC is available)
RUN apt-get -qq install gcc > /dev/null
RUN su -l bash_user -s /bin/bash -c /install.sh
RUN su -l bash_user -s /bin/bash -c "source ~/.bashrc && fury start && fury about"

RUN su -l bash_user -s /bin/bash -c "/opt/go-ipfs/ipfs init"

COPY etc/integration /integration
COPY etc/community /community
COPY test /home/bash_user/test
RUN chown -R bash_user:bash_user /home/bash_user/test

RUN FURY_VERSION=`cat /build/.version`; ln -sf "/home/bash_user/.fury/bin/fury" /usr/local/bin/fury

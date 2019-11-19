# Use an Ubuntu image with Java 8, and update apt
FROM openjdk:8
RUN (apt-get -qq update > /dev/null && apt-get -qq install make gcc > /dev/null)

# Set up Git credentials
RUN (git config --global user.email 'fury@propensive.com' && git config --global user.name 'Fury Test')

# Install IPFS
RUN sh -c "cd /opt && curl -s -L https://dist.ipfs.io/go-ipfs/v0.4.22/go-ipfs_v0.4.22_linux-amd64.tar.gz | tar zxf - go-ipfs/ipfs"

COPY Makefile /build/Makefile
COPY etc /build/etc
COPY .version /build/.version
COPY .focus.fury /build/.focus.fury
COPY layers /build/layers
COPY src /build/src
RUN (cd /build && make dist/install.sh)

# Clean up build
RUN mv /build/dist/install.sh /install.sh
RUN /install.sh
RUN fury about

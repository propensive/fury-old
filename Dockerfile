# Use an Ubuntu image with Java 8, and update apt
FROM openjdk:8
RUN apt-get -q update && \
    apt-get -q -y install make

# Set up Git credentials
RUN git config --global user.email 'fury@propensive.com' && git config --global user.name 'Fury Test'

# Install Scala 2.12.8
RUN mkdir /opt/scala-2.12.8 && \
	curl https://downloads.lightbend.com/scala/2.12.8/scala-2.12.8.tgz | tar xvz -C /opt/scala-2.12.8 --strip 1
ENV PATH="/opt/scala-2.12.8/bin:${PATH}"

# Set up build directory
RUN mkdir -p /build /build/bootstrap
RUN ln -s /opt/scala-2.12.8 /build/bootstrap/scala
ADD Makefile /build/Makefile
ADD .bloop /build/.bloop
ADD etc /build/etc
ADD src /build/src
RUN sh -c "echo test > /build/.version"

# Build a local version of Fury
ENV PATH="/root/.bloop:${PATH}"
RUN (cd /build && make dist/install.sh)

# Clean up build
RUN mv /build/dist/install.sh /install.sh

# Install Fury via multiple shells
ADD etc/testshell.sh /testshell.sh
RUN /testshell.sh bash " && "
RUN /testshell.sh zsh  " && "
RUN /testshell.sh fish  " ; and "

ADD test /test

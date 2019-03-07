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

# Install GraalVM
ENV GRAAL_VERSION "1.0.0-rc11"
RUN sh -c "cd /opt &&  curl -L https://github.com/oracle/graal/releases/download/vm-1.0.0-rc11/graalvm-ce-${GRAAL_VERSION}-linux-amd64.tar.gz | tar zxf -"
ENV GRAAL_HOME "/opt/graalvm-ce-${GRAAL_VERSION}"
RUN apt-get -y install gcc libz-dev

# Set up build directory
RUN mkdir -p /build /build/bootstrap
RUN ln -s /opt/scala-2.12.8 /build/bootstrap/scala
ADD Makefile /build/Makefile
ADD etc /build/etc
ENV PATH="/root/.bloop:${PATH}"
RUN sh -c "echo test > /build/.version"

# Build a local version of Fury
ADD src /build/src
RUN (cd /build && make dist/install.sh)

# Clean up build
RUN mv /build/dist/install.sh /install.sh

# Install Fury via multiple shells
ADD etc/testshell.sh /testshell.sh
RUN /testshell.sh bash " && "
RUN /testshell.sh zsh  " && "
RUN /testshell.sh fish  " ; and "

# Quick tests of native nailgun clietn (enabled when GCC is available)
RUN apt-get -y install gcc
RUN su bash_user -c "/install.sh"
RUN su bash_user -c "source ~/.bashrc && fury start && fury about"

ENV PATH="/home/bash_user/fury-test/bin:${PATH}"
ADD test /home/bash_user/test

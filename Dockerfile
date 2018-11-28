FROM openjdk:8
RUN apt-get update
RUN sh -c 'curl -L https://github.com/scalacenter/bloop/releases/download/v1.0.0/install.py | python'
RUN sh -c 'cd /opt && wget https://downloads.lightbend.com/scala/2.12.7/scala-2.12.7.tgz && tar xf scala-2.12.7.tgz && rm scala-2.12.7.tgz'

FROM openjdk:8
RUN apt-get update && apt-get install -y zsh
ADD install.sh install.sh
ENTRYPOINT ["/bin/bash", "install.sh"]

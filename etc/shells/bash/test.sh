#!/usr/bin/env bash

source common/common-env
source environment

git config --global user.email "${USER}@${USER}"
git config --global user.name "${USER}"
touch $HOME/.bashrc
/install.sh
source ~/.bashrc && fury start && fury about && fury stop

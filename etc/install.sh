#!/usr/bin/env bash
FURY_VERSION=test

echo "     _____"
echo "    / ___/__ __ ____ __ __"
echo "   / __/ / // // ._// // /"
echo "  /_/    \_._//_/   \_. /"
echo "                   \___/"
echo ""
echo "Fury build tool for Scala, version ${FURY_VERSION}."
echo "This software is provided under the Apache 2.0 License."
echo "Copyright 2019 Jon Pretty, Propensive OÜ."
echo ""
echo "This will install Fury on your computer."
echo ""

RED="$(printf '\e[31m')"
GREEN="$(printf '\e[32m')"
YELLOW="$(printf '\e[33m')"
RESET="$(printf '\e[0m')"

if [ -z "$1" ]
then
  if [ $EUID == 0 ]
  then
    DESTINATION="/opt/fury-${FURY_VERSION}"
    CONFIG="/etc/fury"
  else
    if [ -z "${FURYHOME}" ]
    then
      DESTINATION="${HOME}/.fury"
    else
      DESTINATION="${FURYHOME}"
    fi
    CONFIG="${HOME}/.furyrc"
  fi
else
  DESTINATION="$1"
fi

question() {
  printf " ${YELLOW}*${RESET} $1 "
}

message() {
  echo " ${GREEN}*${RESET} $1"
}

warn() {
  echo " ${RED}*${RESET} $1"
}

fail() {
  warn "$1"
  exit 1
}

restartFury() {
  message "Checking for currently-active Fury daemon..."
  type -p fury > /dev/null && \
  fury stop && \
  message "Starting new version of Fury..." && \
  ${DESTINATION}/bin/fury start
}

# Remove an existing installation, if there is one.
prepare() {
  if [ -z "$1" ]
  then
    question "Where should Fury be installed? [${DESTINATION}]"
    read ANSWER
  else
    ANSWER=""
  fi

  [ "${ANSWER}" != "" ] && DESTINATION="${ANSWER}"

  if [ -d "${DESTINATION}" ]
  then
    if [ -z "$1" ]
    then
      question "Target directory ${DESTINATION} already exists. Overwrite? (Y/n)"
      read ANSWER
    else
      ANSWER="Y"
    fi
    if [ "${ANSWER}" = "" ] || [ "${ANSWER}" = "y" ] || [ "${ANSWER}" = "Y" ]
    then
      rm -r "${DESTINATION}"
    fi
  fi
  mkdir -p "${DESTINATION}" || fail "Could not create directory ${DESTINATION}"
}

checkJava() {
  # We only want to capture stderr, stdout can be discarded.
  JAVA=$(java -version 2>&1 >/dev/null | head -n1 | grep -o '".*"' | sed 's/"//g')
  if [ -z "${JAVA}" ]
  then
    warn "Did not find a local installed version of Java."
    warn "Please install Java, or make sure that the 'java' command is on the path."
    warn "Java is available here,"
    warn ""
    fail "  http://www.oracle.com/technetwork/java/javase/downloads/index.html"
  else
    message "Found Java version ${JAVA}."
  fi
}

tryCompilingNailgun() {
  if [ gcc ${DESTINATION}/bin/ng.c -O0 ${DESTINATION}/bin/ng ]
  then
	  message "Native Nailgun client was compiled successfully"
  else
	  warn "Native Nailgun client compilation failed"
	  message "Python Nailgun client will be used"
  fi
  exit 0
}

updateShell() {
  SH="$1"
  RCFILE="${HOME}/.${SH}rc"
  if [ -e "${RCFILE}" ] || [ "${SHELL}" = "/bin/${SH}" ]
  then
    touch "${RCFILE}" && \
    message "Backing up ${RCFILE} as ${RCFILE}.bak" && \
    sed -i.bak -e '/\# Added by Fury/d' "${RCFILE}" && \
    echo "export FURYHOME=${DESTINATION} && source ${CONFIG}/${SH} # Added by Fury" >> "${RCFILE}"
  fi
  cp "${DESTINATION}/etc/${SH}rc" "${CONFIG}/${SH}"
}

updateFish() {
  if [ -d "${HOME}/.config/fish" ] || [ "${SHELL}" = "/usr/bin/fish" ]
  then
    RCFILE="${HOME}/.config/fish/config.fish"
    if [ -e "${RCFILE}" ] || [ "${SHELL}" = "/usr/bin/fish" ]
    then
      touch "${RCFILE}" && \
      message "Backing up ${RCFILE} as ${RCFILE}.bak" && \
      sed -i.bak -e '/\# Added by Fury/d' "${RCFILE}" && \
      echo "set -Ux FURYHOME ${DESTINATION}" >> "${RCFILE}" && \
      echo "set -Ux fish_user_paths ${DESTINATION}/bin \$fish_user_paths" >> "${RCFILE}"
    fi
  fi
  cp "${DESTINATION}/etc/fishrc" "${CONFIG}/fish"
}

updateShells() {
  message "Updating shell configuration"
  mkdir -p "${CONFIG}"
  cp "${DESTINATION}/etc/aliases" "${CONFIG}/aliases"

  updateShell bash
  updateShell zsh
  updateFish
}

untarPayload() {
  # FURY_VERSION will be provided by make.
  message "Installing Fury version ${FURY_VERSION}..."
	MATCH=$(grep --text --line-number '^DATA:$' $0 | cut -d ':' -f 1)
	START=$((MATCH + 1))
	tail -n +${START} $0 | tar xCzf "${DESTINATION}" - --no-same-owner
  message "Installed Fury ${FURY_VERSION}."
}

completion() {
  echo ""
  echo "┌───────────────────────────────────────────────────────────────────┐"
  echo "│                                                                   │"
  echo "│  Fury is now installed. To test it, open a new terminal and run,  │"
  echo "│                                                                   │"
  echo "│  > fury about                                                     │"
  echo "│                                                                   │"
  echo "└───────────────────────────────────────────────────────────────────┘"
  echo ""
}

prepare && checkJava && untarPayload && updateShells && restartFury && completion

exit 0

DATA:

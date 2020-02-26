#!/usr/bin/env bash
FURY_VERSION=test

set -e

echo "     _____"
echo "    / ___/__ __ ____ __ __"
echo "   / __/ / // // ._// // /"
echo "  /_/    \_._//_/   \_. /"
echo "                   \___/"
echo ""
echo "Fury build tool for Scala, version ${FURY_VERSION}."
echo "This software is provided under the Apache 2.0 License."
echo "Copyright 2018-20 Jon Pretty, Propensive OÜ."
echo ""
echo "This will install Fury on your computer."
echo ""

RED="$(printf '\e[31m')"
GREEN="$(printf '\e[32m')"
YELLOW="$(printf '\e[33m')"
RESET="$(printf '\e[0m')"

if [ -z "${XDG_CONFIG_HOME}" ]
then
  CONFIG="${HOME}/.config/fury"
else
  CONFIG="${XDG_CONFIG_HOME}/fury"
fi

mkdir -p "${CONFIG}"

if [ -z "$1" ]
then
  BATCH="0"
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
  fi
else
  DESTINATION="$1"
  BATCH="1"
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
  fury stop 2> /dev/null && \
  message "Starting new version of Fury..." && \
  "${DESTINATION}/bin/fury" start
}

# Remove an existing installation, if there is one.
prepare() {
  if [ "$BATCH" -eq "0" ]
  then
    question "Where should Fury be installed? [${DESTINATION}]"
    read ANSWER
  else
    message "Installing Fury to ${DESTINATION}"
    ANSWER=""
  fi

  [ "${ANSWER}" != "" ] && DESTINATION="${ANSWER}"

  if [ -d "${DESTINATION}" ]
  then
    if [ "$BATCH" -eq "0" ]
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
  pushd "${DESTINATION}"
  cc bin/ng.c -o bin/ng > /dev/null 2> /dev/null && \
      cc bin/procname.c -Wall -Werror -fPIC -shared -o bin/libprocname.so > /dev/null 2> /dev/null &&
      chmod 644 bin/libprocname.so || \
      usePythonNailgun
  popd
}

usePythonNailgun() {
  warn "Native Nailgun client compilation failed"
  message "Python Nailgun client will be used"
}

findConfigFile() {
  FILE=""
  RCFILE=""
  case $SH in
    bash)
      for FILE in ".bash_profile" ".bashrc" ".profile"
      do
        RCFILE="${HOME}/${FILE}"
        if [ -e "${RCFILE}" ]; then
          break
        fi
      done
      if [ -z "${RCFILE}" ]; then
        if [[ "${OSTYPE}" == *"darwin"* ]]; then
          RCFILE="${HOME}/.bash_profile"
        else
          RCFILE="${HOME}/.bashrc"
        fi
      fi
      echo "The configuration for ${SH} will be written to ${RCFILE}."
      ;;
    zsh)
      for FILE in ".zprofile" ".zshrc"
      do
        RCFILE="${HOME}/${FILE}"
        if [ -e "${RCFILE}" ]; then
          break
        fi
      done
      if [ -z "${RCFILE}" ]; then
        if [[ "${OSTYPE}" == *"darwin"* ]]; then
          RCFILE="${HOME}/.zprofile"
        else
          RCFILE="${HOME}/.zshrc"
        fi
      fi
      echo "The configuration for ${SH} will be written to ${RCFILE}."
      ;;
    *)
      echo "Could not recognize the shell: $SH"
      ;;
  esac
}

resolveScala() {
  ${DESTINATION}/bin/coursier fetch --classpath org.scala-lang:scala-reflect:2.12.8 com.facebook:nailgun-server:1.0.0 > ${DESTINATION}/classpath
}

updateShell() {
  SH=$1
  findConfigFile
  if [ -e "${RCFILE}" ]; then
    touch "${RCFILE}" && \
    message "Backing up ${RCFILE} as ${RCFILE}.bak" && \
    sed -i.bak -e '/\# Added by Fury/d' "${RCFILE}"
  fi
  echo "export FURYHOME=${DESTINATION} && source ${CONFIG}/${SH} # Added by Fury" >> "${RCFILE}"
  cp "${DESTINATION}/etc/${SH}rc" "${CONFIG}/${SH}"
}

updateFish() {
  fish "set -Ux FURYHOME ${DESTINATION}"
  fish "if not contains ${DESTINATION}/bin $fish_user_paths; set -Ux fish_user_paths ${DESTINATION}/bin \$fish_user_paths; end"
  fish "if not contains ${DESTINATION}/usr $fish_user_paths; set -Ux fish_user_paths ${DESTINATION}/usr \$fish_user_paths; end"
  cp "${DESTINATION}/etc/fishrc" "${CONFIG}/fish"
}

updateShells() {
  if [ "$BATCH" -eq "0" ]
  then
    message "Updating shell configuration"
    mkdir -p "${CONFIG}"
    cp "${DESTINATION}/etc/aliases" "${CONFIG}/aliases"

    which bash && updateShell "bash" || true
    which zsh && updateShell "zsh" || true
    which fish && updateFish || true
  else
    message "Not updating shell configurations for batch-mode installation"
  fi
}

untarPayload() {
  # FURY_VERSION will be provided by make.
  message "Installing Fury version ${FURY_VERSION}..."
	MATCH=$(grep --text --line-number '^DATA:$' $0 | cut -d ':' -f 1)
	START=$((MATCH + 1))
	tail -n +${START} $0 | tar xCzf "${DESTINATION}" - --no-same-owner 2> /dev/null
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

prepare && checkJava && untarPayload && resolveScala && tryCompilingNailgun && updateShells && restartFury && completion

exit 0

DATA:

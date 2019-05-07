
BLOOP_VERSION="1.2.5"
DESTINATION="$HOME/fury-$FURY_VERSION"
CONFIG="$HOME/.furyrc"

echo "     _____"
echo "    / ___/__ __ ____ __ __"
echo "   / __/ / // // ._// // /"
echo "  /_/    \_._//_/   \_. /"
echo "                   \___/"
echo ""
echo "Fury build tool for Scala, version $FURY_VERSION."
echo "This software is provided under the Apache 2.0 License."
echo "© Copyright 2018 Jon Pretty, Propensive Ltd."
echo ""
echo "This will install Fury on your computer."
echo ""
echo "Checking for dependencies..."

RED=$(printf '\e[31m')
GREEN=$(printf '\e[32m')
YELLOW=$(printf '\e[33m')
RESET=$(printf '\e[0m')

question() {
  printf " $YELLOW*$RESET $1 "
}

message() {
  echo " $GREEN*$RESET $1"
}

warn() {
  echo " $RED*$RESET $1"
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
  question "Where should Fury be installed? [${DESTINATION}]"
  read ANSWER
  [ "$ANSWER" != "" ] && DESTINATION=${ANSWER}

  if [ -d "${DESTINATION}" ]
  then
    question "Target directory ${DESTINATION} already exists. Overwrite? (Y/n)"
    read ANSWER
    if [ "$ANSWER" = "" ] || [ "$ANSWER" = "y" ] || [ "$ANSWER" = "Y" ]
    then
      rm -r "${DESTINATION}"
    fi
  fi
  mkdir -p "${DESTINATION}" || fail "Could not create directory ${DESTINATION}"
}

checkJava() {
  # We only want to capture stderr, stdout can be discarded.
  JAVA=$(java -version 2>&1 >/dev/null | head -n1 | grep -o '".*"' | sed 's/"//g')
  if [ -z "$JAVA" ]
  then
    warn "Did not find a local installed version of Java."
    warn "Please install Java, or make sure that the 'java' command is on the path."
    warn "Java is available here,"
    warn ""
    fail "  http://www.oracle.com/technetwork/java/javase/downloads/index.html"
  else
    message "Found Java version $JAVA."
  fi
}

tryCompilingNailgun() {
    if [ gcc ${DESTINATION}/bin/ng.c -O0 ${DESTINATION}/bin/ng ]; then
	echo "Nailgun native client compilation: SUCCESS"
    else
	echo "Nailgun native client compilation: FAILURE"
	echo "Nailgun python client will be used"
    fi
    exit 0
}

updateShell() {
  SH="$1"
  RCFILE="$HOME/.${SH}rc"
  if [ -e "$RCFILE" ] || [ "$SHELL" = "/bin/$SH" ]
  then
    touch "$RCFILE" && \
    echo "Backing up $RCFILE as $RCFILE.bak" && \
    sed -i.bak -e '/\# Added by Fury/d' "$RCFILE" && \
    echo "export FURYHOME=$DESTINATION && source ~/.furyrc/$SH # Added by Fury" >> "$RCFILE"
  fi
  cp "$DESTINATION/etc/${SH}rc" "$CONFIG/$SH"
}

updateFish() {
  if [ -d "$HOME/.config/fish" ] || [ "$SHELL" = "/usr/bin/fish" ]
  then
    RCFILE="$HOME/.config/fish/config.fish"
    if [ -e "$RCFILE" ]
    then
      touch "$RCFILE" && \
      echo "Backing up $RCFILE as $RCFILE.bak" && \
      sed -i.bak -e '/\# Added by Fury/d' "$RCFILE" && \
      echo "source ~/.furyrc/fish # Added by Fury" >> "$HOME/.config/fish/config.fish"
    fi
    fish -c "set -Ux FURYHOME $DESTINATION"
    fish -c "set -Ux fish_user_paths $DESTINATION/bin \$fish_user_paths"
  fi
  cp "$DESTINATION/etc/fishrc" "$CONFIG/fish"
}

updateShells() {
  echo "Updating shell configuration"
  mkdir -p "$CONFIG"
  cp "$DESTINATION/etc/aliases" "$CONFIG/aliases"

  updateShell bash
  updateShell zsh
  updateFish
}

untarPayload() {
  # FURY_VERSION will be provided by make.
  message "Installing Fury version $FURY_VERSION..."
	MATCH=$(grep --text --line-number '^DATA:$' $0 | cut -d ':' -f 1)
	START=$((MATCH + 1))
	tail -n +$START $0 | tar xCzf "$DESTINATION" - --no-same-owner
  message "Installed Fury $FURY_VERSION."
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

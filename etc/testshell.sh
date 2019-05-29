#!/usr/bin/env bash
# WARNING: Only for execution in docker.
# This script downloads shell from apt repository, creates new user and
# installs Fury using the shell passed as command line parameter
#
# Argument 1: shell type
# Argument 2: conjunction symbol for the specified shell
#
# Example usage:
# ./installation_test bash " && "

SHELL_NAME="$1"
NEW_USER="${SHELL_NAME}_user"
NEW_HOME="/home/${NEW_USER}"
EXPORTS='export PATH="/opt/scala-2.12.8/bin:/usr/local/openjdk-8/bin:$PATH"'

case "$SHELL_NAME" in
  fish)
    EXPORTS='set -Ux fish_user_paths $fish_user_paths /opt/scala-2.12.8/bin /usr/local/openjdk-8/bin'
    NEW_RC="${NEW_HOME}/.config/fish/config.fish"
    NEW_PROFILE="${NEW_RC}"
    ;;
  zsh)
    NEW_RC="${NEW_HOME}/.zshrc"
    NEW_PROFILE="${NEW_HOME}/.zprofile"
    ;;
  *)
    NEW_RC="${NEW_HOME}/.${SHELL_NAME}rc"
    NEW_PROFILE="${NEW_HOME}/.profile"
    ;;
esac

apt-get -qq install "${SHELL_NAME}" > /dev/null
useradd -s "$(which ${SHELL_NAME})" -d "$NEW_HOME" "$NEW_USER"
mkdir -p "$NEW_HOME"
mkdir -p $(dirname "${NEW_RC}")
echo "${EXPORTS}" >>"${NEW_PROFILE}"
touch ${NEW_RC}
cat <<EOF >"$NEW_HOME/commands"
#!/usr/bin/env ${SHELL_NAME}
source ${NEW_RC}
git config --global user.email "$NEW_USER@$NEW_USER"
git config --global user.name "$NEW_USER"
fury start 
fury about 
fury stop
EOF
chown -R "$NEW_USER" "$NEW_HOME"
chmod +x "$NEW_HOME/commands"

su -l "$NEW_USER" -c "/install.sh"

su -l "$NEW_USER" -c "$NEW_HOME/commands"

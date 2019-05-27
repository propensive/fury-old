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

case "$SHELL_NAME" in
  fish)
    JOIN=" ; and " ;;
  *)
    JOIN="&&" ;;
esac

apt-get install -y "${SHELL_NAME}"
NEW_HOME="/home/${SHELL_NAME}_user"
NEW_USER="${SHELL_NAME}_user"
useradd -s "$(which ${SHELL_NAME})" -d "$NEW_HOME" "$NEW_USER"
mkdir -p "$NEW_HOME"
cat <<EOF >"$NEW_HOME/commands"
echo "Hello from $NEW_USER"
env | grep PATH
git config --global user.email "$NEW_USER@$NEW_USER"
git config --global user.name "$NEW_USER"
touch "$NEW_HOME/.${SHELL_NAME}rc"
"$SHELL_NAME" /install.sh
"$SHELL_NAME" source /home/${SHELL_NAME}_user/.${SHELL_NAME}rc ${JOIN} fury start ${JOIN} fury about ${JOIN} fury stop
EOF
chown -R "$NEW_USER" "$NEW_HOME"
chmod +x "$NEW_HOME/commands"

su -l "$NEW_USER" -c "${SHELL_NAME}" -c "$NEW_HOME/commands"

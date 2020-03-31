#!/bin/bash

CACHED=()

function preload {
  echo "preloading $1"
  if printf '%s\n' ${CACHED[@]} | grep -q -P '^'"$1"'$'; then
    echo "$1 is already cached"
  else
    CACHED+=("$1")
    DEPS=$(ipfs cat "$1" | grep -v previous | egrep -o 'Qm[a-zA-Z0-9]{44}')
    # change delimiter (IFS) to new line.
    IFS_BAK=$IFS
    IFS=$'\n'

    for DEP in $DEPS; do

      # return IFS back
      IFS=$IFS_BAK
      IFS_BAK=
      
      preload $DEP
      
      # return IFS back to newline for "for" loop
      IFS_BAK=$IFS
      IFS=$'\n'

    done 

    # return delimiter to previous value
    IFS=$IFS_BAK
    IFS_BAK=
  fi  
}

preload $(egrep -o 'Qm[a-zA-Z0-9]{44}' "$1" | head -n 1)
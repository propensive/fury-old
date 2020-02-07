#!/usr/bin/env bash

# Copied from https://superuser.com/a/406172

success=false
tty
TTY=$(tty)
if [[ ! -e $TTY ]]; then exit 1; fi
exec < $TTY
oldstty=$(stty -g)
stty raw -echo min 0
col=11      # background
#          OSC   Ps  ;Pt ST
echo -en "\033]${col};?\033\\" >$TTY  # echo opts differ w/ OSes
result=
if IFS=';' read -r -d '\' color ; then
    result=$(echo $color | sed 's/^.*\;//;s/[^rgb:0-9a-f/]//g')
    success=true
fi
stty $oldstty
echo $result
$success

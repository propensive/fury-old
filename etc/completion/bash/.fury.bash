#!/usr/bin/env bash

_fury_completions() {
    # Temporary hack: reformat the zsh completions for bash
    CHOICES=$(fury completion --shell zsh --paramNo $COMP_CWORD -- $COMP_LINE | \
                  perl -nle 'print $1 if /.*\(\((.*)\)\)/' - | \
                  awk -F ":'" '{ for (i=1;i<=NF;i++) { print $i } }' | \
                  perl -pe "s/' /\n/g" | \
                  perl -pe "s/'//g" | \
                  paste -s -d":\n" | \
                  perl -nle 'print $1 if /^([a-z]+):/' - | \
                  perl -pe 's/\n/ /g')

    COMPREPLY=( $(compgen -W "$CHOICES" "${COMP_WORDS[$COMP_CWORD]}") )
}

complete -F _fury_completions fury

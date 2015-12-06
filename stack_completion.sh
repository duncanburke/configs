#!/bin/bash

_stack()
{
    local cmdline
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(stack "${CMDLINE[@]}") )
}

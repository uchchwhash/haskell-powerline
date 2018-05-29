export LANG=en_AU.UTF-8

DIR=$(dirname $(readlink -f ${BASH_SOURCE[0]}))

function _update_ps1() {
	PS1="$($DIR/powerline $? 2> /dev/null)"
}

PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"

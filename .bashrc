export BASH_CONF="bashrc"
# don't put duplicate lines in the history. See bash(1) for more options

# Fuck flow control
stty -ixon

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Color ls
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad


# # Window title
# case "$TERM" in
# xterm*|rxvt*|screen*)
#     PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
#     ;;
# *)
#     ;;
# esac

export PATH="/usr/local/bin/python:$PATH"

function gdiff() {
    opendiff $1.THIS $1.OTHER -ancestor $1.BASE -merge $1;
}

export PATH=$PATH:~/bin:~/Library/Haskell/ghc-7.0.3/lib/hlint-1.8.13/bin
export PATH="$HOME/Library/Haskell/bin:$PATH"
export JAVA_HOME=$(/usr/libexec/java_home -v 1.9)
export JAVA_TOOL_OPTIONS='-Dfile.encoding=UTF-8' # WTF
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
# z
. `brew --prefix`/etc/profile.d/z.sh
# bash completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi


eval "$(docker-machine env an-vm 2> /dev/null)"
alias dc=docker-compose
export PATH=$PATH:/usr/local/Cellar/mysql-client/5.7.23/bin/


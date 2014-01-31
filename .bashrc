export BASH_CONF="bashrc"
# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# Fuck flow control
stty -ixon

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Color ls
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad


# Window title
case "$TERM" in
xterm*|rxvt*|screen*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# Setting PATH for MacPython 2.6
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.6/bin:${PATH}"
export PATH

function gdiff() {
    opendiff $1.THIS $1.OTHER -ancestor $1.BASE -merge $1;
}

export PATH=$PATH:~/bin:~/Library/Haskell/ghc-7.0.3/lib/hlint-1.8.13/bin
export PATH="$HOME/Library/Haskell/bin:$PATH"
export JAVA_HOME=$(/usr/libexec/java_home)
export JAVA_TOOL_OPTIONS='-Dfile.encoding=UTF-8' # WTF
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
# z
. `brew --prefix`/etc/profile.d/z.sh
# bash completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

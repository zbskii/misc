# if [ -e /sw/bin/init.sh ]; then
#    source /sw/bin/init.sh
# fi

# Keybindings
bind "'C-f': forward-word"
bind "'C-b': backward-word"
# Left / Right arrow keys. for OSX
bind '"\e[1;5C": forward-word'
bind '"\e[1;5D": backward-word'



# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Color ls
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

# Color grep
export GREP_OPTIONS=--color=AUTO

# vcprompt
export VCPROPMT_FORMAT=' on \033[34m%n\033[00m:\033[00m%[unknown]b\033[32m%m%u'

# Prompt
case "$TERM" in
    xterm-color | xterm-256color | eterm-color | screen*) color_prompt=yes;;
esac
if [ "$color_prompt" = yes ]; then
    PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt


# Window title
case "$TERM" in
xterm*|rxvt*|screen*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

function gdiff() {
    opendiff $1.THIS $1.OTHER -ancestor $1.BASE -merge $1;
}

function pgps() {
    echo "select procpid,usename,current_query,waiting from pg_stat_activity" |\
      psql capture-dev
}

function pgkill() {
    echo "select pg_cancel_backend($1);" | psql ${2:-i3}
}

# Fucking osx.
alias top="top -o cpu"

export PATH=/usr/local/bin:/usr/local/sbin:$PATH:~/bin:~/src/arcanist/bin:~/.gem/ruby/1.8/bin

# Tcpdump for HTTP traffic
alias tcpd8000="sudo tcpdump -s 0 -A -i lo0 'tcp port 8000 and (((ip[2:2] - ((ip[0]&0xf)<<2)) - ((tcp[12]&0xf0)>>2)) != 0)'"
# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

alias gtags="git show-ref --tags"

# Bash completion hacks
if [ -f `brew --prefix`/etc/bash_completion ]; then
  . `brew --prefix`/etc/bash_completion
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*


# AN go command
alias jump='ssh -At jump.adnxs.net'

function go() {
# a function to use go on jump with a couple of improvements

if [ $# -eq 0 ]; then
      # 0 arg supplied, check if clipboard has hostname and if it looks right, ssh to it
      h=`/usr/bin/pbpaste`
if [ `echo $h | perl -ne 'if (/^\d{2,}\.[\w-]+\.[\w-]+\.\w{3,4}$/) {print 1;} else { print 0; }'` -eq 1 ]; then
ssh $h
   exit
  fi
fi


if [ $# -eq 1 ]; then
   if [ `echo $1 | perl -ne 'if (/^\d{2,}\.[\w-]+\.[\w-]+\.\w{3,4}$/) {print 1;} else { print 0; }'` -eq 1 ]; then
  # 1 arg supplied, probably well-formed hostname as it matched regexp, so try ssh directly
   ssh $1
   exit
      fi
  fi

  jump /usr/bin/go $@
}

. ~/.bashrc
export BASH_CONF="bash_profile"

# Keybindings
bind "'C-f': forward-word"
bind "'C-b': backward-word"
# Left / Right arrow keys. for OSX
bind '"\e[1;5C": forward-word'
bind '"\e[1;5D": backward-word'


export HISTFILESIZE=1000000
export HISTSIZE=1000000
export HISTIGNORE='ls:bg:fg:history:cd'
export HISTTIMEFORMAT='%F %T '

# ignore same sucessive entries.
export HISTCONTROL=ignoredups:erasedups

# Append history file
shopt -s histappend


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
xterm*|rxvt*|screen*|eterm-color)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

export PROMPT_COMMAND="history -a; history -n; $PROMPT_COMMAND"

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
export PATH=$PATH:~/src/schema-tool

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

#[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*



function g() {
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

#export JAVA_HOME=$(/usr/libexec/java_home)

. ~/.bashrc
export BASH_CONF="bash_profile"
#export PATH="/usr/local/opt/python/libexec/bin:$PATH"
#export PATH="/usr/local/opt/mysql-client/bin:$PATH"
#export PATH="/usr/local/opt/node@6/bin:$PATH"
#export PATH="/usr/local/opt/php@7.0/bin:$PATH"
#export PATH=$PATH:$HOME/.composer/vendor/bin

#export PATH="/usr/local/opt/awscli@1/bin:$PATH"
#export NVM_DIR="$HOME/.nvm"
#[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
#[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
#export PATH=$PATH:$HOME/node_modules/.bin
## Jesus Christ.
#export BASH_SILENCE_DEPRECATION_WARNING=1
#export PATH="/usr/local/opt/libxml2/bin:$PATH"

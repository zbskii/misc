
set fish_greeting ""

# Set the help browser
set BROWSER open

# These don't work...
bind \cf forward-word
bind \cb backward-word

# Color ls
set CLICOLOR=1
set LSCOLORS=ExFxCxDxBxegedabagacad

# Color grep
set GREP_OPTIONS=--color=AUTO

# Prompt
function fish_prompt -d "Set the prompt"
        set_color -o green
        printf '%s@%s:%s%s%s⚓ ' (whoami) (hostname|cut -d . -f 1) (set_color -o blue) (prompt_pwd) (set_color normal)
end

# i3 stuff
function pgps -d "Show Posgtgresql processes"
    set -l QUERY "select procpid,usename,current_query,waiting from pg_stat_activity"
    echo $QUERY | psql i3
end

function top -d "Fix OSX's fucking broken top command"
         top -o cpu $arv
end

function activate -d "Activate the I3 sandbox"
         set -gx I3_BASE /idealist/sandbox
         set PATH {$I3_BASE}/bin $PATH
         set -gx POSTGRES_HOME $I3_BASE
         set -gx DYLD_LIBRARY_PATH {$I3_BASE}/lib:{$DYLD_LIBRARY_PATH}
         set -gx LD_LIBRARY_PATH {$I3_BASE}/lib:{$LD_LIBRARY_PATH}
end

function unsand
         set --erase DYLD_LIBRARY_PATH
end
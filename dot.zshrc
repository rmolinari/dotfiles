# -*- mode: sh; eval: (sh-set-shell "zsh") -*-

# Path to your oh-my-zsh configuration.
#ZSH=$HOME/.oh-my-zsh

source $HOME/.profile

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
#plugins=(brew git git-extras tmuxinator fasd)
#source $ZSH/oh-my-zsh.sh

# Customize to your needs...

######################################################################
######################################################################
## My stuff follows
##

######################################################################
# Completion
fpath=(/usr/local/share/zsh-completions $fpath)

autoload -U compinit
compinit

# Cursor movement around the completion list
zmodload -i zsh/complist
zstyle ':completion:*' menu select=8

# expand directories in menu; C-_ undoes
bindkey -M menuselect "^o" accept-and-infer-next-history

######################################################################
# History
#
# Cribbed from Oh-my-zsh's history plugin, which I can't seem to get
# to load with zplug

if [ -z "$HISTFILE" ]; then
    HISTFILE=$HOME/.zsh_history
fi

HISTSIZE=10000
SAVEHIST=10000

# Show history
case $HIST_STAMPS in
  "mm/dd/yyyy") alias history='fc -fl 1' ;;
  "dd.mm.yyyy") alias history='fc -El 1' ;;
  "yyyy-mm-dd") alias history='fc -il 1' ;;
  *) alias history='fc -l 1' ;;
esac

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data
setopt inc_append_history
setopt share_history

######################################################################
# Prompt, based on my bash prompt

# Take a seconds-count and format it as
#
# XdYhZmWs
#
# where X, Y, W, Z are the relevant days, hours, minutes, and seconds.  Prefixes
# of "0d", "0d0h", and "0d0h0m" are stripped away.
#
# Supports fractional seconds
function format_seconds {
  local -i decimals
  if (( ${1} < 1 ))
  then
    decimals=3
  elif (( ${1} < 10 * 60 ))
  then
    decimals=1
  else
    decimals=0
  fi

  if (( ${1} < 0 ))
  then
    echo 0s
  else
    local -i secs=${1}

    local days=$((${secs}/(60*60*24)))
    secs=$((${secs} - ${days}*(60*60*24)))

    local hours=$((${secs}/(60*60)))
    secs=$((${secs} - ${hours}*(60*60)))

    local mins=$((${secs}/60))
    secs=$((${secs} - ${mins}*60))

    local excess_secs=$(( ((${days}*24 + ${hours})*60 + ${mins})*60 ))
    local remaining_secs=$((${1} - ${excess_secs} ))

    printf "%dd%dh%dm%.${decimals}fs" ${days} ${hours} ${mins} ${remaining_secs} |
    sed 's/^0d\(0[^.]\)*//'
  fi
}

typeset -F SECONDS
function preexec {
  timer=$SECONDS
}

# Display $2 in the color $1 and then return to BASE_PROMPT_COLOR
function pcol {
  print -P "%F{$1}$2%F{$BASE_PROMPT_COLOR}"
}
function precmd {
  local elapsed=$(($SECONDS - ${timer:-$SECONDS}))
  timer_show=$(pcol ${TIMER_COLOR} $(format_seconds ${elapsed}))
  unset timer
}

# Colors.  Note: numeric values require a 256-color terminal
local BASE_PROMPT_COLOR=024
local TIMER_COLOR=150
local EXIT_VAL_COLOR=magenta
local JOB_COUNT_COLOR=cyan

local exit_val="%(?..%F{${EXIT_VAL_COLOR}}%?%F{${BASE_PROMPT_COLOR}} )"
local jobs_count="%(1j.%F{${JOB_COUNT_COLOR}}j%j%F{${BASE_PROMPT_COLOR}} .)"
local prompt_status="${jobs_count}${exit_val}"

export PROMPT='%F{${BASE_PROMPT_COLOR}}[${timer_show} ${prompt_status}%n:%~]
%* $ %f'

# Don't monkey with the window titles.
# See .oh-my-zsh/lib/termsupport.zsh
autoload -U add-zsh-hook
add-zsh-hook -d precmd  omz_termsupport_precmd
add-zsh-hook -d preexec omz_termsupport_preexec

# git information in prompt. Actually RPROMPT for now
source "/usr/local/opt/zsh-git-prompt/zshrc.sh"
export RPROMPT='$(git_super_status)'

###########
# Homebrew

# Open the packages' home pages
function bhome {
  for i in $*; do
    brew home ${i}
  done
}

##########
# Aliases

#alias ls="ls -F"
alias ls="ls -FG"

# cd to newest subdirectory
alias cdl="cd *(/om[1])"

# Rename like 'mmv foo.* bar.*'
# Type just 'zmv' to get help, or look online
autoload -U zmv
alias mmv='noglob zmv -W'

# The biggest 10 files under .
alias big="ls -lfh **/*~*.(sof|gem)(.OL[1,10])"

# Truncate a file to zero length
alias zap="gtruncate -s0"

# For the output of rake test runs
alias -g RR="2>&1 | egrep 'assertions|^[A-Za-z]*Test$|FAIL |ERROR '"

# Rake test is currently spitting out its own failure stacktrace for me. This removes it
alias -g RT="2>&1 | sed -n '1,/rake aborted/p'"

function set_up_work_path {
    export SC_ROOT=${HOME}/work/${1}
}

function set_up_work_path_trunk {
    set_up_work_path ${1}/trunk
}

alias suwp=set_up_work_path
alias suwt=set_up_work_path_trunk

alias boem="suwp git/boem"
alias abs="suwp git/fcc-smra"
alias tvr="suwp git/fcctv-reverse"

# Default is TV reverse
tvr

alias cdw='cd ${SC_ROOT}'
alias up='(cd $SC_ROOT/..; svn update)'

alias cdp='cd $(pwd)'

alias fuck='eval $(thefuck $(fc -ln -1 | tail -n 1)); fc -R'

alias pe=path-extractor

alias portal='cat ~/projects/FCC/deploy_notes.txt'
alias boem_portal='cat ~/projects/boem/deploy_notes.txt'

##
# Path
export PATH=/Users/rory/bin:/usr/local/sbin:/usr/local/bin:/usr/local/share/npm/bin:/bin:/usr/bin:/usr/sbin:/sbin:{HOME}/.rvm/bin:${PATH}:~/games/chess/scid_resources/Contents/MacOS:.
export PATH=${HOME}/tools/infer-osx-v0.1.0/infer/infer/bin:${PATH}

##
# Emacs

EMACS_APP=/Applications/Emacs.app/
EMACS_APP_MACOS=${EMACS_APP}/Contents/MacOS

emacs=${EMACS_APP_MACOS}/Emacs

##
# Editor tools
export EDITOR=emacs_d_block ALTERNATE_EDITOR=${emacs}
alias e=emacs_d

##
# ls colors

ZENBURN_LSCOLORS="cxfxgxdxbxegedabagcagc"
SOLARIZED_LSCOLORS="gxfxbEaEBxxEhEhBaDaCaD"
export LSCOLORS=${SOLARIZED_LSCOLORS}

# The shwordsplit and FLAGS_PARENT things are due to limitations
# of zsh's interactions with the shflags script.
#
# TODO: remove the dependence of push-pull on shflags'
setopt shwordsplit
FLAGS_PARENT=$0
. ${HOME}/.bashrc.d/shflags
. ${HOME}/.bashrc.d/power.d/push-pull
unsetopt shwordsplit

# As directed by the 5.0.5 install script
unalias run-help &>/dev/null
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

# Rust tools
export RUST_SRC_PATH=${HOME}/.rust/src

# Other command line tools

##
# z
# . $(brew --prefix)/etc/profile.d/z.sh

##
# fzf
#
# Fuzzy completions
source ~/.fzf.zsh
bindkey '^x^t' fzf-file-widget
bindkey '^t' transpose-chars

##
# fasd
#
# Command line expansions

#;; I've installed via oh-my-zsh
# eval "$(fasd --init auto)"

# Find the file with fasd and open with emacs_d (daemon)
alias fe='f -e emacs_d'
#unalias v # this is aliased to use fasd to open vim

##
# ooc / rock
export OOC_LIBS=~/lib/ooc
export PATH=${PATH}:${HOME}/lib/ooc/sam


################
# Miscellaneous

export SIMULATOR_ID=localhost

# Instructed by the 5.0.7 install
unalias run-help 2>/dev/null
autoload run-help
HELPDIR=/usr/local/share/zsh/help

## RVM

export PATH="$PATH:$HOME/.rvm/bin"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

## Helpers

function gsdiff {
    sdiff -w 200 <(egrep $1 $2) <(egrep $1 $3) | less
}

## Node
export PATH="$PATH:./node_modules/.bin"

######################################################################
# Zplug, a zsh plug-in manager. Trying it instead of oh-my-zsh
source /usr/local/opt/zplug/init.zsh
zplug "plugins/brew",       from:oh-my-zsh
zplug "b4b4r07/enhancd",    use:init.sh
#zplug "plugins/fasd",       from:oh-my-zsh
zplug "plugins/git",        from:oh-my-zsh
zplug "plugins/git-extras", from:oh-my-zsh
zplug "plugins/tmuxinator", from:oh-my-zsh

zplug load --verbose


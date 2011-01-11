######################################################################
# Cross-machine

######################################################################
## Shell options

shopt -s cdspell
shopt -s dirspell
shopt -s dotglob
shopt -s globstar

######################################################################
# My own aliases
. $BASH_CONFIG/aliases

######################################################################
# Shell appearance

. $BASH_CONFIG/prompt

BASH_CONFIG=~/.bash.d


if [[ $MACHINE = "SFDC" ]]
then
  ######################################################################
  # Salesforce specific

  ######################################################################
  # My own exports

  export P4PASSWD=rmolinari
  export P4EDITOR=jed
  export P4CLIENT=rmolinari-desktop:main
  export P4USER=rmolinari
  export P4PORT=p4proxy.soma.salesforce.com:1999

  ######################################################################
  # Oracle
  export ORACLE_HOME=/development/dev/tools/Linux/oracle/10.1.0.2
  PATH=$PATH:$ORACLE_HOME/client/bin
  export PATH
  export LD_LIBRARY_PATH=$ORACLE_HOME/client/lib:$LD_LIBRARY_PATH


  ######################################################################
  # Path stuff

  # tex
  TEXLIVE=/usr/local/texlive/2009

  export PATH=${TEXLIVE}/bin/x86_64-linux:$PATH
  export MANPATH=${TEXLIVE}/doc/man:$MANPATH
  export INFOPATH=${TEXLIVE}/doc/info:$INFOPATH

  export PATH=/home/rmolinari/bin:/home/rmolinari/.gem/ruby/1.9.1/bin:$PATH
fi



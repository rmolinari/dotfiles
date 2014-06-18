PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

# Do the heavy lifting

BASHRC_DIR=${HOME}/.bashrc.d
BASHRC_SITE_SPEC=${BASHRC_DIR}/bashrc-power

if [ -f ${BASHRC_SITE_SPEC} ]; then
  . ${BASHRC_SITE_SPEC}
fi

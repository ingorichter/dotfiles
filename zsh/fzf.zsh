# Setup fzf
# ---------
if [[ ! "$PATH" == */usr/local/opt/fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/usr/local/opt/fzf/bin"
fi

FZF_SCRIPTS=$(${HOMEBREW_BIN} --prefix fzf)/shell

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "${FZF_SCRIPTS}/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "${FZF_SCRIPTS}/key-bindings.zsh"

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

# Default command
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

# Previews
# export FZF_DEFAULT_OPTS='--preview "bat --color=always --style=numbers --line-range=:500 {}"'
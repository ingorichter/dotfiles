# Setup fzf
# ---------
_fzf_bin="${HOMEBREW_PREFIX}/opt/fzf/bin"
if [[ -d "$_fzf_bin" ]] && [[ ! "$PATH" == *${_fzf_bin}* ]]; then
  export PATH="${PATH:+${PATH}:}${_fzf_bin}"
fi
unset _fzf_bin

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
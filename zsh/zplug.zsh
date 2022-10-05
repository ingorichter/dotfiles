source ${HOMEBREW_CELLAR}/zplug/2.4.2/init.zsh

zplug "b4b4r07/zplug"
zplug "Game4Move78/zsh-bitwarden"
zplug "wfxr/forgit"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "mattberther/zsh-pyenv"
zplug "romkatv/powerlevel10k", as:theme, depth:1

if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi

zplug load --verbose

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

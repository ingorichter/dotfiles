source ${HOMEBREW_CELLAR}/zplug/2.4.2/init.zsh

zplug "favware/zsh-lerna"
# zplug "Game4Move78/zsh-bitwarden"
# zplug "marlonrichert/zsh-autocomplete", at:3942311
# zplug "mattberther/zsh-pyenv"
zplug "MichaelAquilina/zsh-you-should-use"
zplug "pndurette/zsh-lux"
# zplug "romkatv/powerlevel10k", as:theme, depth:1
zplug "TwoPizza9621536/zsh-eza"
# zplug "wfxr/forgit"
zplug "zsh-users/zsh-autosuggestions"

# zplug "zsh-users/zsh-history-substring-search"
# zplug "zsh-users/zsh-syntax-highlighting", defer:2

if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi

zplug load #--verbose

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source ${HOMEBREW_CELLAR}/zplug/2.4.2/init.zsh

zplug "b4b4r07/zplug"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-autosuggestions"
zplug "wfxr/forgit"
zplug "TwoPizza9621536/zsh-eza"
zplug "romkatv/powerlevel10k", as:theme, depth:1
zplug "pndurette/zsh-lux"
zplug "MichaelAquilina/zsh-you-should-use"
zplug "mattberther/zsh-pyenv"
zplug "marlonrichert/zsh-autocomplete", at:3942311
zplug "Game4Move78/zsh-bitwarden"
zplug "favware/zsh-lerna"

if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi

zplug load #--verbose

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

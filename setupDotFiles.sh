#!/usr/bin/env bash

set -e
# Change default shell to zsh
# chsh -s /bin/zsh

# Link all dotfiles from $(pwd) into their respective location inside $HOME
DOTFILES=$(pwd -P)

# TODO:
# - copy fonts into proper location
# - adjust iterm to use that font

# Use colors, but only if connected to a terminal, and that terminal
# supports them.
if which tput >/dev/null 2>&1; then
	ncolors=$(tput colors)
fi
if [ -t 1 ] && [ -n "$ncolors" ] && [ "$ncolors" -ge 8 ]; then
	RED="$(tput setaf 1)"
	GREEN="$(tput setaf 2)"
	YELLOW="$(tput setaf 3)"
	BLUE="$(tput setaf 4)"
	BOLD="$(tput bold)"
	NORMAL="$(tput sgr0)"
else
	RED=""
	GREEN=""
	YELLOW=""
	BLUE=""
	BOLD=""
	NORMAL=""
fi

printErrorMessage() {
	printf "\e[0;31m  [✖] $1 $2\e[0m\n"
}

link_file() {
	printf "Create link from ${GREEN}$2${NORMAL} to ${GREEN}$1${NORMAL}\n"
	ln -s $1 $2
}

linkDotFiles() {
	link_file "${DOTFILES}/zsh/zshrc" "${HOME}/.zshrc"
	# ln -s "${DOTFILES}/gitconfig" "${HOME}/.gitconfig"
	# ln -s "${DOTFILES}/Gdbinit/gdbinit" "${HOME}/.gdbinit"
	# ln -s "${DOTFILES}/ssh-keys-macpro" "${HOME}/.ssh"
	# ln -s "${DOTFILES}/tmux.conf" "${HOME}/.tmux.conf"

	# ln -s "${HOME}/.spf13-vim-3/.vimrc" "${HOME}/.vimrc"
	# ln -s "${DOTFILES}/vim-config/vimrc.local.mbpro" "${HOME}/.vimrc.local"
    
    # Setup Fonts
    # cp -a "${HOME}/Dropbox/Fonts/" "${HOME}/Library/Fonts"

	link_file "${DOTFILES}/zsh/themes/nick.zsh-theme" "${HOME}/.oh-my-zsh/themes/nick.zsh-theme"
}

unlinkDotFiles() {
	rm "${HOME}/.zshrc"
	rm "${HOME}/.oh-my-zsh/themes/nick.zsh-theme"

	# setup all mvim related stuff
	# rm "${HOME}/.vimrc"
}

case "$1" in
	"link" )
		linkDotFiles;;
	"unlink" )
		unlinkDotFiles;;
	""|"*" )
		printErrorMessage "Provide either link or unlink to specify the action";;
esac


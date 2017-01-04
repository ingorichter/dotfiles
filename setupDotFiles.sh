#!/usr/bin/env bash

set -e
# Change default shell to zsh
# chsh -s /bin/zsh

# Link all dotfiles from $(pwd) into their respective location inside $HOME
DOTFILES=$(pwd -P)

printErrorMessage() {
	printf "\e[0;31m  [✖] $1 $2\e[0m\n"
}

linkDotFiles() {
	ln -s "${DOTFILES}/zsh/zshrc" "${HOME}/.zshrc"
	# ln -s "${DOTFILES}/gitconfig" "${HOME}/.gitconfig"
	# ln -s "${DOTFILES}/Gdbinit/gdbinit" "${HOME}/.gdbinit"
	# ln -s "${DOTFILES}/ssh-keys-macpro" "${HOME}/.ssh"
	# ln -s "${DOTFILES}/tmux.conf" "${HOME}/.tmux.conf"

	# ln -s "${HOME}/.spf13-vim-3/.vimrc" "${HOME}/.vimrc"
	# ln -s "${DOTFILES}/vim-config/vimrc.local.mbpro" "${HOME}/.vimrc.local"
    
    # Setup Fonts
    # cp -a "${HOME}/Dropbox/Fonts/" "${HOME}/Library/Fonts"
}

unlinkDotFiles() {
	rm "${HOME}/.zshrc"

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


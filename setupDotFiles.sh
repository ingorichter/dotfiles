#!/usr/bin/env bash

set -eu

# Colors for output
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

printMessage() {
	local message="$1"
	local extra="${2:-}" # Use empty string if $2 is not provided
	printf "%s  [*] %s %s %s\\n" "${BLUE}" "${message}" "${extra}" "${NORMAL}"
}

printErrorMessage() {
	local message="$1"
	local extra="${2:-}" # Use empty string if $2 is not provided
	printf "%s  [✖] %s %s %s\\n" "${RED}" "${message}" "${extra}" "${NORMAL}"
}

printSuccessMessage() {
	local message="$1"
	local extra="${2:-}" # Use empty string if $2 is not provided
	printf "%s  [✓] %s %s %s\\n" "${GREEN}" "${message}" "${extra}" "${NORMAL}"
}

# Check if dot is installed
check_dot_installed() {
    if ! command -v dot &> /dev/null && ! [ -f "$HOME/.cargo/bin/dot" ]; then
        printMessage "dot command not found, attempting to install it"
        
        # Check if cargo is installed
        if ! command -v cargo &> /dev/null; then
            printErrorMessage "cargo is not installed. Please install Rust and Cargo first."
            printMessage "Visit https://rustup.rs/ for installation instructions"
            exit 1
        fi
        
        # Install dot using cargo
        printMessage "Installing dot using cargo..."
        cargo install --git https://github.com/ubnt-intrepid/dot.git
        
        if ! [ -f "$HOME/.cargo/bin/dot" ]; then
            printErrorMessage "Failed to install dot. Please try to install it manually."
            exit 1
        fi
        
        printSuccessMessage "dot successfully installed"
    else
        printSuccessMessage "dot is already installed"
    fi
}

# Install oh-my-zsh if not already installed
install_oh_my_zsh() {
    if [ ! -d "$HOME/.oh-my-zsh" ]; then
        printMessage "Installing Oh My Zsh..."
        sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
        printSuccessMessage "Oh My Zsh installed"
    else
        printSuccessMessage "Oh My Zsh is already installed"
    fi
}

# Setup dotfiles using dot
setup_dotfiles() {
    printMessage "Setting up dotfiles using dot..."
    
    # Link dotfiles
    "$HOME/.cargo/bin/dot" link
    
    printSuccessMessage "Dotfiles setup complete"
}

# Setup macOS specific configurations
setup_macos() {
    if [[ "$(uname)" == "Darwin" ]]; then
        printMessage "Setting up macOS specific configurations..."
        
        # Run macOS configuration script if it exists
        if [ -f "$DOTFILES/macos/system-config.sh" ]; then
            printMessage "Running macOS system configuration script..."
            bash "$DOTFILES/macos/system-config.sh"
            printSuccessMessage "macOS configuration applied"
        fi
    fi
}

# Unlink dotfiles using dot
unlink_dotfiles() {
    printMessage "Unlinking dotfiles using dot..."
    
    # Unlink dotfiles
    "$HOME/.cargo/bin/dot" unlink
    
    printSuccessMessage "Dotfiles unlinked"
}

# Main function
main() {
    DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    
    # Check if an argument was provided
    if [ $# -eq 0 ]; then
        printErrorMessage "No action specified"
        echo "Usage: $0 {link|unlink}"
        exit 1
    fi
    
    case "$1" in
        "link" )
            check_dot_installed
            install_oh_my_zsh
            setup_dotfiles
            setup_macos
            ;;
        "unlink" )
            check_dot_installed
            unlink_dotfiles
            ;;
        * )
            printErrorMessage "Invalid action: $1"
            echo "Usage: $0 {link|unlink}"
            exit 1
            ;;
    esac
    
    printSuccessMessage "Operation complete! You may need to restart your terminal."
}

# Run main function
main "$@"


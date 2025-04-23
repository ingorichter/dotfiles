# Dotfiles

My personal collection of dotfiles for various tools and applications I use daily.

## Requirements

- [dot](https://github.com/ubnt-intrepid/dot) - A dotfile manager written in Rust
  - Install with: `cargo install --git https://github.com/ubnt-intrepid/dot.git`

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/ingorichter/dotfiles.git ~/.dotfiles
   cd ~/.dotfiles
   ```

2. Install dotfiles using dot:
   ```bash
   ~/.cargo/bin/dot link
   ```

## What's Included

- **ZSH**: Shell configuration with themes and plugins
- **Git**: Global configuration and ignore patterns
- **Terminal**: 
  - iTerm2 settings
  - Alacritty configuration
  - Kitty configuration
- **Emacs**: Configuration files using crafted-emacs
- **Terminal Multiplexers**:
  - tmux configuration
  - zellij setup
- **Text Expansion**: Espanso configurations
- **MacOS**: System preferences and configurations
- **Utility Scripts**: Various helper scripts

## Configuration

The `.mappings` file defines how dotfiles are linked to their proper locations. The format is:

```ini
[general]
"source/path" = "~/target/path"
```

## Special Cases

- **revealjs**: This directory is included in the repository for script compatibility but is not considered a dotfile and is not linked to the home directory.

## Updating

To update your dotfiles after making changes:

```bash
cd ~/.dotfiles
git pull
~/.cargo/bin/dot link
```

## License

MIT

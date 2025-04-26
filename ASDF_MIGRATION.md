# Transitioning from rbenv/pyenv to asdf

This guide will help you transition from using rbenv and pyenv to the more versatile asdf version manager.

## Why asdf?

asdf is a single tool that can manage multiple language runtimes, replacing the need for multiple version managers. Benefits include:

- Manage all languages with a consistent interface
- Single configuration file (.tool-versions) per project
- Easy installation of plugins for different languages
- Less maintenance overhead in your shell configuration

## Installation

If you haven't installed asdf yet:

```bash
brew install asdf
```

## Migration Steps

### 1. Install asdf plugins for Ruby and Python

```bash
# Install Ruby plugin
asdf plugin add ruby

# Install Python plugin
asdf plugin add python
```

### 2. Install the same versions you had with rbenv/pyenv

First, check which versions you currently have installed:

```bash
# For Ruby
rbenv versions

# For Python
pyenv versions
```

Then install those same versions with asdf:

```bash
# For Ruby
asdf install ruby 3.x.x  # Replace with your version

# For Python
asdf install python 3.x.x  # Replace with your version
```

### 3. Set global versions (optional)

```bash
# Set global Ruby version
asdf global ruby 3.x.x

# Set global Python version
asdf global python 3.x.x
```

### 4. Migrate project-specific versions

For each project that has a `.ruby-version` or `.python-version` file, create a `.tool-versions` file:

```bash
# From the project directory
echo "ruby $(cat .ruby-version)" >> .tool-versions
echo "python $(cat .python-version)" >> .tool-versions
```

### 5. Additional Language Support

asdf can manage many other languages too:

```bash
# Node.js
asdf plugin add nodejs
asdf install nodejs latest

# Go
asdf plugin add golang
asdf install golang latest

# Other languages you use...
```

## Uninstalling rbenv and pyenv

After confirming everything works with asdf, you can uninstall rbenv and pyenv:

```bash
# Uninstall rbenv with Homebrew
brew uninstall rbenv

# Uninstall pyenv with Homebrew
brew uninstall pyenv pyenv-virtualenv
```

## Using asdf

Basic asdf commands:

```bash
# List all available plugins
asdf plugin list all

# Install a specific version
asdf install <language> <version>

# Set a global version
asdf global <language> <version>

# Set a local version (project-specific)
asdf local <language> <version>

# View current versions
asdf current
```

For more information, visit the [asdf documentation](https://asdf-vm.com/).
# git-worktree

A git-extras style command to simplify Git worktree creation and management.

## Installation

The script is already executable in your dotfiles. To make it available as `git workspace`:

### Option 1: Create symlink (recommended)
```bash
# Run the installer script
./scripts/git-worktree-link
```

### Option 2: Add to PATH
```bash
# Add this to your shell profile (.zshrc, .bashrc, etc.)
export PATH="$HOME/.dotfiles/scripts:$PATH"
```

### Option 3: Manual symlink
```bash
# Create symlink in a directory that's in your PATH
ln -s "$HOME/.dotfiles/scripts/git-worktree" "/usr/local/bin/git-worktree"
```

## Usage

### Basic Commands

```bash
# Show help
git worktree help

# Create a new worktree
git worktree create feature-auth main
git worktree create hotfix-123     # Uses default base branch

# List all worktrees
git worktree list

# Switch to a worktree (shows cd command)
git worktree switch feature-auth

# Remove a worktree
git worktree remove feature-auth

# Clean up stale references
git worktree clean
```

### Examples

```bash
# Start working on a new feature
git worktree create feature-login-page main
cd ../ccx-sharing-feature-login-page

# Create a hotfix from main
git worktree create hotfix-security-fix

# List all your worktrees
git worktree list

# Remove completed work
git worktree remove feature-login-page
```

## Features

- **Smart branch handling**: Automatically creates new branches or uses existing ones
- **Error validation**: Checks for existing worktrees, invalid names, missing branches
- **Colorized output**: Easy-to-read status messages
- **Interactive removal**: Confirms before deleting worktrees and branches
- **Clean interface**: Follows git-extras conventions and style

## Configuration

The script can be customized by editing these variables at the top:

```bash
DEFAULT_BASE_BRANCH="main"    # Default branch for new worktrees
# Worktrees are created as sibling directories: repo-name-worktree-name
```

## Comparison with git worktree

| Task | git worktree | git worktree |
|------|-------------|---------------|
| Create worktree | `git worktree add -b feature ../repo-feature main` | `git worktree create feature main` |
| List worktrees | `git worktree list` | `git worktree list` |
| Remove worktree | `git worktree remove ../repo-feature && git branch -D feature` | `git worktree remove feature` |
| Clean references | `git worktree prune` | `git worktree clean` |

## Notes

- Worktrees are created as sibling directories: `repo-name-worktree-name`
- New branches are automatically created if they don't exist
- The script validates worktree names and branch existence
- Removal is interactive and can optionally delete the associated branch
- Use `git worktree` directly for advanced operations not covered by this script

## Troubleshooting

### "Not inside a git repository"
Make sure you're running the command from within a git repository.

### "Git worktree command not available"
Update Git to version 2.5 or later.

### "Worktree already exists"
Use `git workspace list` to see existing worktrees, or choose a different name.

### "Base branch does not exist"
Specify a branch that exists locally or on origin, or run `git fetch` first.
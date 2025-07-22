# Oh My Posh Custom Theme with Git Config Display

This directory contains a custom Oh My Posh theme based on the Night Owl theme, enhanced with git configuration display.

## Features

- **Git Config Display**: Shows the current git user email when inside a git repository
- **Visual Indicators**:
  - 🏠 Home icon for personal repositories (green background)
  - 🏢 Building icon for work repositories (red background)
- **Smart Display**: Only shows git config when actually inside a git repository
- **Work Detection**: Automatically detects work repositories based on email patterns

## Available Theme Variants

### 1. Domain Display (`night-owl-custom.omp.json`) - **Currently Active**

Shows just the domain part of your email:

- `@gmail.com` (personal - green with 🏠)
- `@adobe.com` (work - red with 🏢)

### 2. Username Display (`night-owl-username.omp.json`)

Shows just the username part of your email:

- `ingo.richter+github` (personal - green with 🏠)
- `irichter` (work - red with 🏢)

### 3. Simple Labels (`night-owl-simple.omp.json`)

Shows simple work/personal labels:

- `PERSONAL` (green with 🏠)
- `WORK` (red with 🏢)

## How It Works

The theme includes a custom command segment that:

1. Checks if you're inside a git repository
2. If yes, displays git config information in your chosen format
3. Uses different colors and icons based on email patterns:
   - **Personal**: Green background with home icon
   - **Work**: Red background with building icon (detects "adobe", "work", "company", or "corp")

## Configuration

The theme is configured in `night-owl-custom.omp.json` and is automatically loaded by your `.zshrc` file.

### Switching Between Themes

To switch to a different variant, edit your `.zshrc` file and change the theme path:

```bash
# For domain display (current)
oh-my-posh init zsh --config ~/.dotfiles/oh-my-posh/night-owl-custom.omp.json

# For username display
oh-my-posh init zsh --config ~/.dotfiles/oh-my-posh/night-owl-username.omp.json

# For simple labels
oh-my-posh init zsh --config ~/.dotfiles/oh-my-posh/night-owl-simple.omp.json
```

Then reload your shell: `rm ~/.cache/oh-my-posh.zsh && source ~/.zshrc`

## Customization

To customize the work email detection, edit the `background_templates` and `template` sections in the git config segment:

```json
{
  "background_templates": [
    "{{ if or (.Output | contains \"work\") (.Output | contains \"company\") (.Output | contains \"corp\") }}#FF5722{{ end }}"
  ],
  "template": "{{ if .Output }} {{ if or (.Output | contains \"work\") (.Output | contains \"company\") (.Output | contains \"corp\") }}\\uf0b1{{ else }}\\uf015{{ end }} {{ .Output }} {{ end }}"
}
```

### Adding Your Company Domain

To add your specific company domain for work detection, modify the conditions:

```json
"background_templates": [
  "{{ if or (.Output | contains \"work\") (.Output | contains \"company\") (.Output | contains \"@yourcompany.com\") }}#FF5722{{ end }}"
]
```

## Usage

The git config segment will automatically appear in your prompt when you're inside a git repository. The display format is:

```
[icon] user.email@domain.com
```

Examples (depending on chosen theme):

- Domain: `🏠 @gmail.com` vs `🏢 @adobe.com`
- Username: `🏠 ingo.richter+github` vs `🏢 irichter`
- Simple: `🏠 PERSONAL` vs `🏢 WORK`

## Troubleshooting

If the git config doesn't appear:

1. Make sure you're inside a git repository: `git status`
2. Check your git config: `git config user.email`
3. Regenerate the oh-my-posh cache: `rm ~/.cache/oh-my-posh.zsh && source ~/.zshrc`

## File Structure

- `night-owl-custom.omp.json`: The main theme configuration
- `README.md`: This documentation file

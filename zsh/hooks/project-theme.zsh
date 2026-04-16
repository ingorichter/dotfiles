# Ghostty: change window title and background when cd into project dirs
# Detects macOS dark/light mode and applies appropriate colors

# Function to detect if we're in dark mode
is_dark_mode() {
  defaults read -g AppleInterfaceStyle &>/dev/null && return 0 || return 1
}

# Define colors for each project: "path" "dark_color light_color"
# Format: directory path followed by space-separated dark and light colors
typeset -A PROJECT_THEMES_DARK
typeset -A PROJECT_THEMES_LIGHT

# component-core: darker green for dark mode, lighter for light mode
PROJECT_THEMES_DARK[~/develop/work/repos/component-core]="#2d5f1f"  # darker, muted green
PROJECT_THEMES_LIGHT[~/develop/work/repos/component-core]="#90c060" # lighter, softer green

# ccx-sharing: darker blue for dark mode, lighter for light mode  
PROJECT_THEMES_DARK[~/develop/work/repos/ccx-sharing]="#1a3d8f"    # deeper blue
PROJECT_THEMES_LIGHT[~/develop/work/repos/ccx-sharing]="#6090e0"   # lighter blue

project_chpwd() {
  local bg title themes_to_use
  
  # Select the appropriate theme set based on appearance mode
  if is_dark_mode; then
    themes_to_use=(${(kv)PROJECT_THEMES_DARK})
  else
    themes_to_use=(${(kv)PROJECT_THEMES_LIGHT})
  fi
  
  # Check if current directory matches any project
  for dir color in ${themes_to_use}; do
    if [[ $PWD == $dir* ]]; then
      bg=$color
      title=${${PWD:t}//\%/%%}
      printf '\e]11;%s\e\\\e]2;%s\e\\' "$bg" "$title"
      return
    fi
  done
  
  # Reset bg when leaving project
  printf '\e]111\e\\'
}

chpwd_functions+=(project_chpwd)
# Run on shell init if already in a project dir
project_chpwd

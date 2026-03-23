# Ghostty: change window title and background when cd into project dirs
# Edit PROJECT_THEMES to add your paths and colors (Catppuccin-friendly hex)
typeset -A PROJECT_THEMES
PROJECT_THEMES=(
  ~/develop/work/repos/component-core "#419203"
  ~/develop/work/repos/ccx-sharing "#c2d2f6"
)

project_chpwd() {
  local bg title
  for dir color in ${(kv)PROJECT_THEMES}; do
    if [[ $PWD == $dir* ]]; then
      bg=$color title=${${PWD:t}//\%/%%}
      printf '\e]11;%s\e\\\e]2;%s\e\\' "$bg" "$title"
      return
    fi
  done
  printf '\e]111\e\\'  # reset bg when leaving project
}
chpwd_functions+=(project_chpwd)
# Run on shell init if already in a project dir
project_chpwd

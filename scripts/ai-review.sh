#!/bin/bash

# --- Config ---
MODEL="deepseek-coder-v2"
TMP_DIFF_FILE=$(mktemp)
MODE="chunk"
QUIET=false
CHUNK_LINE_COUNT=200
TRUNCATE_BYTE_LIMIT=10000

# --- Cleanup ---
cleanup() {
  rm -f "$TMP_DIFF_FILE"
  rm -f "${TMP_DIFF_FILE}_chunk_"*
}
trap cleanup EXIT

# --- Argument Parsing ---
for arg in "$@"; do
  case $arg in
    --truncate)
      MODE="truncate"
      ;;
    -q|--quiet)
      QUIET=true
      ;;
    *)
      echo "Unknown option: $arg"
      echo "Usage: $0 [--truncate] [--quiet]"
      exit 1
      ;;
  esac
done

# --- Create Diff ---
git diff > "$TMP_DIFF_FILE"
if [ ! -s "$TMP_DIFF_FILE" ]; then
  [ "$QUIET" != true ] && echo "No changes to review."
  exit 0
fi

# --- Truncate Mode ---
if [ "$MODE" = "truncate" ]; then
  DIFF_CONTENT=$(cat "$TMP_DIFF_FILE")

  if [ "$(echo "$DIFF_CONTENT" | wc -c)" -gt "$TRUNCATE_BYTE_LIMIT" ]; then
    [ "$QUIET" != true ] && echo "⚠️ Diff is too large. Truncating to $TRUNCATE_BYTE_LIMIT bytes..." >&2
    DIFF_CONTENT=$(echo "$DIFF_CONTENT" | head -c "$TRUNCATE_BYTE_LIMIT")
  fi

  PROMPT=$(cat <<EOF
You are a code reviewer. Here's a truncated git diff:

$DIFF_CONTENT

Please review the changes. Highlight:
- Potential bugs
- Improvements in clarity or style
- Best practices violations
EOF
)
  echo "$PROMPT" | ollama run "$MODEL"

# --- Chunk Mode ---
else
  # Split the diff into N-line chunks
  split -l "$CHUNK_LINE_COUNT" "$TMP_DIFF_FILE" "${TMP_DIFF_FILE}_chunk_"

  i=0
  for chunk_file in "${TMP_DIFF_FILE}_chunk_"*; do
    ((i++))
    CHUNK_CONTENT=$(cat "$chunk_file")

    PROMPT=$(cat <<EOF
You are a code reviewer. Here's chunk $i of a git diff:

$CHUNK_CONTENT

Please review this part. Highlight:
- Potential bugs
- Improvements in clarity or style
- Best practices violations
EOF
)
    [ "$QUIET" != true ] && echo -e "\n🧩 Reviewing chunk $i...\n"
    echo "$PROMPT" | ollama run "$MODEL"
  done
fi

# MODEL="deepseek-coder-v2"

# PROMPT=$(cat <<EOF
# You are a code reviewer. Review the following simple code diff:

# diff --git a/hello.py b/hello.py
# index abc123..def456 100644
# --- a/hello.py
# +++ b/hello.py
# @@ def greet():
# -    print("Hi")
# +    print("Hello, world!")
# EOF
# )

# echo "$PROMPT" | ollama run "$MODEL"
# # --- Config ---
# # MODEL="deepseek-coder-v2"
# # TMP_DIFF_FILE=$(mktemp)
# # QUIET=false

# # # --- Argument Parsing ---
# # for arg in "$@"; do
# #   case $arg in
# #     -q|--quiet)
# #       QUIET=true
# #       shift
# #       ;;
# #   esac
# # done

# # # --- Cleanup on Exit ---
# # cleanup() {
# #   rm -f "$TMP_DIFF_FILE"
# # }
# # trap cleanup EXIT

# # # --- Create Diff ---
# # git diff > "$TMP_DIFF_FILE"

# # if [ ! -s "$TMP_DIFF_FILE" ]; then
# #   if [ "$QUIET" = false ]; then
# #     echo "No changes to review."
# #   fi
# #   exit 0
# # fi

# # # --- Format Prompt ---
# # PROMPT=$(cat <<EOF
# # You are a code reviewer. Here's the git diff of changed files in the current branch compared to the main branch:

# # $(cat "$TMP_DIFF_FILE")

# # Please review the changes. Highlight:
# # - Potential bugs
# # - Improvements in clarity or style
# # - Best practices violations
# # EOF
# # )

# # # --- Run Ollama ---
# # echo "$PROMPT" | ollama run "$MODEL"
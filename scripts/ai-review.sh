#!/bin/bash
# AI-Review: A script to get AI-powered code reviews for your git changes
# Usage: ai-review.sh [--truncate] [--quiet] [--model MODEL_NAME] [--lines NUM] [--debug]

# --- Config ---
MODEL="deepseek-coder-v2"
TMP_DIFF_FILE=$(mktemp)
TMP_PROMPT_FILE=$(mktemp)
MODE="chunk"
QUIET=false
CHUNK_LINE_COUNT=200
TRUNCATE_BYTE_LIMIT=10000
DEBUG=false

# --- Functions ---
show_help() {
  cat << EOF
Usage: $(basename "$0") [OPTIONS]

Get AI-powered code reviews for your git changes.

Options:
  --truncate         Process the entire diff as one, truncating if too large
  -q, --quiet        Suppress informational messages
  -m, --model NAME   Specify the Ollama model to use (default: deepseek-coder-v2)
  -l, --lines NUM    Number of lines per chunk in chunk mode (default: 200)
  -d, --debug        Enable debug mode
  -h, --help         Show this help message

EOF
  exit 0
}

check_dependencies() {
  for cmd in git ollama; do
    if ! command -v "$cmd" &> /dev/null; then
      echo "Error: '$cmd' is required but not installed." >&2
      exit 1
    fi
  done
  
  # Check if in a git repository
  if ! git rev-parse --is-inside-work-tree &> /dev/null; then
    echo "Error: Not in a git repository." >&2
    exit 1
  fi
  
  # Check if Ollama is running
  if ! ollama list &> /dev/null; then
    echo "Error: Ollama is not running or accessible. Please start the Ollama service." >&2
    exit 1
  fi
  
  # Check if the model exists
  if ! ollama list 2>/dev/null | grep -q "$MODEL"; then
    echo "Warning: Model '$MODEL' may not be available or not showing in list. Available models:" >&2
    ollama list >&2
    echo "You can pull the model with: ollama pull $MODEL" >&2
    read -p "Try to continue anyway? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
      exit 1
    fi
  fi
}

# --- Cleanup ---
cleanup() {
  # Use a function to ensure cleanup commands are reachable
  local cleanup_files=(
    "$TMP_DIFF_FILE"
    "$TMP_PROMPT_FILE"
  )
  
  # Add chunk files using find
  while IFS= read -r file; do
    cleanup_files+=("$file")
  done < <(find "$(dirname "$TMP_DIFF_FILE")" -name "$(basename "${TMP_DIFF_FILE}_chunk_")*" -type f 2>/dev/null)
  
  # Remove all files
  rm -f "${cleanup_files[@]}"
}
trap cleanup EXIT

# --- Argument Parsing ---
while [[ $# -gt 0 ]]; do
  case $1 in
    --truncate)
      MODE="truncate"
      shift
      ;;
    -q|--quiet)
      QUIET=true
      shift
      ;;
    -m|--model)
      if [[ -n "$2" && "$2" != -* ]]; then
        MODEL="$2"
        shift 2
      else
        echo "Error: Argument for $1 is missing" >&2
        exit 1
      fi
      ;;
    -l|--lines)
      if [[ -n "$2" && "$2" != -* && "$2" =~ ^[0-9]+$ ]]; then
        CHUNK_LINE_COUNT="$2"
        shift 2
      else
        echo "Error: Argument for $1 must be a number" >&2
        exit 1
      fi
      ;;
    -d|--debug)
      DEBUG=true
      shift
      ;;
    -h|--help)
      show_help
      ;;
    *)
      echo "Unknown option: $1" >&2
      echo "Use --help for usage information." >&2
      exit 1
      ;;
  esac
done

# --- Check Dependencies ---
check_dependencies

# --- Create Diff ---
if ! git diff > "$TMP_DIFF_FILE" 2>/dev/null; then
  echo "Error: Failed to create git diff." >&2
  exit 1
fi

if [ ! -s "$TMP_DIFF_FILE" ]; then
  [ "$QUIET" != true ] && echo "No changes to review."
  exit 0
fi

# --- Debug Info ---
if [ "$DEBUG" = true ]; then
  echo "Debug info:"
  echo "- Model: $MODEL"
  echo "- Mode: $MODE"
  echo "- Temp diff file: $TMP_DIFF_FILE"
  echo "- Diff size: $(wc -c < "$TMP_DIFF_FILE") bytes"
fi

create_prompt() {
  local content=$1
  local chunk_num=$2
  local total_chunks=$3
  
  {
    echo "You are a code reviewer."
    
    if [ -n "$chunk_num" ] && [ -n "$total_chunks" ]; then
      echo "Here's chunk $chunk_num of $total_chunks of a git diff:"
    else
      echo "Here's a truncated git diff:"
    fi
    
    echo
    echo "$content"
    echo
    echo "Please review the changes. Highlight:"
    echo "- Potential bugs"
    echo "- Improvements in clarity or style"
    echo "- Best practices violations"
    echo "- Security vulnerabilities"
    echo "- Performance issues"
    echo "- Code smells"
    echo "- Documentation issues"
    echo "- Readability issues"
    echo "- Any other issues you find"
  } > "$TMP_PROMPT_FILE"
}

# --- Truncate Mode ---
if [ "$MODE" = "truncate" ]; then
  DIFF_CONTENT=$(cat "$TMP_DIFF_FILE")
  DIFF_SIZE=${#DIFF_CONTENT}

  if [ "$DIFF_SIZE" -gt "$TRUNCATE_BYTE_LIMIT" ]; then
    [ "$QUIET" != true ] && echo "⚠️ Diff is too large. Truncating to $TRUNCATE_BYTE_LIMIT bytes..." >&2
    DIFF_CONTENT="${DIFF_CONTENT:0:$TRUNCATE_BYTE_LIMIT}"
  fi

  create_prompt "$DIFF_CONTENT"
  
  [ "$DEBUG" = true ] && echo "Running Ollama with model: $MODEL"
  
  # Run Ollama with error handling
  if ! ollama run "$MODEL" < "$TMP_PROMPT_FILE"; then
    echo "Error: Failed to run Ollama with model '$MODEL'" >&2
    exit 1
  fi
else
  # --- Chunk Mode ---
  # Split the diff into N-line chunks
  split -l "$CHUNK_LINE_COUNT" "$TMP_DIFF_FILE" "${TMP_DIFF_FILE}_chunk_"
  
  # Count how many chunks we have for better progress reporting
  CHUNK_COUNT=$(find "$(dirname "$TMP_DIFF_FILE")" -name "$(basename "${TMP_DIFF_FILE}_chunk_")*" -type f 2>/dev/null | wc -l)
  [ "$QUIET" != true ] && echo "Reviewing changes in $CHUNK_COUNT chunks..."
  
  # Check if any chunks were created
  if [ "$CHUNK_COUNT" -eq 0 ]; then
    echo "Error: No chunks were created. This might indicate an issue with the 'split' command." >&2
    exit 1
  fi

  # Process each chunk
  i=0
  for chunk_file in "${TMP_DIFF_FILE}_chunk_"*; do
    ((i++))
    [ "$DEBUG" = true ] && echo "Reading chunk file: $chunk_file"
    
    CHUNK_CONTENT=$(cat "$chunk_file")
    CHUNK_SIZE=${#CHUNK_CONTENT}

    [ "$QUIET" != true ] && echo -e "\n🧩 Reviewing chunk $i of $CHUNK_COUNT (${CHUNK_SIZE} bytes)...\n"
    
    create_prompt "$CHUNK_CONTENT" "$i" "$CHUNK_COUNT"
    
    [ "$DEBUG" = true ] && echo "Running Ollama with model: $MODEL"
    
    # Run Ollama with error handling
    if ! ollama run "$MODEL" < "$TMP_PROMPT_FILE"; then
      echo "Error: Failed to run Ollama with model '$MODEL' for chunk $i" >&2
      echo "Try running with --debug for more information." >&2
      exit 1
    fi
    
    [ "$DEBUG" = true ] && echo "Finished processing chunk $i"
  done
fi

exit 0
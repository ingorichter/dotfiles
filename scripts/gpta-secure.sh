#!/bin/bash

# Get the API key securely from the macOS keychain
# security add-generic-password -a "$USER" -s 'OPENAI_CLI_TOKEN' -w 'OPENAI_API_KEY'  # Uncomment this line to add the API key to the Keychain
API_KEY=$(security find-generic-password -a "${USER}" -s OPENAI_CLI_TOKEN -w 2>/dev/null)

if [ -z "$API_KEY" ]; then
  echo "❌ Error: API key not found in Keychain."
  exit 1
fi

# Run gpta with the required parameters
gpta -k "$API_KEY" "$@"
#!/bin/bash

# List all model names from plain text output
models=$(ollama list | tail -n +2 | awk '{print $1}')

for model in $models; do
  echo "🔄 Updating: $model"
  ollama pull "$model"
done
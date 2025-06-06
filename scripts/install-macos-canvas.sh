#!/usr/bin/env bash

set -euo pipefail

echo "🔧 Installing required Homebrew packages..."
brew install \
  pkg-config cairo pango libpng jpeg giflib librsvg \
  zlib freetype fontconfig expat libffi graphite2 \
  harfbuzz gobject-introspection

echo "🐍 Ensuring Python 3.11 is installed with asdf..."
if ! asdf list python | grep -q 3.11.8; then
  asdf install python 3.11.8
fi

echo "📌 Setting Python 3.11.8 as global version manually..."
# Bypass broken `asdf global` by writing .tool-versions
sed -i '' '/^python /d' ~/.tool-versions 2>/dev/null || true
echo "python 3.11.8" >> ~/.tool-versions
asdf reshim python
PYTHON_BIN="$(asdf which python)"

echo "🌍 Setting up PKG_CONFIG_PATH for canvas build..."
export PKG_CONFIG_PATH="/opt/homebrew/opt/zlib/lib/pkgconfig:\
/opt/homebrew/opt/libpng/lib/pkgconfig:\
/opt/homebrew/opt/freetype/lib/pkgconfig:\
/opt/homebrew/opt/fontconfig/lib/pkgconfig:\
/opt/homebrew/opt/expat/lib/pkgconfig:\
/opt/homebrew/opt/libffi/lib/pkgconfig:\
/opt/homebrew/opt/graphite2/lib/pkgconfig:\
/opt/homebrew/opt/harfbuzz/lib/pkgconfig:\
/opt/homebrew/opt/pango/lib/pkgconfig"

echo "🧼 Cleaning npm cache..."
npm uninstall -g canvas || true
npm cache clean --force

echo "🚀 Installing canvas from source..."
PYTHON="$PYTHON_BIN" npm install -g canvas --build-from-source

echo "✅ canvas installed successfully."
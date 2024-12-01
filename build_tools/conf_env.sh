#!/bin/bash

OS=$(uname)

if [[ "$OS" == "Darwin" ]]; then
  echo "Installing GHC, Happy, and Alex on macOS..."

  brew install ghc cabal-install
  cabal update
  cabal install happy alex readline

elif [[ "$OS" == "Linux" ]]; then
  echo "Installing GHC, Happy, and Alex on Linux..."
  sudo apt update
  sudo apt install ghc happy alex

else
  echo "Unsupported operating system. Please check your system and try again."
  exit 1
fi

#!/bin/bash

OS=$(uname)

if [[ "$OS" == "Darwin" ]]; then
  echo "Installing GHC, Happy, and Alex on macOS..."

  brew install cabal-install ghc readline
  cabal update
  cabal install happy alex 
  cabal install --lib readline


elif [[ "$OS" == "Linux" ]]; then
  echo "Installing GHC, Happy, and Alex on Linux..."
  sudo apt update
  sudo apt install ghc happy alex

else
  echo "Unsupported operating system. Please check your system and try again."
  exit 1
fi

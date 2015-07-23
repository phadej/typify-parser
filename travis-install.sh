#!/bin/sh

set -ex

if [ "$TESTSUITE" = "STACK" ]; then
  # Download stack
  mkdir -p ~/.local/bin
  curl -L https://github.com/commercialhaskell/stack/releases/download/v0.1.2.0/stack-0.1.2.0-x86_64-linux.gz | gunzip > ~/.local/bin/stack
  chmod a+x ~/.local/bin/stack

  # Change directory
  cd hs

  # Setup snapshot
  stack --no-terminal --skip-ghc-check setup
  stack --no-terminal --skip-ghc-check test --only-snapshot
else
  npm install
fi

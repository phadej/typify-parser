#!/bin/sh

set -ex

if [ "$TESTSUITE" = "STACK" ]; then
  cd hs
  stack test
else
  npm test
fi

#!/bin/bash -eu

hsTmpDir=$1

stack ghc --                   \
    --make                     \
    -threaded                  \
    -odir $hsTmpDir            \
    -hidir $hsTmpDir           \
    -o $hsTmpDir/simple-client \
    $hsTmpDir/Simple.hs        \
    tests/TestClient.hs        \
    > /dev/null

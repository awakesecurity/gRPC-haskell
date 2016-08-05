#!/bin/bash -eu

hsTmpDir=$1

stack ghc --                   \
    --make                     \
    -threaded                  \
    -odir $hsTmpDir            \
    -hidir $hsTmpDir           \
    -o $hsTmpDir/simple-server \
    $hsTmpDir/Simple.hs        \
    tests/TestServer.hs        \
    > /dev/null

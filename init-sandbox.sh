#! /bin/sh

cabal sandbox init
cabal sandbox add-source deps/snap/deps/io-streams
cabal sandbox add-source deps/snap/deps/io-streams-haproxy
cabal sandbox add-source deps/snap/deps/snap-core
cabal sandbox add-source deps/snap/deps/snap-server
cabal sandbox add-source deps/snap/deps/xmlhtml
cabal sandbox add-source deps/snap/deps/heist
cabal sandbox add-source deps/snap

cabal sandbox add-source deps/servant/servant
cabal sandbox add-source deps/servant/servant-docs
cabal sandbox add-source deps/servant/servant-client
cabal sandbox add-source deps/servant/servant-blaze
cabal sandbox add-source deps/servant/servant-js
cabal sandbox add-source deps/servant/servant-lucid
cabal sandbox add-source deps/servant/servant-mock

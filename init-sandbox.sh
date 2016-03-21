#! /bin/sh

cabal sandbox init

echo "Depend on snap-1.0 from github. Move to Hackage version when snap-1.0 is released"
cabal sandbox add-source deps/snap/deps/io-streams
cabal sandbox add-source deps/snap/deps/io-streams-haproxy
cabal sandbox add-source deps/snap/deps/snap-core
cabal sandbox add-source deps/snap/deps/snap-server
cabal sandbox add-source deps/snap/deps/xmlhtml
cabal sandbox add-source deps/snap/deps/heist
cabal sandbox add-source deps/snap-loader-static
cabal sandbox add-source deps/snap-loader-static
cabal sandbox add-source deps/snap
cabal sandbox add-source deps/hspec-snap

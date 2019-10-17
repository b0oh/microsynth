#!/usr/bin/env bash

rm -rf build
mkdir build

git clone .git --branch gh-pages build

./build.sh

pushd build || exit
git add --all
git commit -m "Release v$(date -u +%FT%TZ)" --amend
git push origin gh-pages -f

popd || exit
git push origin gh-pages -f

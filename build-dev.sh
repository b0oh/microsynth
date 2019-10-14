#!/usr/bin/env bash

mkdir -p build
rm -rf build/*

npm run build-dev

HASH=$(md5sum build/index.js | awk '{print $1}')
FILE=$HASH.js

cp build/index.js "build/$FILE"
sed "s/index.js/$FILE/g" src/index.html > build/index.html

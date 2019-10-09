mkdir -p build
rm -rf build/*

cp src/index.html build/index.html

npm run build-dev

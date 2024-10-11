#!/bin/bash
cd "$(dirname "$0")" || exit
esbuild worklet_processor.js --bundle --minify --format=esm --outfile=../worklet_processor.min.js --platform=browser
echo "Processor minifed succesfully"
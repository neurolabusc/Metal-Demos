#!/bin/sh
# change to working directory to location of command file: http://hints.macworld.com/article.php?story=20041217111834902
here="`dirname \"$0\"`"
cd "$here" || exit 1
find "$here" -name '*.DS_Store' -type f -delete
./basic/_clean.command
cd "$here" || exit 1
./colorbar/_clean.command
cd "$here" || exit 1
./common/_clean.command
cd "$here" || exit 1
./compute/_clean.command
cd "$here" || exit 1
./font/_clean.command
cd "$here" || exit 1
./gl/_clean.command
cd "$here" || exit 1
./lines/_clean.command
cd "$here" || exit 1
./mesh/_clean.command
cd "$here" || exit 1
./metal_or_gl/_clean.command
cd "$here" || exit 1
./texture/_clean.command
cd "$here" || exit 1
./volumerender/_clean.command
cd "$here" || exit 1
./occlusion/_clean.command
cd "$here" || exit 1
find . -name "*.exe" -exec rm '{}' \;
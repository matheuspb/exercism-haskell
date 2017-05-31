#!/bin/sh

set -e
#hlint ${TRAVIS_BUILD_DIR} # Run `hlint` on the entire repository.

for exercise in ./*/ ; do
	[ -d $exercise ] || continue
	cd $exercise && stack test && cd ..
done

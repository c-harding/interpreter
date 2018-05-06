#!/usr/bin/env bash
haddock --html --pretty-html --hyperlinked-source -o docs {,**/}*.hs

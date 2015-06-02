#!/bin/sh
set -e

script() {
    cargo build -v
    if [ "$TRAVIS_RUST_VERSION" = "nightly" ]; then
        cargo test -v
        cargo doc -v --no-deps
    fi
}

after_success() {
    if ([ "$TRAVIS_BRANCH" = master ] &&
        [ "$TRAVIS_RUST_VERSION" = "nightly" ] &&
        [ "$TRAVIS_PULL_REQUEST" = false ]); then
        sudo pip install ghp-import
        ghp-import -n target/doc
        git push -fq https://${GH_TOKEN}@github.com/${TRAVIS_REPO_SLUG}.git gh-pages 2>/dev/null
    fi
}

$@

#!/bin/sh
# TODO: Figure out a better way to do this.
# TODO: If we decide to keep it we might as well rewrite it in Guile

REPOS_ROOT="$HOME/engstrand-config"
PACKAGES_ROOT="$REPOS_ROOT/guix-dotfiles/engstrand/packages"

for repo in dwm dmenu st dsblocks utils
do
    # Make sure to delete all non-tracked file to not get a hash mismatch
    cd $REPOS_ROOT/$repo
    git clean -fd

    git_hash=$(git -C $REPOS_ROOT/$repo log -n 1 --format=format:"%H")
    guix_hash=$(guix hash --hash=sha256 --format=nix-base32 -rx $REPOS_ROOT/$repo)
    echo $repo $git_hash $guix_hash
    sed -i "s/(commit \"[0-9a-f]\{40\}\")/(commit \"$git_hash\")/g" $PACKAGES_ROOT/engstrand-$repo.scm
    sed -i "s/(base32 \".\+\")/(base32 \"$guix_hash\")/g" $PACKAGES_ROOT/engstrand-$repo.scm
done

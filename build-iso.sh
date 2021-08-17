#!/bin/sh
# Based on https://github.com/systemcrafters/guix-install

# -----------------------------------------------------------------------------
# Utilities
# -----------------------------------------------------------------------------

die() {
    # **
    # Prints a message to stderr & exits script with non-successful code "1"
    # *

    printf '%s\n' "$@" >&2
    exit 1
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

# Write out the channels file so it can be included
guix time-machine -C './engstrand/channels.scm' -- \
     describe -f channels > './channels.scm'

# Build the image
printf 'Attempting to build the image...\n\n'
image=$(guix time-machine -C './channels.scm' -- system image -t iso9660 './engstrand/installer.scm') \
    || die 'Could not create image.'

release_tag=$(date +"%Y%m%d%H%M")
cp "${image}" "./guix-installer-${release_tag}.iso" ||
    die 'An error occurred while copying.'

printf 'Image was succesfully built: %s\n' "${image}"

# cleanup
rm -f ./channels.scm
unset -f die
unset -v image release_tag

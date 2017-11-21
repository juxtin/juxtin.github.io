#!/bin/bash

# USAGE:
#  deploy.sh [target_branch]

GREEN='\033[0;32m'
LIGHT_GREEN='\033[1;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

if [[ -n "${1}" ]]; then
    TARGET=$1
else
    TARGET='master'
fi

function step () {
    printf "${LIGHT_GREEN}  $*  ${NC}\n"
}

function substep () {
    printf "${GREEN}      $*  ${NC}\n"
}

function error {
    printf "${RED} $* ${NC}\n"
    exit 1
}

step "Deploying to ${TARGET}:"

substep "Building site..."
stack build && stack exec ppj-blog build || error "Unable to build site!"

substep "Stashing and checking out ${TARGET}..."
git stash save || fail "Unable to save stash!"
git checkout ${TARGET} || git checkout master && git checkout -b ${TARGET} || error "Unable to checkout ${TARGET}"

substep "On ${TARGET}. Copying files..."
cp -a _site/. . || error "Unable to copy!"

substep "Committing new contents..."
git add -A && git commit -m "automated deployment" || error "Unable to commit!"

substep "Pushing to github pages..."
git push holguinj HEAD || error "Unable to push!"

step "Deployed!"

step "Returning things back to normal."

substep "Checking out previous branch..."
git checkout - || error "Unable to return to original branch!"

substep "Applying stash..."
git stash pop || error "Unable to apply stash!"

step "All done!"

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

function error () {
    printf "${RED} $* ${NC}\n"
    exit 1
}

function working_dir_clean () {
    git diff-index --quiet HEAD
}

step "Ensuring that the git working directory is clean..."
if working_dir_clean; then
    substep "all changes are committed."
elif [[ -n $ALLOW_DIRTY_DEPLOY ]]; then
    substep "deploying despite dirt."
else
    error "You have uncommitted changes! Please commit, stash, or reset before deploying."
fi

step "Deploying to ${TARGET}:"

substep "Building site..."
stack build && stack exec ppj-blog build || error "Unable to build site! Check above for errors."

substep "Checking out ${TARGET}..."
git checkout ${TARGET} || error "Unable to checkout ${TARGET}! Please make sure that branch exists."

substep "On ${TARGET}. Copying files..."
cp -a _site/. . || error "Unable to copy!"

substep "Committing new contents..."
git add -A && git commit -m "automated deployment" || error "Unable to commit! Maybe a git config issue?"

substep "Pushing to github pages..."
git push holguinj HEAD || error "Unable to push! That seems weird."

step "Deployed!"

step "Returning things back to normal."

substep "Checking out previous branch..."
git checkout - || error "Unable to return to original branch! What kind of nonsense is this?"

step "All done!"
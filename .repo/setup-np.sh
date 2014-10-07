#!/bin/sh

TOPLEVEL=`git rev-parse --show-toplevel`
SETUP="$TOPLEVEL/.repo/setup.sh"
"$SETUP"

echo "Enabling commit tracking"
echo "\$GIT_DIR/../.repo/repotrack" >> $TOPLEVEL/.git/hooks/post-commit

echo "Setting up multi-remotes"

git remote set-url --add --push origin https://np@scm.icebergsys.net/git/projects/chemlogic
git remote set-url --add --push origin https://nicholaspaun@github.com/nicholaspaun/chemlogic.git

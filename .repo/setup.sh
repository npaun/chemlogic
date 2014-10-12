#!/bin/sh
#setup.sh: A script for setting up the development environment
#(C) Copyright 2014 Nicholas Paun

GITDIR=`git rev-parse --git-dir`
echo "Enabling post-commit hook"
echo "#!/bin/sh" >> $GITDIR/hooks/post-commit
echo >> $GITDIR/hooks/post-commit
chmod u+x $GITDIR/hooks/post-commit


#!/bin/sh

GITDIR=`git rev-parse --git-dir`
echo "Enabling post-commit hook"
echo "#!/bin/sh" >> $GITDIR/hooks/post-commit
chmod u+x $GITDIR/hooks/post-commit


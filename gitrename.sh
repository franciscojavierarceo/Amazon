#!/bin/sh

git filter-branch --env-filter '
OLD_EMAIL="Francsico.Arceo@cba.com.au"
CORRECT_NAME="Francisco Arceo"
CORRECT_EMAIL="arceofrancisco@gmail.com"
if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_COMMITTER_NAME="$CORRECT_NAME"
    export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
fi
if [ "$GIT_AUTHOR_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_AUTHOR_NAME="$CORRECT_NAME"
    export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
fi
' --tag-name-filter cat -- --branches --tags --f

# Run this using the following
# source gitrename.sh
# git push --force --tags origin 'refs/heads/*'
# git log --pretty=format:"%ae - %cd"

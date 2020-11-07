#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

declare tmp=`mktemp -d`
git clone git@github.com:tennysontbardwell/public.git $tmp
rsync -avh $DIR $tmp/ --delete
cd $tmp && git add -A && git commit -m 'automatic sync' && git push
rm -rf $tmp


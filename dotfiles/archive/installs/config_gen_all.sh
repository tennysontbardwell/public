#/usr/bin/env bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOTFILES="$DIR/.."

if [ "$1" == '--root' ];
then
    _ROOT=1
elif [ "$1" == '--user' ];
then
    _USER=1
else
    _ROOT=1
    _USER=1
fi

if [ $_ROOT ];
then
    for f in $(find $DOTFILES -name 'config_gen_root.sh'); do
        sudo $f
    done
fi
if [ $_USER ];
then
    for f in $(find $DOTFILES -name 'config_gen.sh'); do
        $f
    done
fi

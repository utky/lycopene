#!/bin/sh

lycohome="${HOME}/.lyco"

if [ -d ${lycohome} ] ; then
    rm -r ${lycohome}
fi

./dist/build/lyco/lyco configure

#!/bin/sh

rm -rf ${HOME}/.lyco && cp ./dist/build/lyco/lyco ${HOME}/bin && ${HOME}/bin/lyco configure

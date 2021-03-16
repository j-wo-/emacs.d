#!/bin/bash
set -eux

fd -I -e elc -X sudo rm

sudo rm -rf eln-cache 2> /dev/null

sudo rm -rf elpa 2> /dev/null
mkdir elpa
cp -r color-theme-sanityinc-tomorrow elpa/

#emacs -nw --eval "(kill-emacs)"
#emacs -nw --eval "(jeff/native-comp-all)"
#/usr/local/bin/emacs -nw --eval "(progn (add-hook 'comp-async-all-done-hook 'kill-emacs) (jeff/native-comp-all))"

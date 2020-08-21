#!/bin/bash
rm -rf *.elc 2> /dev/null
rm -rf eln-cache 2> /dev/null
rm -rf elpa 2> /dev/null ; mkdir elpa

#emacs -nw --eval "(kill-emacs)"
#/usr/local/bin/emacs -nw --eval "(progn (add-hook 'comp-async-all-done-hook 'kill-emacs) (jeff/native-comp-all))"

#!/bin/sh -e

mkdir -p ~/.emacs.d/site-lisp/cedet/
mkdir -p ~/.emacs.d/site-lisp/ecb/

(cd cedet; make clean; make)

cp -ra cedet/* ~/.emacs.d/site-lisp/cedet/
cp -ra ecb/*	~/.emacs.d/site-lisp/ecb/

(cd ecb; make CEDET=~/.emacs.d/site-lisp/cedet/)

mkdir -p ~/.emacs.d/site-lisp/auto-complete/
(cd auto-complete; make; make install DIR=~/.emacs.d/site-lisp/auto-complete/)
#cp -ra auto-complete	~/emacs.d/site-lisp/auto-complete

cp xcscope.elc ~/.emacs.d/site-lisp/
sudo cp xcscope/cscope-indexer /usr/bin/cscope-indexer

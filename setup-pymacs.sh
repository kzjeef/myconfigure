#/usr/bin/bash -e

curl -L https://github.com/pinard/Pymacs/tarball/v0.24-beta2 | tar zx
cd pinard-Pymacs-016b0bc
make 
cp pymacs.el ~/.emacs.d/site-lisp/
cp pymacs.el ~/.emacs.d/site-lisp/
emacs -batch -eval '(byte-compile-file "~/.emacs.d/site-lisp/pymacs.el")' 

sudo easy_install ropemacs





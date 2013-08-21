#!/bin/sh -e

mkdir -p ~/.emacs.d/site-lisp/cedet/
mkdir -p ~/.emacs.d/site-lisp/ecb/

(cp bison-flex/*.el ~/.emacs.d/site-lisp/)

#(cd color-theme-6.6.0/;cp -r color-theme* themes  ~/.emacs.d/site-lisp/) 

#(cd cedet; find . | xargs touch;  make clean-all; make)

#cp -r cedet/* ~/.emacs.d/site-lisp/cedet/

#(cd ecb; make CEDET=~/.emacs.d/site-lisp/cedet/)
#cp -r ecb/*	~/.emacs.d/site-lisp/ecb/

mkdir -p ~/.emacs.d/site-lisp/auto-complete/
(cd auto-complete; make; make install DIR=~/.emacs.d/site-lisp/auto-complete/)
#cp -ra auto-complete	~/emacs.d/site-lisp/auto-complete

cp xcscope/xcscope.el* ~/.emacs.d/site-lisp/
sudo cp xcscope/cscope-indexer /usr/bin/cscope-indexer

cp fic-mode.el ~/.emacs.d/site-lisp/
cp google-c-style.el ~/.emacs.d/site-lisp/

(cd  magit; make ; sudo make install)

(cd git; make ; make install DESTDIR=~/.emacs.d/)

#(cd cflow-1.4; ./configure &&  make && make install)

(cd elscreen-1.4.6; emacs --batch --eval '(byte-compile-file "elscreen.el")';  cp elscreen.el* ~/.emacs.d/site-lisp/)


#python
(cd python-mode.el-6.1.1;emacs --batch --eval '(byte-compile-file "python-mode.el")';mkdir -p ~/.emacs.d/site-lisp/python/; cp * -R ~/.emacs.d/site-lisp/python/);

(cp -r  nxhtml-2.08 ~/.emacs.d/site-lisp/nxhtml)

emacs --batch --eval '(byte-compile-file "js2-mode.el")';
cp js2-mode.elc ~/.emacs.d/site-lisp/

cp android-mode.el ~/.meacs.d/site-lisp/

sudo apt-get install ttf-inconsolata
sudo fc-cache -fv

#(cp -r multipe-cursors ~/.emacs.d/site-lisp/)


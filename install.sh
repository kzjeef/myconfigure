#!/bin/sh -e

platform='unknown'
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   platform='linux'
elif [[ "$unamestr" == 'Darwin' ]]; then
     platform='mac'
fi

TARGET_DIR=~/.emacs.d/site-lisp/

git submodule init 
git submodule update
git submodule sync


ln -s  `pwd`/bash_env ~/.bash_env

mkdir -p ${TARGET_DIR}

if [[ "$platform" == 'mac' ]]; then
    brew install the_silver_searcher
else
    sudo apt-get install texinfo silversearcher-ag
    sudo apt-get install ttf-inconsolata
    sudo fc-cache -fv
fi

(cp bison-flex/*.el ${TARGET_DIR})

mkdir -p ${TARGET_DIR}auto-complete/
(cd auto-complete; make; make install DIR=${TARGET_DIR}auto-complete/)
#cp -ra auto-complete	~/emacs.d/site-lisp/auto-complete

cp xcscope/xcscope.el* ${TARGET_DIR}
sudo cp xcscope/cscope-indexer /usr/bin/cscope-indexer

cp fic-mode.el ${TARGET_DIR}
cp google-c-style.el ${TARGET_DIR}

(cd  magit; make ; sudo make install)

(cd git; make ; make install DESTDIR=~/.emacs.d/)

(cd elscreen-1.4.6; emacs --batch --eval '(byte-compile-file "elscreen.el")';  cp elscreen.el* ${TARGET_DIR})
#python
(cd python-mode.el-6.1.1;emacs --batch --eval '(byte-compile-file "python-mode.el")';mkdir -p ${TARGET_DIR}python/; cp * -R ${TARGET_DIR}python/);

(cp -r  nxhtml-2.08 ${TARGET_DIR}nxhtml)
(cp kermit.el ${TARGET_DIR})

emacs --batch --eval '(byte-compile-file "js2-mode.el")';
install js2-mode.elc ${TARGET_DIR}
install android-mode.el ${TARGET_DIR}
install highlight-80+.el ${TARGET_DIR}

(cd web-mode; emacs --batch --eval '(byte-compile-file "web-mode.el")'; emacs --batch --eval '(byte-compile-file "wfs-mode.el")'; \
 install web-mode.elc ${TARGET_DIR}; install wfs-mode.elc ${TARGET_DIR}; \
);


(mkdir -p ${TARGET_DIR}/themes/; cp themes/* ${TARGET_DIR}/themes)

(cp enhanced-ruby-mode/enh-ruby-mode.el ${TARGET_DIR}/)

(cp multi-term.el ${TARGET_DIR})

(cp -r yas-rails ${TARGET_DIR})

(cp -r ace-jump-mode ${TARGET_DIR})

# finally need to recompile all files under install dir.
emacs --batch --eval '(byte-recompile-directory "~/.emacs.d/site-lisp/")'


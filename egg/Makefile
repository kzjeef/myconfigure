# Generated automatically from Makefile.in by configure.
# Makefile --- Makefile of EGG V4.0

# Copyright (C) 1999, 2000 Free Software Foundation, Inc
# Author: NIIBE Yutaka <gniibe@chroot.org>
#         TOMURA Satoru <tomura@etl.go.jp>
# Maintaner: Satoru Tomura <tomura@etl.go.jp>

.SUFFIXES:
SHELL	= /bin/sh

srcdir = .
top_srcdir = .

#------------------------------------------------
#  Configuration parameters
#------------------------------------------------
# emacs you use
EMACS	= emacs
# emacs lisp installation directory
lispdir	= ${prefix}/share/emacs/site-lisp
#------------------------------------------------

prefix	= /usr/local
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL_PROGRAM}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_INFO = install-info

#---------------------------------------
# (1) make
#       ;; compile *.el files
# (2) make install
#       ;; install files into the emacs site-lisp directory
#       ;; ex. /usr/local/share/emacs/site-lisp/egg

DEPS	= -l $(top_srcdir)/docomp.el
BATCHFLAGS	= -batch -q -no-site-file -no-init-file

ETCS	= Makefile docomp.el \
	AUTHORS ChangeLog README TODO PROBLEMS

INITELS	= eggrc leim-list.el

SRCS	= ${TOPSRCS} ${EGGSRCS} ${ITSSRCS}

TOPSRCS	= \
	egg.el \
	egg-edep.el \
	egg-com.el \
	egg-cnv.el \
	egg-mlh.el \
	egg-sim.el \
	menudiag.el \
	its.el \
	its-keydef.el \
	leim-list.el \

#
EGGSRCS	= \
	egg/cannarpc.el egg/canna.el \
	egg/sj3rpc.el egg/sj3.el \
	egg/wnnrpc.el egg/wnn.el \
	egg/anthyipc.el egg/anthy.el

#
AYNU	= #  its/aynu.el

ITSSRCS	= \
	its/ascii.el \
	its/bixing.el \
	its/erpin.el \
	its/greek.el \
	its/hankata.el \
	its/hira.el \
	its/jeonkak.el \
        its/greek.el \
	its/pinyin.el \
	its/hangul.el \
	its/kata.el \
	its/thai.el \
	its/quanjiao.el \
	its/zenkaku.el \
	its/zhuyin.el \
	its/jiskana.el \
	${AYNU}

#
ELCS	= ${SRCS:.el=.elc}

TOPELCS	= ${TOPSRCS:.el=.elc}

EGGELCS	= ${EGGSRCS:.el=.elc}

ITSELCS	= ${ITSSRCS:.el=.elc}

DIST	= ${ETCS} ${SRCS} ${INITELS}


all: ${ELCS}

.SUFFIXES: .el .elc

.el.elc:
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile $<

clean: 
	rm -f ${ELCS} *~ */*~ \#* .\#* */\#* */.\#*

distclean: 
	rm -f ${ELCS} config.* Makefile 

install: install-site

install-site: all
	echo "Egg system will be installed in ${lispdir}/egg...."; \
	if [ -d ${lispdir}/egg ]; then \
	  echo "Clean up the previsous installation...."; \
	  rm -rf ${lispdir}/egg/*;  \
	else \
	  echo "Make the directory ${lispdir}/egg..."; \
	  mkdir ${lispdir}/egg; \
	fi; \
#
	for FILE in ${TOPSRCS} ${TOPELCS} ${INITELS}; \
	  do  \
	    ${INSTALL_DATA}  $${FILE} ${lispdir}/egg/; \
	  done; \
#
	mkdir ${lispdir}/egg/egg ; \
	for FILE in ${EGGSRCS} ${EGGELCS} ; \
	  do  \
	    ${INSTALL_DATA}  $${FILE} ${lispdir}/egg/egg; \
	  done; \
	touch ${lispdir}/egg/egg/.nosearch; \
#
	mkdir ${lispdir}/egg/its ; \
	for FILE in ${ITSSRCS} ${ITSELCS} ; \
	  do  \
	    ${INSTALL_DATA}  $${FILE} ${lispdir}/egg/its ; \
	  done; \
	touch ${lispdir}/egg/its/.nosearch; \


uninstall-site:
	if [ -d ${lispdir}/egg ]; then \
	  rm -rf ${lispdir}/egg; \
	fi

# DEPENDENCIES
egg/sj3rpc.elc: egg-com.elc egg/sj3.elc
egg/wnnrpc.elc: egg-com.elc egg/wnn.elc

egg.elc its/ascii.elc its/aynu.elc its/erpin.elc its/greek.elc \
its/hankata.elc its/hira.elc its/jeonkak.elc its/pinyin.elc \
its/hangul.elc its/kata.elc its/quanjiao.elc \
its/zenkaku.elc its/zhuyin.elc: its-keydef.elc

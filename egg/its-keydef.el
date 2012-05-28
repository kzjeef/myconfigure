;;; its-keydef.el

;; Copyright (C) 1999, 2000 PFU LIMITED

;; Author: KATAYAMA Yoshio <kate@pfu.co.jp>

;; Maintainer: TOMURA Satoru <tomura@etl.go.jp>

;; Keywords: mule, multilingual, input method

;; Keywords: mule, multilingual, input method

;; This file is part of EGG.

;; EGG is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; EGG is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;;; Code:



(eval-when-compile
  (require 'cl))

(defvar its-zhuyin nil)
(make-variable-buffer-local 'its-zhuyin)
(put 'its-zhuyin 'permanent-local t)

(defvar its-select-alist nil)
(make-variable-buffer-local 'its-select-func-alist)
(setq-default its-select-func-alist nil)
(put 'its-select-alist 'permanent-local t)

(defvar its-select-func-default-alist nil)

(eval-when-compile
  (defmacro its-set-select-func-alist (list)
    `'(setq ,list (cons (cons lang func)
		       (delq (assq lang ,list) ,list)))))

(eval-and-compile
  (defun its-make-select-func (key1 key2 func file map &optional zhuyin)
    (setq func (intern (concat "its-select-" (symbol-name func)))
	  file (intern (concat "its/" (symbol-name file)))
	  map (intern (concat "its-" (symbol-name map) "-map")))
    (cons
     `(defun ,func (&optional temporally mode-line-unchange)
	(interactive "P")
	(let ((inhibit-read-only t)
	      (func ',func)
	      lang)
	  (if temporally
	      (its-select-mode-temporally func)
	    (require ',file)
	    (cond
	     ((its-in-fence-p)
	      (its-input-end)
	      (its-put-cursor t))
	     ((egg-conversion-fence-p)
	      (egg-exit-conversion)))
	    (setq its-current-select-func func
		  its-current-map ',map
		  lang (its-get-language ,map))
	    (when lang
	      (setq its-current-language lang)
	      ;; avoid overwriting when select temporally
	      (when (and (null its-previous-select-func)
			 (null (assq lang its-select-func-default-alist)))
		,(its-set-select-func-alist its-select-func-alist)
		,(its-set-select-func-alist its-select-func-default-alist)))
	    ,(if zhuyin `(setq its-zhuyin ,(eq zhuyin 'T)))
	    (if (null mode-line-unchange)
		(its-set-mode-line-title)))))
     `(,func ,(concat "\C-x\C-m" key1) ,(concat "\e" key2)))))

(defmacro its-do-list-make-select-func (list)
  (let (funcs keydefs pair)
    (while list
      (setq pair (apply 'its-make-select-func (car list))
	    funcs (cons (car pair) funcs)
	    keydefs (cons (cdr pair) keydefs)
	    list (cdr list)))
    `(progn
       ,@funcs
       (defvar its-define-select-key-list ',keydefs))))

(defmacro its-add-select-funcs (list)
  (let (funcs keydefs pair)
    (while list
      (setq pair (apply 'its-make-select-func (car list))
	    funcs (cons (car pair) funcs)
	    keydefs (cons (cdr pair) keydefs)
	    list (cdr list)))
    `(progn
       ,@funcs
       (setq its-define-select-key-list
	     (append ',keydefs its-define-select-key-list)))))

(defun its-define-select-keys (map &optional fence)
  (let ((key-list its-define-select-key-list))
    (while key-list
      (define-key map (nth 1 (car key-list)) (car (car key-list)))
      (if fence
	  (define-key map (nth 2 (car key-list)) (car (car key-list))))
      (setq key-list (cdr key-list)))))

(its-do-list-make-select-func
 (("Q"    "Q"    upcase               ascii    up)
  ("q"    "q"    downcase             ascii    down)
  ("h"    "\C-h" hiragana             hira     hira)
  ("k"    "\C-k" katakana             kata     kata)
  ("x"    "\C-x" hankaku-katakana     hankata  han-kata)
  ("Z"    "Z"    zenkaku-upcase       zenkaku  zenkaku-up)
  ("z"    "z"    zenkaku-downcase     zenkaku  zenkaku-down)
  ("\C-e" "\C-e" erpin-cn             erpin    erpin-cn          NIL)
  ("\C-p" "\C-p" pinyin-cn            pinyin   pinyin-cn         NIL)
  ("\C-z" "\C-z" zhuyin-cn            zhuyin   zhuyin-cn         T)
  ("\C-q" "\C-q" qianma               bixing   qianma)
  ("\C-w" "\C-w" wubi                 bixing   wubi)
  ("\C-u" "\C-u" quanjiao-upcase-cn   quanjiao quanjiao-up-cn)
  ("\C-d" "\C-d" quanjiao-downcase-cn quanjiao quanjiao-down-cn)
  ("E"    "E"    erpin-tw             erpin    erpin-tw          NIL)
  ("P"    "P"    pinyin-tw            pinyin   pinyin-tw         NIL)
  ("C"    "C"    zhuyin-tw            zhuyin   zhuyin-tw         T)
  ("U"    "U"    quanjiao-upcase-tw   quanjiao quanjiao-up-tw)
  ("D"    "D"    quanjiao-downcase-tw quanjiao quanjiao-down-tw)
  ("H"    "H"    hangul               hangul   hangul)
  ("J"    "J"    jeonkak-upcase       jeonkak  jeonkak-up)
  ("j"    "j"    jeonkak-downcase     jeonkak  jeonkak-down)
  ("W"    "W" jiskana             jiskana     jiskana)
))

(provide 'its-keydef)

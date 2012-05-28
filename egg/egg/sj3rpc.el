;;; egg/sj3rpc.el --- SJ3 Support (low level interface) in Egg
;;;                   Input Method Architecture

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc

;; Author: NIIBE Yutaka <gniibe@chroot.org>

;; Maintainer: TOMURA Satoru <tomura@etl.go.jp>

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

(defvar sj3-server-version 2
  "*Major version number of SJ3 server.")

(defvar sj3-server-coding-system 'shift_jis
  "*Coding system used when decoding and encoding of I/O operation with
SJ3 server.  Valid coding systems are depend on the server spec.")

(eval-when-compile
  (require 'egg-com)
  (defmacro sj3-sjis-p ()
    '(eq 'coding-category-sjis (coding-system-category
				sj3-server-coding-system)))
  (defmacro sj3-const (c)
    (cond ((eq c 'OPEN)            1)
	  ((eq c 'CLOSE)           2)
	  ((eq c 'DICADD)         11)
	  ((eq c 'DICDEL)         12)
	  ((eq c 'OPENSTDY)       21)
	  ((eq c 'CLOSESTDY)      22)
	  ((eq c 'STDYSIZE)       23)
	  ((eq c 'LOCK)           31)
	  ((eq c 'UNLOCK)         32)
	  ((eq c 'BEGIN)   '(if (sj3-sjis-p) 41 111))
	  ((eq c 'TANCONV) '(if (sj3-sjis-p) 51 112))
	  ((eq c 'KOUHO)   '(if (sj3-sjis-p) 54 115))
	  ((eq c 'KOUHOSU) '(if (sj3-sjis-p) 55 116))
	  ((eq c 'STDY)           61)
	  ((eq c 'CLSTDY)  '(if (sj3-sjis-p) 62 117))
	  ((eq c 'WREG)    '(if (sj3-sjis-p) 71 118))
	  ((eq c 'WDEL)    '(if (sj3-sjis-p) 72 119))
	  ((eq c 'MKDIC)          81)
	  ((eq c 'MKSTDY)         82)
	  ((eq c 'MKDIR)          83)
	  ((eq c 'ACCESS)         84)
	  ((eq c 'WSCH)    '(if (sj3-sjis-p) 91 120))
	  ((eq c 'WNSCH)   '(if (sj3-sjis-p) 92 121))
	  ((eq c 'VERSION)       103)
	  (t (error "No such constant")))))

;; XXX
(defconst sj3rpc-error-message (vector ))

(defun sj3rpc-get-error-message (errno)
  (or (and (>= errno 0)
	   (< errno (length sj3rpc-error-message))
	   (aref sj3rpc-error-message errno))
      (format "#%d" errno)))

(defmacro sj3rpc-call-with-environment (e vlist send-expr &rest receive-exprs)
  (let ((v (append
	    `((proc (sj3env-get-proc ,e)))
	    vlist)))
    (list
     'let v
     (append
	`(save-excursion
	   (set-buffer (process-buffer proc))
	   (erase-buffer)
	   ,send-expr
	   (process-send-region proc (point-min) (point-max))
	   (goto-char (prog1 (point) (accept-process-output proc))))
	receive-exprs))))

(defmacro sj3rpc-unpack-mb-string ()
  '(let ((start (point)))
     (while (not (search-forward "\0" nil t))
       (comm-accept-process-output))
     (decode-coding-string (buffer-substring start (1- (point)))
			   sj3-server-coding-system)))

(defun sj3rpc-open (proc myhostname username)
  "Open the session.  Return 0 on success, error code on failure."
  (comm-call-with-proc proc (result)
    (comm-format (u u s s s) (sj3-const OPEN) sj3-server-version
		 myhostname username
		 ;; program name
		 (format "%d.emacs-egg" (emacs-pid)))
    (comm-unpack (i) result)
    (if (= result -2)
	0
      result)))

(defun sj3rpc-close (proc)
  (comm-call-with-proc proc (result)
    (comm-format (u) (sj3-const CLOSE))
    (comm-unpack (i) result)
    result))

(defun sj3rpc-get-stdy-size (proc)
  "Return STDYSIZE of SJ3 server.  On failure, return error code."
  (comm-call-with-proc proc (result)
    (comm-format (u) (sj3-const STDYSIZE))
    (comm-unpack (u) result)
    (if (/= result 0)
	(- result)			; failure
      (comm-unpack (u) result)
      result)))

(defsubst sj3rpc-get-stdy (proc)
  (let ((n 0)
	(stdy (make-vector sj3-stdy-size 0)))
    (while (< n sj3-stdy-size)
      (comm-unpack (b) r)
      (aset stdy n r)
      (setq n (1+ n)))
    stdy))

(defun sj3rpc-begin (env yomi)
  "Begin conversion."
  (let ((yomi-ext (encode-coding-string yomi sj3-server-coding-system))
	(p 0)
	len source converted stdy bunsetsu-list bl)
    (sj3rpc-call-with-environment env (result)
      (comm-format (u s) (sj3-const BEGIN) yomi-ext)
      (comm-unpack (u) result)
      (if (/= result 0)
	  (- result)			; failure
	(comm-unpack (u) result)	; skip
	(while (progn
		 (comm-unpack (b) len)
		 (> len 0))
	  (setq stdy (sj3rpc-get-stdy proc))
	  (setq converted (sj3rpc-unpack-mb-string))
	  (setq source (decode-coding-string (substring yomi-ext p (+ p len))
					     sj3-server-coding-system)
		p (+ p len))
	  (let ((bl1 (cons (sj3-make-bunsetsu env
					      source converted nil stdy) nil)))
	    (if bl
		(setq bl (setcdr bl bl1))
	      (setq bunsetsu-list (setq bl bl1)))))
	bunsetsu-list))))

(defun sj3rpc-open-dictionary (proc dict-file-name password)
  (comm-call-with-proc proc (result)
    (comm-format (u s s) (sj3-const DICADD) dict-file-name password)
    (comm-unpack (u) result)
    (if (/= result 0)
	(- result)			; failure
      (comm-unpack (u) result)
      result)))

(defun sj3rpc-close-dictionary (proc dict-no)
  (comm-call-with-proc proc (result)
    (comm-format (u u) (sj3-const DICDEL) dict-no)
    (comm-unpack (i) result)
    result))

(defun sj3rpc-make-dictionary (proc dict-name)
  (comm-call-with-proc proc (result)
    (comm-format (u s u u u) (sj3-const MKDIC) dict-name
		 2048  ; Index length
		 2048  ; Length
		 256   ; Number
		 )
    (comm-unpack (i) result)
    result))

(defun sj3rpc-open-stdy (proc stdy-name)
  (comm-call-with-proc proc (result)
    (comm-format (u s s) (sj3-const OPENSTDY) stdy-name "")
    (comm-unpack (i) result)
    result))

(defun sj3rpc-close-stdy (proc)
  (comm-call-with-proc proc (result)
    (comm-format (u) (sj3-const CLOSESTDY))
    (comm-unpack (i) result)
    result))

(defun sj3rpc-make-stdy (proc stdy-name)
  (comm-call-with-proc proc (result)
    (comm-format (u s u u u) (sj3-const MKSTDY) stdy-name
		 2048  ; Number
		 1     ; Step
		 2048  ; Length
		 )
    (comm-unpack (i) result)
    result))

(defun sj3rpc-make-directory (proc name)
  (comm-call-with-proc proc (result)
    (comm-format (u s) (sj3-const MKDIR) name)
    (comm-unpack (i) result)
    result))

(defun sj3rpc-get-bunsetsu-candidates-sub (proc env yomi yomi-ext len n)
  (let ((i 0)
	stdy converted bunsetsu bl bunsetsu-list cylen rest)
    (comm-call-with-proc-1 proc (result)
      (comm-format (u u s) (sj3-const KOUHO) len yomi-ext)
      (comm-unpack (u) result)
      (if (/= result 0)
	  (- result)			; failure
	(while (< i n)
	  (comm-unpack (u) cylen)
	  (setq stdy (sj3rpc-get-stdy proc))
	  (setq converted (sj3rpc-unpack-mb-string))
	  (setq rest (decode-coding-string (substring yomi-ext cylen)
					   sj3-server-coding-system))
	  (setq bunsetsu (sj3-make-bunsetsu env yomi converted rest stdy))
	  (if bl
	      (setq bl (setcdr bl (cons bunsetsu nil)))
	    (setq bunsetsu-list (setq bl (cons bunsetsu nil))))
	  (setq i (1+ i)))
	(setq bunsetsu (sj3-make-bunsetsu env yomi yomi nil nil))
	(setq bl (setcdr bl (cons bunsetsu nil)))
	(setq bunsetsu
	      (sj3-make-bunsetsu env yomi (japanese-katakana yomi) nil nil))
	(setq bl (setcdr bl (cons bunsetsu nil)))
	bunsetsu-list))))

(defun sj3rpc-get-bunsetsu-candidates (env yomi)
  (let* ((yomi-ext (encode-coding-string yomi sj3-server-coding-system))
	 (len (length yomi-ext)))
    (sj3rpc-call-with-environment env (result)
      (comm-format (u u s) (sj3-const KOUHOSU) len yomi-ext)
      (comm-unpack (u) result)
      (if (/= result 0)
	  (- result)			; failure
	(comm-unpack (u) result)
	(if (= result 0)
	    (list (sj3-make-bunsetsu env yomi yomi nil nil)) ; XXX
	  (sj3rpc-get-bunsetsu-candidates-sub proc env
					      yomi yomi-ext len result))))))

(defun sj3rpc-tanbunsetsu-conversion (env yomi)
  (let* ((yomi-ext (encode-coding-string yomi sj3-server-coding-system))
	(len (length yomi-ext)) cylen stdy converted rest)
    (sj3rpc-call-with-environment env (result)
      (comm-format (u u s) (sj3-const TANCONV) len yomi-ext)
      (comm-unpack (u) result)
      (if (/= result 0)
	  (- result)
	(comm-unpack (u) cylen)
	(setq stdy (sj3rpc-get-stdy proc))
	(setq converted (sj3rpc-unpack-mb-string))
	(setq rest (decode-coding-string (substring yomi-ext cylen)
					 sj3-server-coding-system))
	(setq bunsetsu (sj3-make-bunsetsu env yomi converted rest stdy))))))

(defun sj3rpc-bunsetsu-stdy (env stdy)
  (sj3rpc-call-with-environment env (result)
     (comm-format (u v) (sj3-const STDY) stdy (length stdy))
     (comm-unpack (u) result)
     (- result)))

(defun sj3rpc-kugiri-stdy (env yomi1 yomi2 stdy)
  (sj3rpc-call-with-environment env (result)
    (comm-format (u s s v) (sj3-const CLSTDY)
		 (encode-coding-string yomi1 sj3-server-coding-system)
		 (encode-coding-string yomi2 sj3-server-coding-system)
		 stdy (length stdy))
    (comm-unpack (u) result)
    (- result)))

(defun sj3rpc-add-word (env dictionary yomi kanji hinshi)
  "Register a word KANJI into DICTIONARY with a pronunciation YOMI and
a part of speech HINSHI.  Where DICTIONARY should be an integer."
  (sj3rpc-call-with-environment env ()
    (comm-format (u u s s u) (sj3-const WREG) dictionary
		 (encode-coding-string yomi sj3-server-coding-system)
		 (encode-coding-string kanji sj3-server-coding-system)
		 hinshi)
    (comm-unpack (u) result)
    (- result)))

;;; egg/sj3rpc.el ends here.

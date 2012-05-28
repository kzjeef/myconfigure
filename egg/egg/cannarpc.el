;;; egg/cannarpc.el --- Canna Support (low level interface) in
;;;                     Egg Input Method Architecture

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



(eval-when-compile
  (require 'egg-com)
;;  (load-library "egg/canna")
  (defmacro canna-const (c)
    (cond ((eq c 'Initialize)            1)
	  ((eq c 'Finalize)              2)
	  ((eq c 'CreateContext)         3)
	  ((eq c 'CloseContext)          5)
	  ((eq c 'GetDictionaryList)     6)
	  ((eq c 'GetDirectoryList)      7)
	  ((eq c 'MountDictionary)       8)
	  ((eq c 'UnmountDictionary)       9)
	  ((eq c 'GetMountDictionaryList) 11)
	  ((eq c 'DefineWord)		  13)
	  ((eq c 'DeleteWord)		  14)
	  ((eq c 'BeginConvert)         15)
	  ((eq c 'EndConvert)           16)
	  ((eq c 'GetCandidacyList)     17)
	  ((eq c 'GetYomi)              18)
	  ((eq c 'ResizePause)          26)
	  ((eq c 'GetHinshi)		  27)
	  ((eq c 'GetLex)		  28)
	  ((eq c 'SetApplicationName)	  33)
	  ((eq c 'NoticeGroupName)        34)

	  ((eq c 'CreateDictionary)      3)
	  ((eq c 'Sync)			   8)
	  (t (error "No such constant")))))

;; XXX
(defconst cannarpc-error-message (vector ))

(defun cannarpc-get-error-message (errno)
  (or (and (>= errno 0)
	   (< errno (length cannarpc-error-message))
	   (aref cannarpc-error-message errno))
      (format "#%d" errno)))

(defmacro cannarpc-call-with-environment (e vlist send-expr &rest receive-exprs)
  (let ((v (append
	    `((proc (cannaenv-get-proc ,e))
	      (context (cannaenv-get-context ,e)))
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

(defconst canna-version-fmt "3.3:%s")

(defun cannarpc-open (proc username)
  "Open the session.  Return 0 on success, error code on failure."
  (let ((verusr (format canna-version-fmt username)))
    (comm-call-with-proc proc (minor context)
      (comm-format (u u s) (canna-const Initialize) (+ (length verusr) 1)
		   verusr)
      (comm-unpack (w w) minor context)
      (cond ((and (= minor 65535) (= context 65535))
	     -1)			; failure
	    ((and (= minor 65535) (= context 65534))
	     -1)			; version miss match
	    (t context)))))

(defun cannarpc-close (proc)
  (comm-call-with-proc proc (dummy result)
    (comm-format (b b w) (canna-const Finalize) 0 0)
    (comm-unpack (u b) dummy result)
    (if (= result 255)
	-1				; failure
      result)))

(defun cannarpc-create-context (proc)
  (comm-call-with-proc proc (dummy result)
    (comm-format (b b w) (canna-const CreateContext) 0 0)
    (comm-unpack (u w) dummy result)
    (if (= result 65535)
	-1				; failure
      result)))

(defun cannarpc-close-context (env)
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w w) (canna-const CloseContext) 0 2 context)
    (comm-unpack (u b) dummy result)
    (if (= result 255)
	-1				; failure
      result)))

(defun cannarpc-get-dictionary-list (env)
  (let ((i 0)
	dic dl dic-list)
    (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w w w) (canna-const GetDictionaryList) 0 4
		 context 1024)
    (comm-unpack (u w) dummy result)
    ;; follow list of dictionaries
      (if (= result 65535)
	  -1				; failure
	(while (< i result)
	  (comm-unpack (s) dic)
	  (if dl
	      (setq dl (setcdr dl (cons dic nil)))
	    (setq dic-list (setq dl (cons dic nil))))
	  (setq i (1+ i)))
	dic-list))))

(defun cannarpc-get-directory-list (env)
  (let ((i 0)
	dir dl dir-list)
    (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w w w) (canna-const GetDirectoryList) 0 4
		 context 1024)
    (comm-unpack (u w) dummy result)
    ;; follow list of directories
      (if (= result 65535)
	  -1				; failure
	(while (< i result)
	  (comm-unpack (s) dir)
	  (if dl
	      (setq dl (setcdr dl (cons dir nil)))
	    (setq dir-list (setq dl (cons dir nil))))
	  (setq i (1+ i)))
	dir-list))))

(defun cannarpc-get-mount-dictionary-list (env)
  (let ((i 0)
	dic dl dic-list)
    (cannarpc-call-with-environment env (dummy result)
      (comm-format (b b w w w) (canna-const GetMountDictionaryList) 0 4
		   context 1024)
      (comm-unpack (u w) dummy result)
      ;; follow list of dictionaries
      (if (= result 65535)
	  -1				; failure
	(while (< i result)
	  (comm-unpack (s) dic)
	  (if dl
	      (setq dl (setcdr dl (cons dic nil)))
	    (setq dic-list (setq dl (cons dic nil))))
	  (setq i (1+ i)))
	dic-list))))

(defun cannarpc-open-dictionary (env dict-file-name mode)
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w u w s) (canna-const MountDictionary) 0
		 (+ (length dict-file-name) 7)
		 mode context dict-file-name)
    (comm-unpack (u b) dummy result)
    result))

(defun cannarpc-close-dictionary (env dict-file-name mode)
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w u w s) (canna-const UnmountDictionary) 0
		 (+ (length dict-file-name) 7)
		 mode context dict-file-name)
    (comm-unpack (u b) dummy result)
    result))

(defun cannarpc-begin-conversion (env yomi)
  "Begin conversion."
  (let ((yomi-ext (encode-coding-string yomi 'fixed-euc-jp))
	(mode (or (cannaenv-get-mode env) 19)) ; 19 kana hiragana
	(i 0)
	converted bunsetsu-list bl)
    (cannarpc-call-with-environment env (dummy result)
      (comm-format (b b w i w S) (canna-const BeginConvert) 0
		   (+ (length yomi-ext) 8) mode context yomi)
      (comm-unpack (u w) dummy result)
      (if (= result 65535)
	  -1				; failure
	(while (< i result)
	  (comm-unpack (S) converted)
	  (let ((bl1 (cons (canna-make-bunsetsu env converted i nil)
			   nil)))
	    (if bl
		(setq bl (setcdr bl bl1))
	      (setq bunsetsu-list (setq bl bl1))))
	  (setq i (1+ i)))
	bunsetsu-list))))

(defun cannarpc-cancel-conversion (env)
  "Cancel conversion."
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w w w u) (canna-const EndConvert) 0 8 context 0 0)
    (comm-unpack (u b) dummy result)
    (if (= result 255)
	-1				; failure
      result)))

(defun cannarpc-end-conversion (env len zenkouho-pos-vector mode)
  "End conversion."
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w w w u v) (canna-const EndConvert) 0
		 (+ (* len 2) 8) context len mode zenkouho-pos-vector
		 (length zenkouho-pos-vector))
    (comm-unpack (u b) dummy result)
    (if (= result 255)
	-1				; failure
      result)))

(defun cannarpc-make-dictionary (env dict-name)
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w u w s) (canna-const CreateDictionary) 1
		 (+ (length dict-name) 7) 0 context dict-name)
    (comm-unpack (u b) dummy result)
    result))

(defun cannarpc-save-dictionary (env dict-name)
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w u w s) (canna-const Sync) 1
		 (+ (length dict-name) 7) 0 context dict-name)
    (comm-unpack (u b) dummy result)
    result))

;;; XXX not used
(defun cannarpc-get-dictionary-data (env dir dic)
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w w s s w) 6 1
		 (+ (length dir) (length dic) 6) context dir dic 4096)
    (comm-unpack (u w) dummy result)
    (if (= result 65535)
	-1
;;      (comm-unpack (S) result)
      result)))

(defun cannarpc-get-bunsetsu-source (env bunsetsu-pos)
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w w w w) (canna-const GetYomi) 0 6 context
		 bunsetsu-pos 1024)
    (comm-unpack (u w) dummy result)
    (if (= result 65535)
	-1
      (comm-unpack (S) result)
      result)))

(defun cannarpc-get-bunsetsu-candidates (env bunsetsu-pos yomi)
  (let ((i 0)
	converted bunsetsu-list bl)
    (cannarpc-call-with-environment env (dummy result)
      (comm-format (b b w w w w) (canna-const GetCandidacyList) 0 6 context
		   bunsetsu-pos 1024)
      (comm-unpack (u w) dummy result)
      (if (= result 65535)
	  -1				; failure
	(while (< i result)
	  (comm-unpack (S) converted)
	  (let ((bl1 (cons (canna-make-bunsetsu env converted
						bunsetsu-pos yomi)
			   nil)))
	    (if bl
		(setq bl (setcdr bl bl1))
	      (setq bunsetsu-list (setq bl bl1))))
	  (setq i (1+ i)))
	bunsetsu-list))))

(defun cannarpc-set-kugiri-changed (env yomi-length bunsetsu-pos)
  ;; yomi-length -2…文節縮め -1…文節伸ばし
  (let* ((i bunsetsu-pos)
	 converted bunsetsu-list bl)
    (cannarpc-call-with-environment env (dummy result)
      (comm-format (b b w w w w) (canna-const ResizePause) 0 6 context
		   bunsetsu-pos yomi-length)
      (comm-unpack (u w) dummy result)
      (if (= result 65535)
	  -1				; failure
	(while (< i result)
	  (comm-unpack (S) converted)
	  (let ((bl1 (cons (canna-make-bunsetsu env converted i nil) nil)))
	    (if bl
		(setq bl (setcdr bl bl1))
	      (setq bunsetsu-list (setq bl bl1))))
	  (setq i (1+ i)))
	bunsetsu-list))))

(defun cannarpc-get-hinshi (env bunsetsu-pos kouho-pos)
  (let (b hinshi)
    (cannarpc-call-with-environment env (dummy result)
      (comm-format (b b w w w w w) (canna-const GetHinshi) 0 8 context
		   bunsetsu-pos kouho-pos 1024)
      (comm-unpack (u w) dummy result)
      (if (= result 65535)
	  -1
	(while (> result 0)
	  (comm-unpack (w) b)
	  (setq hinshi (concat hinshi (char-to-string b)))
	  (setq result (1- result)))
	hinshi))))

(defun cannarpc-get-lex (env bunsetsu-pos kouho-pos)
  (let ((i 0)
	ylen klen rownum coldnum dicnum lex-list ll)
    (cannarpc-call-with-environment env (dummy result)
      (comm-format (b b w w w w w) (canna-const GetLex) 0 8 context
		   bunsetsu-pos kouho-pos 1024)
      (comm-unpack (u w) dummy result)
      (if (= result 65535)
	  -1
	(while (< i result)
	  (comm-unpack (i i i i i) ylen klen rownum coldnum dicnum)
	  (let ((ll1 (cons (list ylen klen rownum coldnum dicnum) nil)))
	    (if ll
		(setq ll (setcdr ll ll1))
	      (setq lex-list (setq ll ll1))))
	  (setq i (1+ i)))
	lex-list))))

(defun cannarpc-add-word (env dictionary yomi kanji hinshi)
  "Register a word KANJI into DICTIONARY with a pronunciation YOMI and
a part of speech HINSHI.  Where DICTIONARY should be an integer."
  (let* ((word-info (concat yomi " " hinshi " " kanji))
	 (word-info-ext (encode-coding-string word-info 'fixed-euc-jp))
	 (length (+ (length word-info-ext) (length dictionary) 5)))
    (cannarpc-call-with-environment env (dummy result)
      (comm-format (b b w w S s) (canna-const DefineWord) 0 length context
		   word-info dictionary)
      (comm-unpack (u b) dummy result)
      (if (= result 255)
	  -1		; failure
	result))))

(defun cannarpc-delete-word (env dictionary yomi kanji hinshi)
  "Delete the registered word KANJI from DICTIONARY with a
pronunciation YOMI and a part of speech HINSHI.  Where DICTIONARY
should be an integer."
  (let* ((word-info (concat yomi " " hinshi " " kanji))
	 (word-info-ext (encode-coding-string word-info 'fixed-euc-jp))
	 (length (+ (length word-info-ext) (length dictionary) 5)))
    (cannarpc-call-with-environment env (dummy result)
      (comm-format (b b w w S s) (canna-const DeleteWord) 0 length context
		   word-info dictionary)
      (comm-unpack (u b) dummy result)
      (if (= result 255)
	  -1		; failure
	result))))

(defun cannarpc-notice-group-name (proc context group)
  (comm-call-with-proc proc (dummy result)
    (comm-format (b b w u w s) (canna-const NoticeGroupName) 0
		 (+ (length group) 7) 0 ;; mode = 0
		 context group)
    (comm-unpack (u b) dummy result)
    (if (= result 255)
	-1
      result)))

(defun cannarpc-set-app-name (proc context name)
  (comm-call-with-proc proc (dummy result)
    (comm-format (b b w u w s) (canna-const SetApplicationName) 0
		 (+ (length name) 7) 0 context name)
    (comm-unpack (u b) dummy result)
    (if (= result 255)
	-1
      result)))

;;; egg/cannarpc.el ends here.

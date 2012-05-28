;; egg-edep.el --- This file serves Emacs version dependent definitions

;; Copyright (C) 1999,2000 PFU LIMITED

;; Author: NIIBE Yutaka <gniibe@chroot.org>
;;         KATAYAMA Yoshio <kate@pfu.co.jp>

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


(if (and (fboundp 'set-buffer-multibyte)
	 (subrp (symbol-function 'set-buffer-multibyte)))
    ;; Emacs 20.3
    (progn
      (defun egg-char-bytes (x) 1)
      (defun egg-charset-bytes (x) 1)
      (defun egg-char-bytes-at (str pos) 1)
      (defun egg-chars-in-period (str pos len) len)
      (defalias 'egg-string-to-vector 'identity)
      (defalias 'egg-string-to-char-at 'aref)
      )
  ;; Emacs 20.2
  (defun set-buffer-multibyte (flag)
    (setq enable-multibyte-characters flag))
  (defalias 'string-as-unibyte 'identity)
  (defalias 'string-as-multibyte 'identity)
  (defalias 'coding-system-put 'put)

  (defalias 'egg-char-bytes 'char-bytes)
  (defalias 'egg-charset-bytes 'charset-bytes)
  (defun egg-char-bytes-at (str pos)
    (char-bytes (egg-string-to-char-at str pos)))
  (defun egg-chars-in-period (str pos len)
    (chars-in-string (substring str pos (+ pos len))))
  (defalias 'egg-string-to-vector 'string-to-vector)
  (defalias 'egg-string-to-char-at 'sref)
  )

;; Elisp bug fix

(defun egg-next-single-property-change (pos prop &optional object limit)
  (if limit
      (min limit (next-single-property-change pos prop object (1+ limit)))
    (next-single-property-change pos prop object)))

(defun egg-string-match-charset (charset string &optional start)
  (let ((cur-ct (category-table))
	category)
    (unwind-protect
	(progn
	  (set-category-table (copy-category-table))
	  (setq category (get-unused-category))
	  (define-category category "")
	  (modify-category-entry (make-char charset) category)
	  (string-match (concat "\\c" (list category) "+") string start))
      (set-category-table cur-ct))))

(unless (egg-string-match-charset 'japanese-jisx0208 "。")
  (defun egg-string-match-charset (charset string &optional start)
    (let (min max)
      (if (= (charset-chars charset) 94)
          (setq min 33 max 126)
        (setq min 32 max 127))
      (string-match (if (= (charset-dimension charset) 1)
                        (concat "[" (list (make-char charset min))
                                "-" (list (make-char charset max))
                                "]+")
                      (concat "[" (list (make-char charset min min))
                              "-" (list (make-char charset max max))
                              "]+"))
                    string start))))

(provide 'egg-edep)

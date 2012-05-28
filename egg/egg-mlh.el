;;; egg-mlh.el --- Modeless Conversion Facility in Egg Input
;;;                   Method Architecture

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc

;; Author: NIIBE Yutaka <gniibe@m17n.org>
;;         KATAYAMA Yoshio <kate@pfu.co.jp>      ; Multilingual Enhancement

;; Maintainer: NIIBE Yutaka <gniibe@m17n.org>

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


(defvar mlh-default-backend "wnn")

(defun mlh-space-bar-backward-henkan ()
  "If the character preceding point is / (slash),
Do `mlh-backward-henkan'.  Then, invoke appropriate conversion, if needed.
Or else, execute command that space-bar invokes usually."
  (interactive)
  (let ((henkan-begin nil)
        (inhibit-henkan t)
        (its-disable-special-action t))
    (if (null (assq 'Japanese egg-conversion-backend-alist))
	(progn
          (setq egg-mode-preference nil)
          (activate-input-method (concat "japanese-egg-" mlh-default-backend)))
      ;; force to Japanese
      (its-select-hiragana))
    (mlh-backward-henkan)
    (if henkan-begin
        (if (or inhibit-henkan (= henkan-begin (point)))
            (egg-do-auto-fill)
          (progn
            (message "Converting...")
            (sit-for 0)
	    (egg-convert-region henkan-begin (point))
            (message "") ))
      (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

(defvar mlh-punctuations nil)
(if mlh-punctuations
    ()
  (setq mlh-punctuations "!()?;:"))

(defvar mlh-conversion-scheme-table
  '(
    (?- . mlh-kanji)
;    (?` . mlh-ltn)
;    (?' . mlh-ltn)
;    (?, . mlh-ltn)
    (?, . mlh-kanji)
    (?. . mlh-kanji)
;    (?^ . mlh-ltn)
;    (?~ . mlh-ltn)
;    (?\". mlh-ltn)
;    (?@ . mlh-ltn)
;    (?< . mlh-ltn)
;    (?> . mlh-ltn)
    (?a . mlh-kanji)
;    (?b . mlh-)
    (?c . mlh-capitalize)
    (?d . mlh-user-defined-conversion)
    (?e . mlh-kanji)
    (?f . mlh-hiragana)
    (?g . mlh-greek)
    (?h . mlh-hangul)
    (?i . mlh-kanji)
    (?j . mlh-jis-code)
    (?k . mlh-katakana)
;    (?l . mlh-ligature)
;    (?m . mlh-)
    (?n . mlh-kanji)
;    (?n . mlh-no-conversion)
    (?o . mlh-kanji)
    (?p . mlh-upcase-letter)
    (?q . mlh-quit)
;    (?r . mlh-)
    (?s . mlh-small-letter)
    (?t . mlh-zhongwen-tw)
    (?u . mlh-kanji)
;    (?v . mlh-)
    (?w . mlh-white-space)
    (?x . mlh-execute)
;    (?y . mlh-)
    (?z . mlh-zhongwen)
    (?H . mlh-hiragana-to-kanji)
    (?L . mlh-lisp-expression)
    (?W . mlh-zenkaku-white)
    (?X . mlh-exit)
    (?Z . mlh-zenkaku)
))

(defun mlh-zenkaku-white ()
  (forward-char -1)
  (skip-chars-backward "0-9")
  (mlh-backward-henkan)
  (setq beg (point))
  (goto-char end-marker)
  (backward-delete-char 2)
  (let* ((str (buffer-substring beg (point)))
         (val (string-to-int str)))
    (delete-region beg (point))
    (if (= val 0)
        (setq val 1))
    (while (> val 0)
      (insert "　")
      (setq val (1- val))))
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-exit ()
  (goto-char end-marker)
  (backward-delete-char 2)
  (insert " ")
  (setq henkan-begin (point)))

(defun mlh-upcase-letter ()
  (forward-char -1)
  (skip-chars-backward "a-zA-Z0-9")
  (mlh-backward-henkan)
  (setq beg (point))
  (goto-char end-marker)
  (backward-delete-char 2)
  (upcase-region beg (point))
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-capitalize ()
  (forward-char -1)
  (skip-chars-backward "a-zA-Z1-9")
  (mlh-backward-henkan)
  (setq beg (point))
  (goto-char end-marker)
  (backward-delete-char 2)
  (capitalize-region beg (point))
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-jis-code ()
  (forward-char -1)
  (skip-chars-backward "0-9a-fA-F")
  (mlh-backward-henkan)
  (if (/= (- end-marker (point)) 6)
      (error "invalid length"))
  (setq beg (point))
  (let ((val (car (read-from-string
		   (concat "?\\x" (buffer-substring beg (- end-marker 2)))))))
    (insert (make-char 'japanese-jisx0208 (/ val 256) (% val 256)))
    (delete-region (point) end-marker))
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-lisp-expression ()
  (forward-char -1)
  (let ((stab (syntax-table)))
    (unwind-protect
        (progn
          (set-syntax-table emacs-lisp-mode-syntax-table)
          (forward-sexp -1))
      (set-syntax-table stab)))
  (mlh-backward-henkan)
  (setq beg (point))
  (goto-char end-marker)
  (backward-delete-char 2)
  (let* ((exp-str
          (buffer-substring beg (point)))
         (exp (car (read-from-string exp-str)))
         (result (eval exp)))
    (delete-region beg (point))
    (insert (format "%s" result)))
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-quit ()
  (goto-char end-marker)
  (backward-delete-char 2)
  (setq henkan-begin (point)))
  
(defun mlh-no-conversion ()
  (forward-char -1)
  (skip-chars-backward "\041-\056\060-\176")
  (mlh-backward-henkan)
  (setq beg (point))
  (goto-char end-marker)
  (backward-delete-char 2)
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(fset 'mlh-small-letter (symbol-function 'mlh-no-conversion))

(defun mlh-white-space ()
  (forward-char -1)
  (skip-chars-backward "0-9")
  (mlh-backward-henkan)
  (setq beg (point))
  (goto-char end-marker)
  (backward-delete-char 2)
  (let* ((str (buffer-substring beg (point)))
         (val (string-to-int str)))
    (delete-region beg (point))
    (if (= val 0)
        (setq val 1))
    (insert (make-string val ?\ )))
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-execute ()
  (forward-char -1)
  (if (fboundp 'mlh-userdef-function)
      (mlh-userdef-function)
    (mlh-backward-henkan)
    (setq beg (point))
    (goto-char end-marker)
    (backward-delete-char 2)
    (if (null henkan-begin)
        (setq henkan-begin beg))))

(defun mlh-backward-henkan ()
  "For each words seperated by / (slash), do conversion.
Accoding to a character preceding slash, conversion scheme are selected.

CHAR.  MNEMONIC             CONVERSION SCHEME

  H    Hiragana to kanji    Convert Hiragana to Kanji
  L    Lisp                 Evaluate as Emacs-Lisp expression
  W    zenkaku White space  Insert Zenkaku spaces
  X    eXit                 Quit going backward, insert space
  Z    Zenkaku              Convert to Zenkaku
  c    Capitalize           Capitalize
  d    user Definition      Convert with user definition table
  f    Firagana ??          Convert to Hiragana
  g    Greek letter         Convert to single greek letter
  h    Hangul               Convert to Hangul
  j    Jis-code             Convert to character which has code
  k    Katakana             Convert to Katakana
  l    Ligature             Ligature (not implemented yet)
  p    uPcase letter        uPcase
  q    Quit                 Quit going backward
  s    Small letter         No conversion
  w    White space          Insert spaces
  x    eXecute              Call user defined function
  z    Zhongwen             Convert to Zhongwen
    OTHERWISE               Convert to KANJI
"
  (if (eq (preceding-char) ?/)
      (let ((end-marker (point-marker))
            (char nil)
            (beg nil))
        (set-marker-insertion-type end-marker t)
        (unwind-protect
            (let (scheme)
              (backward-char 1)
              (setq char (preceding-char))
              (cond 
               ((setq scheme (assq char mlh-conversion-scheme-table))
                (funcall (cdr scheme)))
               (t
                (goto-char end-marker)))
              (if beg
                  (progn
                    (goto-char beg)
                    (mlh-do-spacing)
                    (goto-char end-marker))))
          (set-marker end-marker nil)))))


(defvar mlh-syntax-table nil
  "Syntax table of mlh, which are used to determine spacing.")
(if mlh-syntax-table
    ()
  (setq mlh-syntax-table (copy-syntax-table emacs-lisp-mode-syntax-table))
  (modify-syntax-entry ?! "." mlh-syntax-table)
  (modify-syntax-entry ?$ "'" mlh-syntax-table)
  (modify-syntax-entry ?% "'" mlh-syntax-table)
  (modify-syntax-entry ?& "'" mlh-syntax-table)
  (modify-syntax-entry ?{ "(}" mlh-syntax-table)
  (modify-syntax-entry ?} "){" mlh-syntax-table)
)

;;; XXX RTFM, gniibe!
(defvar mlh-space-control
  '(
    (("al".?w).("al".?w))
    (("al".?w).("al".?_))
    (("al".?w).("Hj|".?e))
    (("al".?w).("Cj|".?e))
    (("al".?_).("al".?w))
    (("al".?_).("al".?_))
    (("al".?_).("Hj|".?e))
    (("al".?_).("Cj|".?e))
    (("al".?.).("al".?w))
    (("al".?.).("al".?_))
    (("al".?_).("Hj|".?e))
    (("al".?_).("Cj|".?e))
    (("Hj|".?e).("al".?w))
    (("Cj|".?e).("al".?w))
    (("Hj|".?e).("al".?_))
    (("Cj|".?e).("al".?_))
    )
  "Alist that determines inserting space.")

(defun mlh-do-spacing ()
  "Arrange spacing as you like."
  (if (bobp)
      ()
    (let ((s-tab (syntax-table))
          s-pc s-fc
          c-pc c-fc)
      (unwind-protect
          (progn
            (set-syntax-table mlh-syntax-table)
            (setq s-pc (char-syntax (preceding-char))
                  s-fc (char-syntax (following-char))))
        (set-syntax-table s-tab))
      (setq c-pc (category-set-mnemonics (char-category-set (preceding-char)))
	    c-fc (category-set-mnemonics (char-category-set (following-char))))
      (if (member (cons (cons c-pc s-pc) (cons c-fc s-fc)) mlh-space-control)
          (progn
            (and henkan-begin
                 (>= henkan-begin (point))
                 (setq henkan-begin (1+ henkan-begin)))
            (insert " "))))))

(defvar mlh-select-mode-map (make-keymap))

;;; acutually this map is not necessary now. for future extention
(defvar mlh-select-mode-esc-map (make-keymap))

(define-key mlh-select-mode-map [t] 'undefined)
(define-key mlh-select-mode-esc-map [t] 'undefined)

(let ((ch 32))
  (while (< ch 127)
    (define-key mlh-select-mode-map (char-to-string ch)
      'mlh-select-kakutei-and-self-insert)
    (setq ch (1+ ch))))

(define-key mlh-select-mode-map "\C-m" 'mlh-select-kakutei-and-self-insert)
(define-key mlh-select-mode-map "\C-b" 'mlh-select-prev-candidate)
(define-key mlh-select-mode-map "\C-f" 'mlh-select-next-candidate)
(define-key mlh-select-mode-map "\177" 'mlh-select-prev-candidate)
(define-key mlh-select-mode-map " " 'mlh-select-next-candidate)
(define-key mlh-select-mode-map "/" 'mlh-select-kakutei)

(if (eq window-system 'x)
    (let ()
      (define-key mlh-select-mode-map [return] 'mlh-select-kakutei-and-self-insert)
      (define-key mlh-select-mode-map [delete] 'mlh-select-prev-candidate)
      ))

(defun mlh-select-insert-candidate (n)
  (delete-region beg (point))
  (insert (nth n candidates)))

(defun mlh-select-prev-candidate ()
  (interactive)
  (setq current-candidate (1- current-candidate))
  (if (< current-candidate 0)
      (setq current-candidate (1- number-of-candidates)))
  (mlh-select-insert-candidate current-candidate))

(defun mlh-select-next-candidate ()
  (interactive)
  (setq current-candidate (1+ current-candidate))
  (if (>= current-candidate number-of-candidates)
      (setq current-candidate 0))
  (mlh-select-insert-candidate current-candidate))

(defun mlh-recursive-edit-select (beg end candidates)
  (mlh-select-insert-candidate 0)
  (and (boundp 'disable-undo) (setq disable-undo t))
  (let ((old-local-map (current-local-map))
	(number-of-candidates (length candidates))
	(current-candidate 0))
    (use-local-map mlh-select-mode-map)
    (recursive-edit)
    (use-local-map old-local-map)))

(defun mlh-select-kakutei-and-self-insert ()
  (interactive)
  (setq unread-command-events (list last-command-event))
  (mlh-select-kakutei))

(defun mlh-select-kakutei ()
  (interactive)
  (and (boundp 'disable-undo) (setq disable-undo nil))
  (exit-recursive-edit))

(defun mlh-user-defined-conversion ()
  (forward-char -1)
  (skip-chars-backward "-a-zA-Z")
  (mlh-backward-henkan)
  (setq beg (point))
  (goto-char end-marker)
  (backward-delete-char 2)
  (let* ((str (buffer-substring beg (point)))
         (userdef (mlh-userdef<==string str)))
    (cond ((stringp userdef)
	   (delete-region beg (point))
	   (insert userdef))
	  ((null userdef)
	   (delete-region beg (point))
	   ;; (add-userdef) (insert new-userdef)
	   (insert "?udef?"))
	  ((consp userdef)
	   (mlh-recursive-edit-select beg (point) userdef))))
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defvar mlh-userdef-table nil
  "Convertion table of words(string) to another words(string).")

(defun mlh-userdef<==string (str)
  "Convert string to another string with `mlh-userdef-table'"
  (cdr (assoc str mlh-userdef-table)))

(defvar mlh-kanji-function 'mlh-kanji-with-henkan-region-function)

(defun mlh-kanji ()
  (funcall mlh-kanji-function))

(defun mlh-kanji-with-henkan-region-function ()
  (skip-chars-backward "-a-z,.'N[]")
  (mlh-backward-henkan)
  (setq inhibit-henkan nil)
  (setq beg (point))
  (goto-char end-marker)
  (forward-char -1)
  (its-translate-region-internal beg (point))
  (delete-region (point) end-marker)
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-hiragana ()
  (forward-char -1)
  (skip-chars-backward "-a-z,.'N[]")
  (mlh-backward-henkan)
  (setq beg (point))
  (goto-char end-marker)
  (forward-char -2)
  (its-translate-region-internal beg (point))
  (delete-region (point) end-marker)
  (setq henkan-begin (point)))

(defun mlh-hiragana-to-kanji ()
  (forward-char -1)
  (skip-chars-backward "ぁ-んー")
  (mlh-backward-henkan)
  (setq beg (point))
  (setq inhibit-henkan nil)
  (goto-char end-marker)
  (backward-delete-char 2)
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-katakana ()
  (forward-char -1)
  (skip-chars-backward "-a-z,.'N[]")
  (mlh-backward-henkan)
  (setq beg (point))
  (goto-char end-marker)
  (forward-char -2)
  (its-translate-region-internal beg (point))
  (japanese-katakana-region beg (point))
  (delete-region (point) end-marker)
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-zenkaku ()
  (forward-char -1)
  (skip-chars-backward "\041-\056\060-\176")
  (mlh-backward-henkan)
  (setq beg (point))
  (goto-char end-marker)
  (backward-delete-char 2)
  (japanese-zenkaku-region beg (point))
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-hangul ()
  (forward-char -1)
  (skip-chars-backward "a-zEO-RTW,.[]")
  (mlh-backward-henkan)
  (setq beg (point))
  (setq inhibit-henkan nil)
  (goto-char end-marker)
  (forward-char -2)
  (let (its-current-map its-current-language)
    (its-select-hangul nil t)
    (its-translate-region-internal beg (point)))
  (delete-region (point) end-marker)
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-zhongwen ()
  (forward-char -1)
  (skip-chars-backward "a-z0-4 ,.[]")
  (mlh-backward-henkan)
  (setq beg (point))
  (setq inhibit-henkan nil)
  (goto-char end-marker)
  (forward-char -2)
  (let (its-current-map its-current-language)
    (its-select-pinyin-cn nil t)
    (its-translate-region-internal beg (point)))
  (delete-region (point) end-marker)
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(defun mlh-zhongwen-tw ()
  (forward-char -1)
  (skip-chars-backward "a-z0-4,.[]")
  (mlh-backward-henkan)
  (setq beg (point))
  (setq inhibit-henkan nil)
  (goto-char end-marker)
  (forward-char -2)
  (let (its-current-map its-current-language)
    (its-select-pinyin-tw nil t)
    (its-translate-region-internal beg (point)))
  (delete-region (point) end-marker)
  (if (null henkan-begin)
      (setq henkan-begin beg)))

(provide 'egg-mlh)
;;; egg-mlh.el ends here.

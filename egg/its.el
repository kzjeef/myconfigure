;;; its.el --- Input Translation System AKA "ITS(uDekirunDa!)"

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

(eval-when-compile
  (require 'cl))

(require 'egg-edep)

(defgroup its nil
  "Input Translation System of Tamago 4."
  :group 'egg)

(defcustom its-enable-fullwidth-alphabet t
  "*Enable fullwidth symbol input."
  :group 'its :type 'boolean)

(defcustom its-barf-on-invalid-keyseq nil
  "*Don't allow invalid key sequence in input buffer, if non-NIL."
  :group 'its :type 'boolean)

(defcustom its-delete-by-keystroke nil
  "*Delete characters as if cancel input keystroke, if nin-NIL.
This variable is overriden by `its-delete-by-character'."
  :group 'its :type 'boolean)

(defcustom its-delete-by-character nil
  "*Delete a character as a unit even if just after input, if nin-NIL.
This variable override `its-delete-by-keystroke'."
  :group 'its :type 'boolean)

(defcustom its-fence-invisible nil
  "*Make fences invisible, if nin-NIL."
  :group 'its :type 'boolean)

(defcustom its-fence-open "|"
  "*String of fence start mark. (should not be null string)"
  :group 'its :type '(string :valid-regexp ".+"))

(defcustom its-fence-continue "+"
  "*String of fence start mark. (should not be null string)"
  :group 'its :type '(string :valid-regexp ".+"))

(defcustom its-fence-close "|"
  "*String of fence end mark. (should not be null string)"
  :group 'its :type '(string :valid-regexp ".+"))

(defcustom its-fence-face nil
  "*Face (or alist of languages and faces) of text in fences."
  :group 'its
  :type '(choice face
		 (repeat :tag "Language-Face alist"
			 (cons :tag "Language-Face"
			       (choice :tag "Language"
				       (const Japanese)
				       (const Chinese-GB)
				       (const Chinese-CNS)
				       (const Korean)
				       (const :tag "Default" t)
				       (symbol :tag "Other"))
			       face))))

(defvar its-current-map nil)
(make-variable-buffer-local 'its-current-map)
(put 'its-current-map 'permanent-local t)

(defvar its-current-select-func nil)
(make-variable-buffer-local 'its-current-select-func)
(put 'its-current-select-func 'permanent-local t)

(defvar its-previous-select-func nil)
(make-variable-buffer-local 'its-previous-select-func)
(put 'its-previous-select-func 'permanent-local t)

(defvar its-current-language nil)
(make-variable-buffer-local 'its-current-language)
(put 'its-current-language 'permanent-local t)

;; Data structure in ITS
;; (1) SYL and CURSOR
;;
;; "SYL" stands for something like a syllable.
;;
;; <SYL> ::= ( <output> . ( <keyseq> . <terminal> ))   ; Determined:   DSYL
;;        |  <state>                            ; Intermediate: ISYL
;;        |  ( <output> . <point> )             ; Verbatim:     VSYL
;;        |  nil                                ; None
;;
;; ;<state> ::=
;; ;          ( <output> . ( <keyseq> . <key-state-table/terminal> ))
;;
;; <keyseq> ::= "string" of key sequence
;; <output> ::= "string"
;;
;; <point> ::= integer which specifies point
;;
;; <cursor> ::= nil        ; Previous SYL is active (input will go that SYL)
;;           |  t          ; input makes new SYL.  DEL deletes previous SYL
;;           |  its-cursor ; DEL breaks previous SYL, input makes new SYL

;; Data structures in ITS
;; (2) State machine which recognizes SYL
;;
;; <state> ::= ( <output> <keyseq> . <key-state-table/terminal> )
;;
;; <key-state-table/terminal> ::= <key-state-table> ; intermediate state
;;                             |  <terminal>        ; terminal state
;;
;; <key-state-table> ::= ( <key-state-alist> . <expr-output-back-list> )
;; <key-state-alist> ::= ( <key-state> ... )
;; <key-state> ::= ( <key> . <state> )
;; <key> ::= Positive INTEGER which specifies KEY STROKE
;;        |  -1 ; means END of key stroke
;;
;; Only applicable for last transition.
;; <expr-output-back-list> ::= ( (<output> . (<keyexpr> . <howmanyback>))... )
;; <keyexpr> ::= something like "[a-z]" which specifies class of key.
;;            |  NIL; means ANY of key (except END of the key stroke)
;;
;;
;; <keyseq> ::= "string"
;;
;; <terminal> ::= nil
;;             |  <howmanyback>
;;
;; <howmanyback> ::= integer which specifies how many key strokes we go back
;;
;; <output> ::= "string"

;; Data structure in ITS (3) Map
;;
;; <map>         ::= ( <name> <indicator> <language> . <start-state> )
;; <name>        ::= "string"
;; <indicator>   ::= "string"
;; <language>    ::= "string"
;; <start-state> ::= <state>
;;

(defsubst its-new-state (output keyseq back)
  (cons output (cons keyseq back)))

(defsubst its-new-map (name indicator language)
  (cons name (cons indicator (cons language (its-new-state "" "" nil)))))

(defsubst its-get-indicator (map)
  (nth 1 map))

(defsubst its-get-language (map)
  (nth 2 map))

(defsubst its-get-start-state (map)
  (nthcdr 3 map))

(defsubst its-get-kst/t (state)
  (cdr (cdr state)))

(defsubst its-set-kst (state kst)
  (setcdr (cdr state) kst))

(defsubst its-get-keyseq (state)
  (car (cdr state)))

(defsubst its-set-keyseq (state keyseq)
  (setcar (cdr state) keyseq))

(defun its-get-keyseq-cooked (state)
  (let ((keyseq (its-get-keyseq state))
	(back (its-get-kst/t state)))
    (if back
	(substring keyseq 0 back)
      keyseq)))

(defsubst its-kst-p (kst/t)
  (not (or (numberp kst/t) (null kst/t))))

(defun its-get-output (syl/state &optional no-eval)
  (setq syl/state (car syl/state))
  (cond ((null (consp syl/state))
	 syl/state)
	((and (null no-eval) (eq (car syl/state) 'eval))
	 (eval (mapcar (lambda (s) (if (stringp s) (copy-sequence s) s))
		       (cdr syl/state))))
	(t
	 (copy-sequence syl/state))))

(defsubst its-set-output (state output)
  (setcar state output))

(defsubst its-get-keyseq-syl (syl)
  (let ((l (cdr syl)))
    (cond ((stringp l)			; DSYL
	   l)
	  ((numberp l)			; VSYL
	   (car syl))
	  ((numberp (cdr l))
	   (substring (car l) 0 (cdr l)))
	  (t
	   (car l)))))

(defsubst its-eob-keyexpr (eob)
  (car (cdr eob)))
(defsubst its-eob-back (eob)
  (cdr (cdr eob)))

(defsubst its-make-class+back (class back)
  (cons class back))
(defsubst its-make-otherwise (output class+back)
  (cons output class+back))

(defsubst its-DSYL-with-back-p (syl)
  (and (consp (cdr syl))
       (numberp (its-get-kst/t syl))))

(defsubst its-concrete-DSYL-p (syl)
  (stringp (cdr syl)))

(defsubst its-make-concrete-DSYL (syl)
  (if (consp (cdr syl))
      (cons (its-get-output syl) (its-get-keyseq-syl syl))
    syl))

;;
;;

(require 'its-keydef)

(defvar its-mode-map
  (let ((map (make-sparse-keymap))
	(i 33))
    (define-key map "\C-a" 'its-beginning-of-input-buffer)
    (define-key map "\C-b" 'its-backward-SYL)
    (define-key map "\C-c" 'its-cancel-input)
    (define-key map "\C-d" 'its-delete-SYL)
    (define-key map "\C-e" 'its-end-of-input-buffer)
    (define-key map "\C-f" 'its-forward-SYL)
    (define-key map "\C-g" 'its-select-previous-mode)
    (define-key map "\C-]" 'its-cancel-input)
    (define-key map "\C-h" 'its-mode-help-command)
    (define-key map "\C-k" 'its-kill-line)
;;    (define-key map "\C-l" 'its-exit-mode)
    (define-key map "\C-m" 'its-exit-mode)	; RET
    (define-key map [return] 'its-exit-mode)
    (define-key map "\C-t" 'its-transpose-chars)
    (define-key map "\C-w" 'its-kick-convert-region)
    (define-key map "\C-y" 'its-yank)
    (define-key map "\M-y" 'its-yank-pop)
    (define-key map [backspace] 'its-delete-backward-SYL)
    (define-key map [delete] 'its-delete-backward-SYL)
    (define-key map [(meta backspace)] 'its-delete-backward-SYL-by-keystroke)
    (define-key map [(meta delete)] 'its-delete-backward-SYL-by-keystroke)
    (define-key map [right] 'its-forward-SYL)
    (define-key map [left] 'its-backward-SYL)
    (while (< i 127)
      (define-key map (vector i) 'its-self-insert-char)
      (setq i (1+ i)))
    (define-key map " "    'its-kick-convert-region-or-self-insert)
    (define-key map "\177" 'its-delete-backward-SYL)
    ;;
    (define-key map "\M-p" 'its-previous-map)
    (define-key map "\M-n" 'its-next-map)
    (define-key map "\M-h" 'its-hiragana) ; hiragana-region for input-buffer
    (define-key map "\M-k" 'its-katakana)
    (define-key map "\M-<" 'its-half-width)
    (define-key map "\M->" 'its-full-width)
    map)
  "Keymap for ITS mode.")
(fset 'its-mode-map its-mode-map)

(defvar its-fence-mode nil)
(make-variable-buffer-local 'its-fence-mode)
(put 'its-fence-mode 'permanent-local t)

(defvar egg-sub-mode-map-alist nil)
(or (assq 'its-fence-mode egg-sub-mode-map-alist)
    (setq egg-sub-mode-map-alist (cons '(its-fence-mode . its-mode-map)
				       egg-sub-mode-map-alist)))

(defun its-enter/leave-fence (&optional old new)
  (setq its-fence-mode (its-in-fence-p)))

(add-hook 'egg-enter/leave-fence-hook 'its-enter/leave-fence)

(defconst its-setup-fence-before-insert-SYL nil)

(defun its-get-fence-face (lang)
  (if (null (consp its-fence-face))
      its-fence-face
    (cdr (or (assq lang its-fence-face)
	     (assq t its-fence-face)))))

(defun its-put-cursor (cursor)
  (unless (eq its-barf-on-invalid-keyseq 'its-keyseq-test)
    (let ((p (point))
	  (str (copy-sequence "!")))
      (set-text-properties 0 1 (list 'read-only          t
				     'invisible          'egg
				     'intangible         'its-part-2
				     'its-cursor         cursor
				     'point-entered      'egg-enter/leave-fence
				     'point-left         'egg-enter/leave-fence
				     'modification-hooks '(egg-modify-fence))
			   str)
      (insert str)
      (goto-char p))))

(defun its-set-cursor-status (cursor)
  (delete-region (point) (1+ (point)))
  (its-put-cursor cursor)
  cursor)

(defvar its-context nil)

;;
;;  +-- START property
;;  |          --- CURSOR Property
;;  |         /
;;  v        v    v-- END Property
;;  |SYL SYL ^ SYL|
;;   ^^^ ^^^   ^^^------ SYL Property
;;  <-------><---->
;; intangible intangible
;;     1       2
;;
(defun its-setup-fence-mode ()
  (let ((open-props '(its-start t intangible its-part-1))
	(close-props '(rear-nonsticky t its-end t intangible its-part-2))
	(p (point)) p1)
    (if (or (null (stringp its-fence-open)) (zerop (length its-fence-open))
	    (null (stringp its-fence-continue)) (zerop (length its-fence-continue))
	    (null (stringp its-fence-close)) (zerop (length its-fence-close)))
	(error "invalid fence"))
    ;; Put open-fence before inhibit-read-only to detect read-only
    (insert (if its-context its-fence-continue its-fence-open))
    (egg-setup-invisibility-spec)
    (let ((inhibit-read-only t))
      (setq p1 (point))
      (add-text-properties p p1 open-props)
      (if its-context
	  (put-text-property p p1 'its-context its-context))
      (insert its-fence-close)
      (add-text-properties p1 (point) close-props)
      (if its-fence-invisible
	  (put-text-property p (point) 'invisible 'egg))
      (put-text-property p (point) 'read-only t)
      (goto-char p1)
      (its-define-select-keys its-mode-map t)
      (its-put-cursor t))))

(defun its-start (key context)
  (let ((its-setup-fence-before-insert-SYL t)
	(its-context context))
    (its-input nil key)))

(defun its-restart (str set-prop beginning context)
  (let ((its-context context)
	p)
    (its-setup-fence-mode)
    (setq p (point))
    (put-text-property 0 (length str) 'intangible 'its-part-1 str)
    (insert str)
    (if set-prop
	(progn
	  (delete-region (point) (1+ (point)))
	  (its-setup-yanked-portion p (point))))
    (if beginning
	(its-beginning-of-input-buffer))))

(defun its-self-insert-char ()
  (interactive)
  (let ((inhibit-read-only t)
	(key last-command-char)
	(cursor (get-text-property (point) 'its-cursor))
	(syl (get-text-property (1- (point)) 'its-syl)))
    (cond
     ((or (eq cursor t)
	  (not (eq (get-text-property (1- (point)) 'its-map) its-current-map)))
      (put-text-property (- (point) (length (its-get-output syl))) (point)
			 'its-syl (its-make-concrete-DSYL syl))
      (setq syl nil))
    (cursor
     (setq syl nil)))
    (its-input syl key)))

(defun its-current-language-length ()
  (+ (if (eq (get-text-property (1- (point)) 'egg-lang) its-current-language)
	 (- (point) (previous-single-property-change (point) 'egg-lang))
       0)
     (if (eq (get-text-property (1+ (point)) 'egg-lang) its-current-language)
	 (- (next-single-property-change (1+ (point)) 'egg-lang) (point) 1)
       0)))

(defun its-initial-ISYL ()
  (its-get-start-state (symbol-value its-current-map)))

(defun its-make-VSYL (keyseq)
  (cons keyseq (length keyseq)))

(defun its-input-error ()
  (error "Invalid Romaji Sequence"))

(defvar its-stroke-input-alist nil)

(defun its-input (syl key)
  (let ((output (car syl))
	(k/kk/s (cdr syl))
	(stroke (assq its-current-language its-stroke-input-alist)))
    (or syl (setq syl (its-initial-ISYL)))
    (cond
     ((numberp k/kk/s)
	;; k/kk/s is "point in keyseq"
	(its-input-to-vsyl syl key k/kk/s output))
     ((and (or its-barf-on-invalid-keyseq stroke)
	   (null (its-keyseq-acceptable-p (vector key) syl)))
      ;; signal before altering
      (its-input-error))
     (t
      ;; It's ISYL
      (its-state-machine syl key 'its-buffer-ins/del-SYL)
      (if (and stroke (>= (its-current-language-length) (cdr stroke)))
	  (its-kick-convert-region))))))

(defun its-input-to-vsyl (syl key point output)
  (if (< key 0)
      (its-set-cursor-status t)
    (let ((len (length output)))
      (if (= len point)
	  ;; point is at end of VSYL.  Don't need to call state machine.
	  (its-buffer-ins/del-SYL
	   (its-make-VSYL (concat output (vector key))) syl nil)
	;; point is at middle of VSYL.
	(let ((new-keyseq (concat (substring output 0 point)
				  (vector key)
				  (substring output point))))
	  (its-state-machine-keyseq new-keyseq 'its-buffer-ins/del-SYL))))))

;;;
;;; ITS State Machine
;;;

(defvar its-disable-special-action nil)

;; Return CURSOR
(defun its-state-machine (state key emit)
  (let ((next-state (its-get-next-state state key))
	expr-output-back kst/t output keyseq back)
    (cond
     ;; proceed to next status
     ((and next-state
           (not (and its-disable-special-action
                     (eq (its-get-kst/t next-state) t))))
      (setq kst/t (its-get-kst/t next-state)
	    output (its-get-output next-state)
	    keyseq (its-get-keyseq next-state))
      (cond
       ;; Special actions.
       ((eq kst/t t)
	(if (stringp output)
	    (let ((its-current-language t))
	      (funcall emit (cons output keyseq) state 'its-cursor))
	  (funcall emit (cons "" keyseq) state 'its-cursor)
	  (apply (car output) (cdr output))))

       ;; Still, it's a intermediate state.
       ((its-kst-p kst/t)
	(funcall emit next-state state nil))

       ;; It's negative integer which specifies how many
       ;; characters we go backwards
       (kst/t
	(funcall emit next-state state 'its-cursor)
	(its-state-machine-keyseq (substring keyseq kst/t) emit (< key 0)))

       ;; Here we arrive to a terminal state.
       ;; Emit a DSYL, and go ahead.
       (t
	(funcall emit next-state state 'its-cursor))))

     ;; push back by otherwise status
     ((and (>= key 0)
	   (setq expr-output-back (its-get-otherwise state key)))
      (setq keyseq (concat (its-get-keyseq state) (vector key))
	    back (its-eob-back expr-output-back))
      (funcall emit
	       (cons (or (its-get-output expr-output-back)
			 (its-get-output
			  (its-goto-state (substring keyseq 0 back))))
		     (cons keyseq back))
	       state t)
      (its-state-machine-keyseq
       (substring keyseq back) emit))

     ((eq its-barf-on-invalid-keyseq 'its-keyseq-test)
      'its-keyseq-test-failed)

     ;; No next state for KEY.  It's invalid sequence.
     (its-barf-on-invalid-keyseq
      (its-input-error))

     (t
      ;; XXX Should make DSYL (instead of VSYL)?
      (setq keyseq (concat (its-get-keyseq state) (if (> key 0) (vector key))))
      (funcall emit (its-make-VSYL keyseq) state nil)))))

(defvar its-latest-SYL nil "The latest SYL inserted.")

(defsubst its-update-latest-SYL (syl)
  (setq its-latest-SYL syl))

;; Return CURSOR
(defun its-state-machine-keyseq (keyseq emit &optional eol)
  (let ((i 0)
	(len (length keyseq))
	(syl (its-initial-ISYL))
	cursor)
    (while (< i len)
      (cond
       ((numberp (cdr syl))
	;; VSYL - no need looping
	(funcall emit
		 (its-make-VSYL (concat (car syl) (substring keyseq i)))
		 syl nil)
	(setq cursor nil
	      i len))
       (t
	(setq cursor (its-state-machine syl (aref keyseq i) emit))))
      (if (eq cursor 'its-keyseq-test-failed)
	  (setq i len)
	(setq syl (if cursor (its-initial-ISYL) its-latest-SYL)
	      i (1+ i))))
    (if (and eol (not (eq cursor 'its-keyseq-test-failed)))
	(its-state-machine syl -1 emit)
      cursor)))

(defun its-buffer-ins/del-SYL (newsyl oldsyl cursor)
  (if its-setup-fence-before-insert-SYL
      (progn
	(setq its-setup-fence-before-insert-SYL nil)
	(its-setup-fence-mode)))
  (let ((inhibit-read-only t)
	(output (copy-sequence (its-get-output newsyl)))
	(face (its-get-fence-face its-current-language)))
    (its-buffer-delete-SYL oldsyl)
    (its-update-latest-SYL newsyl)
    (add-text-properties 0 (length output)
			 (list 'its-map its-current-map
			       'its-syl newsyl
			       'egg-lang its-current-language
			       'read-only t
			       'intangible 'its-part-1)
			 output)
    (if face
	(egg-set-face 0 (length output) face output))
    (insert output)
    (its-set-cursor-status cursor)))

(defun its-buffer-delete-SYL (syl)
  (let ((len (length (its-get-output syl))))
    (delete-region (- (point) len) (point))))

(defun its-get-next-state (state key)
  (let ((kst/t (its-get-kst/t state)))
    (and (listp kst/t)
	 (cdr (assq key (car kst/t))))))

;; XXX XXX XXX
(defun its-otherwise-match (expr key)
  (or (null expr)			; <expr>::= NIL means "ANY"
      (let ((case-fold-search nil))
	(string-match expr (char-to-string key)))))

(defun its-get-otherwise (state key)
  (let* ((kst/t (its-get-kst/t state))
	 (ebl (cdr kst/t))
	 expr-output-back)
      (while ebl
	(setq expr-output-back (car ebl))
	(let ((expr (its-eob-keyexpr expr-output-back)))
	  (if (its-otherwise-match expr key)
	      (setq ebl nil)
	    (setq ebl (cdr ebl)))))
      expr-output-back))

(defun its-keyseq-acceptable-p (keyseq &optional syl eol)
  (let ((i 0)
	(len (length keyseq))
	(its-barf-on-invalid-keyseq 'its-keyseq-test)
	(its-latest-SYL nil)
	(emit (lambda (nsyl osyl cursor)
		(its-update-latest-SYL nsyl)
		cursor))
	(its-current-map its-current-map)
	(its-current-select-func its-current-select-func)
	(its-current-language its-current-language)
	(its-zhuyin its-zhuyin)
	(its-previous-select-func its-previous-select-func)
	cursor)
    (if (null syl)
	(setq syl (its-initial-ISYL)))
    (if (numberp (cdr syl))
	nil
      (while (and syl (< i len))
	(setq cursor (its-state-machine syl (aref keyseq i) emit))
	(cond
	 ((eq cursor 'its-keyseq-test-failed)
	  (setq syl nil))
	 (cursor
	  (setq syl (its-initial-ISYL)))
	 (t
	  its-latest-SYL))
	(setq i (1+ i)))
      (if (and syl eol)
	  (setq cursor (its-state-machine syl -1 emit)))
      (not (eq cursor 'its-keyseq-test-failed)))))

;;;
;;; Name --> map
;;;
;;; ITS name: string

(defvar its-map-alist nil)

(defun its-get-map (name)
  (assoc name its-map-alist))

(defun its-register-map (map)
  (let* ((name (car map))
	 (place (assoc name its-map-alist)))
    (if place
	(setcdr place (cdr map))
      (setq its-map-alist (cons map its-map-alist)))
    map))

(defmacro define-its-state-machine (map name indicator lang doc &rest exprs)
  (let ((its-current-map map))
    (set map (its-new-map name indicator
                          (if (eq (car-safe lang) 'quote) (nth 1 lang) lang)))
    (eval (cons 'progn exprs))
    (set map (its-map-compaction (symbol-value map))))
  `(defconst ,map (its-map-rebuild ',(symbol-value map)) ,doc))

(defmacro define-its-state-machine-append (map &rest exprs)
  `(let ((func (lambda () (let ((its-current-map ',map)) ,@exprs)))
	 (hook ',(intern (concat (symbol-name map) "-hook"))))
     (if (null (boundp ',map))
	 (add-hook hook func t)
       (funcall func)
       (run-hooks hook)
       (set hook nil))))

;; Data structure for map compaction
;;  <node> ::= (<count> <node#> <original node>)   ; atom
;;          |  (<count> <node#> (<node> . <node>)) ; cons cell
;;
;;  <count> ::= integer  ; 0 or negative - usage count
;;                       ; positive      - generated common sub-tree
;;
;;  <node#> ::= integer  ; subject to compaction
;;           |  nil      ; not subject to compaction

(defvar its-compaction-enable nil)
(defvar its-compaction-hash-table)
(defvar its-compaction-integer-table)
(defvar its-compaction-counter-1)
(defvar its-compaction-counter-2)
(defvar its-compaction-list)

(defun its-map-compaction (map)
  (if its-compaction-enable
      (let ((its-compaction-hash-table (make-vector 1000 nil))
	    (its-compaction-integer-table (make-vector 138 nil))
	    (its-compaction-counter-1 1)
	    (its-compaction-counter-2 0)
	    (its-compaction-list nil))
	(its-map-compaction-internal map nil nil)
	(cons (vconcat (nreverse its-compaction-list)) map))
    map))

(defmacro its-compaction-set-lr (node lr val)
  `(if (eq ,lr 'car) (setcar ,node ,val) (setcdr ,node ,val)))

(defmacro its-compaction-new-node ()
  '(1- (setq its-compaction-counter-1 (1+ its-compaction-counter-1))))

(defmacro its-compaction-new-cse (node)
  `(1- (setq its-compaction-list (cons ,node its-compaction-list)
	     its-compaction-counter-2 (1+ its-compaction-counter-2))))

(defmacro its-concat (&rest args)
  `(concat ,@(mapcar (lambda (arg)
		       (if (stringp arg)
			   arg
			 `(if (numberp ,arg) (number-to-string ,arg) ,arg)))
		     args)))

(defmacro its-compaction-hash (name node parent lr type)
  (if (null type)
      `(let ((hash (intern (its-concat ,@name) its-compaction-hash-table)))
	 (if (null (boundp hash))
	     (car (set hash (list* (its-compaction-new-node) ,parent ,lr)))
	   (setq hash (symbol-value hash))
	   (if (consp (cdr hash))
	       (setcdr hash (its-compaction-set-lr
			     (cadr hash) (cddr hash)
			     (its-compaction-new-cse ,node))))
	   (its-compaction-set-lr ,parent ,lr (cdr hash))
	   (car hash)))
    `(let ((hash ,(if (eq type 'integer)
		      `(intern (its-concat ,@name) its-compaction-hash-table)
		    `(aref its-compaction-integer-table (+ ,node 10)))))
       (if (null ,(if (eq type 'integer) '(boundp hash) 'hash))
	   (setq hash (,@(if (eq type 'integer)
			     '(set hash)
			   `(aset its-compaction-integer-table (+ ,node 10)))
			 (cons (its-compaction-new-node)
			       (its-compaction-new-cse ,node))))
	 ,(if (eq type 'integer) '(setq hash (symbol-value hash))))
       (its-compaction-set-lr ,parent ,lr (cdr hash))
       (car hash))))

(defun its-map-compaction-internal (map parent lr &optional force)
  (cond
   ((consp map)
    (let* ((candidate (or (null (stringp (car map))) (cdr map)))
	   (sexp (or force (eq (car map) 'eval)))
	   (l (its-map-compaction-internal (car map) map 'car sexp))
	   (r (its-map-compaction-internal (cdr map) map 'cdr sexp)))
      (if (or sexp (and candidate l r))
	  (its-compaction-hash (l " " r) map parent lr nil))))
   ((stringp map)
    (its-compaction-hash ("STR" map) map parent lr nil))
   ((integerp map)
    (if (and (>= map -10) (< map 128))
	(its-compaction-hash nil map parent lr small-int)
      (its-compaction-hash ("INT" map) map parent lr integer)))
   ((null map) 0)
   ((symbolp map)
    (its-compaction-hash ("SYM" (symbol-name map)) map parent lr nil))))

(defvar its-map-rebuild-subtrees)

(defun its-map-rebuild (map)
  (if (vectorp (car map))
      (let ((its-map-rebuild-subtrees (car map))
	    (len (length (car map)))
	    (i 0)
	    node)
	(while (< i len)
	  (setq node (aref its-map-rebuild-subtrees i))
	  (if (consp node)
	      (its-map-rebuild-1 node))
	  (setq i (1+ i)))
	(its-map-rebuild-1 (cdr map))
	(cdr map))
    map))

(defun its-map-rebuild-1 (map)
  (let (lr)
    (while (consp map)
      (if (consp (setq lr (car map)))
	  (its-map-rebuild-1 lr)
	(if (integerp lr)
	    (setcar map (aref its-map-rebuild-subtrees lr))))
      (setq lr map
	    map (cdr map)))
    (if (integerp map)
	  (setcdr lr (aref its-map-rebuild-subtrees map)))))

;;
;; Construct State Machine
;;
(defun its-defrule (input output &optional back enable-overwrite)
  "$BF~NO(B INPUT $B$rG'<1$7(B, OUTPUT $B$r=PNO$9$k$h$&$K%9%F!<%H%^%7%s$r9=@.$9$k!#(B
BACK $B$,(B($BIi$N(B)$B@0?t$N;~$O(B, OUTPUT $B$r=PNO$7$?8e(B, BACK $B$NJ,(B key stroke $B$r(B
$BLa$C$FF0$/$b$N$H$9$k!#JQ495,B'$O$b$C$H$b:G6a$K(B its-define-state-machine
$B$5$l$?JQ49I=$KEPO?$5$l$k!#(B
Return last state."
  (let ((state (its-goto-state input (if enable-overwrite t 'dup-check))))
    (its-set-output state output)
    (its-set-kst state back)
    state))

(defun its-defrule* (input output &optional interim-output enable-overwrite)
  (let* ((state (its-goto-state input (if enable-overwrite t 'dup-check))))
    (its-set-kst state nil)
    (its-set-interim-terminal-state state output)
    (if interim-output
	(its-set-output state interim-output))
    state))

(defvar its-parent-states)

(defun its-goto-state (input &optional build-if-none)
  (let ((len (length input))
	(i 0)
	(state (its-initial-ISYL))
	brand-new next-state key)
    (setq its-parent-states nil)
    (while (< i len)
      (setq its-parent-states (cons state its-parent-states)
	    key (aref input i)
	    i (1+ i)
	    next-state (its-get-next-state state key))
      (cond
       (next-state
	(setq state next-state))
       ((null build-if-none)
	(error "No such state (%s)" input))
       (t
	(if (not (or brand-new (= i 1) (its-get-kst/t state)))
	    (its-set-interim-terminal-state state))
	(setq state (its-make-next-state state key
					 (concat (its-get-output state)
						 (list key)))
	      brand-new t))))
    (if (and (eq build-if-none 'dup-check) (null brand-new))
	(error "Duplicated definition (%s)" input))
    state))

(defun its-set-interim-terminal-state (state &optional output)
  (its-make-next-state state -1 (or output (its-get-output state t)))
  (its-defrule-otherwise state output))

(defun its-defoutput (input display)
  (let ((state (its-goto-state input)))
    (its-set-output state display)))

(defun its-define-otherwise (state otherwise)
  (let ((kst (its-get-kst/t state)))
    (if kst
	(setcdr kst (cons otherwise (cdr kst)))
      (its-set-kst state (cons nil (cons otherwise nil))))))

(defun its-defrule-otherwise (state output &optional class back)
  (its-define-otherwise
   state
   (its-make-otherwise output (its-make-class+back class (or back -1)))))

(defun its-make-next-state (state key output &optional back)
  (let ((next-state (its-new-state output
				   (concat (its-get-keyseq state)
					   (if (> key 0) (list key)))
				   back))
	(kst (its-get-kst/t state)))
    (cond
     ((null kst)
      (its-set-kst state (list (list (cons key next-state)))))
     ((consp kst)
      (setcar kst (cons (cons key next-state) (car kst))))
     (t
      (error "Can't make new state after %S" (its-get-keyseq state))))
    next-state))

(defmacro its-defrule-select-mode-temporally (input select-func)
  `(its-defrule ,input '(its-select-mode-temporally
			 ,(intern (concat "its-select-"
					  (symbol-name select-func))))
		t))

;;;
(defun its-set-part-1 (beg end)
  (let ((inhibit-point-motion-hooks t)
	(str (buffer-substring beg end)))
    (goto-char beg)
    (delete-region beg end)
    (put-text-property 0 (- end beg) 'intangible 'its-part-1 str)
    (insert str)))

(defun its-set-part-2 (beg end)
  (let ((inhibit-point-motion-hooks t)
	(str (buffer-substring beg end)))
    (goto-char beg)
    (delete-region beg end)
    (put-text-property 0 (- end beg) 'intangible 'its-part-2 str)
    (insert str)))

(defun its-search-beginning ()
  (if (get-text-property (1- (point)) 'its-start)
      (point)
    (previous-single-property-change (point) 'its-start)))

(defun its-search-end ()
  (if (get-text-property (point) 'its-end)
      (point)
    (next-single-property-change (point) 'its-end)))

(defun its-beginning-of-input-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (its-input-end)
    (let ((begpos (its-search-beginning)))
      (its-set-part-2 begpos (point))
      (goto-char begpos))
    (its-put-cursor t)))

(defun its-end-of-input-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (its-input-end)
    (let ((endpos (its-search-end)))
      (its-set-part-1 (point) endpos)
      (goto-char endpos))
    (its-put-cursor t)))

(defun its-kill-line (n)
  (interactive "p")
  (let ((inhibit-read-only t))
    (its-input-end)
    (if (> n 0)
	(if (= (its-search-beginning) (point))
	    (its-cancel-input)
	  (delete-region (its-search-end) (point))
	  (its-put-cursor t))
      (if (= (its-search-end) (point))
	  (its-cancel-input)
	(delete-region (its-search-beginning) (point))
	(its-put-cursor t)))))

(defun its-cancel-input ()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (its-search-beginning) (its-search-end))
    (its-put-cursor t)
    (its-exit-mode-internal)))

;; TODO: move in VSYL
(defun its-backward-SYL (n)
  (interactive "p")
  (let ((inhibit-read-only t)
	syl p old-point)
    (its-input-end)
    (setq syl (get-text-property (1- (point)) 'its-syl)
	  p (point)
	  old-point (point))
    (while (and syl (> n 0))
      (setq p (- p (length (its-get-output syl))))
      (setq syl (get-text-property (1- p) 'its-syl))
      (setq n (1- n)))
    ;; Make SYLs have property of "part 2"
    (its-set-part-2 p old-point)
    (goto-char p)
    (its-put-cursor t)
    (if (> n 0)
	(signal 'beginning-of-buffer nil))))

;; TODO: move in VSYL
(defun its-forward-SYL (n)
  (interactive "p")
  (let ((inhibit-read-only t)
	syl p old-point)
    (its-input-end)
    (setq syl (get-text-property (point) 'its-syl)
	  p (point)
	  old-point (point))
    (while (and syl (> n 0))
      (setq p (+ p (length (its-get-output syl))))
      (setq syl (get-text-property p 'its-syl))
      (setq n (1- n)))
    ;; Make SYLs have property of "part 1"
    (its-set-part-1 old-point p)
    (goto-char p)
    (its-put-cursor t)
    (if (> n 0)
	(signal 'end-of-buffer nil))))

;; TODO: handle VSYL.  KILLFLAG
(defun its-delete-SYL (n killflag)
  (interactive "p\nP")
  (let ((inhibit-read-only t)
	syl p)
    (its-input-end)
    (setq syl (get-text-property (point) 'its-syl)
	  p (point))
    (while (and syl (> n 0))
      (setq p (+ p (length (its-get-output syl))))
      (setq syl (get-text-property p 'its-syl))
      (setq n (1- n)))
    (if (> n 0)
	(progn
	  (its-put-cursor t)
	  (signal 'end-of-buffer nil))
      (delete-region (point) p)
      (its-put-cursor t)
      (its-exit-mode-if-empty))))

;; TODO: killflag
(defun its-delete-backward-SYL (n killflag)
  (interactive "p\nP")
  (let ((inhibit-read-only t)
	(syl (get-text-property (1- (point)) 'its-syl))
	(cursor (get-text-property (point) 'its-cursor)))
    (if (null syl)
	(signal 'beginning-of-buffer nil)
      (if (or (eq cursor t) (and cursor its-delete-by-character))
	  (its-delete-backward-SYL-internal n killflag)
	(its-delete-backward-within-SYL syl n killflag)))))

;; TODO: killflag
(defun its-delete-backward-SYL-internal (n killflag)
  (let ((syl (get-text-property (1- (point)) 'its-syl))
	(p (point)))
    (while (and syl (> n 0))
      (setq p (- p (length (its-get-output syl))))
      (setq syl (get-text-property (1- p) 'its-syl))
      (setq n (1- n)))
    (if (> n 0)
	(signal 'beginning-of-buffer nil)
      (delete-region p (1+ (point)))	; also delete cursor
      (its-put-cursor t)
      (its-exit-mode-if-empty))))

(defun its-delete-backward-SYL-by-keystroke (n killflag)
  (interactive "p\nP")
  (let ((inhibit-read-only t)
	(its-delete-by-keystroke t))
    (its-delete-backward-SYL n killflag)))

;; TODO: killflag
(defun its-delete-backward-within-SYL (syl n killflag)
  (let* ((keyseq (its-get-keyseq-syl syl))
	 (len (length keyseq))
	 (p (- (point) (length (its-get-output syl))))
	 (its-current-map (get-text-property (1- (point)) 'its-map))
	 (its-current-language (get-text-property (1- (point)) 'egg-lang))
	 back pp)
    (if (< n 0)
	(signal 'args-out-of-range (list (- (point) n) (point))))
    (if its-delete-by-keystroke
	(while (null (or (eq p pp) (its-concrete-DSYL-p syl)))
	  (setq pp p)
	  (while (and (setq syl (get-text-property (1- p) 'its-syl))
		      (its-DSYL-with-back-p syl)
		      (<= (setq back (- (its-get-kst/t syl))) len)
		      (> back (- len n))
		      (equal (substring (its-get-keyseq syl) (- back))
			     (substring keyseq 0 back)))
	    (setq keyseq (concat (its-get-keyseq-syl syl) keyseq)
		  len (length keyseq)
		  p (- p (length (its-get-output syl)))))
	  (if (and (eq p pp) syl (> n len))
	      (setq n (- n len)
		    keyseq (its-get-keyseq-syl syl)
		    len (length keyseq)
		    p (- p (length (its-get-output syl))))))
      (if (and (> n len) (its-concrete-DSYL-p syl))
	  (setq len 1)))
    (if (> n len)
	(setq n (- n len)
	      len 0))
    (while (and (> n len) (setq syl (get-text-property (1- p) 'its-syl)))
      (setq n (1- n)
	    p (- p (length (its-get-output syl)))))
    (if (> n len)
	(signal 'beginning-of-buffer nil))
    (delete-region p (point))
    (if (> len n)
	(its-state-machine-keyseq (substring keyseq 0 (- len n))
				  'its-buffer-ins/del-SYL)
      (its-set-cursor-status
       (if (or (null its-delete-by-keystroke)
	       (its-concrete-DSYL-p (get-text-property (1- p) 'its-syl)))
	   t
	 'its-cursor))))
  ;; exit its mode after unbind variables
  (its-exit-mode-if-empty))

(defun its-transpose-chars (n)
  (interactive "p")
  (let ((inhibit-read-only t)
	(syl (get-text-property (1- (point)) 'its-syl))
	(cursor (get-text-property (point) 'its-cursor))
	keyseq len)
    (cond
     ((null syl)
      (signal 'beginning-of-buffer nil))
     ((eq cursor t)
      (if (and (= n 1) (get-text-property (1+ (point)) 'its-end))
	  (progn
	    (its-backward-SYL 1)
	    (setq syl (get-text-property (1- (point)) 'its-syl))
	    (if (null syl)
		(signal 'beginning-of-buffer nil))))
      (its-buffer-delete-SYL syl)
      (while (> n 0)
	(if (get-text-property (1+ (point)) 'its-end)
	    (progn
	      (its-buffer-ins/del-SYL syl nil t)
	      (signal 'end-of-buffer nil)))
	(its-forward-SYL 1)
	(setq n (1- n)))
      (while (< n 0)
	(if (get-text-property (1- (point)) 'its-start)
	    (progn
	      (its-buffer-ins/del-SYL syl nil t)
	      (signal 'beginning-of-buffer nil)))
	(its-backward-SYL 1)
	(setq n (1+ n)))
      (its-buffer-ins/del-SYL syl nil t))
     (t
      (setq keyseq (its-get-keyseq-syl syl)
	    len (length keyseq))
      (cond
       ((or (> n 1) (<= len 1))
	(signal 'end-of-buffer nil))
       ((>= (- n) len)
	(signal 'beginning-of-buffer nil))
       (t
	(setq n (if (> n 0) (- -1 n) (1- n)))
	(setq keyseq (concat (substring keyseq 0 n)
			     (substring keyseq -1)
			     (substring keyseq n -1)))
	(if (and its-barf-on-invalid-keyseq
		 (null (its-keyseq-acceptable-p keyseq)))
	    (its-input-error))
	(delete-region (- (point) (length (its-get-output syl))) (point))
	(its-state-machine-keyseq keyseq 'its-buffer-ins/del-SYL)))))))

(defun its-yank (&optional arg)
  (interactive "*P")
  (let ((inhibit-read-only t))
    (its-input-end)
    (yank arg)
    (its-setup-yanked-portion (region-beginning) (region-end))))

(defun its-yank-pop (arg)
  (interactive "*p")
  (let ((inhibit-read-only t))
    (its-input-end)
    (yank-pop arg)
    (its-setup-yanked-portion (region-beginning) (region-end))))

(defun its-setup-yanked-portion (start end)
  (let ((yank-before (eq (point) end))
	syl face lang source no-prop-source len i j l)
    (setq source (buffer-substring start end)
	  no-prop-source (buffer-substring-no-properties start end)
	  len (length source))
    (remove-text-properties 0 len '(intangible nil) source)
    (egg-separate-languages source (get-text-property (1- start) 'egg-lang))
    (setq i 0)
    (while (< i len)
      (setq lang (get-text-property i 'egg-lang source))
      (if (or (and (or (eq lang 'Chinese-GB) (eq lang 'Chinese-CNS))
		   (setq l (egg-chinese-syllable source i)))
	      (and (setq l (get-text-property i 'composition source))
		   (setq l (if (consp (car l)) (caar l) (cadr l)))
		   (eq (next-single-property-change i 'composition
						    source (length source))
		       l)))
	     (setq j (+ i l))
	(setq j (+ i (egg-char-bytes (egg-string-to-char-at source i)))))
      (setq syl (substring no-prop-source i j))
      (put-text-property i j 'its-syl (cons syl syl) source)
      (setq i j))
    (if its-fence-face
	(progn
	  (setq i 0)
	  (while (< i len)
	    (setq j (egg-next-single-property-change i 'egg-lang source len)
		  face (its-get-fence-face
			(get-text-property i 'egg-lang source)))
	    (if face
		(egg-set-face i j face source))
	    (setq i j))))
    (delete-region start end)
    (if yank-before
	(progn
	  (add-text-properties 0 len '(read-only t intangible its-part-1) source)
	  (insert source))
      (add-text-properties 0 len '(read-only t intangible its-part-2) source)
      (insert source)
      (set-marker (mark-marker) (point) (current-buffer))
      (goto-char start))
    (its-put-cursor t)))

;; Return VOID
(defun its-input-end ()
  (if (null (eq its-barf-on-invalid-keyseq 'its-keyseq-test))
      (let ((cursor (get-text-property (point) 'its-cursor)))
	;; key "END"
	(if (null cursor)
	    (let ((its-current-language (get-text-property (1- (point))
							   'egg-lang)))
	      (its-input (get-text-property (1- (point)) 'its-syl) -1)))
	(delete-region (point) (1+ (point))))))

(defun its-exit-mode ()
  "Exit ITS mode."
  (interactive)
  (if (its-in-fence-p)
      (let ((inhibit-read-only t))
	(its-input-end)
	(its-put-cursor t)
	(its-exit-mode-internal))
    (its-select-previous-mode t)))

(defun its-exit-mode-if-empty ()
  (and (get-text-property (1- (point)) 'its-start)
       (get-text-property (1+ (point)) 'its-end)
       (its-exit-mode-internal)))

;; TODO: handle overwrite-mode, insertion-hook, fill...
(defun its-exit-mode-internal (&optional proceed-to-conversion n)
  (let (start end s context str)
    (its-select-previous-mode t)
    ;; Delete CURSOR
    (delete-region (point) (1+ (point)))
    ;; Delete open fence
    (setq s (its-search-beginning)
	  start (previous-single-property-change s 'its-start nil (point-min))
	  context (get-text-property start 'its-context))
    (delete-region start s)
    ;; Delete close fence
    (setq end (its-search-end))
    (delete-region end
		   (next-single-property-change end 'its-end nil (point-max)))
    (if proceed-to-conversion
	(egg-convert-region start end context n)
      ;; Remove all properties
      (goto-char start)
      (setq str (buffer-substring start end))
      (egg-remove-all-text-properties 0 (length str) str)
      (delete-region start end)
      (insert str)
      (egg-do-auto-fill)
      (run-hooks 'input-method-after-insert-chunk-hook))))

(defun its-kick-convert-region (&optional n)
  (interactive "P")
  (let ((inhibit-read-only t))
    (its-input-end)
    (its-put-cursor t)
    (its-exit-mode-internal t n)))

(defun its-kick-convert-region-or-self-insert (&optional n)
  (interactive "P")
  (let ((syl (and (null (get-text-property (point) 'its-cursor))
		  (get-text-property (1- (point)) 'its-syl))))
    (if (its-keyseq-acceptable-p (vector last-command-char) syl)
	(its-self-insert-char)
      (its-kick-convert-region n))))

(defun its-in-fence-p ()
  (and (eq (get-text-property (point) 'intangible) 'its-part-2)
       (get-text-property (point) 'read-only)))

(defvar its-translation-result "" "")

(defun its-ins/del-SYL-batch (newsyl oldsyl cursor)
  (its-update-latest-SYL newsyl)
  (if (and newsyl
	   (consp (cdr newsyl))
	   (not (its-kst-p (its-get-kst/t newsyl))))
      ;; DSYL
      (let ((output (its-get-output newsyl))
	    (oldlen (length its-translation-result)))
	(setq its-translation-result (concat its-translation-result output))
	(put-text-property oldlen (length its-translation-result)
			   'egg-lang its-current-language
			   its-translation-result)))
  cursor)

(defun its-translate-region (start end)
  (interactive "r")
  (its-translate-region-internal start end)
  (egg-remove-all-text-properties start (point)))

(defun its-translate-region-internal (start end)
  (setq its-translation-result "")
  (goto-char start)
  (let ((i 0)
	(syl (its-initial-ISYL))
	;; temporally enable DING
	(its-barf-on-invalid-keyseq t)
	cursor)
    (while (< (point) end)
      (let ((key (following-char)))
	(setq cursor (its-state-machine syl key 'its-ins/del-SYL-batch))
	(forward-char 1)
	(if cursor
	    (setq syl (its-initial-ISYL))
	  (setq syl its-latest-SYL))))
    (if (eq syl its-latest-SYL)
	(its-state-machine syl -1 'its-ins/del-SYL-batch))
    (delete-region start end)
    (insert its-translation-result)))

(defun its-set-mode-line-title ()
  (let ((title (its-get-indicator (symbol-value its-current-map))))
    (setq current-input-method-title (if its-previous-select-func
					 (concat "<" title ">")
				       title))
    (force-mode-line-update)))

(defun its-select-mode-temporally (func)
  (let ((select-func its-current-select-func))
    (let ((its-previous-select-func t))
      (funcall func))
    (if (null its-previous-select-func)
	(setq its-previous-select-func select-func))
    (its-set-mode-line-title)))

(defun its-select-previous-mode (&optional quiet)
  (interactive)
  (if (null its-previous-select-func)
      (if (null quiet)
	  (beep))
    (funcall its-previous-select-func)
    (setq its-previous-select-func nil)
    (its-set-mode-line-title)))

(defun its-set-stroke-input (alist)
  (let ((a alist))
    (while a
      (setq its-stroke-input-alist
	    (delq (assq (caar a) its-stroke-input-alist)
		  its-stroke-input-alist))
      (setq a (cdr a)))
    (setq its-stroke-input-alist
	  (append alist its-stroke-input-alist))))

;;; its-hiragana : hiragana-region for input-buffer
(defun its-hiragana ()
  (interactive)
  (its-convert (lambda (str lang) (japanese-hiragana str))))

;;; its-katakana : katanaka-region for input-buffer
(defun its-katakana ()
  (interactive)
  (its-convert (lambda (str lang) (japanese-katakana str))))

(defconst its-full-half-table (make-vector 100 nil))
(defconst its-half-full-table (make-vector 100 nil))

(let ((table '((Japanese
		(?$B!!(B . ?\ ) (?$B!$(B . ?,)  (?$B!%(B . ?.)  (?$B!"(B . ?,)  (?$B!#(B . ?.)
		(?$B!'(B . ?:)  (?$B!((B . ?\;) (?$B!)(B . ??)  (?$B!*(B . ?!)
		(?$B!-(B . ?')  (?$B!.(B . ?`)  (?$B!0(B . ?^)  (?$B!2(B . ?_)  (?$B!1(B . ?~)
		(?$B!<(B . ?-)  (?$B!=(B . ?-)  (?$B!>(B . ?-)
		(?$B!?(B . ?/)  (?$B!@(B . ?\\) (?$B!A(B . ?~)  (?$B!C(B . ?|)
		(?$B!F(B . ?`)  (?$B!G(B . ?')  (?$B!H(B . ?\") (?$B!I(B . ?\")
		(?$B!J(B . ?\() (?$B!K(B . ?\)) (?$B!N(B . ?[)  (?$B!O(B . ?])
		(?$B!P(B . ?{)  (?$B!Q(B . ?})  (?$B!R(B . ?<)  (?$B!S(B . ?>)
		(?$B!\(B . ?+)  (?$B!](B . ?-)  (?$B!a(B . ?=)  (?$B!c(B . ?<)  (?$B!d(B . ?>)
		(?$B!l(B . ?')  (?$B!m(B . ?\") (?$B!o(B . ?\\) (?$B!p(B . ?$)  (?$B!s(B . ?%)
		(?$B!t(B . ?#)  (?$B!u(B . ?&)  (?$B!v(B . ?*)  (?$B!w(B . ?@)
		(?$B#0(B . ?0)  (?$B#1(B . ?1)  (?$B#2(B . ?2)  (?$B#3(B . ?3)  (?$B#4(B . ?4)
		(?$B#5(B . ?5)  (?$B#6(B . ?6)  (?$B#7(B . ?7)  (?$B#8(B . ?8)  (?$B#9(B . ?9)
		(?$B#A(B . ?A)  (?$B#B(B . ?B)  (?$B#C(B . ?C)  (?$B#D(B . ?D)  (?$B#E(B . ?E)
		(?$B#F(B . ?F)  (?$B#G(B . ?G)  (?$B#H(B . ?H)  (?$B#I(B . ?I)  (?$B#J(B . ?J)
		(?$B#K(B . ?K)  (?$B#L(B . ?L)  (?$B#M(B . ?M)  (?$B#N(B . ?N)  (?$B#O(B . ?O)
		(?$B#P(B . ?P)  (?$B#Q(B . ?Q)  (?$B#R(B . ?R)  (?$B#S(B . ?S)  (?$B#T(B . ?T)
		(?$B#U(B . ?U)  (?$B#V(B . ?V)  (?$B#W(B . ?W)  (?$B#X(B . ?X)  (?$B#Y(B . ?Y)
		(?$B#Z(B . ?Z)
		(?$B#a(B . ?a)  (?$B#b(B . ?b)  (?$B#c(B . ?c)  (?$B#d(B . ?d)  (?$B#e(B . ?e)
		(?$B#f(B . ?f)  (?$B#g(B . ?g)  (?$B#h(B . ?h)  (?$B#i(B . ?i)  (?$B#j(B . ?j)
		(?$B#k(B . ?k)  (?$B#l(B . ?l)  (?$B#m(B . ?m)  (?$B#n(B . ?n)  (?$B#o(B . ?o)
		(?$B#p(B . ?p)  (?$B#q(B . ?q)  (?$B#r(B . ?r)  (?$B#s(B . ?s)  (?$B#t(B . ?t)
		(?$B#u(B . ?u)  (?$B#v(B . ?v)  (?$B#w(B . ?w)  (?$B#x(B . ?x)  (?$B#y(B . ?y)
		(?$B#z(B . ?z))
	       (Chinese-GB
		(?$A!!(B . ?\ ) (?$A#,(B . ?,)  (?$A#.(B . ?.)  (?$A!"(B . ?,)  (?$A!#(B . ?.)
		(?$A#:(B . ?:)  (?$A#;(B . ?\;) (?$A#?(B . ??)  (?$A#!(B . ?!)
		(?$A#`(B . ?`)  (?$A#^(B . ?^)  (?$A#_(B . ?_)  (?$A#~(B . ?~)
		(?$A!*(B . ?-)
		(?$A#/(B . ?/)  (?$A#\(B . ?\\) (?$A!+(B . ?~)  (?$A#|(B . ?|)
		(?$A!.(B . ?`)  (?$A!/(B . ?')  (?$A!0(B . ?\") (?$A!1(B . ?\")
		(?$A#((B . ?\() (?$A#)(B . ?\)) (?$A#[(B . ?[)  ( ?$A#](B . ?])
		(?$A#{(B . ?{)  (?$A#}(B . ?})
		(?$A#+(B . ?+)  (?$A#-(B . ?-)  (?$A#=(B . ?=)  (?$A#<(B . ?<)  (?$A#>(B . ?>)
		(?$A#'(B . ?')  (?$A#"(B . ?\") (?$A#$(B . ?$)  (?$A#%(B . ?%)
		(?$A##(B . ?#)  (?$A#&(B . ?&)  (?$A#*(B . ?*)  (?$A#@(B . ?@)
		(?$A#0(B . ?0)  (?$A#1(B . ?1)  (?$A#2(B . ?2)  (?$A#3(B . ?3)  (?$A#4(B . ?4)
		(?$A#5(B . ?5)  (?$A#6(B . ?6)  (?$A#7(B . ?7)  (?$A#8(B . ?8)  (?$A#9(B . ?9)
		(?$A#A(B . ?A)  (?$A#B(B . ?B)  (?$A#C(B . ?C)  (?$A#D(B . ?D)  (?$A#E(B . ?E)
		(?$A#F(B . ?F)  (?$A#G(B . ?G)  (?$A#H(B . ?H)  (?$A#I(B . ?I)  (?$A#J(B . ?J)
		(?$A#K(B . ?K)  (?$A#L(B . ?L)  (?$A#M(B . ?M)  (?$A#N(B . ?N)  (?$A#O(B . ?O)
		(?$A#P(B . ?P)  (?$A#Q(B . ?Q)  (?$A#R(B . ?R)  (?$A#S(B . ?S)  (?$A#T(B . ?T)
		(?$A#U(B . ?U)  (?$A#V(B . ?V)  (?$A#W(B . ?W)  (?$A#X(B . ?X)  (?$A#Y(B . ?Y)
		(?$A#Z(B . ?Z)
		(?$A#a(B . ?a)  (?$A#b(B . ?b)  (?$A#c(B . ?c)  (?$A#d(B . ?d)  (?$A#e(B . ?e)
		(?$A#f(B . ?f)  (?$A#g(B . ?g)  (?$A#h(B . ?h)  (?$A#i(B . ?i)  (?$A#j(B . ?j)
		(?$A#k(B . ?k)  (?$A#l(B . ?l)  (?$A#m(B . ?m)  (?$A#n(B . ?n)  (?$A#o(B . ?o)
		(?$A#p(B . ?p)  (?$A#q(B . ?q)  (?$A#r(B . ?r)  (?$A#s(B . ?s)  (?$A#t(B . ?t)
		(?$A#u(B . ?u)  (?$A#v(B . ?v)  (?$A#w(B . ?w)  (?$A#x(B . ?x)  (?$A#y(B . ?y)
		(?$A#z(B . ?z))
	       (Chinese-CNS
		(?$(G!!(B . ?\ ) (?$(G!"(B . ?,)  (?$(G!%(B . ?.)  (?$(G!#(B . ?,)  (?$(G!$(B . ?.)
		(?$(G!((B . ?:)  (?$(G!'(B . ?\;) (?$(G!)(B . ??)  (?$(G!*(B . ?!)
		(?$(G!k(B . ?')  (?$(G!j(B . ?`)  (?$(G!T(B . ?^)  (?$(G"%(B . ?_)  (?$(G"#(B . ?~)
		(?$(G"@(B . ?-)
		(?$(G"_(B . ?/)  (?$(G"`(B . ?\\) (?$(G"a(B . ?/)  (?$(G"b(B . ?\\)
		(?$(G"D(B . ?~)  (?$(G"^(B . ?|)
		(?$(G!d(B . ?`)  (?$(G!e(B . ?')
		(?$(G!h(B . ?\") (?$(G!i(B . ?\") (?$(G!f(B . ?\") (?$(G!g(B . ?\")
		(?$(G!>(B . ?\() (?$(G!?(B . ?\))
		(?$(G!F(B . ?[)  (?$(G!G(B . ?])  (?$(G!b(B . ?[)  (?$(G!c(B . ?])
		(?$(G!B(B . ?{)  (?$(G!C(B . ?})  (?$(G!`(B . ?{)  (?$(G!a(B . ?})
		(?$(G!R(B . ?<)  (?$(G!S(B . ?>)
		(?$(G"0(B . ?+)  (?$(G"1(B . ?-)  (?$(G"8(B . ?=)  (?$(G"6(B . ?<)  (?$(G"7(B . ?>)
		(?$(G"c(B . ?$)  (?$(G"h(B . ?%)
		(?$(G!l(B . ?#)  (?$(G!m(B . ?&)  (?$(G!n(B . ?*)  (?$(G"i(B . ?@)
		(?$(G$!(B . ?0)  (?$(G$"(B . ?1)  (?$(G$#(B . ?2)  (?$(G$$(B . ?3)  (?$(G$%(B . ?4)
		(?$(G$&(B . ?5)  (?$(G$'(B . ?6)  (?$(G$((B . ?7)  (?$(G$)(B . ?8)  (?$(G$*(B . ?9)
		(?$(G$A(B . ?A)  (?$(G$B(B . ?B)  (?$(G$C(B . ?C)  (?$(G$D(B . ?D)  (?$(G$E(B . ?E)
		(?$(G$F(B . ?F)  (?$(G$G(B . ?G)  (?$(G$H(B . ?H)  (?$(G$I(B . ?I)  (?$(G$J(B . ?J)
		(?$(G$K(B . ?K)  (?$(G$L(B . ?L)  (?$(G$M(B . ?M)  (?$(G$N(B . ?N)  (?$(G$O(B . ?O)
		(?$(G$P(B . ?P)  (?$(G$Q(B . ?Q)  (?$(G$R(B . ?R)  (?$(G$S(B . ?S)  (?$(G$T(B . ?T)
		(?$(G$U(B . ?U)  (?$(G$V(B . ?V)  (?$(G$W(B . ?W)  (?$(G$X(B . ?X)  (?$(G$Y(B . ?Y)
		(?$(G$Z(B . ?Z)
		(?$(G$[(B . ?a)  (?$(G$\(B . ?b)  (?$(G$](B . ?c)  (?$(G$^(B . ?d)  (?$(G$_(B . ?e)
		(?$(G$`(B . ?f)  (?$(G$a(B . ?g)  (?$(G$b(B . ?h)  (?$(G$c(B . ?i)  (?$(G$d(B . ?j)
		(?$(G$e(B . ?k)  (?$(G$f(B . ?l)  (?$(G$g(B . ?m)  (?$(G$h(B . ?n)  (?$(G$i(B . ?o)
		(?$(G$j(B . ?p)  (?$(G$k(B . ?q)  (?$(G$l(B . ?r)  (?$(G$m(B . ?s)  (?$(G$n(B . ?t)
		(?$(G$o(B . ?u)  (?$(G$p(B . ?v)  (?$(G$q(B . ?w)  (?$(G$r(B . ?x)  (?$(G$s(B . ?y)
		(?$(G$t(B . ?z))
	       (Korean
		(?$(C!!(B . ?\ ) (?$(C#,(B . ?,)  (?$(C#.(B . ?.)
		(?$(C#:(B . ?:)  (?$(C#;(B . ?\;) (?$(C#?(B . ??)  (?$(C#!(B . ?!)
		(?$(C!/(B . ?')  (?$(C!.(B . ?`)  (?$(C#^(B . ?^)  (?$(C#_(B . ?_)  (?$(C#~(B . ?~)
		(?$(C!*(B . ?-)  (?$(C!)(B . ?-)
		(?$(C#/(B . ?/)  (?$(C!,(B . ?\\) (?$(C!-(B . ?~)  (?$(C#|(B . ?|)
		(?$(C!.(B . ?`)  (?$(C!/(B . ?')  (?$(C!0(B . ?\") (?$(C!1(B . ?\")
		(?$(C#((B . ?\() (?$(C#)(B . ?\)) (?$(C#[(B . ?[)  (?$(C#](B . ?])
		(?$(C#{(B . ?{)  (?$(C#}(B . ?})  (?$(C!4(B . ?<)  (?$(C!5(B . ?>)
		(?$(C#+(B . ?+)  (?$(C#-(B . ?-)  (?$(C#=(B . ?=)  (?$(C#<(B . ?<)  (?$(C#>(B . ?>)
		(?$(C#'(B . ?')  (?$(C#"(B . ?\") (?$(C#\(B . ?\\) (?$(C#$(B . ?$)  (?$(C#%(B . ?%)
		(?$(C##(B . ?#)  (?$(C#&(B . ?&)  (?$(C#*(B . ?*)  (?$(C#@(B . ?@)
		(?$(C#0(B . ?0)  (?$(C#1(B . ?1)  (?$(C#2(B . ?2)  (?$(C#3(B . ?3)  (?$(C#4(B . ?4)
		(?$(C#5(B . ?5)  (?$(C#6(B . ?6)  (?$(C#7(B . ?7)  (?$(C#8(B . ?8)  (?$(C#9(B . ?9)
		(?$(C#A(B . ?A)  (?$(C#B(B . ?B)  (?$(C#C(B . ?C)  (?$(C#D(B . ?D)  (?$(C#E(B . ?E)
		(?$(C#F(B . ?F)  (?$(C#G(B . ?G)  (?$(C#H(B . ?H)  (?$(C#I(B . ?I)  (?$(C#J(B . ?J)
		(?$(C#K(B . ?K)  (?$(C#L(B . ?L)  (?$(C#M(B . ?M)  (?$(C#N(B . ?N)  (?$(C#O(B . ?O)
		(?$(C#P(B . ?P)  (?$(C#Q(B . ?Q)  (?$(C#R(B . ?R)  (?$(C#S(B . ?S)  (?$(C#T(B . ?T)
		(?$(C#U(B . ?U)  (?$(C#V(B . ?V)  (?$(C#W(B . ?W)  (?$(C#X(B . ?X)  (?$(C#Y(B . ?Y)
		(?$(C#Z(B . ?Z)
		(?$(C#a(B . ?a)  (?$(C#b(B . ?b)  (?$(C#c(B . ?c)  (?$(C#d(B . ?d)  (?$(C#e(B . ?e)
		(?$(C#f(B . ?f)  (?$(C#g(B . ?g)  (?$(C#h(B . ?h)  (?$(C#i(B . ?i)  (?$(C#j(B . ?j)
		(?$(C#k(B . ?k)  (?$(C#l(B . ?l)  (?$(C#m(B . ?m)  (?$(C#n(B . ?n)  (?$(C#o(B . ?o)
		(?$(C#p(B . ?p)  (?$(C#q(B . ?q)  (?$(C#r(B . ?r)  (?$(C#s(B . ?s)  (?$(C#t(B . ?t)
		(?$(C#u(B . ?u)  (?$(C#v(B . ?v)  (?$(C#w(B . ?w)  (?$(C#x(B . ?x)  (?$(C#y(B . ?y)
		(?$(C#z(B . ?z))))
      (hash (make-vector 100 nil))
      lang pair)
  (while table
    (setq lang (caar table)
	  pair (cdar table)
	  table (cdr table))
    (while pair
      (set (intern (char-to-string (caar pair)) its-full-half-table)
	   (cdar pair))
      (set (intern (concat (symbol-name lang) (char-to-string (cdar pair)))
		   its-half-full-table)
	   (caar pair))
      (setq pair (cdr pair)))
    hash))

;;; its-half-width : half-width-region for input-buffer
(defun its-half-width ()
  (interactive)
  (its-convert
   (lambda (str lang)
     (concat (mapcar (lambda (c)
		       (or (symbol-value (intern-soft (char-to-string c)
						      its-full-half-table))
			   c))
		     (string-to-sequence str 'list))))))

;;; its-full-width : full-width-region for input-buffer
(defun its-full-width ()
  (interactive)
  (its-convert
   (lambda (str lang)
     (if (egg-chinese-syllable str 0)
	 (copy-sequence str)
       (concat (mapcar (lambda (c)
			 (or (symbol-value
			      (intern-soft (concat (symbol-name lang)
						   (char-to-string c))
					   its-half-full-table))
			     c))
		       (string-to-sequence str 'list)))))))

(defun its-convert (func)
  (let ((inhibit-read-only t))
    (unwind-protect
	(progn
	  (its-input-end)
	  (let* ((start (its-search-beginning))
		 (end (its-search-end))
		 (old-str (buffer-substring start end))
		 (len (length old-str))
		 (p 0)
		 (new-str ""))
	    (put-text-property 0 len 'intangible 'its-part-1 old-str)
	    (while (< p len)
	      (let* ((prop (text-properties-at p old-str))
		     (cmp (memq 'composition prop))
		     (old (its-get-output (plist-get prop 'its-syl)))
		     (new (funcall func old (plist-get prop 'egg-lang)))
		     (new-len (length new))
		     syl)
		(unless (equal new old)
		  (when cmp
		    (if (eq prop cmp)
			(setq prop (cddr prop))
		      (setcdr (nthcdr (- (length prop) (length cmp) 1) prop)
			      (cddr cmp))))
		  (setq syl (copy-sequence new))
		  (plist-put prop 'its-syl (cons syl syl)))
		(add-text-properties 0 new-len prop new)
		(setq new-str (concat new-str new)
		      p (+ p (length old)))))
	    (delete-region start end)
	    (insert new-str)))
      (its-put-cursor t))))

(defun its-mode ()
  "\\{its-mode-map}"
  ;; dummy function to get docstring
  )

(defun its-mode-help-command ()
  "Display documentation for ITS mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "ITS mode:\n")
    (princ (documentation 'its-mode))
    (help-setup-xref (cons #'help-xref-mode (current-buffer)) (interactive-p))))

;; The `point-left' hook function will never be called in Emacs 21.2.50
;; when the command `next-line' is used in the last line of a buffer
;; which isn't terminated with a newline or the command `previous-line'
;; is used in the first line of a buffer.
(defun its-next-line (&optional arg)
  "Go to the end of the line if the line isn't terminated with a newline, otherwise run `next-line' as usual."
  (interactive "p")
  (if (= (line-end-position) (point-max))
      (end-of-line)
    (next-line arg)))

(defun its-previous-line (&optional arg)
  "Go to the beginning of the line if it is called in the first line of a buffer, otherwise run `previous-line' as usual."
  (interactive "p")
  (if (= (line-beginning-position) (point-min))
      (beginning-of-line)
    (previous-line arg)))

(substitute-key-definition 'next-line 'its-next-line
                          its-mode-map global-map)
(substitute-key-definition 'previous-line 'its-previous-line
                          its-mode-map global-map)

(provide 'its)

;;; its.el ends here

;;; egg-cnv.el --- Conversion Backend in Egg Input Method Architecture

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

(require 'egg-edep)

(defgroup egg-conv nil
  "Conversion Backend Interface of Tamago 4."
  :group 'egg)

(defcustom egg-conversion-wrap-select t
  "*Candidate selection wraps around to first candidate, if non-NIL.
Otherwise stop at the last candidate."
  :group 'egg-conv :type 'boolean)

(defcustom egg-conversion-auto-candidate-menu 0
  "*Automatically enter the candidate selection mode at N times
next/previous-candidate, if positive number N."
  :group 'egg-conv :type 'integer)

(defcustom egg-conversion-auto-candidate-menu-show-all nil
  "*Enter show all candiate mode when automatic candidate selection
mode, if non-NIL."
  :group 'egg-conv :type 'boolean)

(defcustom egg-conversion-sort-by-converted-string nil
  "*Sort candidate list by converted string on candidate selection
mode, if non-NIL."
  :group 'egg-conv :type 'boolean)

(defcustom egg-conversion-fence-invisible nil
  "*Make fence marks invisible, if non-NIL."
  :group 'egg-conv :type 'boolean)

(defcustom egg-conversion-fence-open "|"
  "*String of conversion fence start mark. (should not be null string)"
  :group 'egg-conv :type '(string :valid-regexp ".+"))

(defcustom egg-conversion-fence-close "|"
  "*String of conversion fence end mark. (should not be null string)"
  :group 'egg-conv :type '(string :valid-regexp ".+"))

(defcustom egg-conversion-face nil
  "*Face (or alist of languages and faces) of text in conversion fences."
  :group 'egg-conv
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

(defcustom egg-conversion-major-separator " "
  "*Major clause seperator"
  :group 'egg-conv :type 'string)

(defcustom egg-conversion-minor-separator "-"
  "*Minor clause seperator"
  :group 'egg-conv :type 'string)

(defcustom egg-startup-file ".eggrc"
  "*Egg startup file name."
  :group 'egg-conv :type 'string)

(defcustom egg-startup-file-search-path '("~")
  "*List of directories to search for egg-startup-file (default .eggrc)."
  :group 'egg-conv :type '(repeat string))

(egg-add-message
 '((nil
    (no-rcfile     "no egg-startup-file on %S")
    (rcfile-error  "error occured in egg-startup-file")
    (candidate     "candidates:")
    (register-str  "Chinese character:")
    (register-yomi "word registration ``%s''  pronunciation:")
    (registered    "dictionary entry ``%s''(%s: %s) is registerd at %s"))
   (Japanese
    (no-rcfile     "%S $B>e$K(B egg-startup-file $B$,$"$j$^$;$s(B")
    (rcfile-error  "egg-startup-file $B$G%(%i!<$,$"$j$^$7$?(B")
    (candidate     "$B8uJd(B:")
    (register-str  "$B4A;z(B:")
    (register-yomi "$B<-=qEPO?!X(B%s$B!Y(B  $BFI$_(B:")
    (registered    "$B<-=q9`L\!X(B%s$B!Y(B(%s: %s)$B$r(B %s $B$KEPO?$7$^$7$?(B"))
   (Chinese-GB
    (no-rcfile     "$ATZ(B %S $AIOC;SP(B egg-startup-file")
    (rcfile-error  "$ATZ6AH!(B egg-startup-file $AJ1#,SP3v4m7"IzAK(B")
    (candidate     "$A:r29(B:")
    (register-str  "$A::WV(B:")
    (register-yomi "$A4G5d5GB<!:(B%s$A!;(B $A6A7((B:")
    (registered    "$A4G5dOnD?!:(B%s$A!;(B(%s: %s)$ARQ1;5GB<5=(B %s $AVPAK(B"))
   (Chinese-CNS
    (no-rcfile     "$(GGc(B %S $(GD8JtH4(B egg-startup-file")
    (rcfile-error  "$(GGc{tL=(B egg-startup-file $(GUk!"H4Exrc`uFmD'(B")
    (register-str  "$(GiGGs(B:")
    (candidate     "$(GT7fP(B:")
    (register-yomi "$(Gy0L(`trg!Z(B%s$(G![(B  $(G{tNN(B:")
    (registered    "$(Gy0L(bzFx!Z(B%s$(G![(B(%s: %s)$(GDX]7`trgL/(B %s $(GDcD'(B"))
   (Korean
    (no-rcfile     "%S $(C?!(B egg-startup-file $(C@L(B $(C>x@>4O4Y(B")
    (rcfile-error  "egg-startup-file $(C?!(B $(C?!7/0!(B $(C9_;}G_@>4O4Y(B")
    (candidate     "$(CHD:8(B:")
    (register-str  "$(CGQ@Z(B:")
    (register-yomi "$(C;g@|5n7O!:(B%s$(C!;(B  $(C569}(B:")
    (registered    "$(C;g@|GW8q!:(B%s$(C!;(B(%s: %s)$(C@;(B %s$(C?!(B $(C5n7OG_@>4O4Y(B"))))

;;
;; <backend-alist> ::= ( ( <language> ( <stage>... )... )... )
;; <stage> ::= ( <backend> <backend-for-reconvert>... )
;; <backend-for-reconvert> ::= <backend>
;; <backend> ::= symbol
;;

(defvar egg-conversion-backend-alist nil)
(make-variable-buffer-local 'egg-conversion-backend-alist)
(put 'egg-conversion-backend-alist 'permanent-local t)

(defun egg-set-conversion-backend (backend-alist &optional force)
  (let (pair lang backend-set)
    (while backend-alist
      (setq lang (caar backend-alist)
	    backend-set (cdar backend-alist)
	    backend-alist (cdr backend-alist)
	    pair (assq lang egg-conversion-backend-alist))
      (cond
       ((null pair)
	(setq egg-conversion-backend-alist
	      (cons (cons lang backend-set) egg-conversion-backend-alist)))
       (force
	(setcdr pair backend-set))))))

(defun egg-get-conversion-backend (language n use-default)
  (let (backend)
    (cond ((null n) (setq n 0))
	  ((null (integerp n)) (setq n 1)))
    (setq backend (nth (1+ n) (assq language egg-conversion-backend-alist)))
    (if backend
	(and backend (cons 0 (mapcar 'copy-sequence backend)))
      (and use-default (cons 0 egg-default-conversion-backend)))))

(defsubst egg-default-conversion-backend-p (backend)
  (eq (cdr backend) egg-default-conversion-backend))

(defsubst egg-get-current-backend (backend)
  (car (nth (car backend) (cdr backend))))

(defsubst egg-get-reconvert-backend (backend &optional n)
  (cond ((null n) (setq n 0))
	((null (integerp n)) (setq n 1)))
  (nth (1+ n) (nth (car backend) (cdr backend))))

(defmacro egg-bunsetsu-info () ''intangible)

(defsubst egg-get-bunsetsu-info (p &optional object)
  (get-text-property p (egg-bunsetsu-info) object))

(defsubst egg-get-backend (p &optional object)
  (get-text-property p 'egg-backend object))

(defsubst egg-get-language (p &optional object)
  (get-text-property p 'egg-lang object))

(defsubst egg-get-bunsetsu-last (p &optional object)
  (get-text-property p 'egg-bunsetsu-last object))

(defsubst egg-get-major-continue (p &optional object)
  (get-text-property p 'egg-major-continue object))

(defsubst egg-get-char-size (p &optional object)
  (get-text-property p 'egg-char-size object))

;; <bunsetsu-info> ::= ( <backend> . <backend-dependent-info> )

(defsubst egg-bunsetsu-create (backend info)
  (cons backend info))

(defsubst egg-bunsetsu-get-backend (bunsetsu)
  (car bunsetsu))
(defsubst egg-bunsetsu-set-backend (bunsetsu backend)
  (setcar bunsetsu backend))

(defsubst egg-bunsetsu-get-info (bunsetsu)
  (cdr bunsetsu))
(defsubst egg-bunsetsu-set-info (bunsetsu info)
  (setcdr bunsetsu info))

(defun egg-conversion-fence-p ()
  (and (egg-get-backend (point))
       (get-text-property (point) 'read-only)))

(defvar egg-finalize-backend-list nil)

(defun egg-set-finalize-backend (func-list)
  (mapcar (lambda (func)
	    (if (and func
		     (null (memq func egg-finalize-backend-list)))
		(setq egg-finalize-backend-list
		      (cons func egg-finalize-backend-list))))
	  func-list))

(defmacro egg-define-backend-functions (list)
  (cons 'progn
	(mapcar
	 (lambda (def)
	   (let* ((func (car def))
		  (args (nth 1 def))
		  (backend (car args)))
	     (cond ((eq backend 'bunsetsu)
		    (setq backend `(egg-bunsetsu-get-backend ,backend)))
		   ((eq backend 'bunsetsu-list)
		    (setq backend `(egg-bunsetsu-get-backend (car ,backend)))))
	     `(defun ,func ,args
		(let ((func (get ,backend ',func)))
		  (and func
		       (funcall func ,@args))))))
	 list)))

(egg-define-backend-functions
 ((egg-start-conversion (backend source-string context))
  (egg-get-bunsetsu-source (bunsetsu))
  (egg-get-bunsetsu-converted (bunsetsu))
  (egg-get-source-language (bunsetsu))
  (egg-get-converted-language (bunsetsu))
  (egg-major-bunsetsu-continue-p (bunsetsu))
  (egg-list-candidates (bunsetsu-list prev-b next-b major))
  (egg-decide-candidate (bunsetsu-list candidate-pos prev-b next-b))
  (egg-special-candidate (bunsetsu-list prev-b next-b major type))
  (egg-change-bunsetsu-length (bunsetsu-list prev-b next-b length major))
  (egg-bunsetsu-combinable-p (bunsetsu next-b))
  (egg-end-conversion (bunsetsu-list abort))
  (egg-word-inspection (bunsetsu))
  (egg-word-registration (backend source converted))))

(defun egg-finalize-backend ()
  (run-hooks 'egg-finalize-backend-list))

(setplist 'egg-conversion-backend-noconv
	  '(egg-start-conversion          egg-start-conversion-noconv
	    egg-get-bunsetsu-source       egg-get-bunsetsu-source-noconv
	    egg-get-bunsetsu-converted    egg-get-bunsetsu-converted-noconv
	    egg-get-source-language       egg-get-source-language-noconv
	    egg-get-converted-language    egg-get-converted-language-noconv
	    egg-end-conversion            egg-end-conversion-noconv))

(defun egg-start-conversion-noconv (backend yomi-string context)
  (let ((string (copy-sequence yomi-string))
	(language (egg-get-language 0 yomi-string)))
    (egg-remove-all-text-properties 0 (length string) string)
    (list (egg-bunsetsu-create backend (vector string language)))))

(defun egg-get-bunsetsu-source-noconv (bunsetsu)
  (aref (egg-bunsetsu-get-info bunsetsu) 0))
(defun egg-get-bunsetsu-converted-noconv (bunsetsu)
  (aref (egg-bunsetsu-get-info bunsetsu) 0))
(defun egg-get-source-language-noconv (bunsetsu)
  (aref (egg-bunsetsu-get-info bunsetsu) 1))
(defun egg-get-converted-language-noconv (bunsetsu)
  (aref (egg-bunsetsu-get-info bunsetsu) 1))
(defun egg-end-conversion-noconv (bunsetsu-list abort)
  nil)

(defconst egg-default-conversion-backend '((egg-conversion-backend-noconv)))

(defun egg-convert-region (start end &optional context nth-backend)
  (interactive "r\ni\nP")
  (let ((source (buffer-substring start end))
	backend backend-source-list converted converted-list
	lang len s success abort)
    (if (>= start end)
	;; nothing to do
	nil
      (delete-region start end)
      (egg-setup-invisibility-spec)
      (let ((inhibit-read-only t))
	(its-define-select-keys egg-conversion-map)
	(goto-char start)
	(setq s (copy-sequence egg-conversion-fence-open)
	      len (length s)
	      start (+ start len)
	      end (+ end len))
	(set-text-properties 0 len (list 'read-only t
					 'egg-start t
					 'egg-source source)
			     s)
	(if context
	    (put-text-property 0 len 'egg-context context s))
	(if egg-conversion-fence-invisible
	    (put-text-property 0 len 'invisible 'egg s))
	(insert s)
	(setq s (copy-sequence egg-conversion-fence-close)
	      len (length s))
	(set-text-properties 0 len '(read-only t rear-nonsticky t egg-end t) s)
	(if egg-conversion-fence-invisible
	    (put-text-property 0 len 'invisible 'egg s))
	(insert s)
	(goto-char start)
	(insert source)
	(goto-char start)
	(setq source (copy-sequence source))
	(egg-separate-languages source)
	(setq backend-source-list (egg-assign-backend source nth-backend))
	(while (and (null abort) backend-source-list)
	  (setq backend (car (car backend-source-list))
		lang (nth 1 (car backend-source-list))
		source (nth 2 (car backend-source-list))
		backend-source-list (cdr backend-source-list))
	  (condition-case error
	      (progn
		(setq converted (egg-start-conversion
				 (egg-get-current-backend backend)
				 source context))
		(if (null converted)
		    (egg-error "no conversion result"))
		(setq converted-list (nconc converted-list
					    (list (cons backend converted)))
		      context 'continued)
		(or (egg-default-conversion-backend-p backend)
		    (setq success t)))
	    ((egg-error quit)
	     (cond
	      ((null (or success
			 (delq t (mapcar (lambda (s)
					   (egg-default-conversion-backend-p
					    (cdr (car s))))
					 backend-source-list))))
	       (message "egg %s backend: %s"
			(if (cdr lang) lang (car lang))
			(nth (if (eq (car error) 'quit) 0 1) error))
	       (ding)
	       (setq abort t))
	      ((condition-case err
		   (y-or-n-p
		    (format "egg %s backend %s: continue? "
			    lang (nth (if (eq (car error) 'quit) 0 1) error)))
		 ((error quit) nil))
	       (setq backend (egg-get-conversion-backend nil 0 t)
		     converted (egg-start-conversion
				(egg-get-current-backend backend)
				source context)
		     converted-list (nconc converted-list
					   (list (cons backend converted)))
                     context 'continued))
	      (t
	       (setq abort t))))))
	(delete-region start end)
	(while converted-list
	  (egg-insert-bunsetsu-list (caar converted-list) (cdar converted-list)
				    (or (null (cdr converted-list)) 'continue))
	  (setq converted-list (cdr converted-list)))
	(goto-char start)
	(cond (abort
	       (egg-abort-conversion))
	      ((null success)
	       (egg-exit-conversion)))))))

(defun egg-separate-languages (str &optional last-lang)
  (let (lang last-chinese
	(len (length str)) i j l)
    ;; 1st pass -- mark undefined Chinese part
    (if (or (eq last-lang 'Chinese-GB) (eq last-lang 'Chinese-CNS))
	(setq last-chinese last-lang))
    (setq i 0)
    (while (< i len)
      (setq j (egg-next-single-property-change i 'egg-lang str len))
      (if (null (egg-get-language i str))
	  (progn
	    (setq c (egg-string-to-char-at str i)
		  cset (char-charset c))
	    (cond
	     ((eq cset 'chinese-sisheng)
	      (egg-string-match-charset 'chinese-sisheng str i)
	      (setq l (match-end 0)
		    j (min j l)
		    lang 'Chinese))
	     ((setq l (egg-chinese-syllable str i))
	      (setq j (+ i l)
		    lang 'Chinese))
	     ((eq cset 'ascii)
	      (if (eq (string-match "[\0-\177\240-\377]+" str (1+ i)) (1+ i))
		  (setq j (match-end 0))
		(setq j (1+ i)))
	      (if (and (< j len)
		       (eq (char-charset (egg-string-to-char-at str j))
			   'chinese-sisheng))
		  (setq j (max (1+ i) (- j 6))))
	      (setq lang nil))
	     ((eq cset 'composition)
	      (setq j (+ i (egg-char-bytes c))
		    lang (egg-charset-to-language
			  (char-charset
			   (car (decompose-composite-char c 'list))))))
	     (t
	      (egg-string-match-charset cset str i)
	      (setq j (match-end 0)
		    lang (egg-charset-to-language cset))))
	    (if lang
		(put-text-property i j 'egg-lang lang str))))
      (setq i j))
    ;; 2nd pass -- set language property
    (setq i 0)
    (while (< i len)
      (setq lang (egg-get-language i str))
      (cond
       ((null lang)
	(setq lang (or last-lang
		       (egg-next-part-lang str i))))
       ((equal lang 'Chinese)
	(setq lang (or last-chinese
		       (egg-next-chinese-lang str i)))))
      (setq last-lang lang)
      (if (or (eq lang 'Chinese-GB) (eq lang 'Chinese-CNS))
	  (setq last-chinese lang))
      (setq j i
	    i (egg-next-single-property-change i 'egg-lang str len))
      (egg-remove-all-text-properties j i str)
      (put-text-property j i 'egg-lang lang str))))

;;; Should think again the interface to language-info-alist
(defun egg-charset-to-language (charset)
  (let ((list language-info-alist))
    (while (and list
		(null (memq charset (assq 'charset (car list)))))
      (setq list (cdr list)))
    (if list
	(intern (car (car list))))))

(defun egg-next-part-lang (str pos)
  (let ((lang (get-text-property
	       (egg-next-single-property-change pos 'egg-lang str (length str))
	       'egg-lang str)))
    (if (eq lang 'Chinese)
	(egg-next-chinese-lang str pos)
      (or lang
	  its-current-language
	  egg-default-language))))

(defun egg-next-chinese-lang (str pos)
  (let ((len (length str)) lang)
    (while (and (< pos len) (null lang))
      (setq pos (egg-next-single-property-change pos 'egg-lang str len)
	    lang (egg-get-language pos str))
      (if (null (or (eq lang 'Chinese-GB)
		    (eq lang 'Chinese-CNS)))
	  (setq lang nil)))
    (cond
     (lang lang)
     ((eq its-current-language 'Chinese-GB)  'Chinese-GB)
     ((eq its-current-language 'Chinese-CNS) 'Chinese-CNS)
     ((eq egg-default-language 'Chinese-GB)  'Chinese-GB)
     ((eq egg-default-language 'Chinese-CNS) 'Chinese-CNS)
     (t 'Chinese-GB))))

;;
;; return value ::= ( (<backend> ( <lang>... ) <source string> )... )
;;
(defun egg-assign-backend (source n)
  (let ((len (length source))
	(i 0)
	j s lang backend retval)
    (while (< i len)
      (setq j (egg-next-single-property-change i 'egg-lang source len)
	    s (substring source i j)
	    lang (egg-get-language 0 s)
	    backend (egg-get-conversion-backend lang n t))
      (egg-remove-all-text-properties 0 (- j i) s)
      (put-text-property 0 (- j i) 'egg-lang lang s)
      (setq retval (nconc retval (list (list backend (list lang) s)))
	    i j))
    (prog1
	retval
      (while retval
	(if (or (egg-default-conversion-backend-p (car (car retval)))
		(null (equal (car (car retval)) (car (nth 1 retval)))))
	    (setq retval (cdr retval))
	  (nconc (nth 1 (car retval)) (nth 1 (nth 1 retval)))
	  (setcar (nthcdr 2 (car retval))
		  (concat (nth 2 (car retval)) (nth 2 (nth 1 retval))))
	  (setcdr retval (cddr retval)))))))

(defun egg-search-file (filename path)
  (let (file)
    (if (file-name-directory filename)
	(setq file (substitute-in-file-name (expand-file-name filename))
	      file (and (file-readable-p file) file))
      (while (and (null file) path)
	(if (stringp (car path))
	    (setq file (substitute-in-file-name
			(expand-file-name filename (car path)))
		  file (and (file-exists-p file) file)))
	(setq path (cdr path)))
      file)))

(defvar egg-default-startup-file "eggrc"
  "Egg startup file name (system default)")

(defun egg-load-startup-file (backend lang)
  (let ((eggrc (or (egg-search-file egg-startup-file
				    egg-startup-file-search-path)
		   (egg-search-file egg-default-startup-file load-path))))
    (if eggrc
	(condition-case error
	    (let ((egg-backend-type backend) (egg-language lang))
	      (load-file eggrc))
	  (error
	   (message "%s: %s" (car error)
		    (mapconcat (lambda (s) (format "%S" s)) (cdr error) ", "))
	   (egg-error 'rcfile-error))
	  (quit
	   (egg-error 'rcfile-error)))
      (egg-error 'no-rcfile egg-startup-file-search-path))))

(defun egg-get-conversion-face (lang)
  (if (null (consp egg-conversion-face))
      egg-conversion-face
    (cdr (or (assq lang egg-conversion-face)
	     (assq t egg-conversion-face)))))

(defvar egg-conversion-map
  (let ((map (make-sparse-keymap))
	(i 33))
    (while (< i 127)
      (define-key map (vector i) 'egg-exit-conversion-unread-char)
      (setq i (1+ i)))
    (define-key map "\C-@"      'egg-decide-first-char)
    (define-key map [?\C-\ ]    'egg-decide-first-char)
    (define-key map "\C-a"      'egg-beginning-of-conversion-buffer)
    (define-key map "\C-b"      'egg-backward-bunsetsu)
    (define-key map "\C-c"      'egg-abort-conversion)
    (define-key map "\C-e"      'egg-end-of-conversion-buffer)
    (define-key map "\C-f"      'egg-forward-bunsetsu)
    (define-key map "\C-h"      'egg-help-command)
    (define-key map "\C-i"      'egg-shrink-bunsetsu-major)
    (define-key map "\C-k"      'egg-decide-before-point)
;;    (define-key map "\C-l"      'egg-exit-conversion)  ; Don't override C-L
    (define-key map "\C-m"      'egg-exit-conversion)
    (define-key map "\C-n"      'egg-next-candidate-major)
    (define-key map "\C-o"      'egg-enlarge-bunsetsu-major)
    (define-key map "\C-p"      'egg-previous-candidate-major)
    (define-key map "\C-r"      'egg-reconvert-bunsetsu)
    (define-key map "\C-t"      'egg-toroku-bunsetsu)
    (define-key map "\C-v"      'egg-inspect-bunsetsu)
    (define-key map "\M-i"      'egg-shrink-bunsetsu-minor)
    (define-key map "\M-n"      'egg-next-candidate-minor)
    (define-key map "\M-o"      'egg-enlarge-bunsetsu-minor)
    (define-key map "\M-p"      'egg-previous-candidate-minor)
    (define-key map "\M-r"      'egg-reconvert-bunsetsu-from-source)
    (define-key map "\M-s"      'egg-select-candidate-major)
    (define-key map "\M-v"      'egg-toggle-inspect-mode)
    (define-key map "\M-z"      'egg-select-candidate-minor)
    (define-key map "\e\C-s"    'egg-select-candidate-list-all-major)
    (define-key map "\e\C-z"    'egg-select-candidate-list-all-minor)
    (define-key map [return]    'egg-exit-conversion)
    (define-key map [right]     'egg-forward-bunsetsu)
    (define-key map [left]      'egg-backward-bunsetsu)
    (define-key map [up]        'egg-previous-candidate)
    (define-key map [down]      'egg-next-candidate)
    (define-key map [backspace] 'egg-abort-conversion)
    (define-key map [clear]     'egg-abort-conversion)
    (define-key map [delete]    'egg-abort-conversion)
    (define-key map " "         'egg-next-candidate)
    (define-key map "/"         'egg-exit-conversion)
    (define-key map "\M-h"      'egg-hiragana)
    (define-key map "\M-k"      'egg-katakana)
    (define-key map "\M-P"      'egg-pinyin)
    (define-key map "\M-Z"      'egg-zhuyin)
    (define-key map "\M-H"      'egg-hangul)
    map)
  "Keymap for EGG Conversion mode.")
(fset 'egg-conversion-map egg-conversion-map)

(defvar egg-conversion-mode nil)
(make-variable-buffer-local 'egg-conversion-mode)
(put 'egg-conversion-mode 'permanent-local t)

(or (assq 'egg-conversion-mode egg-sub-mode-map-alist)
    (setq egg-sub-mode-map-alist (cons
				  '(egg-conversion-mode . egg-conversion-map)
				  egg-sub-mode-map-alist)))

(defun egg-conversion-enter/leave-fence (&optional old new)
  (setq egg-conversion-mode (egg-conversion-fence-p)))

(add-hook 'egg-enter/leave-fence-hook 'egg-conversion-enter/leave-fence)

(defun egg-exit-conversion-unread-char ()
  (interactive)
  (setq egg-context (egg-exit-conversion)
        unread-command-events (list last-command-event)
	this-command 'egg-use-context))

(defun egg-make-bunsetsu (backend bunsetsu last)
  (let* ((converted (copy-sequence (egg-get-bunsetsu-converted bunsetsu)))
	 (language (egg-get-converted-language bunsetsu))
	 (continue (and (null last) (egg-major-bunsetsu-continue-p bunsetsu)))
	 (face (egg-get-conversion-face language))
	 len len1)
    (setq len1 (length converted))
    (or (eq last t)
	(setq converted (concat converted
				(if continue
				    egg-conversion-minor-separator
				  egg-conversion-major-separator))))
    (setq len (length converted))
    (egg-remove-all-text-properties 0 len converted)
    (add-text-properties 0 len
			 (list 'read-only          t
			       (egg-bunsetsu-info) bunsetsu
			       'egg-backend        backend
			       'egg-lang           language
			       'egg-bunsetsu-last  last
			       'egg-major-continue continue
			       'point-entered      'egg-enter/leave-fence
			       'point-left         'egg-enter/leave-fence
			       'modification-hooks '(egg-modify-fence))
			 converted)
    (if face
	(egg-set-face 0 len1 face converted))
    converted))

(defun egg-insert-bunsetsu-list (backend bunsetsu-list &optional last before)
  (let ((len (length bunsetsu-list)))
    (funcall (if before 'insert-before-markers 'insert)
	     (mapconcat
	      (lambda (b)
		(setq len (1- len))
		(egg-make-bunsetsu backend b (and (= len 0) last)))
	      bunsetsu-list nil))))

(defun egg-beginning-of-conversion-buffer (n)
  (interactive "p")
  (cond
   ((<= n 0)
    (egg-end-of-conversion-buffer 1))
   ((null (get-text-property (1- (point)) 'egg-start))
    (goto-char (previous-single-property-change (point) 'egg-start)))))

(defun egg-end-of-conversion-buffer (n)
  (interactive "p")
  (cond
   ((<= n 0)
    (egg-beginning-of-conversion-buffer 1))
   (t
    (goto-char (egg-next-single-property-change (point) 'egg-end))
    (backward-char))))

(defun egg-backward-bunsetsu (n)
  (interactive "p")
  (while (and (> n 0)
	      (null (get-text-property (1- (point)) 'egg-start)))
    (backward-char)
    (setq n (1- n)))
  (if (> n 0)
      (signal 'beginning-of-buffer nil)))

(defun egg-forward-bunsetsu (n)
  (interactive "p")
  (while (and (>= n 0)
	      (null (get-text-property (point) 'egg-end)))
    (forward-char)
    (setq n (1- n)))
  (backward-char)
  (if (>= n 0)
      (signal 'end-of-buffer nil)))

(defun egg-get-bunsetsu-tail (b)
  (nth (1- (length b)) b))

(defun egg-previous-bunsetsu-point (p &optional n obj lim)
  (or n (setq n 1))
  (while (> n 0)
    (setq p (previous-single-property-change p (egg-bunsetsu-info) obj lim)
	  n (1- n)))
  p)

(defun egg-next-bunsetsu-point (p &optional n obj lim)
  (or n (setq n 1))
  (while (> n 0)
    (setq p (egg-next-single-property-change p (egg-bunsetsu-info) obj lim)
	  n (1- n)))
  p)

(defun egg-get-previous-bunsetsu (p)
  (and (null (egg-get-bunsetsu-last (1- p)))
       (egg-get-bunsetsu-info (1- p))))

(defun egg-get-previous-major-bunsetsu (p)
  (let ((prev (egg-get-previous-bunsetsu p))
	bunsetsu)
    (while prev
      (setq bunsetsu (cons prev bunsetsu)
	    p (egg-previous-bunsetsu-point p)
	    prev (and (egg-get-major-continue (1- p))
		      (egg-get-bunsetsu-info (1- p)))))
    bunsetsu))

(defun egg-get-next-bunsetsu (p)
  (and (null (egg-get-bunsetsu-last p))
       (egg-get-bunsetsu-info (egg-next-bunsetsu-point p))))

(defun egg-get-major-bunsetsu (p)
  (let ((next (egg-get-bunsetsu-info p))
	bunsetsu)
    (while next
      (setq bunsetsu (cons next bunsetsu)
	    p (egg-next-bunsetsu-point p)
	    next (and (egg-get-major-continue (1- p))
		      (egg-get-bunsetsu-info p))))
    (nreverse bunsetsu)))

(defsubst egg-get-major-bunsetsu-source (list)
  (mapconcat 'egg-get-bunsetsu-source list nil))

(defsubst egg-get-major-bunsetsu-converted (list)
  (mapconcat 'egg-get-bunsetsu-converted list nil))

(defvar egg-inspect-mode nil
  "*Display clause information on candidate selection, if non-NIL.")

(defun egg-toggle-inspect-mode ()
  (interactive)
  (if (setq egg-inspect-mode (not egg-inspect-mode))
      (egg-inspect-bunsetsu t)))

(defun egg-inspect-bunsetsu (&optional quiet)
  (interactive)
  (or (egg-word-inspection (egg-get-bunsetsu-info (point)))
      quiet
      (beep)))

(defvar egg-candidate-selection-info nil)
(make-variable-buffer-local 'egg-candidate-selection-info)

(defvar egg-candidate-selection-major t)
(make-variable-buffer-local 'egg-candidate-selection-major)

(defsubst egg-set-candsel-info (b major)
  (setq egg-candidate-selection-info (list (car b) (cadr b) (caddr b) major)))

(defsubst egg-candsel-last-bunsetsu () (car egg-candidate-selection-info))
(defsubst egg-candsel-last-prev-b () (nth 1 egg-candidate-selection-info))
(defsubst egg-candsel-last-next-b () (nth 2 egg-candidate-selection-info))
(defsubst egg-candsel-last-major () (nth 3 egg-candidate-selection-info))

(defun egg-major-bunsetsu-head-p (head bunsetsu)
  (while (and head (eq (car head) (car bunsetsu)))
    (setq head (cdr head)
	  bunsetsu (cdr bunsetsu)))
  (null head))

(defun egg-major-bunsetsu-tail-p (tail bunsetsu)
  (egg-major-bunsetsu-head-p
   tail (nthcdr (- (length bunsetsu) (length tail)) bunsetsu)))

(defun egg-get-candsel-target-major ()
  (let ((bunsetsu (egg-get-major-bunsetsu (point)))
	(prev-b (egg-get-previous-major-bunsetsu (point)))
	next-b)
    (cond
     ((and (egg-candsel-last-major)
	   (egg-major-bunsetsu-tail-p (egg-candsel-last-prev-b) prev-b)
	   (egg-major-bunsetsu-head-p (append (egg-candsel-last-bunsetsu)
					      (egg-candsel-last-next-b))
				      bunsetsu))
      (setq bunsetsu (egg-candsel-last-bunsetsu)
	    prev-b (egg-candsel-last-prev-b)
	    next-b (egg-candsel-last-next-b)))
     ((null (egg-get-bunsetsu-last
	     (egg-next-bunsetsu-point (point) (1- (length bunsetsu)))))
      (setq next-b (egg-get-major-bunsetsu
		    (egg-next-bunsetsu-point (point) (length bunsetsu))))))
    (setq egg-candidate-selection-major t)
    (list bunsetsu prev-b next-b t)))

(defun egg-get-candsel-target-minor ()
  (let* ((bunsetsu (list (egg-get-bunsetsu-info (point))))
	 (prev-b (egg-get-previous-bunsetsu (point)))
	 (next-b (egg-get-next-bunsetsu (point))))
    (setq egg-candidate-selection-major nil)
    (list bunsetsu (and prev-b (list prev-b)) (and next-b (list next-b)) nil)))

(defun egg-check-candsel-target (b prev-b next-b major)
  (if major
      (and (egg-major-bunsetsu-tail-p
	    prev-b (egg-get-previous-major-bunsetsu (point)))
	   (let* ((cur-b (egg-get-major-bunsetsu (point)))
		  (next-p (egg-next-bunsetsu-point (point) (length cur-b))))
	     (egg-major-bunsetsu-head-p
	      (append b next-b)
	      (append cur-b (and (null (egg-get-bunsetsu-last (1- next-p)))
				 (egg-get-major-bunsetsu next-p))))))
    (and (eq (egg-get-bunsetsu-info (point)) (car b))
	 (eq (egg-get-previous-bunsetsu (point)) (car prev-b))
	 (eq (egg-get-next-bunsetsu (point)) (car next-b)))))

(defun egg-insert-new-bunsetsu (b tail new-b)
  (let* ((backend (egg-get-backend (point)))
	 (start (egg-previous-bunsetsu-point (point) (length (cadr new-b))))
	 (end (egg-next-bunsetsu-point (point) (+ (length b) (length tail))))
	 (last (egg-get-bunsetsu-last (1- end)))
	 (insert-before (buffer-has-markers-at end)))
    (cond
     ((buffer-has-markers-at end)
      (delete-region start end)
      (egg-insert-bunsetsu-list backend
				(append (cadr new-b) (car new-b) (caddr new-b))
				last t))
     ((buffer-has-markers-at (egg-next-bunsetsu-point (point) (length b)))
      (delete-region start end)
      (egg-insert-bunsetsu-list backend (append (cadr new-b) (car new-b))
				nil t)
      (egg-insert-bunsetsu-list backend (caddr new-b) last))
     ((buffer-has-markers-at (point))
      (delete-region start end)
      (egg-insert-bunsetsu-list backend (cadr new-b) nil t)
      (egg-insert-bunsetsu-list backend (append (car new-b) (caddr new-b))
				last))
     (t
      (delete-region start end)
      (egg-insert-bunsetsu-list backend
				(append (cadr new-b) (car new-b) (caddr new-b))
				last)))
    (goto-char (egg-next-bunsetsu-point start (length (cadr new-b))))
    (if egg-inspect-mode
	(egg-inspect-bunsetsu t))))

(defun egg-next-candidate (n)
  (interactive "p")
  (if egg-candidate-selection-major
      (egg-next-candidate-major n)
    (egg-next-candidate-minor n)))

(defun egg-next-candidate-major (n)
  (interactive "p")
  (apply 'egg-next-candidate-internal n (egg-get-candsel-target-major)))

(defun egg-next-candidate-minor (n)
  (interactive "p")
  (apply 'egg-next-candidate-internal n (egg-get-candsel-target-minor)))

(defun egg-previous-candidate (n)
  (interactive "p")
  (if egg-candidate-selection-major
      (egg-previous-candidate-major n)
    (egg-previous-candidate-minor n)))

(defun egg-previous-candidate-major (n)
  (interactive "p")
  (apply 'egg-next-candidate-internal (- n) (egg-get-candsel-target-major)))

(defun egg-previous-candidate-minor (n)
  (interactive "p")
  (apply 'egg-next-candidate-internal (- n) (egg-get-candsel-target-minor)))

(defvar egg-candidate-select-counter 1)
(make-variable-buffer-local 'egg-candidate-select-counter)

(defun egg-next-candidate-internal (n b prev-b next-b major)
  (if (eq last-command (if major 'egg-candidate-major 'egg-candidate-minor))
      (setq egg-candidate-select-counter (1+ egg-candidate-select-counter))
    (setq egg-candidate-select-counter 1))
  (if (= egg-candidate-select-counter egg-conversion-auto-candidate-menu)
      (egg-select-candidate-internal
       nil egg-conversion-auto-candidate-menu-show-all
       b prev-b next-b major)
    (setq this-command (if major 'egg-candidate-major 'egg-candidate-minor))
    (let ((inhibit-read-only t)
	  new-b candidates nitem i beep)
      (setq candidates (egg-list-candidates b prev-b next-b major))
      (if (null candidates)
	  (setq beep t)
	(setq i (+ n (car candidates))
	      nitem (length (cdr candidates)))
	(cond
	 ((< i 0)			; go backward as if it is ring
	  (setq i (% i nitem))
	  (if (< i 0)
	      (setq i (+ i nitem))))
	 ((< i nitem))			; OK
	 (egg-conversion-wrap-select	; go backward as if it is ring
	  (setq i (% i nitem)))
	 (t				; don't go forward
	  (setq i (1- nitem)
		beep t)))
	(setq new-b (egg-decide-candidate b i prev-b next-b))
	(egg-set-candsel-info new-b major)
	(egg-insert-new-bunsetsu b (caddr new-b) new-b))
      (if beep
	  (ding)))))

(defun egg-numbering-item (list)
  (let ((n -1))
    (mapcar (lambda (item) (cons item (setq n (1+ n)))) list)))

(defun egg-sort-item (list sort)
  (if (eq (null sort) (null egg-conversion-sort-by-converted-string))
      list
    (sort list (lambda (a b) (string< (car a) (car b))))))

(defun egg-select-candidate-major (sort)
  (interactive "P")
  (apply 'egg-select-candidate-internal sort nil (egg-get-candsel-target-major)))

(defun egg-select-candidate-minor (sort)
  (interactive "P")
  (apply 'egg-select-candidate-internal sort nil (egg-get-candsel-target-minor)))

(defun egg-select-candidate-list-all-major (sort)
  (interactive "P")
  (apply 'egg-select-candidate-internal sort t (egg-get-candsel-target-major)))

(defun egg-select-candidate-list-all-minor (sort)
  (interactive "P")
  (apply 'egg-select-candidate-internal sort t (egg-get-candsel-target-minor)))

(defun egg-select-candidate-internal (sort all b prev-b next-b major)
  (let ((prompt (egg-get-message 'candidate))
	new-b candidates pos clist item-list i)
    (setq candidates (egg-list-candidates b prev-b next-b major))
    (if (null candidates)
	(ding)
      (setq pos (car candidates)
	    clist (cdr candidates)
	    item-list (egg-sort-item (egg-numbering-item clist) sort)
	    i (menudiag-select (list 'menu prompt item-list)
			       all
			       (list (assq (nth pos clist) item-list))))
      (if (or (null (egg-conversion-fence-p))
	      (null (egg-check-candsel-target b prev-b next-b major)))
	  (error "Fence was already modified")
	(let ((inhibit-read-only t))
	  (setq new-b (egg-decide-candidate b i prev-b next-b))
	  (egg-set-candsel-info new-b major)
	  (egg-insert-new-bunsetsu b (caddr new-b) new-b))))))

(defun egg-hiragana (&optional minor)
  (interactive "P")
  (if (null minor)
      (apply 'egg-special-convert this-command (egg-get-candsel-target-major))
    (apply 'egg-special-convert this-command (egg-get-candsel-target-minor))))

(defalias 'egg-katakana 'egg-hiragana)
(defalias 'egg-pinyin 'egg-hiragana)
(defalias 'egg-zhuyin 'egg-hiragana)
(defalias 'egg-hangul 'egg-hiragana)

(defun egg-special-convert (type b prev-b next-b major)
  (let ((inhibit-read-only t)
	(new-b (egg-special-candidate b prev-b next-b major type)))
    (if (null new-b)
	(ding)
      (egg-set-candsel-info new-b major)
      (egg-insert-new-bunsetsu b (caddr new-b) new-b))))

(defun egg-separate-characters (str)
  (let* ((v (egg-string-to-vector str))
	 (len (length v))
	 (i 0) (j 0) m n (nchar 0))
    (while (< i len)
      (if (setq n (egg-chinese-syllable str j))
	  (setq m (egg-chars-in-period str j n))
	(setq m 1 n (egg-char-bytes (aref v i))))
      (put-text-property j (+ j n) 'egg-char-size n str)
      (setq nchar (1+ nchar) i (+ i m) j (+ j n)))
    nchar))

(defun egg-enlarge-bunsetsu-major (n)
  (interactive "p")
  (egg-enlarge-bunsetsu-internal n t))

(defun egg-enlarge-bunsetsu-minor (n)
  (interactive "p")
  (egg-enlarge-bunsetsu-internal n nil))

(defun egg-shrink-bunsetsu-major (n)
  (interactive "p")
  (egg-enlarge-bunsetsu-internal (- n) t))

(defun egg-shrink-bunsetsu-minor (n)
  (interactive "p")
  (egg-enlarge-bunsetsu-internal (- n) nil))

(defun egg-enlarge-bunsetsu-internal (n major)
  (let ((inhibit-read-only t)
	b prev-b next-b new-b s1 s1len s2 s2len nchar i last end beep)
    (if major
	(setq b (egg-get-major-bunsetsu (point))
	      prev-b (egg-get-previous-major-bunsetsu (point)))
      (setq b (list (egg-get-bunsetsu-info (point)))
	    prev-b (egg-get-previous-bunsetsu (point))
	    prev-b (and prev-b (list prev-b))))
    (setq end (egg-next-bunsetsu-point (point) (length b))
	  last (egg-get-bunsetsu-last (1- end)))
    (while (null last)
      (setq next-b (cons (egg-get-bunsetsu-info end) next-b)
	    last (egg-get-bunsetsu-last end)
	    end (egg-next-bunsetsu-point end)))
    (setq next-b (nreverse next-b)
	  s1 (egg-get-major-bunsetsu-source b)
	  s2 (concat s1 (egg-get-major-bunsetsu-source next-b))
	  s1len (egg-separate-characters s1)
	  s2len (egg-separate-characters s2)
	  n (+ n s1len))
    (cond
     ((<= n 0)
      (setq beep t nchar (and (/= s1len 1) (egg-get-char-size 0 s1))))
     ((> n s2len)
      (setq beep t nchar (and (/= s2len s1len) (length s2))))
     (t
      (setq nchar 0)
      (while (> n 0)
	(setq nchar (+ nchar (egg-get-char-size nchar s2))
	      n (1- n)))))
    (when nchar
      (setq next-b (nconc b next-b)
	    i (length (egg-get-bunsetsu-source (car next-b))))
      (while (< i nchar)
	(setq next-b (cdr next-b)
	      i (+ i (length (egg-get-bunsetsu-source (car next-b))))))
      (setq next-b (prog1 (cdr next-b) (setcdr next-b nil))
	    new-b (egg-change-bunsetsu-length b prev-b next-b nchar major))
      (if (null new-b)
	  (setq beep t)
	(egg-insert-new-bunsetsu b (and (caddr new-b) next-b) new-b)))
    (if beep
	(ding))))

(defun egg-reconvert-bunsetsu (n)
  (interactive "P")
  (egg-reconvert-bunsetsu-internal n 'egg-get-bunsetsu-converted))

(defun egg-reconvert-bunsetsu-from-source (n)
  (interactive "P")
  (egg-reconvert-bunsetsu-internal n 'egg-get-bunsetsu-source))

(defun egg-reconvert-bunsetsu-internal (n func)
  (let* ((inhibit-read-only t)
	 (backend (egg-get-backend (point)))
	 (source (funcall func (egg-get-bunsetsu-info (point))))
	 (reconv-backend (egg-get-reconvert-backend backend n))
	 (p (point))
	 (last (egg-get-bunsetsu-last (point)))
	 new prev-b next-b)
    (if (or (null reconv-backend)
	    (null (setq new (egg-start-conversion reconv-backend source nil))))
	(ding)
      (delete-region p (egg-next-bunsetsu-point p))
      (setq next-b (egg-get-bunsetsu-info (point)))
      (if (and (equal (egg-get-backend p) backend)
	       (eq (egg-bunsetsu-get-backend next-b)
		   (egg-bunsetsu-get-backend (car new)))
	       (egg-bunsetsu-combinable-p (egg-get-bunsetsu-tail new) next-b))
	  (setq last nil)
	(setq last (or (eq last t) 'continue)))
      (egg-insert-bunsetsu-list backend new last)
      (goto-char p)
      (setq prev-b (egg-get-bunsetsu-info (1- p)))
      (if prev-b
	  (progn
	    (if (and (equal (egg-get-backend (1- p)) backend)
		     (eq (egg-bunsetsu-get-backend prev-b)
			 (egg-bunsetsu-get-backend (car new)))
		     (egg-bunsetsu-combinable-p prev-b (car new)))
		(setq last nil)
	      (setq last (or (eq last t) 'continue)))
	    (setq backend (egg-get-backend (1- p)))
	    (delete-region (egg-previous-bunsetsu-point p) p)
	    (egg-insert-bunsetsu-list backend (list prev-b) last))))))

(defun egg-decide-before-point ()
  (interactive)
  (let* ((inhibit-read-only t)
	 (start (if (get-text-property (1- (point)) 'egg-start)
		    (point)
		  (previous-single-property-change (point) 'egg-start)))
	 (end (if (get-text-property (point) 'egg-end)
		  (point)
		(egg-next-single-property-change (point) 'egg-end)))
	 (decided (buffer-substring start (point)))
	 (undecided (buffer-substring (point) end))
	 i len bunsetsu source context)
    (delete-region
     (previous-single-property-change start 'egg-start nil (point-min))
     (egg-next-single-property-change end 'egg-end nil (point-max)))
    (setq i 0
	  len (length decided))
    (while (< i len)
      (setq bunsetsu (nconc bunsetsu (list (egg-get-bunsetsu-info i decided)))
	    i (egg-next-bunsetsu-point i 1 decided len))
      (if (or (= i len)
	      (egg-get-bunsetsu-last (1- i) decided))
	  (progn
	    (insert (mapconcat 'egg-get-bunsetsu-converted bunsetsu nil))
	    (setq context (cons (cons (egg-bunsetsu-get-backend (car bunsetsu))
				      (egg-end-conversion bunsetsu nil))
				context)
		  bunsetsu nil))))
    (setq len (length undecided))
    (if (= len 0)
	(progn
	  (egg-do-auto-fill)
	  (run-hooks 'input-method-after-insert-chunk-hook)
	  context)
      (setq i 0)
      (while (< i len)
	(setq bunsetsu (egg-get-bunsetsu-info i undecided)
	      source (cons (egg-get-bunsetsu-source bunsetsu)
			   source))
	(put-text-property 0 (length (car source))
			   'egg-lang
			   (egg-get-source-language bunsetsu)
			   (car source))
	(setq i (egg-next-bunsetsu-point i 1 undecided len)))
      (its-restart (apply 'concat (nreverse source)) t t context))))

(defun egg-decide-first-char ()
  (interactive)
  (let* ((inhibit-read-only t)
	 (start (if (get-text-property (1- (point)) 'egg-start)
		    (point)
		  (previous-single-property-change (point) 'egg-start)))
	 (end (if (get-text-property (point) 'egg-end)
		  (point)
		(egg-next-single-property-change (point) 'egg-end)))
	 (bunsetsu (egg-get-bunsetsu-info start)))
    (delete-region
     (previous-single-property-change start 'egg-start nil (point-min))
     (egg-next-single-property-change end 'egg-end nil (point-max)))
    (egg-end-conversion (list bunsetsu) nil)
    (insert (egg-string-to-char-at (egg-get-bunsetsu-converted bunsetsu) 0))))

(defun egg-exit-conversion ()
  (interactive)
  (if (egg-conversion-fence-p)
      (progn
	(goto-char (egg-next-single-property-change (point) 'egg-end))
	(egg-decide-before-point))))

(defun egg-abort-conversion ()
  (interactive)
  (let ((inhibit-read-only t)
	source context)
    (goto-char (previous-single-property-change
		(if (get-text-property (1- (point)) 'egg-start)
		    (point)
		  (previous-single-property-change (point) 'egg-start))
		'egg-start nil (point-min)))
    (setq source (get-text-property (point) 'egg-source)
	  context (get-text-property (point) 'egg-context))
    (delete-region (point) (egg-next-single-property-change
			    (egg-next-single-property-change (point) 'egg-end)
			    'egg-end nil (point-max)))
    (its-restart source nil nil context)))

(defun egg-toroku-bunsetsu ()
  (interactive)
  (let* ((p (point))
	 (lang (egg-get-source-language (egg-get-bunsetsu-info p)))
	 (egg-mode-hook (or (cdr (assq lang its-select-func-alist))
			    (cdr (assq lang its-select-func-default-alist))))
	 (s "")
	 bunsetsu str yomi last)
    (while (null last)
      (setq bunsetsu (egg-get-bunsetsu-info p)
	    str (concat str (egg-get-bunsetsu-converted bunsetsu))
	    yomi (concat yomi (egg-get-bunsetsu-source bunsetsu))
	    last (egg-get-bunsetsu-last p)
	    p (egg-next-bunsetsu-point p)))
    (while (equal s "")
      (setq s (read-multilingual-string (egg-get-message 'register-str)
					str egg-last-method-name))
      (and (equal s "") (ding)))
    (egg-toroku-string s nil yomi lang (egg-bunsetsu-get-backend bunsetsu))))

(defun egg-toroku-region (start end &optional nth-backend)
  (interactive "r\nP")
  (egg-toroku-string (buffer-substring start end) nil nil nil nil nth-backend))

(defun egg-toroku-string (str &optional yomi guess lang backend nth-backend)
  (let (egg-mode-hook result)
    (if (= (length str) 0)
	(egg-error "Egg word registration: null string"))
    (egg-separate-languages str lang)
    (setq lang (egg-get-language 0 str)
	  egg-mode-hook (or (cdr (assq lang its-select-func-alist))
			    (cdr (assq lang its-select-func-default-alist))))
    (or yomi (setq yomi ""))
    (while (equal yomi "")
      (setq yomi (read-multilingual-string
		  (format (egg-get-message 'register-yomi) str)
		  guess egg-last-method-name))
      (and (equal yomi "") (ding)))
    (egg-separate-languages yomi lang)
    (if (null backend)
	(progn
	  (setq backend (egg-assign-backend str nth-backend))
	  (if (cdr backend)
	      (egg-error "Egg word registration: cannot decide backend"))
	  (setq backend (egg-get-current-backend (caar backend)))))
    (setq result (egg-word-registration backend str yomi))
    (if result
	(apply 'message (egg-get-message 'registered) str yomi result)
      (beep))))

(defun egg-conversion-mode ()
  "\\{egg-conversion-map}"
  ;; dummy function to get docstring
  )

(defun egg-help-command ()
  "Display documentation for EGG Conversion mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "EGG Conversion mode:\n")
    (princ (documentation 'egg-conversion-mode))
    (help-setup-xref (cons #'help-xref-mode (current-buffer)) (interactive-p))))

(provide 'egg-cnv)

;;; egg-cnv.el ends here

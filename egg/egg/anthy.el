;;; egg/anthy.el --- ANTHY Support (high level interface) in Egg
;;;                Input Method Architecture

;; Copyright (C) 2002 The Free Software Initiative of Japan

;; Author: NIIBE Yutaka <gniibe@m17n.org>

;; Maintainer: NIIBE Yutaka <gniibe@m17n.org>
;;             Hideyuki SHIRAI <shirai@meadowy.org>

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

(require 'egg)
(require 'egg-edep)

(defgroup anthy-egg nil
  "Anthy interface for Tamago 4."
  :group 'egg)

(defcustom anthy-egg-use-utf8 nil
  "*Use UTF-8 for anthy-agent and anthy-dic-tool."
  :group 'anthy-egg
  :type '(choice (const :tag "Use UTF8" t)
		 (const :tag "No use UTF8" nil)))

(defcustom anthy-egg-input-method-list '("japanese-egg-anthy")
  "*List of input method to use egg-anthy."
  :group 'anthy-egg
  :type '(repeat (string :format "Input method: %v\n" :size 0)))

(defcustom anthy-egg-use-chinese-korean-server nil
  "*Use egg-anthy with Chinese and/or Korean servers."
  :group 'anthy-egg
  :type '(choice (const :tag "Use Anthy with Chinese and/or Korean servers" t)
		 (const :tag "No use" nil)))

(setplist 'anthy-egg-conversion-backend
	  '(egg-start-conversion          anthy-egg-convert
	    egg-get-bunsetsu-source       anthy-egg-get-bunsetsu-source
	    egg-get-bunsetsu-converted    anthy-egg-get-bunsetsu-converted
	    egg-list-candidates           anthy-egg-get-candidates
	    egg-decide-candidate          anthy-egg-select-candidate
	    egg-change-bunsetsu-length    anthy-egg-resize-segment
	    egg-end-conversion            anthy-egg-commit
	    ;;
	    egg-special-candidate         anthy-egg-special-candidate
	    egg-word-registration         anthy-egg-word-registration
	    ;;
	    egg-get-source-language       anthy-egg-get-source-language
	    egg-get-converted-language    anthy-egg-get-converted-language))

(defconst anthy-egg-backend-alist '((Japanese ((anthy-egg-conversion-backend)))))

(egg-set-finalize-backend '(anthy-egg-finalize-backend))

(defvar anthy-egg-proc nil
  "Process of ANTHY helper agent.")

(defvar anthy-egg-version nil)
(defvar anthy-egg-el-version "20070419")

(defvar anthy-egg-anthy-agent-version "")

(defun anthy-egg-version ()
  (interactive)
  (message "anthy-egg/anthyipc/anthy-agent: %s" anthy-egg-version)
  anthy-egg-version)

;; <env> ::= <context-descriptor>
;; <context-descriptor> ::= <integer>
(defvar anthy-egg-environment-pool nil
  "Environments for ANTHY kana-kanji conversion, to be used.")

(defvar anthy-egg-environments-in-use nil
  "Environments for ANTHY kana-kanji conversion, in use.")

;;
;; <anthy-bunsetsu> ::=
;;  [ <env> <source> <converted> <candidates> <candidate-pos> <seg-no> ]
(defsubst anthy-egg-make-bunsetsu (env source converted seg-no)
  (egg-bunsetsu-create
   'anthy-egg-conversion-backend
   (vector env source converted nil 0 seg-no)))

(defsubst anthy-egg-bunsetsu-get-env (b)
  (aref (egg-bunsetsu-get-info b) 0))
(defsubst anthy-egg-bunsetsu-get-source (b)
  (aref (egg-bunsetsu-get-info b) 1))
(defsubst anthy-egg-bunsetsu-get-converted (b)
  (aref (egg-bunsetsu-get-info b) 2))
(defsubst anthy-egg-bunsetsu-get-candidates (b)
  (aref (egg-bunsetsu-get-info b) 3))
(defsubst anthy-egg-bunsetsu-set-candidates (b z)
  (aset (egg-bunsetsu-get-info b) 3 z))
(defsubst anthy-egg-bunsetsu-get-candidate-pos (b)
  (aref (egg-bunsetsu-get-info b) 4))
(defsubst anthy-egg-bunsetsu-set-candidate-pos (b zp)
  (aset (egg-bunsetsu-get-info b) 4 zp))
(defsubst anthy-egg-bunsetsu-get-seg-no (b)
  (aref (egg-bunsetsu-get-info b) 5))

(defun anthy-egg-get-bunsetsu-source (b)
  (anthy-egg-bunsetsu-get-source b))

(defun anthy-egg-get-bunsetsu-converted (b)
  (let ((cands (anthy-egg-bunsetsu-get-candidates b)))
    (if cands
	(nth (anthy-egg-bunsetsu-get-candidate-pos b) cands)
      (anthy-egg-bunsetsu-get-converted b))))

(defun anthy-egg-get-source-language (b) 'Japanese)
(defun anthy-egg-get-converted-language (b) 'Japanese)

(defvar anthy-egg-agent-buffer-name " *anthy-egg*")

;; Getting new context-descriptor, and returns environment with 'inuse' bit
(defun anthy-egg-new-environment ()
  (if (null anthy-egg-proc)
      (let ((buf (get-buffer-create anthy-egg-agent-buffer-name))
	    (cs (if anthy-egg-use-utf8 'utf-8-dos 'euc-japan-dos))
	    (process-connection-type nil)) ; avoid using pty
	(setq anthy-egg-proc
	      (apply 'start-process "anthy-egg-agent" buf
		     "anthy-agent"
		     (if anthy-egg-use-utf8
			 '("--egg" "--utf8")
		       '("--egg"))))
	(process-kill-without-query anthy-egg-proc)
	(set-process-coding-system anthy-egg-proc cs cs)
	(set-process-sentinel anthy-egg-proc 'anthy-egg-proc-sentinel)
	(set-marker-insertion-type (process-mark anthy-egg-proc) t)
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  (buffer-disable-undo))
	(anthyipc-get-greeting anthy-egg-proc)))
  ;; Patch http://www.freebsd.org/cgi/query-pr.cgi?pr=68617
  (anthyipc-new-context anthy-egg-proc))

;;; XXX: Don't kill buffer (for now) so that I can debug this program
(defun anthy-egg-proc-sentinel (proc reason)
  ;; (kill-buffer (process-buffer proc))
  (setq anthy-egg-proc nil
	anthy-egg-environments-in-use nil
	anthy-egg-environment-pool nil))

(defun anthy-egg-get-environment ()
  "Return the ANTHY environment."
  (if anthy-egg-environment-pool
      (let ((env (car anthy-egg-environment-pool)))
	(setq anthy-egg-environment-pool (cdr anthy-egg-environment-pool))
	(setq anthy-egg-environments-in-use (cons env anthy-egg-environments-in-use))
	env)
    (let ((env (anthy-egg-new-environment)))
      (setq anthy-egg-environments-in-use (cons env anthy-egg-environments-in-use))
      env)))

;;
;; Fake egg functions for UTF-8
;;
(defvar anthy-egg-force-anthy nil)
(make-variable-buffer-local 'anthy-egg-force-anthy)

(defmacro anthy-egg-utf8-p ()
  `(and anthy-egg-use-utf8
	(not anthy-egg-use-chinese-korean-server)
	(or (equal (egg-get-conversion-backend 'Japanese 0 nil)
		   '(0 (anthy-egg-conversion-backend)))
	    (and (not egg-conversion-backend-alist)
		 (member default-input-method anthy-egg-input-method-list)))))

(defadvice egg-toroku-bunsetsu (around force-anthy activate compile)
  "Advice for force-anthy."
  (if (anthy-egg-utf8-p)
      (let ((anthy-egg-force-anthy t))
	ad-do-it)
    ad-do-it))

(defadvice egg-toroku-region (around force-anthy activate compile)
  "Advice for force-anthy."
  (if (anthy-egg-utf8-p)
      (let ((anthy-egg-force-anthy t))
	ad-do-it)
    ad-do-it))

(defadvice egg-convert-region (around force-anthy activate compile)
  "Advice for force-anthy."
  (if (and (interactive-p) (anthy-egg-utf8-p))
      (let ((anthy-egg-force-anthy t))
	ad-do-it)
    ad-do-it))

(defadvice egg-separate-languages (around force-anthy activate compile)
  "Advice for force-anthy."
  (if (and anthy-egg-force-anthy
	   (or (not last-lang)
	       (eq last-lang 'Japanese)))
      (let ((len (length str)))
	(egg-remove-all-text-properties 0 len str)
	(put-text-property 0 len 'egg-lang 'Japanese str))
    ad-do-it))

;;
;; Returns list of bunsetsu
;;
(defun anthy-egg-convert (backend yomi &optional context)
  "Convert YOMI string to kanji, and enter conversion mode.
Return the list of bunsetsu."
  ;; Convert Katakana to Hiragana
  (when (eq last-command 'its-katakana)
    (setq yomi (japanese-hiragana yomi)))
  (let ((env (anthy-egg-get-environment)))
    (anthyipc-convert anthy-egg-proc env yomi)))

;;
;; Pool the context
;;
(defun anthy-egg-commit (bunsetsu-list abort)
  (let ((env (anthy-egg-bunsetsu-get-env (car bunsetsu-list))))
    (anthyipc-commit anthy-egg-proc env (if abort 1 0))
    ;; Guard twice pool in egg-decide-before-point()
    ;; Add. Hideyuki SHIRAI at 2005-02-10
    (unless (memq env anthy-egg-environment-pool)
      (setq anthy-egg-environment-pool (cons env anthy-egg-environment-pool)))
    (setq anthy-egg-environments-in-use (delq env anthy-egg-environments-in-use))
    (anthy-egg-debug-check)))

;;
;; Returns ( <pos> <candidates> )
;;
(defun anthy-egg-get-candidates (bunsetsu-list prev-bunsetsu next-bunsetsu major)
  (let ((bunsetsu (car bunsetsu-list)))
    (if (anthy-egg-bunsetsu-get-candidates bunsetsu)
	(cons (anthy-egg-bunsetsu-get-candidate-pos bunsetsu)
	      (anthy-egg-bunsetsu-get-candidates bunsetsu))
      (let* ((env (anthy-egg-bunsetsu-get-env bunsetsu))
	     (seg-no (anthy-egg-bunsetsu-get-seg-no bunsetsu))
	     (cands (anthyipc-get-candidates anthy-egg-proc env seg-no)))
	(cons (anthy-egg-bunsetsu-set-candidate-pos bunsetsu 0)
	      (anthy-egg-bunsetsu-set-candidates bunsetsu cands))))))

;; Returns list of list of bunsetsu
(defun anthy-egg-select-candidate (bunsetsu-list candidate-pos prev-b next-b)
  (let* ((bunsetsu (car bunsetsu-list))
	 (candidate-list (anthy-egg-bunsetsu-get-candidates bunsetsu))
	 (candidate (nth candidate-pos candidate-list))
	 (env (anthy-egg-bunsetsu-get-env bunsetsu))
	 (seg-no (anthy-egg-bunsetsu-get-seg-no bunsetsu)))
    (anthy-egg-bunsetsu-set-candidate-pos bunsetsu candidate-pos)
    ;; Anthy doesn't have capability of changing another segment
    ;; at the selection of a segment.
    ;; So, just ignore the result of "SELECT-CANDIDATE"
    (anthyipc-select-candidate anthy-egg-proc env seg-no candidate-pos)
    (list (list bunsetsu))))

;; Returns list of list of bunsetsu
(defun anthy-egg-resize-segment (bunsetsu-list prev-b next-b len major)
  (let ((bunsetsu (car bunsetsu-list)))
    (let ((env (anthy-egg-bunsetsu-get-env bunsetsu))
	  (seg-no (anthy-egg-bunsetsu-get-seg-no bunsetsu))
	  (prevlen (length (anthy-egg-bunsetsu-get-source bunsetsu))))
      (let ((r (anthyipc-resize-segment anthy-egg-proc env seg-no
					(if (< prevlen len) 0 1))))
	;; XXX: I don't know what this means,
	;; but this works.  Blame EGG.
	(list (list (car r)) nil (cdr r))))))

(defun anthy-egg-finalize-backend ()
  (when anthy-egg-proc
    (delete-process anthy-egg-proc)
    (setq anthy-egg-proc nil)))

;;
;; Add. Hideyuki SHIRAI at 2005-02-10
(defvar anthy-egg-debug t
  "*Enable debug for egg-anthy.")

(defvar anthy-egg-debug-depth 15
  "*Display message when over this.")

(defadvice egg-abort-conversion (before release-content activate)
  "Advice on anthy.el"
  (anthy-egg-pool-content))

(defadvice egg-decide-before-point (before release-content activate)
  "Advice on anthy.el"
  (anthy-egg-pool-content))

(defun anthy-egg-pool-content ()
  "Used context move to `pool'."
  (let* ((bunsetsu (egg-get-bunsetsu-info (point)))
	 (backend (car bunsetsu))
	 env)
    (when (eq backend 'anthy-egg-conversion-backend)
      (setq env (anthy-egg-bunsetsu-get-env bunsetsu))
      ;; Guard twice pool in egg-decide-before-point()
      (unless (memq env anthy-egg-environment-pool)
	(setq anthy-egg-environment-pool (cons env anthy-egg-environment-pool)))
      (setq anthy-egg-environments-in-use (delq env anthy-egg-environments-in-use))
      (anthy-egg-debug-check))))

(defun anthy-egg-debug-check ()
  "Debug message."
  (when (and anthy-egg-debug
	     (> (length anthy-egg-environments-in-use) anthy-egg-debug-depth))
    (message "egg-anthy debug: in-use %d, pool %d with `%s' => `%s'."
	     (length anthy-egg-environments-in-use)
	     (length anthy-egg-environment-pool)
	     (symbol-name last-command) (symbol-name this-command))
    (sit-for 1.5)))

(defun anthy-egg-recover ()
  "Recover error Anthy."
  (interactive)
  (when anthy-egg-proc
    (delete-process anthy-egg-proc)
    (setq anthy-egg-proc nil))
  (setq anthy-egg-environments-in-use nil)
  (setq anthy-egg-environment-pool nil))

(defun anthy-egg-special-candidate (bunsetsu prev-b next-b major type)
  "Suport Hiragana, Katakana."
  (let* ((head (car bunsetsu))
	 (backend (egg-bunsetsu-get-backend head))
	 (lang (get backend 'language))
	 source converted zenkouho-list kouho-list pos)
    ;; Japnese only
    (setq source (anthy-egg-get-bunsetsu-source head))
    (cond ((eq type 'egg-hiragana)
	   (setq converted source))
	  ((eq type 'egg-katakana)
	   (setq converted (japanese-katakana source))))
    (setq zenkouho-list
	  (cdr (anthy-egg-get-candidates bunsetsu prev-b next-b major)))
    (setq pos
	  (when (setq kouho-list (member converted zenkouho-list))
	    (- (length zenkouho-list) (length kouho-list))))
    (when pos
      (anthy-egg-select-candidate bunsetsu pos prev-b next-b))))

;;
;; Dictionary add
;; freq はいくつが良いのか？ 1, 10, 100, 1000? 選択？
(defvar anthy-egg-hinshi-menu '(("一般名詞" . NOUN)
				("固有名詞" . PROPER_NOUN)
				("形容詞" . ADJECTIVE)
				("副詞" . ADV)
				("動詞" . VERB))
  "*Anthy の辞書登録用品詞.")

(defvar anthy-egg-hinshi-proper-menu '("人名" "地名")
  "*Anthy の固有名詞")

(defvar anthy-egg-hinshi-verb-menu '(("カ行五段活用" . "カ行五段")
				     ("ガ行五段活用" . "ガ行五段")
				     ("サ行五段活用" . "サ行五段")
				     ("タ行五段活用" . "タ行五段")
				     ("ナ行五段活用" . "ナ行五段")
				     ("バ行五段活用" . "バ行五段")
				     ("マ行五段活用" . "マ行五段")
				     ("ラ行五段活用" . "ラ行五段")
				     ("ワ行五段活用" . "ワ行五段"))
  "*Anthy の動詞活用型.")


(defvar anthy-egg-dic-util-command "anthy-dic-tool")
(defvar anthy-egg-dic-buffer-name " *anthy-egg-dic*")

(defun anthy-egg-add-word-compose-paramlist (param)
  (let ((str ""))
    (while param
      (let* ((cur (car param))
	     (var (car cur))
	     (val (if (stringp (cdr cur))
		      (cdr cur)
		    (if (cdr cur) "y" "n"))))
	(setq str (concat str
			  var " = " val "\n")))
      (setq param (cdr param)))
    str))

(defun anthy-egg-add-word (yomi freq word paramlist)
  (let ((buf (get-buffer-create anthy-egg-dic-buffer-name))
	(cs (if anthy-egg-use-utf8 'utf-8-unix 'euc-japan-unix))
	proc)
    (save-excursion
      (set-buffer buf)
      (setq proc (apply 'start-process "anthy-egg-dic" buf
			anthy-egg-dic-util-command
			(if anthy-egg-use-utf8
			    '("--append" "--utf8")
			  '("--append"))))
      (when proc
	(set-process-coding-system proc cs cs)
	(set-process-sentinel proc
			      (lambda (proc event)
				(let ((buf (process-buffer proc)))
				  (when (and (process-buffer proc)
					     (buffer-name (process-buffer proc)))
				    (kill-buffer (process-buffer proc))))))
	(process-send-string proc
			     (concat yomi " "
				     (int-to-string freq) " "
				     word "\n"))
	(process-send-string proc
			     (anthy-egg-add-word-compose-paramlist paramlist))
	(process-send-string proc "\n")
	(process-send-eof proc)
	t))))

(defun anthy-egg-hinshi-NOUN (kanji)
  (let ((res '(("品詞" . "名詞")))
	(na (y-or-n-p (format " 「%sな」と言いますか? " kanji)))
	(sa (y-or-n-p (format " 「%sさ」と言いますか? " kanji)))
	(suru (y-or-n-p (format " 「%sする」と言いますか? " kanji)))
	(ind (y-or-n-p (format " 「%s」は単独で文節になりますか? " kanji)))
	(kaku (y-or-n-p (format " 「%sと」と言いますか? " kanji))))
    (setq res (cons `("な接続" . ,na) res))
    (setq res (cons `("さ接続" . ,sa) res))
    (setq res (cons `("する接続" . ,suru) res))
    (setq res (cons `("語幹のみで文節" . ,ind) res))
    (setq res (cons `("格助詞接続" . ,kaku) res))
    res))

(defun anthy-egg-hinshi-PROPER_NOUN (kanji)
  `(("品詞" . ,(menudiag-select (list 'menu
				      (format "(%s) 活用系:" kanji)
				      anthy-egg-hinshi-proper-menu)))))

(defun anthy-egg-hinshi-PERSONAL (kanji)
  '(("品詞" . "人名")))

(defun anthy-egg-hinshi-PLACE (kanji)
  '(("品詞" . "地名")))

(defun anthy-egg-hinshi-ADJECTIVE (kanji)
  '(("品詞" . "形容詞")))

(defun anthy-egg-hinshi-ADV (kanji)
  (let ((res '(("品詞" . "副詞")))
	(to (y-or-n-p (format " 「%sと」と言いますか? " kanji)))
	(taru (y-or-n-p (format " 「%sたる」と言いますか? " kanji)))
	(suru (y-or-n-p (format " 「%sする」と言いますか? " kanji)))
	(ind (y-or-n-p (format " 「%s」は単独で文節になりますか? " kanji))))
    (setq res (cons `("と接続" . ,to) res))
    (setq res (cons `("たる接続" . ,taru) res))
    (setq res (cons `("する接続" . ,suru) res))
    (setq res (cons `("語幹のみで文節" . ,ind) res))
    res))

(defun anthy-egg-hinshi-VERB (kanji)
  (let* ((res '(("品詞" . "動詞")))
	 (key (menudiag-select (list 'menu
				     (format "(%s) 活用系:" kanji)
				     anthy-egg-hinshi-verb-menu)))
	 (meishi  (y-or-n-p (format "%s: 連体形を名詞化しますか? " kanji))))
    (setq res (cons `("活用" . ,key) res))
    (setq res (cons `("連用形の名詞化" . ,meishi) res))
    res))

(defun anthy-egg-hinshi-select (kanji yomi)
  (let ((key (menudiag-select (list 'menu
				    (concat kanji"(" yomi ") " "品詞:")
				    anthy-egg-hinshi-menu))))
    (cond ((symbolp key)
	   (funcall (intern (concat "anthy-egg-hinshi-" (symbol-name key)))
		    kanji))
	  ((stringp key)
	   (cdr (assoc key anthy-egg-hinshi-menu))))))

(defun anthy-egg-word-registration-add (kanji yomi)
  (let (param freq)
    (setq param (nreverse (anthy-egg-hinshi-select kanji yomi)))
    (if (anthy-egg-add-word yomi 1000 kanji param)
	(list (cdr (car param)) "ユーザ辞書")
      (message "%s (%s) の登録に失敗しました" kanji yomi))))

(defun anthy-egg-word-registration (backend kanji yomi)
  "Register a word KANJI with a pronunciation YOMI."
  (if (or (null (eq (egg-get-language 0 kanji)
		    (anthy-egg-get-converted-language backend)))
	  (next-single-property-change 0 'egg-lang kanji)
	  (null (eq (egg-get-language 0 yomi)
		    (anthy-egg-get-source-language backend)))
	  (next-single-property-change 0 'egg-lang yomi))
      (egg-error "word registration: invalid character")
    (anthy-egg-word-registration-add kanji yomi)))

;;; setup

(load "egg/anthyipc")
(run-hooks 'anthy-egg-load-hook)

;;;###autoload
(defun egg-activate-anthy (&rest arg)
  "Activate ANTHY backend of Tamago 4."
  (apply 'egg-mode (append arg anthy-egg-backend-alist)))

;;; egg/anthy.el ends here.

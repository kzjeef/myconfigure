;;; egg/canna.el --- Canna Support (high level interface) in
;;;                  Egg Input Method Architecture

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

(require 'egg)
(require 'egg-edep)

(defgroup canna nil
  "CANNA interface for Tamago 4."
  :group 'egg)

(defcustom canna-hostname "unix/"
  "Hostname of CANNA server"
  :group 'canna :type 'string)

(defcustom canna-server-port "/tmp/.iroha_unix/IROHA"
  "A service name or a port number (should be a string) of CANNA server"
  :group 'canna :type 'string)

(defcustom canna-user-name nil
  "User Name on CANNA server"
  :group 'canna :type 'string)

(defcustom canna-group-name nil
  "Group Name on CANNA server"
  :group 'canna :type 'string)

(defcustom egg-canna-helper-path "egg-helper"
  "path of canna unix domain connection helper program"
  :group 'canna :type 'file)

; (eval-when-compile
;   (defmacro CANNA-const (c)
;     (cond ((eq c 'FileNotExist) xxxxxxxxxxxxxx)
; 	  )))

(egg-add-message
 '((Japanese
    (canna-connect-error  "サーバと接続できませんでした")
    (canna-fail-make-env  "環境を作ることはできませんでした")
    (canna-dict-missing-1 "辞書ファイル %s がありません。")
    (canna-dict-missing-2 "辞書ファイル %s がありません。作りますか? ")
    (canna-dict-created   "辞書ファイル %s を作りました")
    (canna-dict-saving    "%s の頻度情報・辞書情報を退避しています")
    (canna-dict-saved     "%s の頻度情報・辞書情報を退避しました")
    (canna-register-1     "登録辞書名:")
    (canna-register-2     "品詞名"))))

(defvar canna-hinshi-alist
  '(("人名" . "#JN") ("地名" . "#CN") ("固有名詞" . "#KK")
    ("一般名詞" . "#T35") ("名詞(例)強力な" . "#T15")
    ("サ変名詞" . "#T30") ("サ変名詞(例)安心な" . "#T10") ("単漢字" . "#KJ")
    ("動詞カ行変格活用" . "#KX") ("動詞ンザ行変格活用" . "#NZX")
    ("動詞ザ行変格活用" . "#ZX") ("動詞サ行変格活用" . "#SX")
    ("動詞カ行五段活用" . "#K5") ("動詞ガ行五段活用" . "#G5")
    ("動詞サ行五段活用" . "#S5") ("動詞タ行五段活用" . "#T5")
    ("動詞ナ行五段活用" . "#N5") ("動詞バ行五段活用" . "#B5")
    ("動詞マ行五段活用" . "#M5") ("動詞ラ行五段活用" . "#R5")
    ("動詞ワ行五段活用" . "#W5") ("動詞上下一段活用" . "#KS")
    ("動詞カ行五段連用名詞" . "#K5r") ("動詞ガ行五段連用名詞" . "#G5r")
    ("動詞サ行五段連用名詞" . "#S5r") ("動詞タ行五段連用名詞" . "#T5r")
    ("動詞ナ行五段連用名詞" . "#N5r") ("動詞バ行五段連用名詞" . "#B5r")
    ("動詞マ行五段連用名詞" . "#M5r") ("動詞ラ行五段連用名詞" . "#R5r")
    ("動詞ワ行五段連用名詞" . "#W5r") ("動詞上下一段語幹名詞" . "#KSr")
    ("形容詞" . "#KY") ("形容詞(例)きいろい" . "#KYT")
    ("形容動詞" . "#T05")
    ("形容動詞(例)関心だ" . "#T10") ("形容動詞(例)多慌てだ" . "#T13")
    ("形容動詞(例)意外だ" . "#T15") ("形容動詞(例)便利だ" . "#T18")
    ("副詞" . "#F14") ("副詞(例)ふっくら" . "#F04")
    ("副詞(例)そっと" . "#F12") ("副詞(例)突然" . "#F06")
    ("数詞" . "#NN") ("接続詞・感動詞" . "#CJ") ("連体詞" . "#RT")))

(defvar canna-hinshi-menu
  '("人名" "地名" ("団体・会社名" . "固有名詞") ("名詞" . MEISHI)
    ("サ変名詞" . SAHEN-MEISHI) "単漢字" ("動詞" . DOUSHI)
    ("形容詞" . KEIYOUSHI) ("形容動詞" . KEIYOUDOUSHI) ("副詞" . FUKUSHI)
    "数詞" "接続詞・感動詞" "連体詞" ("その他の固有名詞" . "固有名詞"))
  "Menu data for a hinshi (a part of speech) selection.")

(defun canna-hinshi-name (id &optional reverse)
  (if reverse
      (cdr (assoc id canna-hinshi-alist))
    (car (rassoc id canna-hinshi-alist))))

(defmacro canna-backend-plist ()
  ''(egg-start-conversion          canna-start-conversion
     egg-get-bunsetsu-source       canna-get-bunsetsu-source
     egg-get-bunsetsu-converted    canna-get-bunsetsu-converted
     egg-get-source-language       canna-get-source-language
     egg-get-converted-language    canna-get-converted-language
     egg-list-candidates           canna-list-candidates
     egg-decide-candidate          canna-decide-candidate
     egg-special-candidate         canna-special-candidate
     egg-change-bunsetsu-length    canna-change-bunsetsu-length
     egg-end-conversion            canna-end-conversion
     egg-word-registration         canna-word-registration))

(defconst canna-backend-language-alist nil)

(defvar canna-backend-alist nil)

(defun canna-backend-func-name (name lang &optional env)
  (intern (concat name "-" (symbol-name lang)
		  (and env "-") (and env (symbol-name env)))))

(defun canna-make-backend (lang env &optional source-lang converted-lang)
  (let ((finalize (canna-backend-func-name "canna-finalize-backend" lang))
	(backend (canna-backend-func-name "canna-backend" lang env)))
    (if (null (fboundp finalize))
	(progn
	  (fset finalize (function (lambda () (canna-finalize-backend))))
	  (egg-set-finalize-backend (list finalize))))
    (if (null (get backend 'egg-start-conversion))
	(setplist backend (apply 'list
				 'language lang
				 'source-language (or source-lang lang)
				 'converted-language (or converted-lang lang)
				 (canna-backend-plist))))
    backend))

(defun canna-define-backend (lang env-name-list)
  (mapcar (lambda (env)
	    (if (consp env)
		(canna-define-backend lang env)
	      (canna-make-backend lang env)))
	  env-name-list))

(defun canna-define-backend-alist (deflist)
  (setq canna-backend-alist
	(mapcar (lambda (slot)
		  (let* ((lang (car slot))
			 (alt (cdr (assq lang canna-backend-language-alist))))
		    (cons lang (canna-define-backend (or alt lang) (cdr slot)))))
		deflist)))

(defcustom canna-backend-define-list
  '((Japanese    ((nil nil nil))
		 ((Bushu Bushu Bushu))))
  "Alist of Japanese language and lists of the Canna backend suffixes."
  :group 'canna
  :set (lambda (sym value)
	 (set-default sym value)
	 (canna-define-backend-alist value))
  :type '(repeat
	  (cons
	   :tag "Language - Backend"
	   (choice :tag "Language"
		   (const Japanese)
		   (symbol :tag "Other"))
	   (repeat
	    (cons
	     :tag "Backend Sequece"
	     (cons :tag "First Conversion Stage"
		   (symbol :tag "Backend for Start Conversion")
		   (repeat :tag "Backends for Reconvert"
			   (symbol :tag "Backend")))
	     (repeat
	      :tag "Following Conversion Stages"
	      (cons
	       :tag "N-th Stage"
	       (symbol :tag "Backend for This Stage")
	       (repeat :tag "Backends for Reconvert"
		       (symbol :tag "Backend")))))))))

(defsubst canna-backend-get-language (backend)
  (get backend 'language))

(defsubst canna-backend-get-source-language (backend)
  (get backend 'source-language))

(defsubst canna-backend-get-converted-language (backend)
  (get backend 'converted-language))

(defvar canna-envspec-list nil)
(defvar canna-current-envspec nil)

;; Should support multiple outstanding context
;; <env> ::= [ <proc> <context> <backend> <convert-mode> <nostudy> <dic-list> ]
(defvar canna-environments nil
  "Environment for CANNA kana-kanji conversion")

(defun cannaenv-create (proc context &optional backend mode nostudy)
  (vector proc context backend mode nostudy (list nil)))

(defsubst cannaenv-get-proc (env)    (aref env 0))
(defsubst cannaenv-get-context (env) (aref env 1))
(defsubst cannaenv-get-backend (env) (aref env 2))
(defsubst cannaenv-get-mode (env)    (aref env 3))
(defsubst cannaenv-get-nostudy (env) (aref env 4))
(defsubst cannaenv-get-dic-list (env) (cdr (aref env 5)))

(defsubst cannaenv-add-dic-list (env &rest dic)
  (nconc (aref env 5) (list (apply 'vector dic))))

;; <canna-bunsetsu> ::=
;;  [ <env> <converted> <bunsetsu-pos> <source>
;;    <zenkouho-pos> <zenkouho> <zenkouho-converted> ]
(defsubst canna-make-bunsetsu (env converted bunsetsu-pos source)
  (egg-bunsetsu-create
   (cannaenv-get-backend env)
   (vector env converted bunsetsu-pos source nil nil nil)))

(defsubst canna-bunsetsu-get-env (b)
  (aref (egg-bunsetsu-get-info b) 0))
(defsubst canna-bunsetsu-get-converted (b)
  (aref (egg-bunsetsu-get-info b) 1))
(defsubst canna-bunsetsu-get-bunsetsu-pos (b)
  (aref (egg-bunsetsu-get-info b) 2))
(defsubst canna-bunsetsu-get-source (b)
  (aref (egg-bunsetsu-get-info b) 3))
(defsubst canna-bunsetsu-set-source (b s)
  (aset (egg-bunsetsu-get-info b) 3 s))
(defsubst canna-bunsetsu-get-zenkouho-pos (b)
  (aref (egg-bunsetsu-get-info b) 4))
(defsubst canna-bunsetsu-set-zenkouho-pos (b p)
  (aset (egg-bunsetsu-get-info b) 4 p))
(defsubst canna-bunsetsu-get-zenkouho (b)
  (aref (egg-bunsetsu-get-info b) 5))
(defsubst canna-bunsetsu-set-zenkouho (b z)
  (aset (egg-bunsetsu-get-info b) 5 z))
(defsubst canna-bunsetsu-get-zenkouho-converted (b)
  (aref (egg-bunsetsu-get-info b) 6))
(defsubst canna-bunsetsu-set-zenkouho-converted (b zc)
  (aset (egg-bunsetsu-get-info b) 6 zc))

(defun canna-get-bunsetsu-source (b)
  (let ((s (canna-bunsetsu-get-source b)))
    (or s
	(let* ((env (canna-bunsetsu-get-env b))
	       (bp (canna-bunsetsu-get-bunsetsu-pos b))
	       (s (cannarpc-get-bunsetsu-source env bp)))
	  (canna-bunsetsu-set-source b s)))))
(defun canna-get-bunsetsu-converted (b) (canna-bunsetsu-get-converted b))
(defun canna-get-source-language (b) 'Japanese)
(defun canna-get-converted-language (b) 'Japanese)

(defun canna-envspec-create (env-name convert-mode nostudy)
  (vector (and env-name (setq env-name (intern env-name)))
	  (canna-make-backend egg-language env-name)
	  convert-mode nostudy (list nil)))

(defsubst canna-envspec-env-type (spec)           (aref spec 0))
(defsubst canna-envspec-backend (spec)            (aref spec 1))
(defsubst canna-envspec-mode (spec)               (aref spec 2))
(defsubst canna-envspec-nostudy (spec)            (aref spec 3))
(defsubst canna-envspec-dic-list (spec)           (cdr (aref spec 4)))

(defsubst canna-envspec-add-dic-list (spec &rest dic)
  (nconc (aref spec 4) (list (apply 'vector dic))))

(defmacro canna-arg-type-error (func)
  `(egg-error ,(format "%s: Wrong type argument" func)))

(defun canna-define-environment (&optional env-name convert-mode nostudy)
  "Define a Canna environment. ENV-NAME specifies suffix of the Canna
environment name. CONVERT-MODE specifies including hiragana or
katakana to candidates list. NOSTUDY specifies not study."
  (if (and env-name (null (stringp env-name)))
      (canna-arg-type-error canna-define-environment))
  (setq canna-current-envspec (canna-envspec-create env-name
						    convert-mode nostudy)
	canna-envspec-list (nconc canna-envspec-list
				  (list canna-current-envspec))))

(defun canna-add-dict (dict dict-rw)
  (canna-envspec-add-dic-list canna-current-envspec dict dict-rw))

(defun canna-comm-sentinel (proc reason)	; assume it is close
  (let ((inhibit-quit t))
    (kill-buffer (process-buffer proc))
    ;; delete env from the list.
    (setq canna-environments
	  (delq nil (mapcar (lambda (env)
			      (if (null (eq (cannaenv-get-proc env) proc))
				  env))
			    canna-environments)))))

(defun canna-open (hostname-list)
  "Establish the connection to CANNA server.  Return environment object."
  (let* ((save-inhibit-quit inhibit-quit)
	 (inhibit-quit t)
	 (proc-name "CANNA")
	 (msg-form "Canna: connecting to %S at %s...")
	 (user-name (or canna-user-name (user-login-name)))
	 (id (shell-command-to-string "id"))
	 (group (or canna-group-name
		    (if (string-match "gid=[0-9]+(\\([^)]+\\))" id)
			(match-string 1 id)
		      "user")))
	 buf hostname port proc result msg)
    (unwind-protect
	(progn
	  (setq buf (generate-new-buffer " *CANNA*"))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (buffer-disable-undo)
	    (set-buffer-multibyte nil)
	    (setq egg-fixed-euc 'fixed-euc-jp))
	  (or (consp hostname-list)
	      (setq hostname-list (list hostname-list)))
	  (while (and hostname-list (null proc))
	    (setq hostname (or (car hostname-list) "")
		  hostname-list (cdr hostname-list))
	    (if (null (string-match "^unix/" hostname))
		(progn
		  (if (null (string-match ":" hostname))
		      (setq port canna-server-port)
		    (setq port (substring hostname (match-end 0))
			  hostname (substring hostname 0 (match-beginning 0))))
		  (if (and (stringp port) (string-match "^[0-9]+$" port))
		      (setq port (string-to-int port)))
		  (and (equal hostname "")
		       (setq hostname (or (getenv "CANNAHOST") "localhost")))
		  (setq host hostname)
		  (setq family nil))
	      (setq family 'local)
	      (setq host nil)
	      (setq port canna-server-port)
	      (if (null (and (stringp port) (string-match "IROHA$" port)))
		  (setq port "/tmp/.iroha_unix/IROHA")))
	    (let ((inhibit-quit save-inhibit-quit))
	      (if (and msg
		       (null (y-or-n-p (format "%s failed. Try to %s? "
					       msg hostname))))
		  (egg-error "abort connect")))
	    (setq msg (format "Canna: connecting to %s..." hostname))
	    (message "%s" msg)
	    (let ((inhibit-quit save-inhibit-quit))
	      (if (fboundp 'make-network-process)
		  (condition-case nil
		      (setq proc (make-network-process :name proc-name :buffer buf :host host :service port :family family))
		    ((error quit)))
		; for old emacs (<= 21.3) bellow
		(if (string-match "^unix/" hostname)
		    (let ((process-connection-type nil))
		      (setq proc (start-process proc-name buf egg-canna-helper-path port)))
		  (condition-case nil
		      (setq proc (open-network-stream proc-name buf hostname port))
		    (error quit)))))
	    (when (processp proc)
	      (process-kill-without-query proc)
	      (set-process-coding-system proc 'binary 'binary)
	      (set-process-sentinel proc 'canna-comm-sentinel)
	      (set-marker-insertion-type (process-mark proc) t)
	      (setq result (cannarpc-open proc user-name)) ;; result is context
	      (if (= result -1)
		  (progn
		    (delete-process proc)
		    (setq proc nil))
		(cannarpc-notice-group-name proc result group)
		(cannarpc-set-app-name proc result "EGG4"))))
	  (cons proc result))
      (if proc
	  (message (concat msg "done"))
	(if buf (kill-buffer buf))
	(egg-error 'canna-connect-error)))))

(defun canna-filename (p)
  ""
  (cond ((consp p) (concat (car p) "/" (user-login-name)))
	(t p)))

(defun canna-search-environment (backend)
  (let ((env-list canna-environments)
	env)
    (while (and (null env) env-list)
      (setq env (and (eq (cannaenv-get-backend (car env-list)) backend)
		     (car env-list))
	    env-list (cdr env-list)))
    env))

(defun canna-get-environment (backend)
  "Return the backend of CANNA environment."
  (let ((env (canna-search-environment backend))
	proc context error)
    (or env
	(unwind-protect
	    (let* ((language (canna-backend-get-language backend))
		   specs)
	      (setq proc (canna-open canna-hostname)
		    context (cdr proc)
		    proc (car proc)
		    canna-envspec-list nil)
	      (condition-case err
		  (egg-load-startup-file 'canna language)
		(egg-error
		 (setq error err)
		 (signal (car error) (cdr error))))
	      (setq specs canna-envspec-list)
	      (while specs
		(canna-create-environment proc context (car specs))
		(setq context nil)
		(setq specs (cdr specs)))
	      (setq env (canna-search-environment backend)))
	  (when (and proc (null env))
	    (cannarpc-close proc)
	    (if error
		(signal (car error) (cdr error))
	      (egg-error 'canna-fail-make-env)))
	    ))))

(defun canna-create-environment (proc context spec)
  (let* ((save-inhibit-quit inhibit-quit)
	 (inhibit-quit t)
	 (backend (canna-envspec-backend spec))
	 (convert-mode (canna-envspec-mode spec))
	 (nostudy (canna-envspec-nostudy spec))
	 (dic-list (canna-envspec-dic-list spec))
	 env)
    (condition-case err
	(progn
	  (if (not context)
	      (setq context (cannarpc-create-context proc)))
	  (if (< context 0)
	      (egg-error "%s" (cannarpc-get-error-message (- context))))
	  (setq env (cannaenv-create proc context backend convert-mode nostudy))
	  (let ((inhibit-quit save-inhibit-quit))
	    (while dic-list
	      (canna-set-dictionary env (car dic-list))
	      (setq dic-list (cdr dic-list))))
	  (setq canna-environments (nconc canna-environments (list env))))
      ((egg-error quit)
       (if (eq (car err) 'egg-error)
	   (message "%s" (nth 1 err)))
       (if env
	   (progn
	     (cannarpc-close-context env)
	     (setq canna-environments (delq env canna-environments))))
       (if (eq (car err) 'quit)
	   (signal 'quit (cdr err)))))))

(defun canna-set-dictionary (env dic-spec)
  (let ((dname (aref dic-spec 0))
	(drw   (aref dic-spec 1))
	did result)
    (if (= 0 (canna-open-dictionary env dname drw))
	(cannaenv-add-dic-list env dname drw))))

(defun canna-open-dictionary (env name rw)
  (let ((trying t)
	ret)
    (while trying
      (setq ret (cannarpc-open-dictionary env name 0)) ; XXX MODE=0
      (if (= ret 0)
	  (setq trying nil)
	(message (egg-get-message 'canna-dict-missing-1) name)
	(if rw
	(if (and (y-or-n-p
		      (format (egg-get-message 'canna-dict-missing-2) name))
		 (= (cannarpc-make-dictionary env name) 0))
		(message (egg-get-message 'canna-dict-created) name)
	      (message "%s" (cannarpc-get-error-message (- ret))))
	  (setq trying nil))))
    ret))

(defun canna-save-dictionaries (env)
  (let ((dic-list (canna-list-writable-dictionaries-byname env))
	dic)
    (while dic-list
      (setq dic (car dic-list)
	    dic-list (cdr dic-list))
      (cannarpc-save-dictionary env dic))))

(defun canna-init ()
  )

(defun canna-set-converted-yomi (bunsetsu-pos bunsetsu-list)
  (let ((bl bunsetsu-list)
	(i bunsetsu-pos)
	b)
    (while bl
      (setq b (car bl))
      (canna-bunsetsu-set-source b (cannarpc-get-bunsetsu-source env i))
      (setq i (1+ i)
	    bl (cdr bl)))
    bunsetsu-list))

(defun canna-start-conversion (backend yomi &optional context)
  "Convert YOMI string to kanji, and enter conversion mode.
Return the list of bunsetsu."
  (let* ((env (canna-get-environment backend))
	 (bunsetsu-list (cannarpc-begin-conversion env yomi)))
    (if (numberp bunsetsu-list) ; XXX error の処理適当
	(progn
	  (if (= -1 (cannarpc-cancel-conversion env))
	      (progn
	  (setq env (canna-get-environment backend))
		(canna-finalize-backend)))
	  (setq bunsetsu-list (cannarpc-begin-conversion env yomi))))
    (canna-set-converted-yomi 0 bunsetsu-list)))

(defun canna-end-conversion (bunsetsu-list abort)
  (let* ((env (canna-bunsetsu-get-env (car bunsetsu-list)))
	 (l bunsetsu-list)
	 (len (length bunsetsu-list))
	 (zenkouho-pos-vector (make-vector (* 2 len) 0))
	 (i 0)
	 (mode (if (cannaenv-get-nostudy env) 0 1)) ; MODE=1 学習  0 しない
	 bunsetsu zenkouho-pos)
    (if abort
	(setq mode 0))
    (while l
      (setq bunsetsu (car l))
      (setq l (cdr l))
      (setq zenkouho-pos (canna-bunsetsu-get-zenkouho-pos bunsetsu))
      (if (null zenkouho-pos)
	  () ; XXX: NIL--> 0 atteru???
	(aset zenkouho-pos-vector i 0)	; XXX Don't support >=256
	(aset zenkouho-pos-vector (1+ i) zenkouho-pos))
      (setq i (+ i 2)))
    (cannarpc-end-conversion env len zenkouho-pos-vector mode)))

(defun canna-list-candidates (bunsetsu prev-b next-b major)
  (setq bunsetsu (car bunsetsu))
  (if (canna-bunsetsu-get-zenkouho bunsetsu)
      (cons (canna-bunsetsu-get-zenkouho-pos bunsetsu)
	    (canna-bunsetsu-get-zenkouho-converted bunsetsu))
    (let* ((env (canna-bunsetsu-get-env bunsetsu))
	   (yomi (canna-get-bunsetsu-source bunsetsu))
	   (bunsetsu-pos (canna-bunsetsu-get-bunsetsu-pos bunsetsu))
	   (z (cannarpc-get-bunsetsu-candidates env bunsetsu-pos yomi)))
      (canna-bunsetsu-set-zenkouho bunsetsu z)
      (cons (canna-bunsetsu-set-zenkouho-pos bunsetsu 0)
	    (canna-bunsetsu-set-zenkouho-converted
	     bunsetsu
	     (mapcar 'canna-bunsetsu-get-converted z))))))

;;; XXX not use ?
(defun canna-get-number-of-candidates (bunsetsu)
  (let ((l (canna-bunsetsu-get-zenkouho bunsetsu)))
    (if l
	(length l)
      nil)))

(defun canna-decide-candidate (bunsetsu pos prev-b next-b)
  (let* ((head (car bunsetsu))
	 (candidate-list (canna-bunsetsu-get-zenkouho head))
	 (candidate (nth pos candidate-list)))
    (canna-bunsetsu-set-zenkouho candidate candidate-list)
    (canna-bunsetsu-set-zenkouho-pos candidate pos)
    (canna-bunsetsu-set-zenkouho-converted
     candidate (canna-bunsetsu-get-zenkouho-converted head))
    (list (list candidate))))

(defun canna-special-candidate (bunsetsu prev-b next-b major type)
  (let* ((head (car bunsetsu))
	 (env (canna-bunsetsu-get-env head))
	 (backend (egg-bunsetsu-get-backend head))
	 (lang (get backend 'language))
	 source converted zenkouho-list kouho-list pos)
    (when (and (eq lang (get backend 'source-language))
	       (eq lang (get backend 'converted-language)))
      (cond ((eq lang 'Japanese)
	     (setq source (canna-get-bunsetsu-source head))
	     (cond ((eq type 'egg-hiragana)
		    (setq converted source))
		   ((eq type 'egg-katakana)
		    (setq converted (japanese-katakana source))))
	     (setq zenkouho-list
		   (cdr (canna-list-candidates bunsetsu prev-b next-b major)))
	     (setq pos
		   (when (setq kouho-list (member converted zenkouho-list))
		     (- (length zenkouho-list) (length kouho-list))))))
      (when pos
	(canna-decide-candidate bunsetsu pos prev-b next-b)))))

;;; XXX not used ?
(defun canna-get-current-candidate-number (bunsetsu)
  (canna-bunsetsu-get-zenkouho-pos bunsetsu))

;;; XXX not used ?
(defun canna-get-all-candidates (bunsetsu)
  (let* ((l (canna-bunsetsu-get-zenkouho bunsetsu))
	 (result (cons nil nil))
	 (r result))
    (catch 'break
      (while t
	(let ((candidate (car l)))
	  (setcar r (canna-bunsetsu-get-converted candidate))
	  (if (null (setq l (cdr l)))
	      (throw 'break nil)
	    (setq r (setcdr r (cons nil nil)))))))
    result))

(defun canna-change-bunsetsu-length (bunsetsu prev-b next-b len major)
  (let* ((env (canna-bunsetsu-get-env (car bunsetsu)))
	 (yomi (canna-get-bunsetsu-source (car bunsetsu)))
	 (yomi-length (cond ((< (length yomi) len) -1)
			    ((> (length yomi) len) -2)
			    (t nil)))
	 (bunsetsu-pos (canna-bunsetsu-get-bunsetsu-pos (car bunsetsu)))
	 new)
    (if yomi-length
	(setq new (canna-set-converted-yomi
		   bunsetsu-pos
		   (cannarpc-set-kugiri-changed env yomi-length bunsetsu-pos)))
      (setq new bunsetsu))
    (list (list (car new)) prev-b (cdr new))))

(defun canna-finalize-backend (&optional action)
  (let* ((save-inhibit-quit inhibit-quit)
	 (inhibit-quit t)
	 (env-list canna-environments)
	 env proc-list saved)
    (while env-list
      (setq env (car env-list)
	    env-list (cdr env-list))
      (condition-case err
	  (progn
	    (unless (memq (cannaenv-get-proc env) proc-list)
	      (setq proc-list (cons (cannaenv-get-proc env) proc-list)))
	    (unless (eq action 'disconnect-only)
	      (unless saved
		(setq saved t)
		(message (egg-get-message 'canna-dict-saving) "Canna"))
	      (let ((inhibit-quit save-inhibit-quit))
		(canna-save-dictionaries env)))
	    (unless (eq action 'save-only)
	      (cannarpc-close-context env)))
	((error quit)
	 (message "signal %S occured when dictionary saving" err))))
    (if saved
	(message (egg-get-message 'canna-dict-saved) "Canna"))
    (unless (eq action 'save-only)
      (while proc-list
	(if (and (car proc-list)
		 (memq (process-status (car proc-list)) '(open run)))
	    (cannarpc-close (car proc-list)))
	(setq proc-list (cdr proc-list)))))
  (setq canna-environments nil))

;;; word registration

(defun canna-list-writable-dictionaries-byname (env)
  (let ((dic-list (cannaenv-get-dic-list env)))
    (delq nil
	  (mapcar (lambda (dic)
		    (let ((dname (aref dic 0))
			  (drw   (aref dic 1)))
		      (and drw dname)))
		  dic-list))))

(defun canna-dictionary-select (env)
  (let ((dic-list (canna-list-writable-dictionaries-byname env)))
    (if (= 1 (length dic-list))
	(car dic-list)
      (menudiag-select (list 'menu
			     (egg-get-message 'canna-register-1)
			     dic-list)))))

(defun canna-hinshi-MEISHI (kanji yomi)
  (if (y-or-n-p (concat "「" kanji "な」は正しいですか。")) "#T15" "#T35"))

(defun canna-hinshi-SAHEN-MEISHI (kanji yomi)
  (if (y-or-n-p (concat "「" kanji "な」は正しいですか。")) "#T10" "#T30"))

(defmacro canna-hinshi-DOUSHI-check-gobi ()
  '(progn
     (setq i 0)
     (while (> 9 i)
       (if (string-match (concat (substring gobi i (1+ i)) "$") kanji)
	   (progn
	     (setq renyou  (substring re-gobi i (1+ i)))
	     (setq mizen   (substring mi-gobi i (1+ i)))
	     (setq kanji-gobi   (substring kanji (match-beginning 0)))
	     (setq kanji-gokan (substring kanji 0 (match-beginning 0)))
	     (setq ret (nth i hinshi))
	     (setq i 9)))
       (setq i (1+ i)))
     (setq i 0)
     (while (> 9 i)
       (if (string-match (concat (substring gobi i (1+ i)) "$") yomi)
	   (progn
	     (setq yomi-gobi  (substring yomi (match-beginning 0)))
	     (setq yomi-gokan (substring yomi 0 (match-beginning 0)))
	     (setq i 9)))
       (setq i (1+ i)))))

(defun canna-hinshi-DOUSHI (kanji yomi)
  (let ((gobi    "くぐすつぬぶむるう")
	(re-gobi "きぎしちにびみりい")
	(mi-gobi "かがさたなばまらわ")
	(hinshi (list "#K5" "#G5" "#S5" "#T5" "#N5" "#B5" "#M5" "#R5" "#W5"))
	kanji-gokan yomi-gokan kanji-gobi yomi-gobi mizen renyou
	i ret1 ret2 ret)
    (canna-hinshi-DOUSHI-check-gobi)
    (if (not (and (> (length kanji) 1) (> (length yomi) 1)
		  (and kanji-gobi yomi-gobi (equal kanji-gobi yomi-gobi))))
	(if (and kanji-gobi yomi-gobi)
	    (egg-error "読みと候補の活用が違います。入力しなおしてください。")
	  (egg-error "読みと候補を終止形で入力してください。")))
    (cond ((and (> (length kanji) 2) (> (length yomi) 2)
		(string-match "くる$" kanji) (string-match "くる$" yomi))
	   (setq ret "#KX")
	   (setq kanji-gokan (substring kanji 0 (- (length kanji) 2)))
	   (setq yomi-gokan  (substring yomi  0 (- (length  yomi) 2))))
	  ((and (> (length kanji) 3) (> (length yomi) 3)
		(string-match "んずる$" kanji) (string-match "んずる$" yomi))
	   (setq ret "#NZX")
	   (setq kanji-gokan (substring kanji 0 (- (length kanji) 3)))
	   (setq yomi-gokan  (substring yomi  0 (- (length  yomi) 3))))
	  ((and (> (length kanji) 2) (> (length yomi) 2)
		(string-match "ずる$" kanji) (string-match "ずる$" yomi))
	   (setq ret "#ZX")
	   (setq kanji-gokan (substring kanji 0 (- (length kanji) 2)))
	   (setq yomi-gokan  (substring yomi  0 (- (length  yomi) 2))))
	  ((and (> (length kanji) 2) (> (length yomi) 2)
		(string-match "する$" kanji) (string-match "する$" yomi))
	   (setq ret "#SX")
	   (setq kanji-gokan (substring kanji 0 (- (length kanji) 2)))
	   (setq yomi-gokan  (substring yomi  0 (- (length  yomi) 2)))))
    (if (not (string-match "5$" ret))
	(if (y-or-n-p (concat "『" kanji "』を (" (canna-hinshi-name ret)
			      ") として登録しますか? "))
	    (setq ret (list kanji-gokan yomi-gokan ret))
	  (setq ret "#R5")
	  (setq kanji-gokan (substring kanji 0 (- (length kanji) 1)))
	  (setq yomi-gokan  (substring yomi  0 (- (length  yomi) 1)))))
    (if (listp ret)
	ret
      (if (y-or-n-p "さらに細かい品詞分けのための質問をしても良いですか? ")
	  (progn
	    (setq ret1 (y-or-n-p (concat "「" kanji-gokan mizen
					 "ない」は正しいですか。")))
	    (setq i 0)
	    (if (eq "#R5" ret)
		(while (> 9 i)
		  (if (string-match (concat (substring re-gobi i (1+ i)) "$")
				    kanji-gokan)
		      (progn (setq renyou nil)
			     (setq i 9)))
		  (setq i (1+ i))))
	    (setq ret2 (y-or-n-p (concat "「" kanji-gokan renyou
					 "がいい」は正しいですか。")))
	    (setq ret (if ret1 (if ret2 (concat ret "r") ret)
			(if ret2 "#KSr" "#KS")))))
      (list kanji-gokan yomi-gokan ret))))

(defun canna-hinshi-KEIYOUSHI (kanji yomi)
  (let (ret)
    (if (not (and (> (length kanji) 1) (> (length yomi) 1)
		  (string-match "い$" yomi) (string-match "い$" kanji)))
	(egg-error "読みと候補を 終止形で入力してください。例) 早い"))
    (setq kanji (substring kanji 0 (1- (length kanji))))
    (setq yomi (substring yomi 0 (1- (length yomi))))
    (setq ret
	  (if (y-or-n-p "さらに細かい品詞分けのための質問をしても良いですか? ")
	      (if (y-or-n-p (concat "「" kanji "」は正しいですか。"))
		  "#KYT" "#KY")
	    "#KY"))
    (list kanji yomi ret)))

(defun canna-hinshi-KEIYOUDOUSHI (kanji yomi)
  (let (ret1 ret2 ret)
    (if (not (and (> (length kanji) 1) (> (length yomi) 1)
		  (string-match "だ$" yomi) (string-match "だ$" kanji)))
	(egg-error "読みと候補を 終止形で入力してください。例) 静かだ"))
    (setq kanji (substring kanji 0 (1- (length kanji))))
    (setq yomi (substring yomi 0 (1- (length yomi))))
    (setq ret
	  (if (y-or-n-p "さらに細かい品詞分けのための質問をしても良いですか? ")
	      (progn
		(setq ret1 (y-or-n-p
			    (concat "「" kanji "する」は正しいですか。")))
		(setq ret2 (y-or-n-p
			    (concat "「" kanji "がある」は正しいですか。")))
		(if ret1 (if ret2 "#T10" "#T13") (if ret2 "#T15" "#T18")))
	    "#T05"))
    (list kanji yomi ret)))

(defun canna-hinshi-FUKUSHI (kanji yomi)
  (let (ret1 ret2)
    (if (y-or-n-p "さらに細かい品詞分けのための質問をしても良いですか? ")
	(progn
	  (setq ret1 (y-or-n-p (concat "「" kanji "する」は正しいですか。")))
	  (setq ret2 (y-or-n-p (concat "「" kanji "と」は正しいですか。")))
	  (if ret1 (if ret2 "#F04" "#F12") (if ret2 "#F06" "#F14")))
      "#F14")))

(defun canna-hinshi-select (kanji yomi)
  (let ((key (menudiag-select (list 'menu
				    (egg-get-message 'canna-register-2)
				    canna-hinshi-menu))))
    (cond ((symbolp key) (funcall
			  (intern (concat "canna-hinshi-" (symbol-name key)))
			  kanji yomi))
	  ((stringp key) (cdr (assoc key canna-hinshi-alist))))))

(defun canna-word-registration (backend kanji yomi)
  "Register a word KANJI with a pronunciation YOMI."
  (if (or (null (eq (egg-get-language 0 kanji)
		    (canna-get-converted-language backend)))
	  (next-single-property-change 0 'egg-lang kanji)
	  (null (eq (egg-get-language 0 yomi)
		    (canna-get-source-language backend)))
	  (next-single-property-change 0 'egg-lang yomi))
      (egg-error "word registration: invalid character")
    (let* ((env (canna-get-environment backend))
	   (dic (canna-dictionary-select env))
	   (hinshi-id (canna-hinshi-select kanji yomi))
	   result)
      (if (listp hinshi-id)
	  (progn (setq kanji     (car hinshi-id))
		 (setq yomi      (nth 1 hinshi-id))
		 (setq hinshi-id (nth 2 hinshi-id))))
      (setq result (cannarpc-add-word env dic yomi kanji hinshi-id))
      (if (>= result 0)
	  (progn
	    (cannarpc-save-dictionary env dic)
	    (list (canna-hinshi-name hinshi-id) dic))
	(egg-error (cannarpc-get-error-message (- result)))))))

;;; word delete registration

(defun canna-word-delete-regist (backend yomi)
  "Delete a word KANJI from dictionary."
  (if (= (length yomi) 0)
      (egg-error "Canna word delete registration: null string"))
  (let* ((env (canna-get-environment backend))
	 (dic (canna-dictionary-select env))
	 proc context envd bunsetsu bunsetsu-pos z zpos kouho-list hinshi i
	 kanji lex result)
    (setq proc (cannaenv-get-proc env))
    (setq context (cannarpc-create-context proc))
    (setq envd (cannaenv-create proc context
				'canna-backend-Japanese-tmp-delete-regist
				1 t))
    (canna-set-dictionary envd (vector dic t))
    (canna-set-dictionary envd (vector "fuzokugo" nil))
    (setq bunsetsu (car (cannarpc-begin-conversion envd yomi)))
    (setq bunsetsu-pos (canna-bunsetsu-get-bunsetsu-pos bunsetsu))
    (setq z (cannarpc-get-bunsetsu-candidates envd bunsetsu-pos yomi))
    (canna-bunsetsu-set-zenkouho bunsetsu z)
    (canna-bunsetsu-set-zenkouho-pos bunsetsu 0)
    (setq kouho-list
	  (canna-bunsetsu-set-zenkouho-converted
	   bunsetsu
	   (mapcar 'canna-bunsetsu-get-converted z)))
    (setq yomi  (car (last kouho-list)))
    (setq kouho-list (cdr (reverse kouho-list)))
    (setq kouho-list (reverse kouho-list))
    (setq i 0)
    (setq kouho-list (mapcar '(lambda (k)
				(prog1
				    (cons k i)
				  (setq i (1+ i))))
			     kouho-list))
    (let ((hiragana (assoc yomi kouho-list))
	  hinshi)
      (if hiragana
	  (setq hinshi (cannarpc-get-hinshi envd bunsetsu-pos (cdr hiragana))))
      (if (stringp hinshi)
	  (if (equal "#T35" hinshi)
	      (setq kouho-list (delete hiragana kouho-list)))
	(setq kouho-list (delete hiragana kouho-list))))
    (cond
     ((null kouho-list)
      (cannarpc-close-context envd)
      (egg-error "登録されていません。"))
     ((eq 1 (length kouho-list))
      (setq zpos 0)
      (setq kanji (car (car kouho-list))))
     (t
      (setq kanji (menudiag-select (list 'menu "削除:" kouho-list) nil nil t))
      (setq zpos (cdr (car kanji)))
      (setq kanji (car (car kanji)))))
    (setq hinshi (cannarpc-get-hinshi envd bunsetsu-pos zpos))
    (setq lex (cannarpc-get-lex envd bunsetsu-pos zpos))
    (cannarpc-cancel-conversion envd)
    (if (string-match "#[^#]+" hinshi)
	(setq hinshi (substring hinshi 0 (match-end 0)))
      (egg-error "品詞情報が取得できません。"))
    (setq kanji (substring kanji 0 (nth 1 (car lex))))
    (setq yomi (substring yomi 0 (car (car lex))))
    (if (y-or-n-p (concat "『" kanji "』(" yomi ": "
			  (canna-hinshi-name hinshi) ")を "
			  dic " から削除しますか? "))
	(setq result
	      (cannarpc-delete-word envd dic yomi kanji hinshi))
      (setq result -1))
    (if (>= result 0)
	(progn
	  (cannarpc-save-dictionary envd dic)
	  (cannarpc-close-context envd)
	  (list kanji yomi (canna-hinshi-name hinshi) dic))
      (cannarpc-close-context envd)
      (egg-error "削除されませんでした。"))
    ))

;;; setup
(load "egg/cannarpc")
(run-hooks 'canna-load-hook)

;;;###autoload
(defun egg-activate-canna (&rest arg)
  "Activate CANNA backend of Tamago 4."
  (apply 'egg-mode (append arg canna-backend-alist)))

;;; egg/canna.el ends here.

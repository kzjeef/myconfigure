;;; egg/wnn.el --- WNN Support (high level interface) in Egg
;;;                Input Method Architecture

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

(require 'egg)
(require 'egg-edep)

(defgroup wnn nil
  "Wnn interface for Tamago 4."
  :group 'egg)

(defcustom wnn-auto-save-dictionaries 0
  "*Save dictionaries automatically after N-th end conversion, if positive"
  :group 'wnn :type 'integer)

(defcustom wnn-use-v3-eggrc nil
  "*Enable old style eggrc, if non-NIL"
  :group 'wnn :type 'boolean)

(defcustom wnn-use-bixing (not wnn-use-v3-eggrc)
  "*Enable bixing (stroke) input-method, if non-NIL"
  :group 'wnn :type 'boolean)

(defcustom wnn-force-set-environment nil
  "*Regardless the existence of the Wnn environment in the server,
reset the environment, if non-NIL.  Otherwise, reset the environment
only when it is created."
  :group 'wnn :type 'boolean)

(defcustom wnn-one-level-conversion nil
  "*Don't use major clause (dai bunsetu/da wenjie/dae munjeol), if non-NIL."
  :group 'wnn :type 'boolean)

(defcustom wnn-usr-dic-dir (concat "usr/" (user-login-name))
  "*Directory of user dictionary for Wnn."
  :group 'wnn
  :type 'string)

(defcustom wnn-user-name (or (getenv "WNNUSER") (user-login-name))
  "User name at Wnn server.  Default value is login name or
WNNUSER evironment variable, if it is defined."
  :group 'wnn :type 'string)

(defcustom wnn-uniq-level 'wnn-uniq
  "Uniq level for candidate selection.
wnn-no-uniq:    Use all candidates.
wnn-uniq-entry: Use only one among same dictionary entry candidates.
wnn-uniq:       Use only one among same hinshi candidates. (default)
wnn-uniq-kanji: Use only one among same kanji candidates."
  :group 'wnn :type '(choice (const wnn-no-uniq)
			     (const wnn-uniq-entry)
			     (const wnn-uniq)
			     (const wnn-uniq-kanji)))

(defcustom wnn-jserver nil
  "jserver hostname list.  Use N-th port, if hostname is followed
by ':' and digit N."
  :group 'wnn :type '(repeat string))
(defcustom wnn-cserver nil
  "cserver hostname list.  Use N-th port, if hostname is followed
by ':' and digit N."
  :group 'wnn :type '(repeat string))
(defcustom wnn-tserver nil
  "tserver hostname list.  Use N-th port, if hostname is followed
by ':' and digit N."
  :group 'wnn :type '(repeat string))
(defcustom wnn-kserver nil
  "kserver hostname list.  Use N-th port, if hostname is followed
 by ':' and digit N."
  :group 'wnn :type '(repeat string))

(defcustom wnn-jport 22273 "jserver port number" :group 'wnn :type 'integer)
(defcustom wnn-cport 22289 "cserver port number" :group 'wnn :type 'integer)
(defcustom wnn-tport 22321 "tserver port number" :group 'wnn :type 'integer)
(defcustom wnn-kport 22305 "kserver port number" :group 'wnn :type 'integer)

(defmacro wnn-backend-plist ()
  ''(egg-initialize-backend        wnn-init
     egg-start-conversion          wnn-start-conversion
     egg-get-bunsetsu-source       wnn-get-bunsetsu-source
     egg-get-bunsetsu-converted    wnn-get-bunsetsu-converted
     egg-get-source-language       wnn-get-source-language
     egg-get-converted-language    wnn-get-converted-language
     egg-major-bunsetsu-continue-p wnn-major-bunsetsu-continue-p
     egg-list-candidates           wnn-list-candidates
     egg-decide-candidate          wnn-decide-candidate
     egg-special-candidate         wnn-special-candidate
     egg-change-bunsetsu-length    wnn-change-bunsetsu-length
     egg-bunsetsu-combinable-p     wnn-bunsetsu-combinable-p
     egg-end-conversion            wnn-end-conversion
     egg-word-inspection           wnn-word-inspection
     egg-word-registration         wnn-word-registration))

(defun wnn-backend-func-name (name lang &optional env)
  (intern (concat name "-" (symbol-name lang)
		  (and env "-") (and env (symbol-name env)))))

(defun wnn-make-backend (lang env &optional source-lang converted-lang)
  (let ((finalize (wnn-backend-func-name "wnn-finalize-backend" lang))
	(backend (wnn-backend-func-name "wnn-backend" lang env)))
    (if (null (fboundp finalize))
	(progn
	  (fset finalize `(lambda () (wnn-finalize-backend ',lang)))
	  (egg-set-finalize-backend (list finalize))))
    (if (null (get backend 'egg-start-conversion))
	(setplist backend (apply 'list
				 'language lang
				 'source-language (or source-lang lang)
				 'converted-language (or converted-lang lang)
				 (wnn-backend-plist))))
    backend))

(defun wnn-define-backend (lang env-name-list)
  (mapcar (lambda (env)
	    (if (consp env)
		(wnn-define-backend lang env)
	      (wnn-make-backend lang env)))
	  env-name-list))

(wnn-make-backend 'Chinese-GB 'Q  'QianMa 'Chinese-GB)
(wnn-make-backend 'Chinese-GB 'QR 'Chinese-GB 'QianMa)
(wnn-make-backend 'Chinese-GB 'W  'WuBi 'Chinese-GB)
(wnn-make-backend 'Chinese-GB 'WR 'Chinese-GB 'WuBi)

(defconst wnn-backend-language-alist '((QianMa . Chinese-GB)
				       (WuBi . Chinese-GB)))

(defvar wnn-backend-alist nil)

(defun wnn-define-backend-alist (deflist)
  (setq wnn-backend-alist
	(mapcar (lambda (slot)
		  (let* ((lang (car slot))
			 (alt (cdr (assq lang wnn-backend-language-alist))))
		    (cons lang (wnn-define-backend (or alt lang) (cdr slot)))))
		deflist)))

(defcustom wnn-backend-define-list
  (if wnn-use-bixing
      '((Japanese    ((nil nil R))   ((R   nil R)))
	(Chinese-GB  ((PZ  PZ  PZR)) ((PZR PZ  PZR))
		     ((QR  Q   QR))  ((WR  W   WR)))
	(Chinese-CNS ((PZ  PZ  PZR)) ((PZR PZ  PZR)))
	(Korean      ((nil nil R))   ((R   nil R)))
	(QianMa      ((Q Q QR)))
	(WuBi        ((W W WR))))
    '((Japanese    ((nil nil R))   ((R   nil R)))
      (Chinese-GB  ((PZ  PZ  PZR)) ((PZR PZ  PZR)))
      (Chinese-CNS ((PZ  PZ  PZR)) ((PZR PZ  PZR)))
      (Korean      ((nil nil R))   ((R   nil R)))))
  "Alist of language and lists of the Wnn backend suffixes."
  :group 'wnn
  :set (lambda (sym value)
	 (set-default sym value)
	 (wnn-define-backend-alist value))
  :type '(repeat
	  (cons
	   :tag "Language - Backend"
	   (choice :tag "Language"
		   (const Japanese)
		   (const Chinese-GB)
		   (const Chinese-CNS)
		   (const Korean)
		   (const QianMa)
		   (const WuBi)
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

(eval-when-compile
  (defmacro WNN-const (c)
    (cond ((eq c 'BUN_SENTOU)    -1)
	  ((eq c 'NO_EXIST)       1)
	  ((eq c 'NO_MATCH)      10)
	  ((eq c 'IMA_OFF)       -4)
	  ((eq c 'IMA_ON)        -3)
	  ((eq c 'CONNECT)        1)
	  ((eq c 'CONNECT_BK)     1)
	  ((eq c 'HIRAGANA)      -1)
	  ((eq c 'KATAKANA)     -11)
	  ((eq c 'IKEIJI_ENTRY) -50)
	  ((eq c 'LEARNING_LEN)   3)
	  ((eq c 'MUHENKAN_DIC)  -3)
	  ((eq c 'HINDO_NOP)     -2)
	  ((eq c 'HINDO_INC)     -3)
	  ((eq c 'DIC_RW)         0)
	  ((eq c 'DIC_RDONLY)     1)
	  ((eq c 'DIC_GROUP)      3)
	  ((eq c 'DIC_MERGE)      4)
	  ((eq c 'NOTRANS_LEARN)  1)
	  ((eq c 'BMODIFY_LEARN)  2)
	  ((eq c 'DIC_NO_TEMPS)   ?\x3f))))

;; Retern value of system-name may differ from hostname.
(defconst wnn-system-name
  (or (with-temp-buffer
	(condition-case nil
	    (call-process "hostname"
			  nil `(,(current-buffer) nil) "hostname")
	  (error))
	(goto-char (point-min))
	(if (re-search-forward "[\0- ]" nil 0)
	    (goto-char (1- (point))))
	(if (> (point) 1)
	    (buffer-substring 1 (point))))
      (system-name)))

(egg-add-message
 '((nil
    (wnn-connect-error  "cannot connect to the server")
    (wnn-fail-make-env  "cannot make the Wnn environment")
    (wnn-dict-saving    "saving %s's frequency/dictionary information")
    (wnn-dict-saved     "finish to save %s's frequency/dictionary information")
    (wnn-dir-missing    "directory %s missing. Create it? ")
    (wnn-dir-failed     "failed to create directory %s")
    (wnn-dir-created    "directory %s created")
    (wnn-dict-missing-1 "dictionary file %s is missing: %s")
    (wnn-dict-missing-2 "dictionary file %s is missing. Create it? ")
    (wnn-dict-created   "dictionary file %s is created")
    (wnn-freq-missing-1 "frequency file %s is missing: %s")
    (wnn-freq-missing-2 "frequency file %s is missing. Create it? ")
    (wnn-freq-created   "frequency file %s is created")
    (wnn-no-match       "unmatch dictionary and freq. file %s. Re-create it? ")
    (wnn-re-create-freq "frequency file %s is re-created")
    (wnn-pseud-bunsetsu "pseud clause")
    (wnn-register-1     "dictionary name:")
    (wnn-register-2     "clause class name")
    (wnn-no-writable-d  "no writable dictionary"))
   (Japanese
    (wnn-connect-error  "$B%5!<%P$H@\B3$G$-$^$;$s$G$7$?(B")
    (wnn-fail-make-env  "$B4D6-$r:n$k$3$H$O$G$-$^$;$s$G$7$?(B")
    (wnn-dict-saving    "%s $B$NIQEY>pJs!&<-=q>pJs$rB`Hr$7$F$$$^$9(B")
    (wnn-dict-saved     "%s $B$NIQEY>pJs!&<-=q>pJs$rB`Hr$7$^$7$?(B")
    (wnn-dir-missing    "$B%G%#%l%/%H%j(B %s $B$,$"$j$^$;$s!#:n$j$^$9$+(B? ")
    (wnn-dir-failed     "$B%G%#%l%/%H%j(B %s $B$N:n@.$K<:GT$7$^$7$?(B")
    (wnn-dir-created    "$B%G%#%l%/%H%j(B %s $B$r:n$j$^$7$?(B")
    (wnn-dict-missing-1 "$B<-=q%U%!%$%k(B %s $B$,$"$j$^$;$s(B: %s")
    (wnn-dict-missing-2 "$B<-=q%U%!%$%k(B %s $B$,$"$j$^$;$s!#:n$j$^$9$+(B? ")
    (wnn-dict-created   "$B<-=q%U%!%$%k(B %s $B$r:n$j$^$7$?(B")
    (wnn-freq-missing-1 "$BIQEY%U%!%$%k(B %s $B$,$"$j$^$;$s(B: %s")
    (wnn-freq-missing-2 "$BIQEY%U%!%$%k(B %s $B$,$"$j$^$;$s!#:n$j$^$9$+(B? ")
    (wnn-freq-created   "$BIQEY%U%!%$%k(B %s $B$r:n$j$^$7$?(B")
    (wnn-no-match       "$B<-=q$HIQEY(B %s $B$N@09g@-$,$"$j$^$;$s!#:n$jD>$7$^$9$+(B? ")
    (wnn-re-create-freq "$BIQEY%U%!%$%k(B %s $B$r:n$jD>$7$^$7$?(B")
    (wnn-pseud-bunsetsu "$B5?;wJ8@a(B")
    (wnn-register-1     "$BEPO?<-=qL>(B:")
    (wnn-register-2     "$BIJ;lL>(B")
    (wnn-no-writable-d  "$BEPO?2DG=$J<-=q$,$"$j$^$;$s(B"))
   (Chinese-GB
    (wnn-connect-error  "$A2;D\:M(Bserver$AA,=S(B")
    (wnn-fail-make-env  "$A;7>32;D\44=((B")
    (wnn-dict-saving    "%s $A5DF56HND<~:M4G5dPEO"U}TZMK3v(B")
    (wnn-dict-saved     "%s $A5DF56HND<~:M4G5dPEO"RQ>-MK3vAK(B")
    (wnn-dir-missing    "$AD?B<(B %s $AC;SP!#R*=(A"Bp(B? ")
    (wnn-dir-failed     "$AD?B<(B %s $A=(A"J'0\AK(B")
    (wnn-dir-created    "$AD?B<(B %s $A=(A"AK(B")
    (wnn-dict-missing-1 "$AWV5dND<~(B %s $AC;SP(B: %s")
    (wnn-dict-missing-2 "$AWV5dND<~(B %s $AC;SP!#R*=(A"Bp(B? ")
    (wnn-dict-created   "$AWV5dND<~(B %s $A=(A"AK(B")
    (wnn-freq-missing-1 "$AF56HND<~(B %s $AC;SP(B: %s")
    (wnn-freq-missing-2 "$AF56HND<~(B %s $AC;SP!#R*=(A"Bp(B? ")
    (wnn-freq-created   "$AF56HND<~(B %s $A=(A"AK(B")
    (wnn-no-match       "$AWV5d:MF56H(B %s $A5DU{:OPTC;SP!#R*TY=(A"Bp(B? ")
    (wnn-re-create-freq "$AF56HND<~(B %s $ATY=(A"AK(B")
    (wnn-pseud-bunsetsu "$ARIKFND=Z(B")
    (wnn-register-1     "$A5GB<4G5dC{(B:")
    (wnn-register-2     "$A4JPTC{(B")
    (wnn-no-writable-d  "$AC;SP?ID\5GB<5D4G5d(B"))
   (Chinese-CNS
    (wnn-connect-error  "$(GDbWdLO(Bserver$(G]YZY(B")
    (wnn-fail-make-env  "$(Gt?h:DbWd^6Pz(B")
    (wnn-dict-saving    "%s $(GN{s"PyEFG5LOy0L(OjUIF_GcXMEx(B")
    (wnn-dict-saved     "%s $(GN{s"PyEFG5LOy0L(OjUIDXenXMExD'(B")
    (wnn-dir-missing    "$(GFxrg(B %s $(GJtH4!$SQPzG!cC(B? ")
    (wnn-dir-failed     "$(GFxrg(B %s $(GPzG!FBZuD'(B")
    (wnn-dir-created    "$(GFxrg(B %s $(GPzG!D'(B")
    (wnn-dict-missing-1 "$(GGsL(EFG5(B %s $(GJtH4(B: %s")
    (wnn-dict-missing-2 "$(GGsL(EFG5(B %s $(GJtH4!$SQPzG!cC(B? ")
    (wnn-dict-created   "$(GGsL(EFG5(B %s $(GPzG!D'(B")
    (wnn-freq-missing-1 "$(Gs"PyEFG5(B %s $(GJtH4(B: %s")
    (wnn-freq-missing-2 "$(Gs"PyEFG5(B %s $(GJtH4!$SQPzG!cC(B? ")
    (wnn-freq-created   "$(Gs"PyEFG5(B %s $(GPzG!D'(B")
    (wnn-no-match       "$(GGsL(LOs"Py(B %s $(GN{plLOMLJtH4!$SQGBPzG!cC(B? ")
    (wnn-re-create-freq "$(Gs"PyEFG5(B %s $(GGBPzG!D'(B")
    (wnn-pseud-bunsetsu "$(GijH}EFeg(B")
    (wnn-register-1     "$(G`trgy0L(GX(B:")
    (wnn-register-2     "$(Gb$MLGX(B")
    (wnn-no-writable-d  "$(GJtH4F+Wd`trgN{GsL((B"))
   (Korean
    (wnn-connect-error  "$(C<-9v(B(Server) $(C?M(B $(CA"CKGR(B $(C<v(B $(C>x@>4O4Y(B")
    (wnn-fail-make-env  "$(CH/0f@;(B $(C@[<:GR(B $(C<v(B $(C>x@>4O4Y(B")
    (wnn-dict-saving    "%s $(C@G(B $(C:s55A$:8?M(B $(C;g@|A$:88&(B $(C<<@L:jGO0m(B $(C@V=@4O4Y(B")
    (wnn-dict-saved     "%s $(C@G(B $(C:s55A$:8?M(B $(C;g@|A$:88&(B $(C<<@L:jG_=@4O4Y(B")
    (wnn-dir-missing    "$(C5p7:Ed8.(B %s $(C@L(B $(C>x@>4O4Y#.@[<:GO0Z=@4O1n(B? ")
    (wnn-dir-failed     "$(C5p7:Ed8.(B %s $(C@;(B $(C@[<:GR(B $(C<v(B $(C>x@>4O4Y(B")
    (wnn-dir-created    "$(C5p7:Ed8.(B %s $(C@;(B $(C@[<:G_=@4O4Y(B")
    (wnn-dict-missing-1 "$(C;g@|H-@O(B %s $(C@L(B $(C>x@>4O4Y(B: %s")
    (wnn-dict-missing-2 "$(C;g@|H-@O(B %s $(C@L(B $(C>x@>4O4Y#.4Y=C(B $(C@[<:GO0Z=@4O1n(B? ")
    (wnn-dict-created   "$(C;g@|H-@O(B %s $(C@;(B $(C@[<:G_=@4O4Y(B")
    (wnn-freq-missing-1 "$(C:s55H-@O(B %s $(C@L(B $(C>x@>4O4Y(B: %s")
    (wnn-freq-missing-2 "$(C:s55H-@O(B %s $(C@L(B $(C>x@>4O4Y#.4Y=C(B $(C@[<:GO0Z=@4O1n(B? ")
    (wnn-freq-created   "$(C:s55H-@O(B %s $(C@;(B $(C@[<:G_=@4O4Y(B")
    (wnn-no-match       "$(C;g@|0z(B $(C:s55(B %s $(C0!(B $(C8BAv(B $(C>J=@4O4Y#.4Y=C(B $(C@[<:GO0Z=@4O1n(B? ")
    (wnn-re-create-freq "$(C:s55H-@O(B %s $(C@;(B $(C4Y=C(B $(C@[<:G_=@4O4Y(B")
    (wnn-pseud-bunsetsu "$(C@G;g9.@}(B")
    (wnn-register-1     "$(C5n7O;g@|8m(B:")
    (wnn-register-2     "$(CG0;g8m(B")
    (wnn-no-writable-d  "$(C5n7O(B $(C0!4IGQ(B $(CAvA$@L(B $(C>F4U4O4Y(B"))))

;; <env> ::= [ <proc> <env-id> <lang> <server-type> <wnn-version>
;;             <backend> <tankan> <env-name> <auto-learn> <hinshi> ]

(defun wnnenv-create (proc env-id &optional server-type wnn-version
		      backend tankan name)
  (if name
      (set (setq name (make-symbol name)) (make-vector 5 nil)))
  (vector proc env-id server-type wnn-version backend tankan name
	  (make-vector 2 (WNN-const DIC_RDONLY))))

(defsubst wnnenv-get-proc (env)        (aref env 0))
(defsubst wnnenv-get-env-id (env)      (aref env 1))
(defsubst wnnenv-get-server-type (env) (aref env 2))
(defsubst wnnenv-get-wnn-version (env) (aref env 3))
(defsubst wnnenv-get-backend (env)     (aref env 4))
(defsubst wnnenv-get-tankan (env)      (aref env 5))

(defun wnnenv-get-client-file (env name)
  (let ((hash (intern-soft name (symbol-value (aref env 6)))))
    (and hash (symbol-value hash))))

(defun wnnenv-set-client-file (env name)
  (set (intern (concat wnn-system-name "!" name) (symbol-value (aref env 6)))
       name))

(defsubst wnnenv-get-hinshi (env h)    (or (get (aref env 6) h) -1))
(defsubst wnnenv-set-hinshi (env h v)  (put (aref env 6) h v))

(defsubst wnnenv-get-auto-learn (env)  (aref env 7))
(defsubst wnnenv-get-notrans (env)     (aref (wnnenv-get-auto-learn env) 0))
(defsubst wnnenv-get-bmodify (env)     (aref (wnnenv-get-auto-learn env) 1))
(defsubst wnnenv-set-notrans (env v)   (aset (wnnenv-get-auto-learn env) 0 v))
(defsubst wnnenv-set-bmodify (env v)   (aset (wnnenv-get-auto-learn env) 1 v))

(defsubst wnnenv-is-wnn6 (env)         (eq (wnnenv-get-wnn-version env) 'wnn6))
(defsubst wnnenv-is-wnn7 (env)         (eq (wnnenv-get-wnn-version env) 'wnn7))
(defsubst wnnenv-is-wnn8 (env)         (eq (wnnenv-get-wnn-version env) 'wnn8))

(defvar wnn-environments nil
  "Environment for Wnn conversion server")

;; <wnn-bunsetsu> ::= [ <env>
;;                      <jirilen> <dic-no> <entry> <freq> <right-now> <hinshi>
;;                      <status> <status-backward> <kangovect> <evaluation>
;;                      <converted> <yomi> <fuzokugo>
;;                      <dai-evaluation> <dai-continue> <change-top>
;;                      <zenkouho-info> <freq-down> <fi-rel> <context> ]
;;
;; <zenkouho-info> ::= [ <pos> <list> <converted> <dai> <prev-b> <nxet-b> ]

(defsubst wnn-bunsetsu-create (env jirilen dic-no entry freq right-now hinshi
			       status status-backward kangovect evaluation)
  (egg-bunsetsu-create (wnnenv-get-backend env)
		       (vector env jirilen dic-no entry freq right-now hinshi
			       status status-backward kangovect evaluation
			       nil nil nil nil nil nil nil nil nil nil)))

(defsubst wnn-bunsetsu-get-env (b)
  (aref (egg-bunsetsu-get-info b) 0))
(defsubst wnn-bunsetsu-get-jirilen (b)
  (aref (egg-bunsetsu-get-info b) 1))
(defsubst wnn-bunsetsu-get-dic-no (b)
  (aref (egg-bunsetsu-get-info b) 2))
(defsubst wnn-bunsetsu-set-dic-no (b dic)
  (aset (egg-bunsetsu-get-info b) 2 dic))
(defsubst wnn-bunsetsu-get-entry (b)
  (aref (egg-bunsetsu-get-info b) 3))
(defsubst wnn-bunsetsu-set-entry (b ent)
  (aset (egg-bunsetsu-get-info b) 3 ent))
(defsubst wnn-bunsetsu-get-freq (b)
  (aref (egg-bunsetsu-get-info b) 4))
(defsubst wnn-bunsetsu-get-right-now (b)
  (aref (egg-bunsetsu-get-info b) 5))
(defsubst wnn-bunsetsu-get-hinshi (b)
  (aref (egg-bunsetsu-get-info b) 6))
(defsubst wnn-bunsetsu-get-status (b)
  (aref (egg-bunsetsu-get-info b) 7))
(defsubst wnn-bunsetsu-get-status-backward (b)
  (aref (egg-bunsetsu-get-info b) 8))
(defsubst wnn-bunsetsu-get-kangovect (b)
  (aref (egg-bunsetsu-get-info b) 9))
(defsubst wnn-bunsetsu-get-evaluation (b)
  (aref (egg-bunsetsu-get-info b) 10))

(defsubst wnn-bunsetsu-get-converted (b)
  (aref (egg-bunsetsu-get-info b) 11))
(defsubst wnn-bunsetsu-set-converted (b cvt)
  (aset (egg-bunsetsu-get-info b) 11 cvt))

(defsubst wnn-bunsetsu-get-yomi (b)
  (aref (egg-bunsetsu-get-info b) 12))
(defsubst wnn-bunsetsu-set-yomi (b yomi)
  (aset (egg-bunsetsu-get-info b) 12 yomi))

(defsubst wnn-bunsetsu-get-fuzokugo (b)
  (aref (egg-bunsetsu-get-info b) 13))
(defsubst wnn-bunsetsu-set-fuzokugo (b fuzokugo)
  (aset (egg-bunsetsu-get-info b) 13 fuzokugo))

(defsubst wnn-bunsetsu-get-dai-evaluation (b)
  (aref (egg-bunsetsu-get-info b) 14))
(defsubst wnn-bunsetsu-set-dai-evaluation (b de)
  (aset (egg-bunsetsu-get-info b) 14 de))

(defsubst wnn-bunsetsu-get-dai-continue (b)
  (aref (egg-bunsetsu-get-info b) 15))
(defsubst wnn-bunsetsu-set-dai-continue (b dc)
  (aset (egg-bunsetsu-get-info b) 15 dc))

(defsubst wnn-bunsetsu-get-change-top (b)
  (aref (egg-bunsetsu-get-info b) 16))
(defsubst wnn-bunsetsu-set-change-top (b top)
  (aset (egg-bunsetsu-get-info b) 16 top))

(defsubst wnn-bunsetsu-get-zenkouho (b)
  (aref (egg-bunsetsu-get-info b) 17))
(defsubst wnn-bunsetsu-set-zenkouho (b z)
  (aset (egg-bunsetsu-get-info b) 17 z))

(defsubst wnn-bunsetsu-get-freq-down (b)
  (aref (egg-bunsetsu-get-info b) 18))
(defsubst wnn-bunsetsu-set-freq-down (b fd)
  (aset (egg-bunsetsu-get-info b) 18 fd))

(defsubst wnn-bunsetsu-get-fi-rel (b)
  (aref (egg-bunsetsu-get-info b) 19))
(defsubst wnn-bunsetsu-set-fi-rel (b fr)
  (aset (egg-bunsetsu-get-info b) 19 fr))

(defsubst wnn-bunsetsu-get-context (b)
  (aref (egg-bunsetsu-get-info b) 20))
(defsubst wnn-bunsetsu-set-context (b c)
  (aset (egg-bunsetsu-get-info b) 20 c))

(defsubst wnn-zenkouho-create (pos list converted dai prev-b nxet-b)
  (vector pos list converted dai prev-b nxet-b))

(defsubst wnn-bunsetsu-get-zenkouho-pos (b)
  (aref (wnn-bunsetsu-get-zenkouho b) 0))
(defsubst wnn-bunsetsu-set-zenkouho-pos (b p)
  (aset (wnn-bunsetsu-get-zenkouho b) 0 p))

(defsubst wnn-bunsetsu-get-zenkouho-list (b)
  (aref (wnn-bunsetsu-get-zenkouho b) 1))
(defsubst wnn-bunsetsu-get-zenkouho-converted (b)
  (aref (wnn-bunsetsu-get-zenkouho b) 2))
(defsubst wnn-bunsetsu-get-zenkouho-dai (b)
  (aref (wnn-bunsetsu-get-zenkouho b) 3))
(defsubst wnn-bunsetsu-get-zenkouho-prev-b (b)
  (aref (wnn-bunsetsu-get-zenkouho b) 4))
(defsubst wnn-bunsetsu-get-zenkouho-next-b (b)
  (aref (wnn-bunsetsu-get-zenkouho b) 5))

(defsubst wnn-bunsetsu-connect-prev (bunsetsu)
  (= (wnn-bunsetsu-get-status bunsetsu) (WNN-const CONNECT)))
(defsubst wnn-bunsetsu-connect-next (bunsetsu)
  (= (wnn-bunsetsu-get-status-backward bunsetsu) (WNN-const CONNECT_BK)))

(defsubst wnn-context-create (dic-no entry jirilen hinshi fuzokugo
			      converted freq right-now)
  (vector dic-no entry jirilen hinshi fuzokugo
	  converted freq right-now
	  (egg-chars-in-period converted 0 (length converted))))

(defsubst wnn-context-dic-no (context)          (aref context 0))
(defsubst wnn-context-entry (context)           (aref context 1))
(defsubst wnn-context-jirilen (context)         (aref context 2))
(defsubst wnn-context-hinshi (context)          (aref context 3))
(defsubst wnn-context-fuzokugo (context)        (aref context 4))
(defsubst wnn-context-converted (context)       (aref context 5))
(defsubst wnn-context-right-now (context)       (aref context 6))
(defsubst wnn-context-set-right-now (context r) (aset context 6 r))
(defsubst wnn-context-freq (context)            (aref context 7))
(defsubst wnn-context-set-freq (context f)      (aset context 7 f))
(defsubst wnn-context-length (context)          (aref context 8))

(defun wnn-null-context ()
  (list (wnn-context-create -2 0 0 0 "" "" 0 0)
	(wnn-context-create -2 0 0 0 "" "" 0 0)))

(defun wnn-major-bunsetsu-set-context (bunsetsu-list context)
  (while bunsetsu-list
    (wnn-bunsetsu-set-context (car bunsetsu-list) context)
    (setq bunsetsu-list (cdr bunsetsu-list))))

(defsubst wnn-bunsetsu-equal (bunsetsu-1 bunsetsu-2)
  (and (= (wnn-bunsetsu-get-dic-no bunsetsu-1)
	  (wnn-bunsetsu-get-dic-no bunsetsu-2))
       (= (wnn-bunsetsu-get-entry bunsetsu-1)
	  (wnn-bunsetsu-get-entry bunsetsu-2))
       (= (wnn-bunsetsu-get-kangovect bunsetsu-1)
	  (wnn-bunsetsu-get-kangovect bunsetsu-2))
       (equal (wnn-bunsetsu-get-converted bunsetsu-1)
	      (wnn-bunsetsu-get-converted bunsetsu-2))
       (equal (wnn-bunsetsu-get-fuzokugo bunsetsu-1)
	      (wnn-bunsetsu-get-fuzokugo bunsetsu-2))))

(defun wnn-bunsetsu-list-equal (b1 b2)
  (while (and b1 b2 (wnn-bunsetsu-equal (car b1) (car b2)))
    (setq b1 (cdr b1)
	  b2 (cdr b2)))
  (and (null b1) (null b2)))

(defun wnn-bunsetsu-list-copy (bunsetsu)
  (mapcar (lambda (b)
	    (egg-bunsetsu-create (egg-bunsetsu-get-backend b)
				 (copy-sequence (egg-bunsetsu-get-info b))))
	  bunsetsu))

(defvar wnn-server-info-list
  ;; language    server  port      hostname    proc   coding-system
  '((Japanese    jserver wnn-jport wnn-jserver "Wnn"  (fixed-euc-jp    fixed-euc-jp))
    (Chinese-GB  cserver wnn-cport wnn-cserver "cWnn" (fixed-euc-py-cn fixed-euc-zy-cn))
    (Chinese-CNS tserver wnn-tport wnn-tserver "tWnn" (fixed-euc-py-tw fixed-euc-zy-tw))
    (Korean      kserver wnn-kport wnn-kserver "kWnn" (fixed-euc-kr    fixed-euc-kr))))

(defsubst wnn-server-get-info (lang)
  (or (assq lang wnn-server-info-list)
      (egg-error "unsupported language: %s" lang)))

(defsubst wnn-server-language (info)
  (car info))
(defsubst wnn-server-type (info)
  (nth 1 info))
(defsubst wnn-server-port (info)
  (symbol-value (nth 2 info)))
(defsubst wnn-server-hostname (info)
  (symbol-value (nth 3 info)))
(defsubst wnn-server-proc-name (info)
  (nth 4 info))
(defsubst wnn-server-buffer-name (info)
  (concat " *" (wnn-server-proc-name info) "*"))
(defsubst wnn-server-coding-system (info)
  (nth 5 info))

(defconst wnn-accept-charset-alist
  '((Chinese-CNS ascii chinese-sisheng chinese-cns11643-1 chinese-cns11643-2)))

(defsubst wnn-backend-get-language (backend)
  (get backend 'language))

(defsubst wnn-backend-get-source-language (backend)
  (get backend 'source-language))

(defsubst wnn-backend-get-converted-language (backend)
  (get backend 'converted-language))

(defvar wnn-prev-context nil)

(defun wnn-start-conversion (backend yomi &optional context)
  "Convert YOMI string to kanji, and enter conversion mode.
Return the list of bunsetsu."
  (let ((accepts (cdr (assq (wnn-backend-get-source-language backend)
			    wnn-accept-charset-alist)))
	env hinshi fuzokugo result)
    (if accepts
	(let ((charsets (mapcar 'char-charset (string-to-list yomi))))
	  (while accepts
	     (setq charsets (delq (car accepts) charsets)
		   accepts (cdr accepts)))
	  (if charsets
	      (egg-error "cannot handle %s" (car charsets)))))
    (setq env (wnn-get-environment backend))
    (cond ((eq (car (car-safe  context)) backend)
	   (setq wnn-prev-context (car context)
		 context (cdr wnn-prev-context)
		 hinshi (wnn-context-hinshi (nth 1 context))
		 fuzokugo (wnn-context-fuzokugo (nth 1 context))))
	  ((listp context)
	   (setq wnn-prev-context (cons nil (wnn-null-context))
		 context (cdr wnn-prev-context)
		 hinshi (WNN-const BUN_SENTOU)
		 fuzokugo ""))
	  ((eq (car wnn-prev-context) backend)
	   (setq context (cdr wnn-prev-context)
		 hinshi (wnnenv-get-hinshi env 'noun)
		 fuzokugo ""))
	  (t
	   (setq context (wnn-null-context)
		 hinshi (wnnenv-get-hinshi env 'noun)
		 fuzokugo "")))
    (setq result (wnn-renbunsetsu-conversion env yomi hinshi fuzokugo nil
					     context))
    (if (numberp result)
	(egg-error "%s" (wnnrpc-get-error-message (- result))))
    result))

(defun wnn-get-source-language (bunsetsu)
  (wnn-backend-get-source-language (egg-bunsetsu-get-backend bunsetsu)))

(defun wnn-get-converted-language (bunsetsu)
  (wnn-backend-get-converted-language (egg-bunsetsu-get-backend bunsetsu)))

(defun wnn-get-bunsetsu-converted (bunsetsu)
  (concat (wnn-bunsetsu-get-converted bunsetsu)
	  (wnn-bunsetsu-get-fuzokugo  bunsetsu)))

(defun wnn-get-bunsetsu-source (bunsetsu)
  (concat (wnn-bunsetsu-get-yomi bunsetsu)
	  (wnn-bunsetsu-get-fuzokugo bunsetsu)))

(defun wnn-get-major-bunsetsu-converted (bunsetsu)
  (mapconcat 'wnn-get-bunsetsu-converted bunsetsu ""))

(defun wnn-get-major-bunsetsu-source (bunsetsu)
  (mapconcat 'wnn-get-bunsetsu-source bunsetsu ""))

(defun wnn-major-bunsetsu-continue-p (bunsetsu)
  (wnn-bunsetsu-get-dai-continue bunsetsu))

(defmacro wnn-uniq-hash-string (uniq-level)
  `(mapconcat
    (lambda (b)
      (concat ,@(cond ((eq uniq-level 'wnn-uniq)
		       '((number-to-string (wnn-bunsetsu-get-hinshi b))))
		      ((eq uniq-level 'wnn-uniq-entry)
		       '((number-to-string (wnn-bunsetsu-get-dic-no b))
			 "+"
			 (number-to-string (wnn-bunsetsu-get-entry b)))))
	      "\0"
	      (wnn-bunsetsu-get-converted b)
	      "\0"
	      (wnn-bunsetsu-get-fuzokugo b)))
    bunsetsu "\0"))

(defun wnn-uniq-hash (bunsetsu hash-table)
  (intern (cond ((eq wnn-uniq-level 'wnn-uniq)
		 (wnn-uniq-hash-string wnn-uniq))
		((eq wnn-uniq-level 'wnn-uniq-entry)
		 (wnn-uniq-hash-string wnn-uniq-entry))
		(t
		 (wnn-uniq-hash-string nil)))
	  hash-table))

(defun wnn-uniq-candidates (candidates)
  (if (eq wnn-uniq-level 'wnn-no-uniq)
      candidates
    (let ((hash-table (make-vector (length candidates) 0)))
      (delq nil (mapcar (lambda (b)
			  (let ((sym (wnn-uniq-hash b hash-table)))
			    (if (null (boundp sym))
				(set sym b))))
			candidates)))))

(defsubst wnn-uniq-bunsetsu-equal (bunsetsu-1 bunsetsu-2)
  (and (or (eq wnn-uniq-level 'wnn-uniq-kanji)
	   (and (eq wnn-uniq-level 'wnn-uniq)
		(= (wnn-bunsetsu-get-hinshi bunsetsu-1)
		   (wnn-bunsetsu-get-hinshi bunsetsu-2)))
	   (and (= (wnn-bunsetsu-get-dic-no bunsetsu-1)
		   (wnn-bunsetsu-get-dic-no bunsetsu-2))
		(= (wnn-bunsetsu-get-entry bunsetsu-1)
		   (wnn-bunsetsu-get-entry bunsetsu-2))
		(or (eq wnn-uniq-level 'wnn-uniq-entry)
		    (= (wnn-bunsetsu-get-kangovect bunsetsu-1)
		       (wnn-bunsetsu-get-kangovect bunsetsu-2)))))
       (equal (wnn-bunsetsu-get-converted bunsetsu-1)
	      (wnn-bunsetsu-get-converted bunsetsu-2))
       (equal (wnn-bunsetsu-get-fuzokugo bunsetsu-1)
	      (wnn-bunsetsu-get-fuzokugo bunsetsu-2))))

(defun wnn-uniq-bunsetsu-list-equal (b1 b2)
  (while (and b1 b2 (wnn-uniq-bunsetsu-equal (car b1) (car b2)))
    (setq b1 (cdr b1)
	  b2 (cdr b2)))
  (and (null b1) (null b2)))

(defun wnn-candidate-pos (bunsetsu candidates)
  (let ((n 0)
	pos)
    (while (and (null pos) candidates)
      (if (wnn-uniq-bunsetsu-list-equal (car candidates) bunsetsu)
	  (setq pos n)
	(setq candidates (cdr candidates)
	      n (1+ n))))
    (or pos -1)))

(defun wnn-get-candidates-converted (candidates)
  (mapcar 'wnn-get-major-bunsetsu-converted candidates))

(defun wnn-set-candidate-info (bunsetsu zenkouho)
  (wnn-bunsetsu-set-zenkouho (car bunsetsu) zenkouho)
  (mapcar (lambda (b) (wnn-bunsetsu-set-zenkouho b t)) (cdr bunsetsu)))

(defun wnn-list-candidates (bunsetsu prev-b next-b major)
  (let* ((head (car bunsetsu))
	 (backend (egg-bunsetsu-get-backend head))
	 (env (wnn-bunsetsu-get-env head))
	 (yomi (wnn-get-major-bunsetsu-source bunsetsu))
	 (continue (eq (wnn-bunsetsu-get-zenkouho head) t))
	 pos cand converted hinshi fuzokugo v)
    (if prev-b
	(setq prev-b (egg-get-bunsetsu-tail prev-b)
	      hinshi (wnn-bunsetsu-get-hinshi prev-b)
	      fuzokugo (wnn-bunsetsu-get-fuzokugo prev-b))
      (setq hinshi -1
	    fuzokugo ""))
    (if next-b
	(setq next-b (car next-b)
	      v (wnn-bunsetsu-get-kangovect next-b)))
    (if (vectorp (wnn-bunsetsu-get-zenkouho head))
	(setq pos (wnn-bunsetsu-get-zenkouho-pos head)
	      cand (wnn-bunsetsu-get-zenkouho-list head)))
    (if (and pos
	     (wnn-bunsetsu-list-equal bunsetsu (nth pos cand))
	     (eq major (wnn-bunsetsu-get-zenkouho-dai head))
	     (eq prev-b (wnn-bunsetsu-get-zenkouho-prev-b head))
	     (eq next-b (wnn-bunsetsu-get-zenkouho-next-b head)))
	(cons pos (wnn-bunsetsu-get-zenkouho-converted head))
      (setq cand (wnn-get-bunsetsu-candidates env yomi hinshi fuzokugo v major))
      (if (numberp cand)
	  (egg-error "%s" (wnnrpc-get-error-message (- cand))))
      (setq pos (wnn-candidate-pos bunsetsu cand))
      (cond ((< pos 0)
	     (setq cand (cons (wnn-bunsetsu-list-copy bunsetsu) cand)))
	    ((and (> pos 0)
		  (null (eq (wnn-bunsetsu-get-zenkouho head) t)))
	     (setq cand (cons (nth pos cand) (delq (nth pos cand) cand)))))
      (setq cand (wnn-uniq-candidates cand)
	    pos (wnn-candidate-pos bunsetsu cand)
	    converted (wnn-get-candidates-converted cand))
      (wnn-set-candidate-info bunsetsu
			      (wnn-zenkouho-create pos cand converted
						   major prev-b next-b))
      (wnn-add-freq-down head cand)
      (cons pos converted))))

(defun wnn-decide-candidate (bunsetsu pos prev-b next-b)
  (let* ((head (car bunsetsu))
	 (cand-list (wnn-bunsetsu-get-zenkouho-list head))
	 (cand (nth pos cand-list))
	 (c-head (car cand)))
    (wnn-bunsetsu-set-zenkouho-pos head pos)
    (wnn-bunsetsu-set-change-top c-head (wnn-bunsetsu-get-change-top head))
    (wnn-bunsetsu-set-freq-down c-head (wnn-bunsetsu-get-freq-down head))
    (wnn-merge-fi-rel c-head bunsetsu)
    (wnn-major-bunsetsu-set-context cand (wnn-bunsetsu-get-context head))
    (wnn-set-candidate-info cand (wnn-bunsetsu-get-zenkouho head))
    (if (and prev-b (null wnn-one-level-conversion))
	(progn
	  (setq prev-b (list (egg-get-bunsetsu-tail prev-b)))
	  (wnn-bunsetsu-set-dai-continue (car prev-b)
					 (wnn-bunsetsu-connect-prev c-head))))
    (if next-b
	(setq next-b (list (car next-b))))
    (list cand prev-b next-b)))

(defun wnn-special-candidate (bunsetsu prev-b next-b major type)
  (let* ((backend (egg-bunsetsu-get-backend (car bunsetsu)))
	 (lang (get backend 'language))
	 pos cand)
    (when (and (eq lang (get backend 'source-language))
	       (eq lang (get backend 'converted-language)))
      (setq pos (and (eq lang (get backend 'source-language))
		     (eq lang (get backend 'converted-language))
		     (cond ((eq lang 'Japanese)
			    (cond ((eq type 'egg-hiragana) -1)
				  ((eq type 'egg-katakana) -2)))
			   ((or (eq lang 'Chinese-GB) (eq lang 'Chinese-CNS))
			    (cond ((eq type 'egg-pinyin) -1)
				  ((eq type 'egg-zhuyin) -1)))
			   ((eq lang 'Korean)
			    (cond ((eq type 'egg-hangul) -1))))))
      (when pos
	(setq cand (cdr (wnn-list-candidates bunsetsu prev-b next-b major))
	      pos (+ pos (length cand)))
	(when (and (or (eq lang 'Chinese-GB) (eq lang 'Chinese-CNS)))
	  (let ((converted (nth pos cand)))
	    (cond ((egg-pinyin-syllable converted)
		   (cond ((eq type 'egg-pinyin)) ; OK
			 ((eq type 'egg-zhuyin)
			  (wnn-pinyin-zhuyin-bunsetsu bunsetsu pos lang type))
			 (t (setq pos nil))))
		  ((egg-zhuyin-syllable converted)
		   (cond ((eq type 'egg-pinyin)
			  (wnn-pinyin-zhuyin-bunsetsu bunsetsu pos lang type))
			 ((eq type 'egg-zhuyin)) ; OK
			 (t (setq pos nil))))
		  (t (setq pos nil))))))
      (when pos
	(wnn-decide-candidate bunsetsu pos prev-b next-b)))))

(defun wnn-pinyin-zhuyin-bunsetsu (bunsetsu pos lang type)
  (let ((b (nth pos (wnn-bunsetsu-get-zenkouho-list (car bunsetsu))))
	(encoding (if (eq lang 'Chinese-GB)
		      (if (eq type 'egg-pinyin)
			  'fixed-euc-py-cn 'fixed-euc-zy-cn)
		    (if (eq type 'egg-pinyin)
			'fixed-euc-py-tw 'fixed-euc-zy-tw)))
	(converted (wnn-bunsetsu-get-zenkouho-converted (car bunsetsu)))
	str)
    (setcar (nthcdr pos converted)
	    (wnn-pinyin-zhuyin-string (nth pos converted) encoding))
    (while b
      (setq str (wnn-bunsetsu-get-converted (car b)))
      (when str
	(wnn-bunsetsu-set-converted
	 (car b)
	 (wnn-pinyin-zhuyin-string str encoding)))
      (setq str (wnn-bunsetsu-get-fuzokugo (car b)))
      (when str
	(wnn-bunsetsu-set-fuzokugo
	 (car b)
	 (wnn-pinyin-zhuyin-string str encoding)))
      (setq b (cdr b)))))

(defun wnn-pinyin-zhuyin-string (str encoding)
  (decode-coding-string (encode-coding-string str encoding) encoding))

(defun wnn-change-bunsetsu-length (bunsetsu prev-b next-b len major)
  (let ((backend (egg-bunsetsu-get-backend (car bunsetsu)))
	(env (wnn-bunsetsu-get-env (car bunsetsu)))
	(tail (egg-get-bunsetsu-tail prev-b))
	(yomi (wnn-get-major-bunsetsu-source bunsetsu))
	(context (wnn-bunsetsu-get-context (car bunsetsu)))
	yomi1 yomi2 hinshi fuzokugo new)
    (if tail
	(setq hinshi (wnn-bunsetsu-get-hinshi tail)
	      fuzokugo (wnn-bunsetsu-get-fuzokugo tail))
      (setq hinshi -1
	    fuzokugo ""))
    (setq yomi1 (substring yomi 0 len)
	  yomi2 (concat (substring yomi len)
			(wnn-get-major-bunsetsu-source next-b)))
    (setq new (wnn-tanbunsetsu-conversion env yomi1 hinshi fuzokugo nil major))
    (if (numberp new)
	(egg-error "%s" (wnnrpc-get-error-message (- new))))
    (if (and prev-b (null wnn-one-level-conversion))
	(wnn-bunsetsu-set-dai-continue tail
				       (wnn-bunsetsu-connect-prev (car new))))
    (wnn-bunsetsu-set-change-top (car new) t)
    (wnn-merge-freq-down (car new) bunsetsu)
    (wnn-merge-fi-rel (car new) bunsetsu)
    (wnn-merge-fi-rel (car new) next-b)
    (wnn-major-bunsetsu-set-context new context)
    (if (= (length yomi2) 0)
	(setq next-b nil)
      (setq tail (egg-get-bunsetsu-tail new)
	    next-b (wnn-renbunsetsu-conversion env yomi2
					       (wnn-bunsetsu-get-hinshi tail)
					       (wnn-bunsetsu-get-fuzokugo tail)
					       nil context))
      (if (numberp next-b)
	  (egg-error "%s" (wnnrpc-get-error-message (- next-b))))
      (if (and (null major) (null wnn-one-level-conversion))
	  (wnn-bunsetsu-set-dai-continue
	   tail
	   (wnn-bunsetsu-connect-prev (car next-b)))))
    (list new prev-b next-b)))

(defun wnn-add-freq-down (bunsetsu down-list)
  (let ((freq-down (wnn-bunsetsu-get-freq-down bunsetsu))
	b-list b pair)
    (while down-list
      (setq b-list (car down-list)
	    down-list (cdr down-list))
      (while b-list
	(setq b (car b-list)
	      b-list (cdr b-list)
	      pair (cons (wnn-bunsetsu-get-dic-no b)
			 (wnn-bunsetsu-get-entry b)))
	(if (and (/= (wnn-bunsetsu-get-right-now b) 0)
		 (/= (car pair) -1)
		 (null (member pair freq-down)))
	    (setq freq-down (cons pair freq-down)))))
    (wnn-bunsetsu-set-freq-down bunsetsu freq-down)))

(defun wnn-merge-freq-down (bunsetsu b-list)
  (let ((freq-down0 (wnn-bunsetsu-get-freq-down bunsetsu))
	freq-down1)
    (while b-list
      (setq freq-down1 (wnn-bunsetsu-get-freq-down (car b-list))
	    b-list (cdr b-list))
      (while freq-down1
	(if (null (member (car freq-down1) freq-down0))
	    (setq freq-down0 (cons (car freq-down1) freq-down0)))
	(setq freq-down1 (cdr freq-down1)))
    (wnn-bunsetsu-set-freq-down bunsetsu freq-down0))))

(defun wnn-merge-fi-rel (bunsetsu b-list)
  (let ((fi-rel (cons nil (wnn-bunsetsu-get-fi-rel bunsetsu))))
    (if (eq bunsetsu (car b-list))
	(setq b-list (cdr b-list)))
    (while b-list
      (nconc fi-rel (wnn-bunsetsu-get-fi-rel (car b-list)))
      (wnn-bunsetsu-set-fi-rel (car b-list) nil)
      (setq b-list (cdr b-list)))
    (wnn-bunsetsu-set-fi-rel bunsetsu (cdr fi-rel))))

(defun wnn-bunsetsu-combinable-p (bunsetsu1 bunsetsu2)
  (eq (wnn-bunsetsu-get-env bunsetsu1)
      (wnn-bunsetsu-get-env bunsetsu2)))

(defvar wnn-auto-save-dic-count 0)

(defun wnn-end-conversion (bunsetsu-list abort)
  (unless abort
    (let* ((head (car bunsetsu-list))
	   (env (wnn-bunsetsu-get-env head)))
      (prog1
	  (if (or (wnnenv-is-wnn6 env) (wnnenv-is-wnn7 env) (wnnenv-is-wnn8 env))
	      (progn
		(wnn-clear-now-flag bunsetsu-list)
		(wnn-merge-fi-rel head (cdr bunsetsu-list))
		(wnnrpc-set-fi-priority env (wnn-bunsetsu-get-fi-rel head))
		(wnn-optimize-in-local bunsetsu-list)
		(wnn-optimize-in-server bunsetsu-list))
	    (wnn-clear-now-flag bunsetsu-list)
	    (wnn-count-up-frequency bunsetsu-list))
	(setq wnn-auto-save-dic-count (1+ wnn-auto-save-dic-count))
	(when (eq wnn-auto-save-dic-count wnn-auto-save-dictionaries)
	  (wnn-save-dictionaries env)
	  (setq wnn-auto-save-dic-count 0))))))

(defun wnn-clear-now-flag (bunsetsu-list)
  (let ((env (wnn-bunsetsu-get-env (car bunsetsu-list))))
    (while bunsetsu-list
      (setq fd (wnn-bunsetsu-get-freq-down (car bunsetsu-list))
	    bunsetsu-list (cdr bunsetsu-list))
      (while fd
	(wnnrpc-set-frequency env (caar fd) (cdar fd)
			      (WNN-const IMA_OFF) (WNN-const HINDO_NOP))
	(setq fd (cdr fd))))))

(defun wnn-count-up-frequency (bunsetsu-list)
  (let ((env (wnn-bunsetsu-get-env (car bunsetsu-list)))
	(context (wnn-null-context))
	dic-no entry b)
    (while bunsetsu-list
      (setq b (car bunsetsu-list)
	    bunsetsu-list (cdr bunsetsu-list)
	    dic-no (wnn-bunsetsu-get-dic-no b)
	    entry (wnn-bunsetsu-get-entry b)
	    context (cons (wnn-context-create dic-no entry
					      (wnn-bunsetsu-get-jirilen b)
					      (wnn-bunsetsu-get-hinshi b)
					      (wnn-bunsetsu-get-fuzokugo b)
					      (wnn-bunsetsu-get-converted b)
					      (wnn-bunsetsu-get-right-now b)
					      (wnn-bunsetsu-get-freq b))
			  context))
      (wnnrpc-set-frequency env dic-no entry
			    (WNN-const IMA_ON) (WNN-const HINDO_INC)))
    (list (car context) (nth 1 context))))

(defun wnn-optimize-in-local (bunsetsu-list)
  (let ((env (wnn-bunsetsu-get-env (car bunsetsu-list)))
	b prev-b next-b major-top entry hinshi)
    (setq next-b (car bunsetsu-list)
	  bunsetsu-list (cdr bunsetsu-list))
    (cond
     ((eq (wnnenv-get-server-type env) 'jserver)
      (while next-b
	(setq major-top (null (and b (wnn-bunsetsu-get-dai-continue b)))
	      prev-b b
	      b next-b
	      next-b (car bunsetsu-list)
	      bunsetsu-list (cdr bunsetsu-list)
	      hinshi (wnn-bunsetsu-get-hinshi b))
	(when (or
	       (and (/= (wnnenv-get-notrans env) (WNN-const DIC_RDONLY))
		    (= (wnn-bunsetsu-get-dic-no b) -1)
		    (or (= (wnn-bunsetsu-get-entry b) (WNN-const HIRAGANA))
			(= (wnn-bunsetsu-get-entry b) (WNN-const KATAKANA)))
		    (>= (wnn-bunsetsu-get-jirilen b) (WNN-const LEARNING_LEN)))
	       (= (wnn-bunsetsu-get-entry b) (WNN-const IKEIJI_ENTRY)))
	  (setq entry (wnn-notrans-auto-learning b))
	  (when (/= entry -1)
	    (wnn-bunsetsu-set-dic-no b (WNN-const MUHENKAN_DIC))
	    (wnn-bunsetsu-set-entry b entry)))
	(cond
	 ((and next-b
	       major-top
	       (wnn-bunsetsu-get-dai-continue b))
	  (wnn-adjacent-learning b next-b))
	 ((and prev-b
	       (= hinshi (wnnenv-get-hinshi env 'rendaku))
	       (equal (wnn-bunsetsu-get-fuzokugo prev-b) ""))
	  (wnn-adjacent-learning prev-b b))
	 ((and next-b
	       (= hinshi (wnnenv-get-hinshi env 'settou)))
	  (wnn-adjacent-learning b next-b))
	 ((and (/= (wnnenv-get-bmodify env) (WNN-const DIC_RDONLY))
	       (wnn-bunsetsu-get-change-top b)
	       next-b
	       (/= (wnn-bunsetsu-get-hinshi next-b)
		   (wnnenv-get-hinshi env 'rendaku))
	       (/= hinshi (wnnenv-get-hinshi env 'settou)))
	  (wnn-bmodify-learning b next-b)))))
     ((eq (wnnenv-get-server-type env) 'kserver)
      ;; Soory, not implemented
      nil))))

(defun wnn-notrans-auto-learning (bunsetsu)
  (let ((env (wnn-bunsetsu-get-env bunsetsu)))
    (wnnrpc-auto-learning env (WNN-const NOTRANS_LEARN)
			  (wnn-bunsetsu-get-yomi bunsetsu)
			  (wnn-bunsetsu-get-converted bunsetsu)
			  ""
			  (if (= (wnn-bunsetsu-get-entry bunsetsu)
				 (WNN-const IKEIJI_ENTRY))
			      (wnn-bunsetsu-get-hinshi bunsetsu)
			    (wnnenv-get-hinshi env 'noun))
			  0)))

(defun wnn-adjacent-learning (bunsetsu1 bunsetsu2)
  (let ((env (wnn-bunsetsu-get-env bunsetsu1))
	(yomi (concat (wnn-bunsetsu-get-yomi bunsetsu1)
		      (wnn-bunsetsu-get-yomi bunsetsu2)))
	(kanji (concat (wnn-bunsetsu-get-converted bunsetsu1)
		       (wnn-bunsetsu-get-converted bunsetsu2)))
	(hinshi (wnnenv-get-hinshi env 'noun)))
    (if (= (wnnenv-get-bmodify env) (WNN-const DIC_RW))
	(wnnrpc-auto-learning env (WNN-const BMODIFY_LEARN)
			      yomi kanji "" hinshi 0)
      (wnnrpc-temporary-learning env yomi kanji "" hinshi 0))))

(defun wnn-bmodify-learning (bunsetsu1 bunsetsu2)
  (let ((env (wnn-bunsetsu-get-env bunsetsu1))
	(yomi (concat (wnn-bunsetsu-get-yomi bunsetsu1)
		      (wnn-bunsetsu-get-fuzokugo bunsetsu1)
		      (wnn-bunsetsu-get-yomi bunsetsu2)))
	(kanji (concat (wnn-bunsetsu-get-converted bunsetsu1)
		       (wnn-bunsetsu-get-fuzokugo bunsetsu1)
		       (wnn-bunsetsu-get-converted bunsetsu2)))
	(hinshi (wnn-bunsetsu-get-hinshi bunsetsu2)))
    (wnnrpc-auto-learning env (WNN-const BMODIFY_LEARN)
			  yomi kanji "" hinshi 0)))

(defun wnn-optimize-in-server (bunsetsu-list)
  (let ((env (wnn-bunsetsu-get-env (car bunsetsu-list)))
	(context (wnn-bunsetsu-get-context (car bunsetsu-list)))
	b)
    (when (eq (wnnenv-get-server-type env) 'jserver)
      (wnn-context-set-right-now (car context) (WNN-const HINDO_NOP))
      (wnn-context-set-freq (car context) (WNN-const HINDO_NOP))
      (wnn-context-set-right-now (nth 1 context) (WNN-const HINDO_NOP))
      (wnn-context-set-freq (nth 1 context) (WNN-const HINDO_NOP))
      (while bunsetsu-list
	(setq b (car bunsetsu-list)
	      bunsetsu-list (cdr bunsetsu-list)
	      context (cons (wnn-context-create (wnn-bunsetsu-get-dic-no b)
						(wnn-bunsetsu-get-entry b)
						(wnn-bunsetsu-get-jirilen b)
						(wnn-bunsetsu-get-hinshi b)
						(wnn-bunsetsu-get-fuzokugo b)
						(wnn-bunsetsu-get-converted b)
						(WNN-const IMA_ON)
						(WNN-const HINDO_INC))
			    context)))
      (prog1
	  (list (car context) (nth 1 context))
	(wnnrpc-optimize-fi env (nreverse context))))))

(defun wnn-finalize-backend (lang &optional action)
  (let* ((save-inhibit-quit inhibit-quit)
	 (inhibit-quit t)
	 (server-info (wnn-server-get-info lang))
	 (server-type (wnn-server-type server-info))
	 (env-list wnn-environments)
	 env proc-list saved)
    (when server-type
      (while env-list
	(setq env (car env-list)
	      env-list (cdr env-list))
	(if (eq (wnnenv-get-server-type env) server-type)
	    (condition-case err
		(progn
		  (unless (memq (wnnenv-get-proc env) proc-list)
		    (setq proc-list (cons (wnnenv-get-proc env) proc-list)))
		  (unless (eq action 'disconnect-only)
		    (unless saved
		      (setq saved t)
		      (message (egg-get-message 'wnn-dict-saving)
			       (wnn-server-proc-name server-info)))
		    (let ((inhibit-quit save-inhibit-quit))
		      (wnn-save-dictionaries env)))
		  (unless (eq action 'save-only)
		    (wnnrpc-disconnect env)))
	      ((error quit)
	       (message "signal %S occured when dictionary saving" err)))))
      (if saved
	  (message (egg-get-message 'wnn-dict-saved)
		   (wnn-server-proc-name server-info)))
      (unless (eq action 'save-only)
	(while proc-list
	  (if (and (car proc-list)
		   (eq (process-status (car proc-list)) 'open))
	      (wnnrpc-close (car proc-list)))
	  (setq proc-list (cdr proc-list)))))))

(defun wnn-close (lang)
  "Save dictionaries and close the Wnn session."
  (interactive (list (wnn-read-active-lang)))
  (or (listp lang)
      (setq lang (list lang)))
  (while lang
    (wnn-finalize-backend (car lang))
    (setq lang (cdr lang))))

(defun wnn-disconnect (lang)
  "Disconnect the Wnn session without dictionary saving."
  (interactive (list (wnn-read-active-lang)))
  (or (listp lang)
      (setq lang (list lang)))
  (while lang
    (wnn-finalize-backend (car lang) 'disconnect-only)
    (setq lang (cdr lang))))

(defun wnn-dictionary-save (lang)
  "Save Wnn dictionaries."
  (interactive (list (wnn-read-active-lang)))
  (or (listp lang)
      (setq lang (list lang)))
  (while lang
    (wnn-finalize-backend (car lang) 'save-only)
    (setq lang (cdr lang))))

(defun wnn-read-active-lang ()
  (let ((completion-ignore-case t)
	(env wnn-environments)
	langs server server-list)
    (while env
      (setq server (wnnenv-get-server-type (car env))
	    env (cdr env))
      (if (null (memq server server-list))
	  (setq server-list (cons server server-list))))
    (setq langs (delq nil
		      (mapcar (lambda (info)
				(if (memq (wnn-server-type info) server-list)
				    (wnn-server-language info)))
			      wnn-server-info-list)))
    (if (<= (length langs) 1)
	langs
      (setq langs (cons (cons "All" langs)
			(mapcar (lambda (lang) (cons (symbol-name lang) lang))
				langs)))
      (cdr (assoc (completing-read "language? " langs nil t nil nil "All")
		  langs)))))

;;
(defun wnn-comm-sentinel (proc reason)	; assume it is close
  (let ((inhibit-quit t))
    (kill-buffer (process-buffer proc))
    ;; delete env from the list.
    (setq wnn-environments
	  (delq nil (mapcar (lambda (env)
			      (if (null (eq (wnnenv-get-proc env) proc))
				  env))
			      wnn-environments)))))

(defun wnn-open (server-info)
  "Establish the connection to WNN server.  Return process object."
  ;; Open the session to WNN server,
  (let ((save-inhibit-quit inhibit-quit)
	(inhibit-quit t)
	(server-type (wnn-server-type server-info))
	(port (wnn-server-port server-info))
	(hostname-list (wnn-server-hostname server-info))
	(proc-name (wnn-server-proc-name server-info))
	(msg-form "Wnn: connecting to %S at %s...")
	(user-name (user-login-name))
	buf hostname myname port-off proc result msg)
    (unwind-protect
	(progn
	  (setq buf (generate-new-buffer (wnn-server-buffer-name server-info)))
	  (save-excursion
	    (set-buffer buf)
	    (erase-buffer)
	    (buffer-disable-undo)
	    (set-buffer-multibyte nil)
	    (setq egg-fixed-euc (wnn-server-coding-system server-info)))
	  (or (consp hostname-list)
	      (setq hostname-list (list hostname-list)))
	  (while (and hostname-list (null proc))
	    (setq hostname (or (car hostname-list) "")
		  hostname-list (cdr hostname-list)
		  myname (if (equal hostname "") "unix" wnn-system-name))
	    (if (null (string-match ":" hostname))
		(setq port-off 0)
	      (setq port-off (string-to-int (substring hostname (match-end 0)))
		    hostname (substring hostname 0 (match-beginning 0))))
	    (and (equal hostname "") (setq hostname "localhost"))
	    (let ((inhibit-quit save-inhibit-quit))
	      (if (and msg
		       (null (y-or-n-p (format "%s failed. Try to %s? "
					       msg hostname))))
		  (egg-error "abort connect")))
	    (setq msg (format "Wnn: connecting to %S at %s..."
			      server-type hostname))
	    (message "%s" msg)
	    (let ((inhibit-quit save-inhibit-quit))
	      (condition-case nil
		  (setq proc (open-network-stream proc-name buf hostname
						  (+ port port-off)))
		((error quit))))
	    (when proc
	      (process-kill-without-query proc)
	      (set-process-coding-system proc 'binary 'binary)
	      (set-process-sentinel proc 'wnn-comm-sentinel)
	      (set-marker-insertion-type (process-mark proc) t)
	      (setq result (wnnrpc-open proc myname user-name))
	      (when (numberp result)
		(delete-process proc)
		(setq proc nil))))
	  (cons proc result))
      (if proc
	  (message (concat msg "done"))
	(if buf (kill-buffer buf))
	(egg-error 'wnn-connect-error)))))

(defvar wnn-envspec-list nil)
(defvar wnn-current-envspec nil)
(defvar wnn-current-envspec-reverse nil)
(defvar wnn-server-type nil)
(defvar wnn-wnn6-server nil)
(defvar wnn-wnn7-server nil)
(defvar wnn-wnn8-server nil)

(defmacro wnn-envspec-conv-param-name-list ()
  ''(last-is-first complex okuri-learn okuri
     prefix-learn prefix suffix-learn common-learn freq-func
     numeric alphabet symbol yuragi rendaku bunsetsugiri muhenkan
     fi-relation-learn fi-freq-func
     ;; Wnn7/8 funcs bellow
     yosoku-learn yosoku-max-disp yosoku-last-is-first
     boin-kabusoku shiin-choka n-choka nihongo-kosei))

(defmacro wnn-envspec-conv-param-length ()
  (length (wnn-envspec-conv-param-name-list)))

(defun wnn-envspec-create (env-name tankan stickey)
  (vector (and env-name (setq env-name (intern env-name)))
	  (wnn-make-backend egg-language env-name)
	  tankan stickey nil nil
	  0 (make-vector (wnn-envspec-conv-param-length) 0)
	  (list nil) (list nil) (list nil)))

(defsubst wnn-envspec-env-type (spec)           (aref spec 0))
(defsubst wnn-envspec-backend (spec)            (aref spec 1))
(defsubst wnn-envspec-tankan (spec)             (aref spec 2))
(defsubst wnn-envspec-sticky (spec)             (aref spec 3))
(defsubst wnn-envspec-param (spec)              (aref spec 4))
(defsubst wnn-envspec-fuzokugo (spec)           (aref spec 5))
(defsubst wnn-envspec-conv-vmask (spec)         (aref spec 6))
(defsubst wnn-envspec-conv-param (spec)         (aref spec 7))
(defsubst wnn-envspec-dic-list (spec)           (cdr (aref spec 8)))
(defsubst wnn-envspec-fi-dic-list (spec)        (cdr (aref spec 9)))
(defsubst wnn-envspec-autolearn-dic-list (spec) (cdr (aref spec 10)))

(defsubst wnn-envspec-set-param (spec param)
  (aset spec 4 param))
(defsubst wnn-envspec-set-fuzokugo (spec fzk)
  (aset spec 5 fzk))
(defsubst wnn-envspec-set-conv-vmask (spec val)
  (aset spec 6 val))
(defsubst wnn-envspec-set-conv-param (spec num val)
  (aset (aref spec 7) num val))
(defsubst wnn-envspec-add-dic-list (spec &rest dic)
  (nconc (aref spec 8) (list (apply 'vector dic))))
(defsubst wnn-envspec-add-fi-dic-list (spec &rest dic)
  (nconc (aref spec 9) (list (apply 'vector dic))))
(defsubst wnn-envspec-add-autolearn-dic-list (spec type &rest dic)
  (nconc (aref spec 10) (list (cons type (apply 'vector dic)))))

(eval-when-compile
  (defun wnn-conv-param (param)
    (- (wnn-envspec-conv-param-length)
       (length (memq param (wnn-envspec-conv-param-name-list))))))

(defmacro define-wnn-conv-param-func ()
  (let ((name-list (wnn-envspec-conv-param-name-list))
	(defs (list 'progn))
	n set get)
    (while name-list
      (setq n (car name-list)
	    name-list (cdr name-list)
	    set (intern (format "wnn-envspec-set-conv-param-%s" n))
	    get (intern (format "wnn-get-conv-param-%s" n)))
      (nconc defs `((defsubst ,set (spec val)
		      (wnn-envspec-set-conv-param spec ,(wnn-conv-param n) val)
		      (wnn-envspec-set-conv-vmask
		       spec (logior (wnn-envspec-conv-vmask spec)
				    ,(lsh 1 (wnn-conv-param n)))))
		    (defsubst ,get (param)
		      (aref param ,(wnn-conv-param n))))))
    defs))

(define-wnn-conv-param-func)

(defmacro wnn-arg-type-error (func)
  `(egg-error ,(format "%s: Wrong type argument" func)))

(defun wnn-define-environment (reverse &optional env-name tankan stickey)
  "Define a Wnn environment for normal/reverse conversion according
to REVERSE.  ENV-NAME specifies suffix of the Wnn environment name.
Make single character conversion (Tan-Kanji conversion) environment,
if tankan is non-NIL.  Make the environment as sticky, if STICKEY
is non-NIL."
  (if (and env-name (null (stringp env-name)))
      (wnn-arg-type-error wnn-define-environment))
  (setq env-name (if reverse (concat env-name "R") env-name)
	wnn-current-envspec (wnn-envspec-create env-name tankan stickey)
	wnn-current-envspec-reverse reverse
	wnn-envspec-list (nconc wnn-envspec-list
				(list wnn-current-envspec))))

(defun wnn-set-fuzokugo (filename)
  (cond ((equal filename "")
	 (setq filename nil))
	((null (stringp filename))
	 (wnn-arg-type-error wnn-set-fuzokugo)))
  (wnn-envspec-set-fuzokugo wnn-current-envspec filename))

(defmacro wnn-add-dict-param-check (func
				    dict &optional freq prior drw dmax frw fmax
				    dpass fpass rev)
  `(progn
     (if (or (and (null (stringp ,dict)) (null (stringp (car-safe ,dict))))
	     ,@(if freq
		   `((and ,freq (null (stringp ,freq))
			  (null (stringp (car-safe ,freq))))))
	     ,@(if prior `((null (integerp ,prior))))
	     ,@(if drw
		   `((null (or (eq ,drw nil) (eq ,drw t)
			       (eq ,drw 0) (eq ,drw 1)
			       ,@(if dmax
				     `((and (or wnn-wnn6-server wnn-wnn7-server wnn-wnn8-server)
					    ,@(let ((x `((eq ,drw 2))))
						(when (>= dmax 3)
						  (nconc x `((eq ,drw 3))))
						(when (>= dmax 4)
						  (nconc x `((eq ,drw 4))))
						x))))))))
	     ,@(if frw
		   `((null (or (eq ,frw nil) (eq ,frw t)
			       (eq ,frw 0) (eq ,frw 1)
			       ,@(if fmax
				     `((and (or wnn-wnn6-server wnn-wnn7-server wnn-wnn8-server)
					    ,@(let ((x `((eq ,frw 2))))
						(when (>= fmax 3)
						  (nconc x `((eq ,frw 3))))
						(when (>= fmax 4)
						  (nconc x `((eq ,frw 4))))
						x))))))))
	     ,@(if dpass `((and ,dpass (null (stringp ,dpass)))))
	     ,@(if fpass `((and ,fpass (null (stringp ,fpass))))))
	 (wnn-arg-type-error ,func))
     (if (or (equal ,dict "") (equal (car-safe ,dict) ""))
	 (egg-error ,(format "%s: Dictionary name should not be null." func)))
     ,@(if freq
	   `((if (or (equal ,freq "") (equal (car-safe ,freq) ""))
		 (setq ,freq nil))))
     ,@(if rev
	   `((setq ,rev (if ,rev (car ,rev) wnn-current-envspec-reverse))))))

(defmacro wnn-wnn6-env-func (func)
  `(or wnn-wnn6-server
       (egg-error ,(format "%s is available only on Wnn6" func))))

(defmacro wnn-wnn7-env-func (func)
  `(or wnn-wnn7-server
       (egg-error ,(format "%s is available only on Wnn7" func))))

(defmacro wnn-wnn8-env-func (func)
  `(or wnn-wnn8-server
       (egg-error ,(format "%s is available only on Wnn8" func))))

(defmacro wnn-wnn7-and-wnn8-env-func (func)
  `(or (or wnn-wnn7-server wnn-wnn8-server)
       (egg-error ,(format "%s is available only on Wnn7/Wnn8" func))))

(defmacro wnn-wnn678-env-func (func)
  `(or (or wnn-wnn6-server wnn-wnn7-server wnn-wnn8-server)
       (egg-error ,(format "%s is available only on Wnn6/Wnn7/Wnn8" func))))

(defun wnn-add-dict (dict freq priority dict-rw freq-rw
		     &optional dict-passwd freq-passwd &rest reverse)
  (wnn-add-dict-param-check wnn-add-dict
			    dict freq priority dict-rw 4 freq-rw 2
			    dict-passwd freq-passwd reverse)
  (wnn-envspec-add-dic-list wnn-current-envspec
			    dict freq priority dict-rw freq-rw
			    dict-passwd freq-passwd reverse))

(defun wnn-add-fisys-dict (dict freq freq-rw &optional freq-passwd)
  (wnn-wnn678-env-func wnn-add-fisys-dict)
  (wnn-add-dict-param-check wnn-add-fisys-dict
			    dict freq nil nil nil freq-rw 3
			    nil freq-passwd)
  (wnn-envspec-add-fi-dic-list wnn-current-envspec
			       dict freq t nil freq-rw nil freq-passwd nil))

(defun wnn-add-fiusr-dict (dict freq dict-rw freq-rw
			   &optional dict-passwd freq-passwd)
  (wnn-wnn678-env-func wnn-add-fiusr-dict)
  (wnn-add-dict-param-check wnn-add-fiusr-dict
			    dict freq nil dict-rw 3 freq-rw 3
			    dict-passwd freq-passwd)
  (wnn-envspec-add-fi-dic-list wnn-current-envspec
			       dict freq nil dict-rw freq-rw
			       dict-passwd freq-passwd nil))

(defun wnn-add-notrans-dict (dict priority dict-rw
			     &optional dict-passwd &rest reverse)
  (wnn-wnn678-env-func wnn-add-notrans-dict)
  (wnn-add-dict-param-check wnn-add-notrans-dict
			    dict nil priority dict-rw nil nil nil
			    dict-passwd nil reverse)
  (wnn-envspec-add-autolearn-dic-list wnn-current-envspec
				      (WNN-const NOTRANS_LEARN)
				      dict nil priority dict-rw nil
				      dict-passwd nil reverse))

(defun wnn-add-bmodify-dict (dict priority dict-rw
			     &optional dict-passwd &rest reverse)
  (wnn-wnn678-env-func wnn-add-notrans-dict)
  (wnn-add-dict-param-check wnn-add-bmodify-dict
			    dict nil priority dict-rw nil nil nil
			    dict-passwd nil reverse)
  (wnn-envspec-add-autolearn-dic-list wnn-current-envspec
				      (WNN-const BMODIFY_LEARN)
				      dict nil priority dict-rw nil
				      dict-passwd nil reverse))

(defun wnn-set-param (&rest args)
  (if (/= (length args) 17)
      (egg-error "wnn-set-param: Wrong number of arguments"))
  (mapcar (lambda (n)
	    (if (null (integerp n))
		(wnn-arg-type-error wnn-set-param)))
	  args)
  (wnn-envspec-set-param wnn-current-envspec (apply 'vector args)))

(defmacro wnn-boolean-param-check (func flag)
  `(setq ,flag (cond ((or (eq ,flag 0) (eq ,flag nil)) 0)
		     ((or (eq ,flag 1) (eq ,flag t))   1)
		     (t (wnn-arg-type-error ,func)))))

(defun wnn-set-last-is-first-mode (flag)
  (wnn-wnn678-env-func wnn-set-last-is-first-mode)
  (wnn-boolean-param-check wnn-set-last-is-first-mode flag)
  (wnn-envspec-set-conv-param-last-is-first wnn-current-envspec flag))

(defun wnn-set-complex-conv-mode (flag)
  (wnn-wnn678-env-func wnn-set-complex-conv-mode)
  (wnn-boolean-param-check wnn-set-complex-conv-mode flag)
  (wnn-envspec-set-conv-param-complex wnn-current-envspec flag))

(defun wnn-set-okuri-learn-mode (flag)
  (wnn-wnn678-env-func wnn-set-okuri-learn-mode)
  (wnn-boolean-param-check wnn-set-okuri-learn-mode flag)
  (wnn-envspec-set-conv-param-okuri-learn wnn-current-envspec flag))

(defun wnn-set-okuri-flag (mode)
  (wnn-wnn678-env-func wnn-set-okuri-flag)
  (setq mode (cond ((or (eq mode -1) (eq mode 'regulation)) -1)
		   ((or (eq mode  0) (eq mode 'no))          0)
		   ((or (eq mode  1) (eq mode 'yes))         1)
		   (t (wnn-arg-type-error wnn-set-okuri-flag))))
  (wnn-envspec-set-conv-param-okuri wnn-current-envspec mode))

(defun wnn-set-prefix-learn-mode (flag)
  (wnn-wnn678-env-func wnn-set-prefix-learn-mode)
  (wnn-boolean-param-check wnn-set-prefix-learn-mode flag)
  (wnn-envspec-set-conv-param-prefix-learn wnn-current-envspec flag))

(defun wnn-set-prefix-flag (mode)
  (wnn-wnn678-env-func wnn-set-prefix-flag)
  (setq mode (cond ((or (eq mode 0) (eq mode 'hiragana)) 0)
		   ((or (eq mode 1) (eq mode 'kanji))    1)
		   (t (wnn-arg-type-error wnn-set-prefix-flag))))
  (wnn-envspec-set-conv-param-prefix wnn-current-envspec mode))

(defun wnn-set-suffix-learn-mode (flag)
  (wnn-wnn678-env-func wnn-set-suffix-learn-mode)
  (wnn-boolean-param-check wnn-set-suffix-learn-mode flag)
  (wnn-envspec-set-conv-param-suffix-learn wnn-current-envspec flag))

(defun wnn-set-common-learn-mode (flag)
  (wnn-wnn678-env-func wnn-set-common-learn-mode)
  (wnn-boolean-param-check wnn-set-common-learn-mode flag)
  (wnn-envspec-set-conv-param-common-learn wnn-current-envspec flag))

(defun wnn-set-freq-func-mode (mode)
  (wnn-wnn678-env-func wnn-set-freq-func-mode)
  (setq mode (cond ((or (eq mode 0) (eq mode 'not))    0)
		   ((or (eq mode 1) (eq mode 'always)) 1)
		   ((or (eq mode 2) (eq mode 'high))   2)
		   ((or (eq mode 3) (eq mode 'normal)) 3)
		   ((or (eq mode 4) (eq mode 'low))    4)
		   (t (wnn-arg-type-error wnn-set-freq-func-mode))))
  (wnn-envspec-set-conv-param-freq-func wnn-current-envspec mode))

(defun wnn-set-numeric-mode (mode)
  (wnn-wnn678-env-func wnn-set-numeric-mode)
  (setq mode (cond ((or (eq mode  -2) (eq mode 'han))       -2)
		   ((or (eq mode -12) (eq mode 'zen))      -12)
		   ((or (eq mode -13) (eq mode 'kan))      -13)
		   ((or (eq mode -15) (eq mode 'kansuuji)) -15)
		   ((or (eq mode -16) (eq mode 'kanold))   -16)
		   ((or (eq mode -17) (eq mode 'hancan))   -17)
		   ((or (eq mode -18) (eq mode 'zencan))   -18)
		   (t (wnn-arg-type-error wnn-set-numeric-mode))))
  (wnn-envspec-set-conv-param-numeric wnn-current-envspec mode))

(defun wnn-set-alphabet-mode (mode)
  (wnn-wnn678-env-func wnn-set-alphabet-mode)
  (setq mode (cond ((or (eq mode  -4) (eq mode 'han))  -4)
		   ((or (eq mode -30) (eq mode 'zen)) -30)
		   (t (wnn-arg-type-error wnn-set-alphabet-mode))))
  (wnn-envspec-set-conv-param-alphabet wnn-current-envspec mode))

(defun wnn-set-symbol-mode (mode)
  (wnn-wnn678-env-func wnn-set-symbol-mode)
  (setq mode (cond ((or (eq mode  -5) (eq mode 'han))  -5)
		   ((or (eq mode -40) (eq mode 'jis)) -40)
		   ((or (eq mode -41) (eq mode 'asc)) -41)
		   (t (wnn-arg-type-error wnn-set-symbol-mode))))
  (wnn-envspec-set-conv-param-symbol wnn-current-envspec mode))

(defun wnn-set-yuragi-mode (flag)
  (wnn-wnn678-env-func wnn-set-yuragi-mode)
  (wnn-boolean-param-check wnn-set-yuragi-mode flag)
  (wnn-envspec-set-conv-param-yuragi wnn-current-envspec flag))

(defun wnn-set-rendaku-mode (flag)
  (wnn-wnn678-env-func wnn-set-rendaku-mode)
  (wnn-boolean-param-check wnn-set-rendaku-mode flag)
  (wnn-envspec-set-conv-param-rendaku wnn-current-envspec flag))

;; added for Wnn7
(defun wnn-set-yosoku-learn-mode (flag)
  (wnn-wnn7-and-wnn8-env-func wnn-set-yosoku-learn-mode)
  (wnn-boolean-param-check wnn-set-yosoku-learn-mode flag)
  (wnn-envspec-set-conv-param-yosoku-learn wnn-current-envspec flag))

(defun wnn-set-yosoku-max-disp (mode)
  (wnn-wnn7-and-wnn8-env-func wnn-set-yosoku-max-disp)
  (setq mode (cond ((and (<= mode 10) (>= mode 1) mode))
		   (t (wnn-arg-type-error wnn-set-yosoku-max-disp))))
  (wnn-envspec-set-conv-param-yosoku-max-disp wnn-current-envspec mode))

(defun wnn-set-yosoku-last-is-first-mode (flag)
  (wnn-wnn7-and-wnn8-env-func wnn-set-yosoku-last-is-first-mode)
  (wnn-boolean-param-check wnn-set-yosoku-last-is-first-mode flag)
  (wnn-envspec-set-conv-param-yosoku-last-is-first wnn-current-envspec flag))

(defun wnn-set-boin-kabusoku-mode (flag)
  (wnn-wnn7-and-wnn8-env-func wnn-set-boin-kabusoku-mode)
  (wnn-boolean-param-check wnn-set-boin-kabusoku-mode flag)
  (wnn-envspec-set-conv-param-boin-kabusoku wnn-current-envspec flag))

(defun wnn-set-shiin-choka-mode (flag)
  (wnn-wnn7-and-wnn8-env-func wnn-set-shiin-choka-mode)
  (wnn-boolean-param-check wnn-set-shiin-choka-mode flag)
  (wnn-envspec-set-conv-param-shiin-choka wnn-current-envspec flag))

(defun wnn-set-n-choka-mode (flag)
  (wnn-wnn7-and-wnn8-env-func wnn-set-n-choka-mode)
  (wnn-boolean-param-check wnn-set-n-choka-mode flag)
  (wnn-envspec-set-conv-param-n-choka wnn-current-envspec flag))

(defun wnn-set-nihongo-kosei-mode (flag)
  (wnn-wnn7-and-wnn8-env-func wnn-set-nihongo-kosei-mode)
  (wnn-boolean-param-check wnn-set-nihongo-kosei-mode flag)
  (wnn-envspec-set-conv-param-nihongo-kosei wnn-current-envspec flag))


(defun wnn-renbunsetsu-conversion (env yomi hinshi fuzokugo v context)
  (let ((result
	 (cond
	  ((wnnenv-get-tankan env)
	   (wnnrpc-tanbunsetsu-conversion env yomi hinshi fuzokugo v))
	  ((or (wnnenv-is-wnn6 env) (wnnenv-is-wnn7 env) (wnnenv-is-wnn8 env))
	   (wnnrpc-fi-renbunsetsu-conversion env yomi hinshi fuzokugo v
					     context))
	  (t
	   (wnnrpc-renbunsetsu-conversion env yomi hinshi fuzokugo v)))))
    (prog1
	result
      (if wnn-one-level-conversion
	  (while (consp result)
	    (wnn-bunsetsu-set-dai-continue (car result) nil)
	    (setq result (cdr result)))))))

(defun wnn-tanbunsetsu-conversion (env yomi hinshi fuzokugo v major)
  (if (or (null major)
	  wnn-one-level-conversion
	  (wnnenv-get-tankan env))
      (wnnrpc-tanbunsetsu-conversion env yomi hinshi fuzokugo v)
    (wnnrpc-daibunsetsu-conversion env yomi hinshi fuzokugo v)))

(defun wnn-get-bunsetsu-candidates (env yomi hinshi fuzokugo v major)
  (cond
   ((or wnn-one-level-conversion
	(wnnenv-get-tankan env))
    (let ((result (wnnrpc-get-bunsetsu-candidates env yomi hinshi fuzokugo v)))
      (prog1
	  result
	(while (consp result)
	  (wnn-bunsetsu-set-dai-continue (caar result) nil)
	  (setq result (cdr result))))))
   ((null major)
    (wnnrpc-get-bunsetsu-candidates env yomi hinshi fuzokugo v))
   (t
    (wnnrpc-get-daibunsetsu-candidates env yomi hinshi fuzokugo v))))

(defsubst wnn-filename (p)
  (substitute-in-file-name
   (if (consp p) (concat wnn-usr-dic-dir "/" (car p)) p)))

(defsubst wnn-client-file-p (filename)
  (and (stringp filename)
       (= (aref filename 0) ?!)))

(defsubst wnn-client-filename (filename)
  (substitute-in-file-name (expand-file-name (substring filename 1) "~")))

(defun wnn-open-file (env filename)
  "Open the file FILENAME on the environment ENV.
Return file ID.  NIL means NO-file.
On failure, return negative error code."
  (and filename
       (if (wnn-client-file-p filename)
	   (wnnrpc-file-send env (wnn-client-filename filename))
	 (wnnrpc-file-read env (wnn-filename filename)))))

(defun wnn-create-directory (env path noquery)
  "Create directory to the path.  Retun non-NIL value on success."
  (if (wnn-client-file-p path)
      (let ((local-name (directory-file-name (file-name-directory
					      (wnn-client-filename path)))))
	(cond
	 ((file-directory-p local-name) t)
	 ((or noquery
	      (y-or-n-p (format (egg-get-message 'wnn-dir-missing)
				(file-name-directory path))))
	  (make-directory local-name t)
	  (if (file-directory-p local-name)
	      (progn
		(message (egg-get-message 'wnn-dir-created) path)
		t)
	    (message (egg-get-message 'wnn-dir-failed) path)
	    nil))))
    (let ((name (directory-file-name (file-name-directory
				      (wnn-filename path))))
	  create-list)
      (setq path name)
      (while (and name (/= (wnnrpc-access env name 0) 0))
	(setq create-list (cons name create-list)
	      name (file-name-directory name)
	      name (and name (directory-file-name name))))
      (or (null create-list)
	  (if (or noquery
		  (y-or-n-p (format (egg-get-message 'wnn-dir-missing) path)))
	      (let ((result 0))
		(while (and (>= result 0) create-list)
		  (setq result (wnnrpc-mkdir env (car create-list))
			create-list (cdr create-list)))
		(if (>= result 0)
		    (progn
		      (message (egg-get-message 'wnn-dir-created) path)
		      t)
		  (message (egg-get-message 'wnn-dir-failed) path)
		  nil)))))))

(defun wnn-file-remove (proc filename passwd)
  (let ((result (if (wnn-client-file-p filename)
		    (wnnrpc-file-remove-client
		     proc (wnn-client-filename filename) passwd)
		  (wnnrpc-file-remove proc (wnn-filename filename) passwd))))
    (or (= result 0)
	(progn
	  (message (wnnrpc-get-error-message (- result)))
	  nil))))

(defun wnn-open-dictionary (env fi name rw comment dpasswd fpasswd
				&optional noquery)
  (let ((dic-id (wnn-open-file env name)))
    (cond
     ((null dic-id)
      (message "Wnn: cannot omit dictionary name")
      nil)
     ((>= dic-id 0) dic-id)
     ((or (null rw) (/= dic-id (- (WNN-const NO_EXIST))))
      (message (egg-get-message 'wnn-dict-missing-1)
	       name (wnnrpc-get-error-message (- dic-id)))
      nil)
     ((and (or noquery
	       (y-or-n-p (format (egg-get-message 'wnn-dict-missing-2) name)))
	   (wnn-create-directory env name noquery)
	   (wnn-create-dictionary env name (wnnrpc-writable-dic-type env fi rw)
				  comment dpasswd fpasswd))
      (message (egg-get-message 'wnn-dict-created) name)
      (setq dic-id (wnn-open-file env name))
      (if (>= dic-id 0)
	  dic-id
	(message "%s" (wnnrpc-get-error-message (- dic-id)))
	nil)))))

(defun wnn-create-dictionary (env name type comment dpasswd fpasswd)
  "Create a dictionary file on the server or the client depending on name."
  (let ((result (if (wnn-client-file-p name)
		    (wnnrpc-dic-file-create-client
		     env (wnn-client-filename name) type
		     comment dpasswd fpasswd)
		  (wnnrpc-dic-file-create
		   env (wnn-filename name) type comment dpasswd fpasswd))))
    (or (= result 0)
	(progn
	  (message (wnnrpc-get-error-message (- result)))
	  nil))))

(defun wnn-open-frequency (env fi dic-id name rw comment passwd)
  (let ((freq-id (wnn-open-file env name)))
    (cond
     ((null freq-id) -1)
     ((>= freq-id 0) freq-id)
     ((or (null rw) (/= freq-id (- (WNN-const NO_EXIST))))
      (message (egg-get-message 'wnn-freq-missing-1)
	       name (wnnrpc-get-error-message (- freq-id)))
      nil)
     ((and (y-or-n-p
	    (format (egg-get-message 'wnn-freq-missing-2) name))
	   (wnn-create-directory env name nil)
	   (wnn-create-frequency env fi dic-id name comment passwd))
      (message (egg-get-message 'wnn-freq-created) name)
      (setq freq-id (wnn-open-file env name))
      (if (>= freq-id 0)
	  freq-id
	(message "%s" (wnnrpc-get-error-message (- dic-id)))
	nil)))))

(defun wnn-create-frequency (env fi dic-id name comment passwd)
  "Create a frequency file on the server or the client depending on name."
  (let ((result (if (wnn-client-file-p name)
		    (wnnrpc-hindo-file-create-client
		     env fi dic-id (wnn-client-filename name) comment passwd)
		  (wnnrpc-hindo-file-create
		   env fi dic-id (wnn-filename name) comment passwd))))
    (or (= result 0)
	(progn
	  (message (wnnrpc-get-error-message (- result)))
	  nil))))

(defun wnn-set-dictionary (env fi dic-spec)
  ""
  (let ((dname (aref dic-spec 0))
	(fname (aref dic-spec 1))
	(prior (aref dic-spec 2))
	(drw   (aref dic-spec 3))
	(frw   (aref dic-spec 4))
	(dpass (aref dic-spec 5))
	(fpass (aref dic-spec 6))
	(rev   (aref dic-spec 7))
	did fid result)
    (cond
     ((numberp (setq dpass (wnnrpc-read-passwd-file dpass)))
      (message "%s" (wnnrpc-get-error-message (- dpass)))
      nil)
     ((numberp (setq fpass (if fname (wnnrpc-read-passwd-file fpass) "")))
      (message "%s" (wnnrpc-get-error-message (- fpass)))
      nil)
     ((and (setq did (wnn-open-dictionary env fi dname drw "" dpass fpass))
	   (setq fid (wnn-open-frequency env fi did fname frw "" fpass)))
      (if fi
	  (setq result (wnnrpc-set-fi-dictionary env did fid prior drw frw
						 dpass fpass))
	(setq drw (cond ((eq drw (WNN-const DIC_GROUP)) (WNN-const DIC_RW))
			((eq drw (WNN-const DIC_MERGE)) (WNN-const DIC_RDONLY))
			(t drw))
	      result (wnnrpc-set-dictionary env did fid prior drw frw
					    dpass fpass rev)))
      (cond
       ((>= result 0) t)
       ((or (null frw) (/= result (- (WNN-const NO_MATCH))))
	(message "%s (%s): %s"
		 dname (if fname fname "")
		 (wnnrpc-get-error-message (- result)))
	nil)
       ((and (y-or-n-p (format (egg-get-message 'wnn-no-match) fname))
	     (>= (wnnrpc-file-discard env fid) 0)
	     (wnn-file-remove proc fname fpass)
	     (wnn-create-frequency env fi did fname "" fpass))
	(message (egg-get-message 'wnn-re-create-freq) fname)
	(and (>= (setq fid (wnn-open-file env fname)) 0)
	     (>= (wnnrpc-set-dictionary env
					did fid prior drw frw
					dpass fpass rev)
		 0))))))))

(defun wnn-temporary-dic-add (env rev)
  (let ((result (wnnrpc-temporary-dic-loaded env)))
    (if (= result 0)
	(wnnrpc-temporary-dic-add env rev)
      result)))

(defun wnn-set-autolearn-dictionary (env type dic-spec)
  (let ((dname (aref dic-spec 0))
	(prior (aref dic-spec 2))
	(drw   (aref dic-spec 3))
	(dpass (aref dic-spec 5))
	(rev   (aref dic-spec 7))
	(did (wnnrpc-get-autolearning-dic env type))
	result)
    (or (numberp drw)
	(setq drw (if drw 0 1)))
    (cond
     ((< did 0)
      (message "%s" (wnnrpc-get-error-message (- did)))
      nil)
     ((> did 0)
      (setq result (wnn-temporary-dic-add env rev))
      (if (>= result 0)
	  drw
	(message "%s" (wnnrpc-get-error-message (- result)))
	nil))
     ((numberp (setq dpass (wnnrpc-read-passwd-file dpass)))
      (message "%s" (wnnrpc-get-error-message (- dpass)))
      nil)
     ((setq did (wnn-open-dictionary env nil dname t "" dpass "" t))
      (if (and (>= (setq did (wnnrpc-set-dictionary env did -1 prior drw drw
						    dpass "" rev))
		   0)
	       (>= (setq did (wnnrpc-set-autolearning-dic env type did)) 0)
	       (>= (setq did (wnn-temporary-dic-add env rev)) 0))
	  drw
	(message "%s" (wnnrpc-get-error-message (- did)))
	nil)))))

(defun wnn-search-environment (backend)
  (let ((env-list wnn-environments)
	env)
    (while (and (null env) env-list)
      (setq env (and (eq (wnnenv-get-backend (car env-list)) backend)
		     (car env-list))
	    env-list (cdr env-list)))
    env))

(defun wnn-v3-eggrc-defines ()
  (if (null (fboundp 'set-wnn-reverse))
      (progn
	(fset 'set-wnn-reverse
	      (lambda (arg)
		(wnn-define-environment arg
					(and (or (eq wnn-server-type 'cserver)
						 (eq wnn-server-type 'tserver))
					     "PZ"))))
	(fset 'is-wnn6-server (lambda () wnn-wnn6-server))
	(fset 'is-wnn7-server (lambda () wnn-wnn7-server))
	(fset 'is-wnn8-server (lambda () wnn-wnn8-server))
	(fset 'set-wnn-fuzokugo 'wnn-set-fuzokugo)
	(fset 'add-wnn-dict 'wnn-add-dict)
	(fset 'set-wnn-param 'wnn-set-param)
	(fset 'add-wnn-fisys-dict 'wnn-add-fisys-dict)
	(fset 'add-wnn-fiusr-dict 'wnn-add-fiusr-dict)
	(fset 'add-wnn-notrans-dict 'wnn-add-notrans-dict)
	(fset 'add-wnn-bmodify-dict 'wnn-add-bmodify-dict)
	(fset 'set-last-is-first-mode 'wnn-set-last-is-first-mode)
	(fset 'set-complex-conv-mode 'wnn-set-complex-conv-mode)
	(fset 'set-okuri-flag 'wnn-set-okuri-flag)
	(fset 'set-prefix-learn-mode 'wnn-set-prefix-learn-mode)
	(fset 'set-suffix-learn-mode 'wnn-set-suffix-learn-mode)
	(fset 'set-common-learn-mode 'wnn-set-common-learn-mode)
	(fset 'set-yuragi-mode 'wnn-set-yuragi-mode)
	(fset 'set-freq-func-mode 'wnn-set-freq-func-mode)
	(fset 'set-numeric-mode 'wnn-set-numeric-mode)
	(fset 'set-alphabet-mode 'wnn-set-alphabet-mode)
	(fset 'set-symbol-mode 'wnn-set-symbol-mode)
	(setq wnn-v3-defined t))))

(defun wnn-get-environment (backend)
  "Return Wnn Environemt for BACKEND.  If none, create new
environment."
  (let ((env (wnn-search-environment backend))
	proc error)
    (or env
	(unwind-protect
	    (let* ((language (wnn-backend-get-language backend))
		   (server-info (wnn-server-get-info language))
		   (server-type (wnn-server-type server-info))
		   version specs)
	      (setq proc (wnn-open server-info)
		    version (cdr proc)
		    proc (car proc)
		    wnn-envspec-list nil)
	      (condition-case err
		  (let ((wnn-server-type server-type)
			(wnn-wnn8-server (eq version 'wnn8))
			(wnn-wnn7-server (eq version 'wnn7))
			(wnn-wnn6-server (eq version 'wnn6)))
		    (if wnn-use-v3-eggrc
			(wnn-v3-eggrc-defines))
		    (egg-load-startup-file 'wnn language))
		(egg-error
		 (setq error err)
		 (signal (car error) (cdr error))))
	      (setq specs wnn-envspec-list)
	      (while specs
		(wnn-create-environment proc server-type version (car specs))
		(setq specs (cdr specs)))
	      (setq env (wnn-search-environment backend)))
	  (if (and proc (null env))
	      (progn
		(wnnrpc-close proc)
		(if error
		    (signal (car error) (cdr error))
		  (egg-error 'wnn-fail-make-env))))))))

;; Create a new environment in the conversion server, if necessary.
(defun wnn-create-environment (proc server-type version spec)
  (let* ((save-inhibit-quit inhibit-quit)
	 (inhibit-quit t)
	 (name (wnn-make-env-name spec))
	 (backend (wnn-envspec-backend spec))
	 (tankan (wnn-envspec-tankan spec))
	 (sticky (wnn-envspec-sticky spec))
	 (parameter (wnn-envspec-param spec))
	 (fzk (wnn-envspec-fuzokugo spec))
	 (dic-list (wnn-envspec-dic-list spec))
	 (fi-dic-list (wnn-envspec-fi-dic-list spec))
	 (autolearn-dic-list (wnn-envspec-autolearn-dic-list spec))
	 exist env-id env fid cvmask param mode type dic-spec)
    (condition-case err
	(progn
	  (setq exist (wnnrpc-env-exist proc name)
		env-id (wnnrpc-connect proc name))
	  (if (< env-id 0)
	      (egg-error "%s" (wnnrpc-get-error-message (- env-id))))
	  (setq env (wnnenv-create proc env-id server-type version
				   backend tankan name))
	  (cond
	   ((or wnn-force-set-environment (= exist 0))
	    (let ((inhibit-quit save-inhibit-quit))
	      (and fzk
		   (or (< (setq fid (wnn-open-file env fzk)) 0)
		       (< (setq fid (wnnrpc-set-fuzokugo-file env fid)) 0))
		   (message "%s" (wnnrpc-get-error-message (- fid))))
	      (while fi-dic-list
		(wnn-set-dictionary env t (car fi-dic-list))
		(setq fi-dic-list (cdr fi-dic-list)))
	      (while dic-list
		(wnn-set-dictionary env nil (car dic-list))
		(setq dic-list (cdr dic-list)))
	      (while autolearn-dic-list
		(setq type (caar autolearn-dic-list)
		      dic-spec (cdar autolearn-dic-list)
		      autolearn-dic-list (cdr autolearn-dic-list)
		      mode (wnn-set-autolearn-dictionary env type dic-spec))
		(if mode
		    (if (eq type (WNN-const NOTRANS_LEARN))
			(progn
			  (wnnenv-set-notrans env mode)
			  (wnn-envspec-set-conv-param-muhenkan spec mode))
		      (wnnenv-set-bmodify env mode)
		      (wnn-envspec-set-conv-param-bunsetsugiri spec mode))))
	      (if parameter
		  (wnnrpc-set-conversion-parameter env parameter))
	      (setq cvmask (wnn-envspec-conv-vmask spec)
		    param (wnn-envspec-conv-param spec))
	      (if (/= cvmask 0)
		  (if (or (wnnenv-is-wnn7 env) (wnnenv-is-wnn8 env))
		      (wnn7rpc-set-conversion-env-param env cvmask param)
		    (wnnrpc-set-conversion-env-param env cvmask param)))))
	   ((or (eq version 'wnn6) (eq version 'wnn7) (eq version 'wnn8))
	    (wnnenv-set-bmodify env (wnn-get-autolearning-dic-mode
				     env (WNN-const BMODIFY_LEARN)))
	    (wnnenv-set-notrans env (wnn-get-autolearning-dic-mode
				     env (WNN-const NOTRANS_LEARN)))))
	  (cond
	   ((eq (wnnenv-get-server-type env) 'jserver)
	    (wnn-set-hinshi env 'noun "$BL>;l(B")
	    (when (or (wnnenv-is-wnn6 env) (wnnenv-is-wnn7 env) (wnnenv-is-wnn8 env))
	      (wnn-set-hinshi env 'settou "$B@\F,8l(B($B$*(B)")
	      (wnn-set-hinshi env 'rendaku "$BO"By(B")))
	   ((eq (wnnenv-get-server-type env) 'cserver)
	    (wnn-set-hinshi env 'noun "$AFUM(C{(B"))
	   ((eq (wnnenv-get-server-type env) 'tserver)
	    (wnn-set-hinshi env 'noun "$(G_[]WGX(B"))
	   ((eq (wnnenv-get-server-type env) 'kserver)
	    (wnn-set-hinshi env 'noun "$(CY#^r(B")))
	  (if sticky
	      (wnnrpc-make-env-sticky env)
	    (wnnrpc-make-env-unsticky env))
	  (setq wnn-environments (nconc wnn-environments (list env))))
      ((egg-error quit)
       (if (eq (car err) 'egg-error)
	   (message "%s" (nth 1 err)))
       (if env
	   (progn
	     (wnnrpc-disconnect env)
	     (setq wnn-environments (delq env wnn-environments))))
       (if (eq (car err) 'quit)
	   (signal 'quit (cdr err)))))))

(defun wnn-make-env-name (spec)
  (let ((env-type (wnn-envspec-env-type spec)))
    (concat wnn-user-name (if env-type (symbol-name env-type) ""))))

(defun wnn-set-hinshi (env sym name)
  (let ((hinshi (wnnrpc-hinshi-number (wnnenv-get-proc env) name)))
    (if (>= hinshi 0)
	(wnnenv-set-hinshi env sym hinshi))))

(defsubst wnn-dicinfo-entry (info)       (aref info 0))
(defsubst wnn-dicinfo-id (info freq)     (aref info (+ 1 freq)))
(defsubst wnn-dicinfo-mode (info freq)   (aref info (+ 3 freq)))
(defsubst wnn-dicinfo-enable (info)      (aref info 5))
(defsubst wnn-dicinfo-nice (info)        (aref info 6))
(defsubst wnn-dicinfo-reverse (info)     (aref info 7))
(defsubst wnn-dicinfo-comment (info)     (aref info 8))
(defsubst wnn-dicinfo-name (info freq)   (aref info (+ 9 freq)))
(defsubst wnn-dicinfo-passwd (info freq) (aref info (+ 11 freq)))
(defsubst wnn-dicinfo-type (info)        (aref info 13))
(defsubst wnn-dicinfo-words (info)       (aref info 14))
(defsubst wnn-dicinfo-local (info freq)  (aref info (+ 15 freq)))

(defun wnn-get-autolearning-dic-mode (env type)
  (let* ((dic (wnnrpc-get-autolearning-dic env type))
	 (info (and (> dic 0) (wnnrpc-get-dictionary-info env (1- dic)))))
    (if (vectorp (car-safe info))
	(wnn-dicinfo-mode (car info) 0)
      (WNN-const DIC_RDONLY))))

(defun wnn-get-dictionary-list-with-environment (env)
  (if (or (wnnenv-is-wnn6 env) (wnnenv-is-wnn7 env) (wnnenv-is-wnn8 env))
      (wnnrpc-get-fi-dictionary-list-with-environment env
						      (WNN-const DIC_NO_TEMPS))
    (wnnrpc-get-dictionary-list-with-environment env)))

(defun wnn-save-dictionaries (env)
  (let ((dic-list (wnn-get-dictionary-list-with-environment env))
	(result 0) info freq fid name local-name)
    (while dic-list
      (setq info (car dic-list)
	    dic-list (cdr dic-list)
	    freq 0)
      (while (<= freq 1)
	(setq fid (wnn-dicinfo-id info freq)
	      name (wnn-dicinfo-name info freq))
	(if (and (> fid 0) (= (wnn-dicinfo-mode info freq) 0))
	    (cond
	     ((= (wnn-dicinfo-local info freq) 1)
	      (wnnrpc-write-file env fid name))
	     ((setq local-name (wnnenv-get-client-file env name))
	      (wnnrpc-file-receive env fid local-name))
	     ((and (setq local-name (wnn-file-loaded-client env name fid))
		   (file-writable-p local-name))
	      (wnnrpc-file-receive env fid local-name))))
	(setq freq (1+ freq))))))

(defun wnn-file-loaded-client (env name fid)
  (let ((len (length wnn-system-name))
	local-name)
    (and (> (length name) len)
	 (equal (substring name 0 len) wnn-system-name)
	 (prog1
	     (wnn-client-file-p (substring name len))
	   (setq local-name (wnn-client-filename (substring name len))))
	 (= (wnnrpc-file-loaded-local (wnnenv-get-proc env) local-name t) fid)
	 local-name)))

(defun wnn-word-inspection (bunsetsu)
  (let ((env (wnn-bunsetsu-get-env bunsetsu))
	(converted (wnn-get-bunsetsu-converted bunsetsu))
	(yomi (wnn-bunsetsu-get-yomi bunsetsu))
	(fuzokugo (wnn-bunsetsu-get-fuzokugo bunsetsu))
	(hinshi-no (wnn-bunsetsu-get-hinshi bunsetsu))
	(dic-no (wnn-bunsetsu-get-dic-no bunsetsu))
	(entry (wnn-bunsetsu-get-entry bunsetsu))
	(now (wnn-bunsetsu-get-right-now bunsetsu))
	(freq (wnn-bunsetsu-get-freq bunsetsu))
	(evaluation (wnn-bunsetsu-get-evaluation bunsetsu))
	(evaluation-dai (or (wnn-bunsetsu-get-dai-evaluation bunsetsu) "---"))
	(kangovect (wnn-bunsetsu-get-kangovect bunsetsu))
	hinsi dic)
    (setq hinshi (wnnrpc-hinshi-name (wnnenv-get-proc env) hinshi-no))
    (setq dic (if (>= dic-no 0)
		  (wnn-dict-name (car (wnnrpc-get-dictionary-info env dic-no)))
		(egg-get-message 'wnn-pseud-bunsetsu)))
    (message "%s %s+%s(%s %s:%s Freq:%s%s) S:%s D:%s V:%s "
	     converted yomi fuzokugo hinshi dic entry
	     (if (= now 1) "*" " ") freq evaluation evaluation-dai kangovect)))

;;; not implemented yet (NIY)
(defun wnn-delete-dictionary ()
  (dj-delete-dic XXX))

;;; NIY, might never be implemented
(defun wnn-server-inspect ())

;;; NIY
(defun wnn-get-conversion-parameter ()
  (js-get-parameter))

;;; Dictionary management (word registration) is not implemented yet.

(defun wnn-find-dictionary-by-id (id dic-list)
  (catch 'return
    (while dic-list
      (let ((dic (car dic-list)))
	(if (= (wnndic-get-id dic) id)
	    (throw 'return dic)
	  (setq dic-list (cdr dic-list)))))))

(defun wnn-dict-name (dic-info)
  (let ((comment (wnndic-get-comment dic-info))
	(name (wnndic-get-dictname dic-info)))
    (cond ((null (string= comment "")) comment)
	  ((wnn-client-file-p name) name)
	  (t (file-name-nondirectory name)))))

(defun wnn-list-writable-dictionaries-byname (env)
  (let ((dic-list (wnn-get-dictionary-list-with-environment env))
	(w-id-list (wnnrpc-get-writable-dictionary-id-list env)))
    (cond ((numberp w-id-list)
	   (egg-error "%s" (wnnrpc-get-error-message (- w-id-list))))
	  ((null w-id-list)
	   (egg-error 'wnn-no-writable-d))
	  (t
	   (delq nil
		 (mapcar (lambda (id)
			   (let ((dic (wnn-find-dictionary-by-id id dic-list)))
			     (and dic (cons (wnn-dict-name dic) dic))))
			 w-id-list))))))

(defun wnn-word-registration (backend kanji yomi)
  (let (env dic dic-id hinshi result)
    (if (or (null (eq (egg-get-language 0 kanji)
		      (wnn-backend-get-converted-language backend)))
	    (next-single-property-change 0 'egg-lang kanji)
	    (null (eq (egg-get-language 0 yomi)
		      (wnn-backend-get-source-language backend)))
	    (next-single-property-change 0 'egg-lang yomi))
	(egg-error "word registration: invalid character")
      (setq env (wnn-get-environment backend)
	    dic (wnn-dictionary-select env)
	    dic-id (wnndic-get-id dic)
	    hinshi (wnn-hinshi-select env dic-id)
	    result (wnnrpc-hinshi-number (wnnenv-get-proc env) hinshi))
      (or (< result 0)
	  (setq result (wnnrpc-add-word env dic-id yomi kanji "" result 0)))
      (if (>= result 0)
	  (list hinshi (wnn-dict-name dic))
	(egg-error (wnnrpc-get-error-message (- result)))))))

(defun wnn-dictionary-select (env)
  (menudiag-select (list 'menu
			 (egg-get-message 'wnn-register-1)
			 (wnn-list-writable-dictionaries-byname env))))

(defun wnn-hinshi-select (env dic-id)
  (menudiag-select (wnn-make-hinshi-menu
		    env dic-id "/"
		    (egg-get-message 'wnn-register-2))))

(defun wnn-make-hinshi-menu (env dic-id hinshi prompt)
  (let ((hinshi-list (wnnrpc-get-hinshi-list env dic-id hinshi)))
    (if (numberp hinshi-list)
	(egg-error "%s" (wnnrpc-get-error-message (- hinshi-list)))
      (list 'menu
	    (format (if (equal hinshi "/") "%s:" "%s[%s]:")
		    prompt
		    (substring hinshi 0 (1- (length hinshi))))
	    (mapcar (lambda (h)
		      (if (= (aref h (1- (length h))) ?/)
			  (cons h (wnn-make-hinshi-menu env dic-id h prompt))
			h))
		    hinshi-list)))))

;;; setup

(load "egg/wnnrpc")
(run-hooks 'wnn-load-hook)

;;;###autoload
(defun egg-activate-wnn (&rest arg)
  "Activate Wnn backend of Tamago 4."
  (apply 'egg-mode (append arg wnn-backend-alist)))

;;; egg/wnn.el ends here.

;;; egg/anthyipc.el --- ANTHY IPC Support (low level interface) in Egg
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
(defvar anthy-egg-anthyipc-version "20070419")

(eval-when-compile
  (defvar anthy-egg-proc)
  (defvar anthy-egg-version)
  (defvar anthy-egg-el-version)
  (defvar anthy-egg-anthyipc-version)
  (defvar anthy-egg-anthy-agent-version))

(defmacro anthyipc-call-with-proc (proc vlist send-expr &rest receive-exprs)
  `(let* ((proc ,proc)
	  (buffer (process-buffer proc))
	  ,@vlist)
     (if (and (eq (process-status proc) 'run)
	      (buffer-live-p buffer))
	 (save-excursion
	   (set-buffer buffer)
	   (erase-buffer)
	   ,send-expr
	   (goto-char (point-max))
	   (process-send-region proc (point-min) (point-max))
	   ,@receive-exprs)
       (egg-error "process %s was killed" proc))))

(defsubst anthyipc-wait-line ()
  (let ((start (point)))
    (while (not (search-forward "\n" nil 1))
      (accept-process-output anthy-egg-proc 1000)
      (goto-char start))
    (goto-char start)))

(defsubst anthyipc-accept-ok ()
  (anthyipc-wait-line)
  (if (eq (char-after) ?+)
      ;; "+OK"
      (goto-char (point-max))
    (egg-error "protocol error: %s" (buffer-substring (point) (point-max)))))

(defsubst anthyipc-accept-number ()
  (anthyipc-wait-line)
  (if (eq (char-after)  ?+)
      ;; "+OK <number>"
      (progn
	(forward-char 4)
	(prog1
	    (read (current-buffer))
	  (goto-char (point-max))))
    (egg-error "protocol error: %s" (buffer-substring (point) (point-max)))))

(if (equal (split-string " " " ") '("" ""))
    (defalias 'egg-anthy-split-string 'split-string)
  (defun egg-anthy-split-string (string separators)
    "Split STRING from Emacs-22."
    (let ((keep-nulls t)
	  (rexp separators)
	  (start 0)
	  notfirst
	  (list nil))
      (while (and (string-match rexp string
				(if (and notfirst
					 (= start (match-beginning 0))
					 (< start (length string)))
				    (1+ start) start))
		  (< start (length string)))
	(setq notfirst t)
	(if (or keep-nulls (< start (match-beginning 0)))
	    (setq list
		  (cons (substring string start (match-beginning 0))
			list)))
	(setq start (match-end 0)))
      (if (or keep-nulls (< start (length string)))
	  (setq list
		(cons (substring string start)
		      list)))
      (nreverse list))))

(defsubst anthyipc-read-string-type1 ()
  (if (eq (char-after) ?\ )
      (forward-char 1))
  (cond
   ((looking-at "\\([^ ]+\\) \\([^ \n]+\\)\n")
    ;; スペースがひとつ
    ;; 変換後(スペース)読みがな の一般的なパターン (高速化のため)
    (prog1
	(cons (match-string-no-properties 1) (match-string-no-properties 2))
      (end-of-line)))
   ((not (looking-at ".* .*$"))
    ;; スペースがひとつも無かったらエラー
    (egg-error "protocol error: %s"
	       (buffer-substring-no-properties (point) (line-end-position))))
   (t
    ;; 複数個のスペースがあるので解析
    (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
	   (elements (egg-anthy-split-string line " "))
	   (sum (length elements)))
      (if (or (< sum 3) (= (% sum 2) 1))
	  ;; 偶数個のスペースは変換後にスペースが入っているときだけなので、
	  ;; 最後のスペース以降を読みがなにする <= 自信無し
	  (if (looking-at "\\(.+\\) \\([^ \n]+\\)\n")
	      (prog1
		  (cons (match-string-no-properties 1) (match-string-no-properties 2))
		(end-of-line))
	    (egg-error "protocol error: %s" line))
	(let* ((div (/ sum 2))
	       conv yomi yomilst)
	  (setq yomilst (nthcdr div elements))
	  (setcdr (nthcdr (1- div) elements) nil)
	  (setq conv (mapconcat 'identity elements " "))
	  (setq yomi (mapconcat 'identity yomilst " "))
	  ;; 奇数個のスペースは
	  ;; alphabet/ａｌｐｈａｂｅｔ+スペース, スペースのみの変換の結果
	  ;; 読みと変換後でスペースの数が変わることはない <= 自信無し
	  (if (string= conv yomi)
	      (prog1
		  (cons conv yomi)
		(end-of-line))
	    ;; 変換後の単語にスペースが偶数個含まれたときは最後のスペース以降
	    ;; を読みがなにする <= 自信無し
	    (if (looking-at "\\(.+\\) \\([^ \n]+\\)\n")
		(prog1
		    (cons (match-string-no-properties 1) (match-string-no-properties 2))
		  (end-of-line))
	      (egg-error "protocol error: %s" line)))))))))
   
(defsubst anthyipc-read-string-type2 ()
  (prog1
      (buffer-substring-no-properties (point) (line-end-position))
    (end-of-line)))

(defsubst anthyipc-egg-make-bunsetsu (env source converted seg-no)
  (egg-bunsetsu-create
   'anthy-egg-conversion-backend
   (vector env source converted nil 0 seg-no)))

(defun anthyipc-accept-segments (env seg-no-orig)
  (anthyipc-wait-line)
  (if (eq (char-after) ?+)
      (progn
	(forward-char 1)
	(if (eq (char-after) ?O)
	    ;; "+OK"
	    (progn
	      (goto-char (point-max))
	      t)
	  ;; "+DATA <seg-no> <num-segments-removed> <num-segments-inserted>"
	  ;; "<num-candidates> <converted> <yomi>"*N
	  ;; ""
	  ;;
	  (forward-char 5)
	  (let* ((seg-no (read (current-buffer)))
		 (num-segments-removed (read (current-buffer)))
		 (num-segments-inserted (read (current-buffer)))
		 (segment-list nil)
		 (in-loop t)
		 (i seg-no))
	    (while in-loop
	      (forward-char 1)
	      (anthyipc-wait-line)
	      (if (eq (char-after) ?\n)
		  (setq in-loop nil)
		(let* ((num-candidates (read (current-buffer)))
		       (conv-source (anthyipc-read-string-type1))
		       (converted (car conv-source))
		       (source (cdr conv-source))
		       (segment (anthyipc-egg-make-bunsetsu env source converted i)))
		  (setq i (1+ i))
		  (setq segment-list (cons segment segment-list)))))
	    ;; XXX check if seg-no == seg-no-orig
	    ;; XXX check inserted and length of segment-list???
	    (forward-char 1)
	    (cons seg-no (cons num-segments-removed (reverse segment-list))))))
    (egg-error "protocol error: %s" (buffer-substring (point) (point-max)))))

(defun anthyipc-accept-candidates ()
  (anthyipc-wait-line)
  (if (eq (char-after) ?+)
      (progn
	;; "+DATA <offset> <num-candidates>"
	;; "<converted>"*N
	;; ""
	(forward-char 6)
	(let* ((offset (read (current-buffer)))
	       (num-candidates (read (current-buffer)))
	       (candidate-list nil)
	       (in-loop t))
	  (while in-loop
	    (forward-char 1)
	    (anthyipc-wait-line)
	    (if (eq (char-after) ?\n)
		(setq in-loop nil)
	      (let ((candidate (anthyipc-read-string-type2)))
		(setq candidate-list (cons candidate candidate-list)))))
	  ;; XXX check num-candidates and length of candidate-list???
	  (forward-char 1)
	  (cons offset (reverse candidate-list))))
    (egg-error "protocol error: %s" (buffer-substring (point) (point-max)))))

(defun anthyipc-get-greeting (proc)
  (anthyipc-call-with-proc proc ()
    nil
    (anthyipc-wait-line)
    (goto-char (point-min))
    (when (looking-at "^Anthy (Version \\([^)]+\\))")
      (setq anthy-egg-anthy-agent-version (match-string 1)))
    (setq anthy-egg-version
	  (concat anthy-egg-el-version
		  "/"
		  anthy-egg-anthyipc-version
		  "/"
		  anthy-egg-anthy-agent-version))
    (unless (window-minibuffer-p (selected-window))
      (message (buffer-substring (point-min) (1- (point-max)))))))

(defun anthyipc-new-context (proc)
  (anthyipc-call-with-proc proc ()
    (insert "NEW-CONTEXT INPUT=#18 OUTPUT=#18\n")
    (anthyipc-accept-number)))

(defun anthyipc-release-context (proc cont)
  (anthyipc-call-with-proc proc ()
    (insert (format "RELEASE-CONTEXT %d\n" cont))
    (anthyipc-accept-ok)))

;; Returns list of bunsetsu
(defun anthyipc-convert (proc cont yomi)
  (anthyipc-call-with-proc proc ()
    (insert (format "CONVERT %d %s\n" cont yomi))
    (let ((r (anthyipc-accept-segments cont 0)))
      (cdr (cdr r)))))

(defun anthyipc-commit (proc cont cancel)
  (anthyipc-call-with-proc proc ()
    (insert (format "COMMIT %d %d\n" cont cancel))
    (anthyipc-accept-ok)))

;;; Returns list of candidate
(defconst anthy-egg-max-candidates 9999)
(defun anthyipc-get-candidates (proc cont seg-no)
  (anthyipc-call-with-proc proc ()
    (insert
     (format "GET-CANDIDATES %d %d %d %d\n" cont seg-no 0 anthy-egg-max-candidates))
    (let ((r (anthyipc-accept-candidates)))
      (cdr r))))

;;; Returns segments
(defun anthyipc-select-candidate (proc cont seg-no candidate-no)
  (anthyipc-call-with-proc proc ()
    (insert (format "SELECT-CANDIDATE %d %d %d\n" cont seg-no candidate-no))
    (anthyipc-accept-segments cont seg-no)))

;;; Returns segments
(defun anthyipc-resize-segment (proc cont seg-no inc-dec)
  (anthyipc-call-with-proc proc ()
    (insert (format "RESIZE-SEGMENT %d %d %d\n" cont seg-no inc-dec))
    (cddr (anthyipc-accept-segments cont seg-no))))

;;; egg/anthyipc.el ends here.

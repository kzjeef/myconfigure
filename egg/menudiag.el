;;; menudiag.el --- Minibuffer Menu System

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
;; Inspired by the menu subsystem of EGG V3.0
;;
;; Completely different implementation, using keymap and recursive edit.

;;; Code:

;;
;; Data structure of MENU
;;
;; <menu> ::= ( menu <prompt> <item-list> )
;; <prompt> ::= STRING
;; <item-list> ::= ( <item> ... )
;; <item> ::= <string> | ( <string> . <value> )
;;
;; <value> ::=  <menu> | INTEGER | STRING  (Must *NOT* cons cell)
;;
;;
;
;;
;; <selection-list> ::= ( <line>... )
;; <line>  ::= ( <item>... )
;;

(defgroup menudiag nil
  "Input Translation System of Tamago 4."
  :group 'egg)

(defcustom menudiag-select-without-return nil
  "*Number keys not only goes the item, but also select the item, if non-NIL."
  :group 'menudiag :type 'boolean)

(defvar menudiag-mode-map
  (let ((map (make-sparse-keymap))
	ch)
    (setq ch ?0)
    (while (<= ch ?9)
      (define-key map (char-to-string ch) 'menudiag-goto-item)
      (setq ch (1+ ch)))
    (setq ch ?a)
    (while (<= ch ?z)
      (define-key map (char-to-string ch) 'menudiag-goto-item)
      (setq ch (1+ ch)))
    (setq ch ?A)
    (while (<= ch ?Z)
      (define-key map (char-to-string ch) 'menudiag-goto-item)
      (setq ch (1+ ch)))
    (setq ch ?\C-0)
    (while (<= ch ?\C-9)
      (define-key map (vector ch) 'digit-argument)
      (setq ch (1+ ch)))
    (define-key map [?\C--]  'negative-argument)
    (define-key map [?\C-u]  'universal-argument)
    (define-key map " "      'menudiag-forward-item)
    (define-key map "\C-a"   'menudiag-beginning-of-line)
    (define-key map "\C-e"   'menudiag-end-of-line)
    (define-key map "\M-<"   'menudiag-beginning-of-items)
    (define-key map "\M->"   'menudiag-end-of-items)
    (define-key map "\C-f"   'menudiag-forward-item)
    (define-key map "\C-b"   'menudiag-backward-item)
    (define-key map "\C-n"   'menudiag-next-line)
    (define-key map "\C-p"   'menudiag-previous-line)
    (define-key map "\C-]"   'menudiag-exit)
    (define-key map "\C-g"   'menudiag-exit-one-level)
    (define-key map "\C-l"   'menudiag-redraw)
    (define-key map "\C-m"   'menudiag-select-this-item)
    (define-key map "\M-v"   'menudiag-list-other-window)
    (define-key map "?"      'menudiag-list-other-window)
    (define-key map [return] 'menudiag-select-this-item)
    (define-key map [left]   'menudiag-backward-item)
    (define-key map [right]  'menudiag-forward-item)
    (define-key map [up]     'menudiag-previous-line)
    (define-key map [down]   'menudiag-next-line)
    (define-key map [exit]   'menudiag-exit)
    (define-key map [t]      'undefined)
    map)
  "Keymap for MENU.")

(defun menudiag-menu-p (item)
  (and (consp item) (eq 'menu (car item))))

(defun menudiag-item-string (item)
  (if (stringp item)
      item
    (format "%s" (car item))))

(defun menudiag-item-value (item)
  (if (stringp item)
      item
    (cdr item)))

(defsubst menudiag-item-width (item)
  (+ 4 (string-width (menudiag-item-string item))))

(defun menudiag-make-selection-list (item-list line-width)
  (let ((l nil)
	(line nil)
	(width 0)
	(i 0))
    (while item-list
      (let* ((item (car item-list))
	     (item-width (menudiag-item-width item)))
	(if (and line (or (>= (+ width item-width) line-width)
                          (>= i 36)))
	    (setq l (cons (reverse line) l)
		  line nil
		  width 0
		  i 0))
	(setq line (cons item line)
	      width (+ width (menudiag-item-width item))
	      i (1+ i)
	      item-list (cdr item-list))))
    (if line
	(reverse (cons (reverse line) l))
      (reverse l))))

(defvar menudiag-show-all nil)
(make-variable-buffer-local 'menudiag-show-all)

(defvar menudiag-continuation nil)
(make-variable-buffer-local 'menudiag-continuation)

(defvar menudiag-return-contin nil)
(make-variable-buffer-local 'menudiag-return-contin)

(defvar menudiag-value nil)
(make-variable-buffer-local 'menudiag-value)

(defvar menudiag-done nil)
(make-variable-buffer-local 'menudiag-done)

;; Entry function
(defun menudiag-select (menu &optional list-all continuation return-contin)
  (let ((enable-recursive-minibuffers t))
    (setq menudiag-return-contin return-contin)
    (menudiag-select-internal menu list-all continuation)
    (if (eq menudiag-done t)
	menudiag-value
      (signal 'quit ""))))

(defvar menudiag-line nil)
(make-variable-buffer-local 'menudiag-line)

(defvar menudiag-linepos 0)
(make-variable-buffer-local 'menudiag-linepos)

(defvar menudiag-pos-in-line 0)
(make-variable-buffer-local 'menudiag-pos-in-line)

(defun menudiag-follow-continuation ()
  (let* ((item (car menudiag-continuation))
	 (value (menudiag-item-value item))
	 (pos (menudiag-search-item item)))
    (unless pos
      (error "no such item: %s" (menudiag-item-string item)))
    (menudiag-goto-line (car pos))
    (menudiag-goto-item-internal (cdr pos))
    (when (menudiag-menu-p value)
      (menudiag-select-internal value
				menudiag-show-all
				(cdr menudiag-continuation))
      (menudiag-redraw)
      (when menudiag-done
	(when menudiag-return-contin
	  (setq menudiag-value (cons item menudiag-value)))
	(setq unread-command-events (cons 'exit unread-command-events))))))

(defvar menudiag-minibuffer-list nil)
(defvar menudiag-variable-alist nil)

(defmacro menudiag-send-variables (&rest args)
  `(setq menudiag-variable-alist
	 (list ,@(mapcar (lambda (var) `(cons ',var ,var)) args))))

(defmacro menudiag-send-variables-with-value (&rest args)
  `(setq menudiag-variable-alist
	 ,(let ((alist (list 'list)))
	    (while args
	      (nconc alist `((cons ',(car args) ,(cadr args))))
	      (setq args (cddr args)))
	    alist)))

(defun menudiag-receive-variables ()
  (while menudiag-variable-alist
    (set (caar menudiag-variable-alist) (cdar menudiag-variable-alist))
    (setq menudiag-variable-alist (cdr menudiag-variable-alist))))

(defvar menudiag-minibuf-prompt nil)
(make-variable-buffer-local 'menudiag-minibuf-prompt)

(defvar menudiag-current-items nil)
(make-variable-buffer-local 'menudiag-current-items)

(defvar menudiag-selection-list nil)
(make-variable-buffer-local 'menudiag-selection-list)

(defun menudiag-minibuffer-hook ()
  (interactive)
  (remove-hook 'minibuffer-setup-hook 'menudiag-minibuffer-hook)
  (setq menudiag-minibuffer-list (cons (current-buffer)
				       menudiag-minibuffer-list))
  (buffer-disable-undo)
  (menudiag-receive-variables)
  (menudiag-beginning-of-items)
  (when menudiag-continuation
    (menudiag-follow-continuation))
  (when (and menudiag-show-all (null menudiag-done))
    (menudiag-list-other-window)))

(defun menudiag-select-internal (menu all &optional continuation)
  (menudiag-send-variables-with-value
   menudiag-value          menudiag-value
   menudiag-continuation   continuation
   menudiag-return-contin  menudiag-return-contin
   menudiag-show-all       all
   menudiag-minibuf-prompt (cadr menu)
   menudiag-current-items  (car (cddr menu))
   menudiag-selection-list (menudiag-make-selection-list 
			    (car (cddr menu))
			    (- (window-width (minibuffer-window))
			       (string-width (cadr menu)))))
  (add-hook 'minibuffer-setup-hook 'menudiag-minibuffer-hook)
  (unwind-protect
      (progn
	(read-from-minibuffer "" "" menudiag-mode-map)
	(menudiag-receive-variables))
    (setq menudiag-minibuffer-list (cdr menudiag-minibuffer-list))
    (remove-hook 'minibuffer-setup-hook 'menudiag-minibuffer-hook)
    ;; for egg's point-enterd/left hooks
    (save-excursion
      (goto-char (point-min)))))

(defun menudiag-make-menu-formatted-string (item-list)
  (let ((i -1))
    (mapconcat
     (function (lambda (item)
		 (setq i (1+ i))
		 (format "  %c.%s" (menudiag-item-num-to-char i) 
                           (menudiag-item-string item))))
     item-list "")))


;; ITEM No --> Character
(defun menudiag-item-num-to-char (num)
  (let ((char))
    (cond ((<= num 9)
           (setq char (+ ?0 num)))
          (t
           (setq char (+ ?a (- num 10))))
          )
    char))

;; Character --> ITEM No
(defun menudiag-char-to-item-num (ch)
  (let ((num))
    (cond ((and (<= ?0 ch) (<= ch ?9))
           (setq num (- ch ?0)))
          ((and (<= ?a ch) (<= ch ?z))
           (setq num (+ 10 (- ch ?a))))
          ((and (<= ?A ch) (<= ch ?Z))
           (setq num (+ 10 (- ch ?A))))
          (t (setq num 1000)))
    num))

(defun menudiag-check-current-menu ()
  (or (eq (current-buffer) (car menudiag-minibuffer-list))
      (error "menudiag: not current menu")))

(defun menudiag-goto-item ()
  (interactive)
  (menudiag-check-current-menu)
  (let ((ch last-command-char)
	(n 0))
    (setq n (menudiag-char-to-item-num ch))
    (if (>= n (length menudiag-line))
	(error "No such item")
      (menudiag-goto-item-internal n)
      (if menudiag-select-without-return
	  (menudiag-select-this-item)))))

(defun menudiag-goto-item-internal (n)
  (let ((p (+ (length menudiag-minibuf-prompt) 3))
	(i 0))
    (setq menudiag-pos-in-line n)
    (while (< i menudiag-pos-in-line)
      (setq p (+ p (length (menudiag-item-string (nth i menudiag-line))) 4))
      (setq i (1+ i)))
    (goto-char p)))

(defun menudiag-beginning-of-items ()
  (interactive)
  (menudiag-check-current-menu)
  (menudiag-goto-line 0)
  (menudiag-beginning-of-line))

(defun menudiag-end-of-items ()
  (interactive)
  (menudiag-check-current-menu)
  (menudiag-goto-line (1- (length menudiag-selection-list)))
  (menudiag-end-of-line))

(defun menudiag-beginning-of-line ()
  (interactive)
  (menudiag-check-current-menu)
  (menudiag-goto-item-internal 0))

(defun menudiag-end-of-line ()
  (interactive)
  (menudiag-check-current-menu)
  (menudiag-goto-item-internal (1- (length menudiag-line))))

;; Should retain compatibility.  Must.
;;
;;(defun menudiag-forward-item ()
;;  (interactive)
;;  (if (< pos-in-line (1- (length line)))
;;      (menudiag-goto-item-internal (1+ pos-in-line))
;;    (if (>= linepos (1- (length selection-list)))
;;	(signal 'end-of-buffer "")
;;      (menudiag-goto-line (1+ linepos))
;;      (menudiag-beginning-of-line))))
;;
;;(defun menudiag-backward-item ()
;;  (interactive)
;;  (if (< 0 pos-in-line)
;;      (menudiag-goto-item-internal (1- pos-in-line))
;;    (if (< linepos 1)
;;	(signal 'beginning-of-buffer "")
;;      (menudiag-goto-line (1- linepos))
;;      (menudiag-end-of-line))))
;;
;;(defun menudiag-goto-line (n)
;;  (if (or (>= n (length selection-list)) (< n 0))
;;      (ding)
;;    (setq line (nth n selection-list)
;;	  linepos n)
;;    (delete-region (point-min) (point-max))
;;    (insert (menudiag-make-menu-formatted-string line))))
;;

(defun menudiag-forward-item (n)
  (interactive "p")
  (menudiag-forward-item-internal n))

(defun menudiag-backward-item (n)
  (interactive "p")
  (menudiag-forward-item-internal (- n)))

(defun menudiag-forward-item-internal (n)
  (menudiag-check-current-menu)
  (setq n (+ n menudiag-pos-in-line))
  (while (< n 0)
    (menudiag-goto-line (1- menudiag-linepos))
    (setq n (+ n (length menudiag-line))))
  (while (>= n (length menudiag-line))
    (setq n (- n (length menudiag-line)))
    (menudiag-goto-line (1+ menudiag-linepos)))
  (menudiag-goto-item-internal n))

(defun menudiag-goto-line (n)
  (let ((len (length menudiag-selection-list)))
    (when (< n 0)
      (setq n (+ (% n len) len)))
    (when (>= n len)
      (setq n (% n len)))
    (setq menudiag-line (nth n menudiag-selection-list)
	  menudiag-linepos n)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert menudiag-minibuf-prompt
	      (menudiag-make-menu-formatted-string menudiag-line))
      (set-text-properties (point-min) (point-max) '(read-only t)))))

(defun menudiag-next-line (n)
  (interactive "p")
  (menudiag-next-line-internal n))

(defun menudiag-previous-line (n)
  (interactive "p")
  (menudiag-next-line-internal (- n)))

(defun menudiag-next-line-internal (n)
  (menudiag-check-current-menu)
  (menudiag-goto-line (+ menudiag-linepos n))
  (if (< menudiag-pos-in-line (length menudiag-line))
      (menudiag-goto-item-internal menudiag-pos-in-line)
    (menudiag-end-of-line)))

(defun menudiag-redraw ()
  (interactive)
  (menudiag-check-current-menu)
  (menudiag-goto-line menudiag-linepos)
  (menudiag-goto-item-internal menudiag-pos-in-line))

(defun menudiag-exit-one-level ()
  (interactive)
  (menudiag-check-current-menu)
  (menudiag-exit-minibuffer))

(defun menudiag-exit ()
  (interactive)
  (menudiag-check-current-menu)
  (unless menudiag-done
    (setq menudiag-done 'quit))
  (menudiag-exit-minibuffer))

(defun menudiag-select-this-item (&optional all)
  (interactive)
  (menudiag-check-current-menu)
  (let* ((item (nth menudiag-pos-in-line menudiag-line))
	 (v (menudiag-item-value item)))
    (if (menudiag-menu-p v)
	(progn
	  (menudiag-restore-window)
	  (menudiag-select-internal v all)
	  (menudiag-redraw)
	  (cond (menudiag-done
		 (when menudiag-return-contin
		   (setq menudiag-value (cons item menudiag-value)))
		 (menudiag-exit-minibuffer))
		(all
		 (menudiag-list-other-window))))
      (setq menudiag-value (if menudiag-return-contin
			       (list item)
			     (menudiag-item-value item))
	    menudiag-done t)
      (menudiag-exit-minibuffer))))

(defun menudiag-search-item (item)
  (let ((selection-list menudiag-selection-list)
	(line 0)
	rest)
    (while (and selection-list
		(null (setq rest (memq item (car selection-list)))))
      (setq selection-list (cdr selection-list)
	    line (1+ line)))
    (and selection-list
	 (cons line (- (length (car selection-list)) (length rest))))))

(defconst menudiag-selection-map
  (let ((map (make-sparse-keymap))
	(ch ?0))
    (while (<= ch ?9)
      (define-key map (char-to-string ch) 'menudiag-selection-goto)
      (setq ch (1+ ch)))
    (define-key map "q"            'menudiag-retun-to-minibuf)
    (define-key map "\C-b"         'previous-completion)
    (define-key map "\M-b"         'previous-completion)
    (define-key map "\C-f"         'next-completion)
    (define-key map "\M-f"         'next-completion)
    (define-key map " "            'next-completion)
    (define-key map "\C-g"         'menudiag-selection-exit-one-level)
    (define-key map "\C-m"         'menudiag-choose-item)
    (define-key map "\C-]"         'menudiag-selection-exit)
    (define-key map "\177"         'menudiag-selection-goto-delete)
    (define-key map [delete]       'menudiag-selection-goto-delete)
    (define-key map [backspace]    'menudiag-selection-goto-delete)
    (define-key map [right]        'next-completion)
    (define-key map [left]         'previous-completion)
    (define-key map [return]       'menudiag-choose-item)
    (define-key map [mouse-2]      'menudiag-mouse-choose-item)
    map)
  "keymap for menu selection mode")

(defvar menudiag-window-conf nil)
(make-variable-buffer-local 'menudiag-window-conf)

(defvar menudiag-selection-buffer nil)
(make-variable-buffer-local 'menudiag-selection-buffer)

(defvar menudiag-selection-main-buffer nil)
(make-variable-buffer-local 'menudiag-selection-main-buffer)

(defun menudiag-selection-mode ()
  (kill-all-local-variables)
  (make-local-variable 'inhibit-read-only)
  (setq buffer-read-only t
	inhibit-read-only nil)
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'menudiag-selection-align-to-item nil t)
  (use-local-map menudiag-selection-map)
  (setq mode-name "Menudiag Selection")
  (setq major-mode 'menudiag-selection-mode))

(defun menudiag-max-item-width (items)
  (apply 'max (mapcar 'menudiag-item-width items)))

(defun menudiag-buffer-show-function ()
  (menudiag-receive-variables)
  (let* ((items menudiag-current-items)
	 (digits (length (number-to-string (length items))))
	 (form (concat "%" (number-to-string digits) "d. %s"))
	 (columns (max 1 (/ (window-width (selected-window))
			    (+ digits (menudiag-max-item-width items)))))
	 (width (/ (window-width (selected-window)) columns))
	 (col 0) (n 0) str p)
    (insert " ")
    (while items
      (setq p (point)
	    str (format form n (menudiag-item-string (car items))))
      (insert str)
      (set-text-properties p (point) '(mouse-face highlight))
      (setq col (1+ col)
	    n (1+ n)
	    items (cdr items))
      (if items
	  (if (/= col columns)
	      (insert (make-string (- width (string-width str)) ?\ ))
	    (insert "\n ")
	    (setq col 0))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (menudiag-selection-mode)))

(defun menudiag-buffer-name (prompt)
  (let ((len (1- (length prompt))))
    (generate-new-buffer-name
     (if (= (aref prompt len) ?:) (substring prompt 0 len) prompt))))

(defun menudiag-list-other-window ()
  (interactive)
  (menudiag-check-current-menu)
  (let ((window (and menudiag-selection-buffer
		     (get-buffer-window menudiag-selection-buffer))))
    (if window
	(select-window window)
      (let ((temp-buffer-show-hook 'menudiag-buffer-show-function)
	    (main-buf (current-buffer))
	    (selection-list menudiag-selection-list)
	    (linepos menudiag-linepos)
	    (n (1+ menudiag-pos-in-line)))
	(setq menudiag-window-conf (current-window-configuration))
	(menudiag-send-variables menudiag-current-items)
	(with-output-to-temp-buffer
	    (menudiag-buffer-name menudiag-minibuf-prompt)
	  (setq menudiag-selection-buffer standard-output))
	(switch-to-buffer-other-window menudiag-selection-buffer)
	(setq menudiag-selection-main-buffer main-buf
	      menudiag-selection-list selection-list)
	(while (> linepos 0)
	  (setq linepos (1- linepos)
		n (+ n (length (car selection-list)))
		selection-list (cdr selection-list)))
	(next-completion n)))))

(defun menudiag-check-current-menu-list ()
  (or (eq menudiag-selection-main-buffer (car menudiag-minibuffer-list))
      (error "menudiag: not current menu list")))

(defun menudiag-choose-item ()
  (interactive)
  (menudiag-choose-item-internal nil))

(defun menudiag-mouse-choose-item (event)
  (interactive "e")
  (set-buffer (window-buffer (caadr event)))
  (menudiag-choose-item-internal event))

(defun menudiag-choose-item-internal (event)
  (menudiag-check-current-menu-list)
  (let ((org-buf menudiag-selection-main-buffer)
	(sel-buf (current-buffer))
	(item-list menudiag-selection-list)
	(l 0)
	tmp-buf n)
    (with-temp-buffer
      (setq tmp-buf (current-buffer))
      (set-buffer sel-buf)
      (setq completion-reference-buffer tmp-buf)
      (if event
	  (mouse-choose-completion event)
	(choose-completion))
      (set-buffer tmp-buf)
      (setq n (string-to-int (buffer-string))))
    (pop-to-buffer org-buf)
    (while (and item-list (>= n (length (car item-list))))
      (setq l (1+ l)
	    n (- n (length (car item-list)))
	    item-list (cdr item-list)))
    (menudiag-goto-line l)
    (menudiag-goto-item-internal n)
    (menudiag-select-this-item t)))

(defvar menudiag-goto-number-list nil)
(make-variable-buffer-local 'menudiag-goto-number-list)

(defvar menudiag-original-point nil)
(make-variable-buffer-local' menudiag-original-point)

(defun menudiag-selection-goto ()
  (interactive)
  (unless (eq last-command 'menudiag-selection-goto)
    (setq menudiag-goto-number-list nil
	  menudiag-original-point (point)))
  (setq menudiag-goto-number-list (cons (- last-command-char ?0)
					menudiag-goto-number-list))
  (menudiag-selection-goto-internal))

(defun menudiag-selection-goto-internal ()
  (let* ((list menudiag-goto-number-list)
	 (n (menudiag-selection-item-number list))
	 (len (save-excursion
		(set-buffer menudiag-selection-main-buffer)
		(length menudiag-current-items))))
    (setq this-command 'menudiag-selection-goto)
    (if (>= n len)
	(progn
	  (ding)
	  (setq menudiag-goto-number-list (cdr list)))
    (goto-char (point-min))
    (next-completion (1+ n)))))

(defun menudiag-selection-item-number (list)
  (let ((n 0)
	(exp 1))
    (while list
      (setq n (+ (* (car list) exp) n)
	    exp (* 10 exp)
	    list (cdr list)))
    n))

(defun menudiag-selection-goto-delete (n)
  (interactive "p")
  (if (null (eq last-command 'menudiag-selection-goto))
      (ding)
    (setq menudiag-goto-number-list (nthcdr n menudiag-goto-number-list))
    (if (null menudiag-goto-number-list)
	(goto-char menudiag-original-point)
      (menudiag-selection-goto-internal))))

(defun menudiag-selection-align-to-item ()
  (cond ((bolp)
	 (next-completion 1))
	((get-text-property (1- (point)) 'mouse-face)
	 (goto-char (previous-single-property-change (point) 'mouse-face)))))

(defun menudiag-restore-window ()
  (when menudiag-window-conf
    (set-window-configuration menudiag-window-conf)
    (kill-buffer menudiag-selection-buffer)))

(defun menudiag-exit-minibuffer ()
  (menudiag-restore-window)
  (menudiag-send-variables menudiag-done menudiag-value)
  (buffer-enable-undo)
  (exit-minibuffer))

(defun menudiag-retun-to-minibuf ()
  (interactive)
  (menudiag-check-current-menu-list)
  (unless (minibuffer-window-active-p (minibuffer-window))
    (set-minibuffer-window (minibuffer-window)))
  (let ((window (get-buffer-window menudiag-selection-main-buffer)))
    (if window
	(select-window window)
      (error "menudiag: cannot find minibuffer"))))

(defun menudiag-selection-exit-one-level ()
  (interactive)
  (set-buffer menudiag-selection-main-buffer)
  (menudiag-exit-one-level))

(defun menudiag-selection-exit ()
  (interactive)
  (set-buffer menudiag-selection-main-buffer)
  (menudiag-exit))

(provide 'menudiag)
;;; menudiag.el ends here.

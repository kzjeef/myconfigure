;;; its/bixing.el --- Bixing (stroke) Input in Egg Input Method Architecture

;; Copyright (C) 1999,2000 PFU LIMITED

;; Author: KATAYAMA Yoshio <kate@pfu.co.jp>

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
  (require 'its)
  (require 'cl))

(defvar its-qianma-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-qianma-open-braket  "「" "*[") ; "［"
(defvar its-qianma-close-braket "」" "*]") ; "］"

(defvar its-wubi-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-wubi-open-braket  "「" "*[") ; "［"
(defvar its-wubi-close-braket "」" "*]") ; "］"

(its-set-stroke-input '((QianMa . 3) (WuBi . 4)))

(egg-set-message-language-alist '((QianMa . Chinese-GB) (WuBi . Chinese-GB)))

(define-its-state-machine its-qianma-map
  "qinama" "钱" QianMa
  "Map for QianMa input. (Chinese-GB)"

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (let ((ch (string-to-list "0123456789abcdefghijklmnopqrstuvwxyz;=@[]")))
    (while ch
      (its-defrule (char-to-string (car ch)) (char-to-string (car ch)))
      (setq ch (cdr ch))))

  (dolist (ascii '(("0" . "０")  ("1" . "１")  ("2" . "２")  ("3" . "３")
		   ("4" . "４")  ("5" . "５")  ("6" . "６")  ("7" . "７")
		   ("8" . "８")  ("9" . "９") 
		   (" " . "　")  ("!" . "！")  ("@" . "＠")  ("#" . "＃")
		   ("$" . "＄")  ("%" . "％")  ("^" . "＾")  ("&" . "＆")
		   ("*" . "＊")  ("(" . "（")  (")" . "）")
		   ("-" . "－")  ("=" . "＝")  ("`" . "｀")  ("\\" . "＼")
		   ("|" . "｜")  ("_" . "＿")  ("+" . "＋")  ("~" . "～")
		   ("[" . "［")  ("]" . "］")  ("{" . "｛")  ("}" . "｝")
		   (":" . "：")  (";" . "；")  ("\"" . "＂") ("'" . "＇")
		   ("<" . "＜")  (">" . "＞")  ("?" . "？")  ("/" . "／")
		   ("," . "，")  ("." . "．")
		   ("a" . "ａ")  ("b" . "ｂ")  ("c" . "ｃ")  ("d" . "ｄ")
		   ("e" . "ｅ")  ("f" . "ｆ")  ("g" . "ｇ")  ("h" . "ｈ")
		   ("i" . "ｉ")  ("j" . "ｊ")  ("k" . "ｋ")  ("l" . "ｌ")
		   ("m" . "ｍ")  ("n" . "ｎ")  ("o" . "ｏ")  ("p" . "ｐ")
		   ("q" . "ｑ")  ("r" . "ｒ")  ("s" . "ｓ")  ("t" . "ｔ")
		   ("u" . "ｕ")  ("v" . "ｖ")  ("w" . "ｗ")  ("x" . "ｘ")
		   ("y" . "ｙ")  ("z" . "ｚ")
		   ("A" . "Ａ")  ("B" . "Ｂ")  ("C" . "Ｃ")  ("D" . "Ｄ")
		   ("E" . "Ｅ")  ("F" . "Ｆ")  ("G" . "Ｇ")  ("H" . "Ｈ")
		   ("I" . "Ｉ")  ("J" . "Ｊ")  ("K" . "Ｋ")  ("L" . "Ｌ")
		   ("M" . "Ｍ")  ("N" . "Ｎ")  ("O" . "Ｏ")  ("P" . "Ｐ")
		   ("Q" . "Ｑ")  ("R" . "Ｒ")  ("S" . "Ｓ")  ("T" . "Ｔ")
		   ("U" . "Ｕ")  ("V" . "Ｖ")  ("W" . "Ｗ")  ("X" . "Ｘ")
		   ("Y" . "Ｙ")  ("Z" . "Ｚ")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule (concat its-banjiao-escape in) in t)
      (its-defrule (concat its-quanjiao-escape in) out t)))

  (its-defrule	","	"，"	t)
  (its-defrule	"."	"。"	t)
  (its-defrule	"/"	"、"	t)
  (its-defrule	":"	"："	t)
  (its-defrule	"?"	"？"	t)
  (its-defrule	"!"	"！"	t))

(define-its-state-machine its-wubi-map
  "wubi" "五" WuBi
  "Map for WuBi input. (Chinese-GB)"

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (let ((ch (string-to-list "abcdefghijklmnopqrstuvwxy")))
    (while ch
      (its-defrule (char-to-string (car ch)) (char-to-string (car ch)))
      (setq ch (cdr ch))))

  (dolist (ascii '(("0" . "０")  ("1" . "１")  ("2" . "２")  ("3" . "３")
		   ("4" . "４")  ("5" . "５")  ("6" . "６")  ("7" . "７")
		   ("8" . "８")  ("9" . "９") 
		   (" " . "　")  ("!" . "！")  ("@" . "＠")  ("#" . "＃")
		   ("$" . "＄")  ("%" . "％")  ("^" . "＾")  ("&" . "＆")
		   ("*" . "＊")  ("(" . "（")  (")" . "）")
		   ("-" . "－")  ("=" . "＝")  ("`" . "｀")  ("\\" . "＼")
		   ("|" . "｜")  ("_" . "＿")  ("+" . "＋")  ("~" . "～")
		   ("[" . "［")  ("]" . "］")  ("{" . "｛")  ("}" . "｝")
		   (":" . "：")  (";" . "；")  ("\"" . "＂") ("'" . "＇")
		   ("<" . "＜")  (">" . "＞")  ("?" . "？")  ("/" . "／")
		   ("," . "，")  ("." . "．")
		   ("a" . "ａ")  ("b" . "ｂ")  ("c" . "ｃ")  ("d" . "ｄ")
		   ("e" . "ｅ")  ("f" . "ｆ")  ("g" . "ｇ")  ("h" . "ｈ")
		   ("i" . "ｉ")  ("j" . "ｊ")  ("k" . "ｋ")  ("l" . "ｌ")
		   ("m" . "ｍ")  ("n" . "ｎ")  ("o" . "ｏ")  ("p" . "ｐ")
		   ("q" . "ｑ")  ("r" . "ｒ")  ("s" . "ｓ")  ("t" . "ｔ")
		   ("u" . "ｕ")  ("v" . "ｖ")  ("w" . "ｗ")  ("x" . "ｘ")
		   ("y" . "ｙ")  ("z" . "ｚ")
		   ("A" . "Ａ")  ("B" . "Ｂ")  ("C" . "Ｃ")  ("D" . "Ｄ")
		   ("E" . "Ｅ")  ("F" . "Ｆ")  ("G" . "Ｇ")  ("H" . "Ｈ")
		   ("I" . "Ｉ")  ("J" . "Ｊ")  ("K" . "Ｋ")  ("L" . "Ｌ")
		   ("M" . "Ｍ")  ("N" . "Ｎ")  ("O" . "Ｏ")  ("P" . "Ｐ")
		   ("Q" . "Ｑ")  ("R" . "Ｒ")  ("S" . "Ｓ")  ("T" . "Ｔ")
		   ("U" . "Ｕ")  ("V" . "Ｖ")  ("W" . "Ｗ")  ("X" . "Ｘ")
		   ("Y" . "Ｙ")  ("Z" . "Ｚ")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule (concat its-banjiao-escape in) in t)
      (its-defrule (concat its-quanjiao-escape in) out t)))

  (its-defrule	","	"，"	t)
  (its-defrule	"."	"。"	t)
  (its-defrule	"/"	"、"	t)
  (its-defrule	";"	"；"	t)
  (its-defrule	":"	"："	t)
  (its-defrule	"?"	"？"	t)
  (its-defrule	"!"	"！"	t))

(define-its-state-machine-append its-qianma-map
  (its-defrule "{" its-qianma-open-braket)
  (its-defrule "}" its-qianma-close-braket)

  (if its-qianma-enable-quanjioao-alphabet
      (progn
	(its-defrule "#"  "＃"  t)  (its-defrule "$"  "＄"  t)
	(its-defrule "%"  "％"  t)  (its-defrule "^"  "＾"  t)
	(its-defrule "&"  "＆"  t)  (its-defrule "*"  "＊"  t)
	(its-defrule "("  "（"  t)  (its-defrule ")"  "）"  t)
	(its-defrule "-"  "－"  t)  (its-defrule "~"  "～"  t)
	(its-defrule "`"  "｀"  t)
	(its-defrule "\\" "＼"  t)  (its-defrule "|"  "｜"  t)
	(its-defrule "_"  "＿"  t)  (its-defrule "+"  "＋"  t)
	(its-defrule "\"" "＂"  t)  (its-defrule "'"  "＇"  t)
	(its-defrule "<"  "＜"  t)  (its-defrule ">"  "＞"  t))
    (progn
      (its-defrule "#"  "#"  t)  (its-defrule "$"  "$"  t)
      (its-defrule "%"  "%"  t)  (its-defrule "^"  "^"  t)
      (its-defrule "&"  "&"  t)  (its-defrule "*"  "*"  t)
      (its-defrule "("  "("  t)  (its-defrule ")"  ")"  t)
      (its-defrule "-"  "-"  t)  (its-defrule "~"  "~"  t)
      (its-defrule "`"  "`"  t)
      (its-defrule "\\" "\\" t)  (its-defrule "|"  "|"  t)
      (its-defrule "_"  "_"  t)  (its-defrule "+"  "+"  t)
      (its-defrule "\"" "\"" t)  (its-defrule "'"  "'"  t)
      (its-defrule "<"  "<"  t)  (its-defrule ">"  ">"  t))))

(define-its-state-machine-append its-wubi-map
  (its-defrule "[" its-wubi-open-braket)
  (its-defrule "]" its-wubi-close-braket)

  (if its-wubi-enable-quanjioao-alphabet
      (progn
	(its-defrule "1"  "１"  t)  (its-defrule "2"  "２"  t)
	(its-defrule "3"  "３"  t)  (its-defrule "4"  "４"  t)
	(its-defrule "5"  "５"  t)  (its-defrule "6"  "６"  t)
	(its-defrule "7"  "７"  t)  (its-defrule "8"  "８"  t)
	(its-defrule "9"  "９"  t)  (its-defrule "0"  "０"  t)
	(its-defrule "@"  "＠"  t)
	(its-defrule "#"  "＃"  t)  (its-defrule "$"  "＄"  t)
	(its-defrule "%"  "％"  t)  (its-defrule "^"  "＾"  t)
	(its-defrule "&"  "＆"  t)  (its-defrule "*"  "＊"  t)
	(its-defrule "("  "（"  t)  (its-defrule ")"  "）"  t)
	(its-defrule "-"  "－"  t)  (its-defrule "~"  "～"  t)
	(its-defrule "="  "＝"  t)  (its-defrule "`"  "｀"  t)
	(its-defrule "\\" "＼"  t)  (its-defrule "|"  "｜"  t)
	(its-defrule "_"  "＿"  t)  (its-defrule "+"  "＋"  t)
	(its-defrule "{"  "｛"  t)  (its-defrule "}"  "｝"  t)
	(its-defrule "\"" "＂"  t)  (its-defrule "'"  "＇"  t)
	(its-defrule "<"  "＜"  t)  (its-defrule ">"  "＞"  t))
    (progn
      (its-defrule "1"  "1"  t)  (its-defrule "2"  "2"  t)
      (its-defrule "3"  "3"  t)  (its-defrule "4"  "4"  t)
      (its-defrule "5"  "5"  t)  (its-defrule "6"  "6"  t)
      (its-defrule "7"  "7"  t)  (its-defrule "8"  "8"  t)
      (its-defrule "9"  "9"  t)  (its-defrule "0"  "0"  t)
      (its-defrule "@"  "@"  t)
      (its-defrule "#"  "#"  t)  (its-defrule "$"  "$"  t)
      (its-defrule "%"  "%"  t)  (its-defrule "^"  "^"  t)
      (its-defrule "&"  "&"  t)  (its-defrule "*"  "*"  t)
      (its-defrule "("  "("  t)  (its-defrule ")"  ")"  t)
      (its-defrule "-"  "-"  t)  (its-defrule "~"  "~"  t)
      (its-defrule "="  "="  t)  (its-defrule "`"  "`"  t)
      (its-defrule "\\" "\\" t)  (its-defrule "|"  "|"  t)
      (its-defrule "_"  "_"  t)  (its-defrule "+"  "+"  t)
      (its-defrule "{"  "{"  t)  (its-defrule "}"  "}"  t)
      (its-defrule "\"" "\"" t)  (its-defrule "'"  "'"  t)
      (its-defrule "<"  "<"  t)  (its-defrule ">"  ">"  t))))

(provide 'its/bixing)

;;; its/jeonkak.el --- Jeonkak ASCII Input in Egg Input Method Architecture

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
  (require 'its))

(define-its-state-machine its-jeonkak-up-map
  "jeonkak-upcase" "Ａ" Korean
  "Map for jeonkak-upcase input."

  (dolist (ascii '(("1" . "１")  ("2" . "２")  ("3" . "３")  ("4" . "４")
		   ("5" . "５")  ("6" . "６")  ("7" . "７")  ("8" . "８")
		   ("9" . "９")  ("0" . "０")
		   (" " . "　")  ("!" . "！")  ("@" . "＠")  ("#" . "＃")
		   ("$" . "＄")  ("%" . "％")  ("^" . "＾")  ("&" . "＆")
		   ("*" . "＊")  ("(" . "（")  (")" . "）")
		   ("-" . "－")  ("=" . "＝")  ("`" . "｀")  ("\\" . "￦")
		   ("|" . "｜")  ("_" . "＿")  ("+" . "＋")  ("~" . "￣")
		   ("[" . "「")  ("]" . "」")  ("{" . "｛")  ("}" . "｝")
		   (":" . "：")  (";" . "；")  ("\"" . "＂") ("'" . "＇")
		   ("<" . "＜")  (">" . "＞")  ("?" . "？")  ("/" . "／")
		   ("," . "，")  ("." . "．")
		   ("a" . "Ａ")  ("b" . "Ｂ")  ("c" . "Ｃ")  ("d" . "Ｄ")
		   ("e" . "Ｅ")  ("f" . "Ｆ")  ("g" . "Ｇ")  ("h" . "Ｈ")
		   ("i" . "Ｉ")  ("j" . "Ｊ")  ("k" . "Ｋ")  ("l" . "Ｌ")
		   ("m" . "Ｍ")  ("n" . "Ｎ")  ("o" . "Ｏ")  ("p" . "Ｐ")
		   ("q" . "Ｑ")  ("r" . "Ｒ")  ("s" . "Ｓ")  ("t" . "Ｔ")
		   ("u" . "Ｕ")  ("v" . "Ｖ")  ("w" . "Ｗ")  ("x" . "Ｘ")
		   ("y" . "Ｙ")  ("z" . "Ｚ")
		   ("A" . "Ａ")  ("B" . "Ｂ")  ("C" . "Ｃ")  ("D" . "Ｄ")
		   ("E" . "Ｅ")  ("F" . "Ｆ")  ("G" . "Ｇ")  ("H" . "Ｈ")
		   ("I" . "Ｉ")  ("J" . "Ｊ")  ("K" . "Ｋ")  ("L" . "Ｌ")
		   ("M" . "Ｍ")  ("N" . "Ｎ")  ("O" . "Ｏ")  ("P" . "Ｐ")
		   ("Q" . "Ｑ")  ("R" . "Ｒ")  ("S" . "Ｓ")  ("T" . "Ｔ")
		   ("U" . "Ｕ")  ("V" . "Ｖ")  ("W" . "Ｗ")  ("X" . "Ｘ")
		   ("Y" . "Ｙ")  ("Z" . "Ｚ")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule in out))))

(define-its-state-machine-append its-jeonkak-up-map)

(define-its-state-machine its-jeonkak-down-map
  "jeonkak-downcase" "ａ" Korean
  "Map for jeonkak-downcase input."

  (dolist (ascii '(("1" . "１")  ("2" . "２")  ("3" . "３")  ("4" . "４")
		   ("5" . "５")  ("6" . "６")  ("7" . "７")  ("8" . "８")
		   ("9" . "９")  ("0" . "０")
		   (" " . "　")  ("!" . "！")  ("@" . "＠")  ("#" . "＃")
		   ("$" . "＄")  ("%" . "％")  ("^" . "＾")  ("&" . "＆")
		   ("*" . "＊")  ("(" . "（")  (")" . "）")
		   ("-" . "－")  ("=" . "＝")  ("`" . "｀")  ("\\" . "￦")
		   ("|" . "｜")  ("_" . "＿")  ("+" . "＋")  ("~" . "￣")
		   ("[" . "「")  ("]" . "」")  ("{" . "｛")  ("}" . "｝")
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
      (its-defrule in out))))

(define-its-state-machine-append its-jeonkak-down-map)

(provide 'its/jeonkak)

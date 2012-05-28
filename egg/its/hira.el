;;; its/hira.el --- Hiragana Input in Egg Input Method Architecture

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc

;; Author: NIIBE Yutaka <gniibe@chroot.org>

;; Maintainer: TOMURA Satoru <tomura@etl.go.jp>

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
;;
;; Symbol input is desined by jiro@math.keio.ac.jp (TANAKA Jiro)
;; This file is based on the rules of its/hira.el in Mule-2.3 distribution.
;;

;;; Code:

(eval-when-compile
  (require 'its)
  (require 'cl))

(eval-when (compile)
  (defconst its-compaction-enable t))

(defgroup hira nil
  "Hiragana Input Method"
  :group 'its)

(defvar its-hira-enable-zenkaku-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Zenkaku alphabet")

(defcustom its-hira-enable-double-n t
  "*Enable \"nn\" input for \"ん\" "
  :group 'hira :type 'boolean)

(defcustom its-hira-period "。" 
  "* .(ピリオド)を入力したときの句点の文字: \"。\"  \". \" \"．\""
  :group 'hira :type 'string)

(defcustom its-hira-comma  "、"
 "* ,(コンマ)を入力したときの読点の文字: \"、\" \", \" \"，\""
  :group 'hira :type 'string)

(defcustom its-hira-open-bracket  "「"
 "* [ を入力したときのかぎ括弧開けの文字: \"「\" \"［\""
  :group 'hira :type 'string)

(defcustom its-hira-close-bracket "」"
 "* ] を入力したときのかぎ括弧閉じの文字: \"」\" \"］\""
  :group 'hira :type 'string)

(defcustom its-hira-horizontal  "ー"
  "* - を入力したときの長音記号の文字: \"ー\" \"−\""
  :group 'hira :type 'string)

(define-its-state-machine its-hira-map
  "roma-kana" "あ" Japanese
  "Map for Romaji-Hiragana translation. (Japanese)"

  (defconst its-zenkaku-escape "Z")  ;; Escape character to Zenkaku inputs
  (defconst its-hankaku-escape "~")  ;; Escape character to Hankaku inputs

  (its-defrule-select-mode-temporally "q" downcase)
  (its-defrule-select-mode-temporally "Q" zenkaku-downcase)

;;; k      k
;;; kk     っk
;;; kka    っか
;;;
;;; kkk    っk DING!

  (its-defrule "tch"  "っ" -2)

;;; 「ん」の入力

  (dolist (q1 '("b" "m" "p"))
    (its-defrule (concat "m" q1) "ん" -1))

  (its-defrule* "n"  "ん")
  (its-defrule  "n'" "ん")
  (its-defrule  "N"  "ん")

  (let ((small '"x" ))
    (its-defrule (concat small "a") "ぁ")
    (its-defrule (concat small "i") "ぃ")
    (its-defrule (concat small "u") "ぅ")
    (its-defrule (concat small "e") "ぇ")
    (its-defrule (concat small "o") "ぉ")
    (its-defrule (concat small "ya") "ゃ")
    (its-defrule (concat small "yu") "ゅ")
    (its-defrule (concat small "yo") "ょ")
    (its-defrule (concat small "tu") "っ")
    (its-defrule (concat small "tsu") "っ")
    (its-defrule (concat small "wa") "ゎ")
    )

  (its-defrule   "a"    "あ")
  (its-defrule   "i"    "い")
  (its-defrule   "u"    "う")
  (its-defrule   "e"    "え")
  (its-defrule   "o"    "お")

  (dolist (k '(("ka"  "か") ("ki"  "き") ("ku"  "く") ("ke"  "け") ("ko"  "こ")
	       ("kya" "きゃ") ("kyu"  "きゅ") ("kye"  "きぇ") ("kyo"  "きょ")))
    (its-defrule (car k) (cadr k))
    (its-defrule (concat "k" (car k)) (concat "っ" (cadr k))))
  (its-defoutput "kk" "っk")
  (its-defoutput "kky" "っky")

  (dolist (s '(("sa"  "さ") ("si"  "し") ("su"  "す") ("se"  "せ") ("so"  "そ")
	       ("sya"  "しゃ") ("syu"  "しゅ") ("sye"  "しぇ") ("syo"  "しょ")
	       ("sha"  "しゃ") ("shi"  "し") ("shu"  "しゅ") ("she"  "しぇ")
	       ("sho"  "しょ")))
    (its-defrule (car s) (cadr s))
    (its-defrule (concat "s" (car s)) (concat "っ" (cadr s))))
  (its-defoutput "ss" "っs")
  (its-defoutput "ssy" "っsy")
  (its-defoutput "ssh" "っsh")

  (dolist (T '(("ta"  "た") ("ti"  "ち") ("tu"  "つ") ("te"  "て") ("to"  "と")
	       ("tya"  "ちゃ") ("tyi"  "てぃ") ("tyu"  "ちゅ") ("tye"  "ちぇ")
	       ("tyo"  "ちょ") ("tsu"  "つ")))
    (its-defrule (car T) (cadr T))
    (its-defrule (concat "t" (car T)) (concat "っ" (cadr T))))
  (its-defoutput "tt" "っt")
  (its-defoutput "tty" "っty")
  (its-defoutput "tts" "っts")

  (dolist (c '(("cha"  "ちゃ") ("chi"  "ち") ("chu"  "ちゅ")
	       ("che"  "ちぇ") ("cho"  "ちょ")))
    (its-defrule (car c) (cadr c))
    (its-defrule (concat "c" (car c)) (concat "っ" (cadr c))))
  (its-defoutput "cc" "っc")
  (its-defoutput "cch" "っch")

  (dolist (h '(("ha"  "は") ("hi"  "ひ") ("hu"  "ふ") ("he"  "へ") ("ho"  "ほ")
	       ("hya"  "ひゃ") ("hyu"  "ひゅ") ("hye"  "ひぇ") ("hyo"  "ひょ")))
    (its-defrule (car h) (cadr h))
    (its-defrule (concat "h" (car h)) (concat "っ" (cadr h))))
  (its-defoutput "hh" "っh")
  (its-defoutput "hhy" "っhy")

  (dolist (f '(("fa"  "ふぁ") ("fi"  "ふぃ") ("fu"  "ふ") ("fe"  "ふぇ")
	       ("fo"  "ふぉ")))
    (its-defrule (car f) (cadr f))
    (its-defrule (concat "f" (car f)) (concat "っ" (cadr f))))
  (its-defoutput "ff" "っf")

  (dolist (r '(("ra"  "ら") ("ri"  "り") ("ru"  "る") ("re"  "れ") ("ro"  "ろ")
	       ("rya"  "りゃ") ("ryu"  "りゅ") ("rye"  "りぇ") ("ryo"  "りょ")))
    (its-defrule (car r) (cadr r))
    (its-defrule (concat "r" (car r)) (concat "っ" (cadr r))))
  (its-defoutput "rr" "っr")
  (its-defoutput "rry" "っry")

  (dolist (l '(("la"  "ら") ("li"  "り") ("lu"  "る") ("le"  "れ") ("lo"  "ろ")
	       ("lya"  "りゃ") ("lyu"  "りゅ") ("lye"  "りぇ") ("lyo"  "りょ")))
    (its-defrule (car l) (cadr l))
    (its-defrule (concat "l" (car l)) (concat "っ" (cadr l))))
  (its-defoutput "ll" "っl")
  (its-defoutput "lly" "っly")

  (dolist (g '(("ga"  "が") ("gi"  "ぎ") ("gu"  "ぐ") ("ge"  "げ") ("go"  "ご")
	       ("gya"  "ぎゃ") ("gyu"  "ぎゅ") ("gye"  "ぎぇ") ("gyo"  "ぎょ")))
    (its-defrule (car g) (cadr g))
    (its-defrule (concat "g" (car g)) (concat "っ" (cadr g))))
  (its-defoutput "gg" "っg")
  (its-defoutput "ggy" "っgy")

  (dolist (z '(("za"  "ざ") ("zi"  "じ") ("zu"  "ず") ("ze"  "ぜ") ("zo"  "ぞ")
	       ("zya"  "じゃ") ("zyu"  "じゅ") ("zye"  "じぇ") ("zyo"  "じょ")))
    (its-defrule (car z) (cadr z))
    (its-defrule (concat "z" (car z)) (concat "っ" (cadr z))))
  (its-defoutput "zz" "っz")
  (its-defoutput "zzy" "っzy")

  (dolist (j '(("ja"  "じゃ") ("ji"  "じ") ("ju"  "じゅ") ("je"  "じぇ")
	       ("jo"  "じょ") ("jya"  "じゃ") ("jyu"  "じゅ") ("jye"  "じぇ")
	       ("jyo"  "じょ")))
    (its-defrule (car j) (cadr j))
    (its-defrule (concat "j" (car j)) (concat "っ" (cadr j))))
  (its-defoutput "jj" "っj")
  (its-defoutput "jjy" "っjy")

  (dolist (d '(("da"  "だ") ("di"  "ぢ") ("du"  "づ") ("de"  "で") ("do"  "ど")
	       ("dya"  "ぢゃ") ("dyi"  "でぃ") ("dyu"  "ぢゅ") ("dye"  "ぢぇ")
	       ("dyo"  "ぢょ")))
    (its-defrule (car d) (cadr d))
    (its-defrule (concat "d" (car d)) (concat "っ" (cadr d))))
  (its-defoutput "dd" "っd")
  (its-defoutput "ddy" "っdy")

  (dolist (b '(("ba"  "ば") ("bi"  "び") ("bu"  "ぶ") ("be"  "べ") ("bo"  "ぼ")
	       ("bya"  "びゃ") ("byu"  "びゅ") ("bye"  "びぇ") ("byo"  "びょ")))
    (its-defrule (car b) (cadr b))
    (its-defrule (concat "b" (car b)) (concat "っ" (cadr b))))
  (its-defoutput "bb" "っb")
  (its-defoutput "bby" "っby")

  (dolist (p '(("pa"  "ぱ") ("pi"  "ぴ") ("pu"  "ぷ") ("pe"  "ぺ") ("po"   "ぽ")
	       ("pya"  "ぴゃ") ("pyu"  "ぴゅ") ("pye"  "ぴぇ") ("pyo"  "ぴょ")))
    (its-defrule (car p) (cadr p))
    (its-defrule (concat "p" (car p)) (concat "っ" (cadr p))))
  (its-defoutput "pp" "っp")
  (its-defoutput "ppy" "っpy")

  (dolist (v '(("va" "ヴぁ") ("vi" "ヴぃ") ("vu" "ヴ") ("ve" "ヴぇ")
	       ("vo" "ヴぉ")))
    (its-defrule (car v) (cadr v))
    (its-defrule (concat "v" (car v)) (concat "っ" (cadr v))))
  (its-defoutput "vv" "っv")

  (its-defrule   "ma"   "ま")
  (its-defrule   "mi"   "み")
  (its-defrule   "mu"   "む")
  (its-defrule   "me"   "め")
  (its-defrule   "mo"   "も")
  (its-defrule   "mya"  "みゃ")
  (its-defrule   "myu"  "みゅ")
  (its-defrule   "mye"  "みぇ")
  (its-defrule   "myo"  "みょ")
  (its-defrule   "ya"   "や")
  (its-defrule   "yi"   "い")
  (its-defrule   "yu"   "ゆ")
  (its-defrule   "yo"   "よ")
  (its-defrule   "ye"   "いぇ")
  (its-defrule   "wa"   "わ")
  (its-defrule   "wi"   "ゐ")
  (its-defrule   "wu"   "う")
  (its-defrule   "we"   "ゑ")
  (its-defrule   "wo"   "を")

  (its-defrule   "kwa"  "くゎ")
  (its-defrule   "kwi"  "くぃ")
  (its-defrule   "kwu"  "く")
  (its-defrule   "kwe"  "くぇ")
  (its-defrule   "kwo"  "くぉ")
  (its-defrule   "gwa"  "ぐゎ")
  (its-defrule   "gwi"  "ぐぃ")
  (its-defrule   "gwu"  "ぐ")
  (its-defrule   "gwe"  "ぐぇ")
  (its-defrule   "gwo"  "ぐぉ")
  (its-defrule   "tsa"  "つぁ")
  (its-defrule   "tsi"  "つぃ")
  (its-defrule   "tse"  "つぇ")
  (its-defrule   "tso"  "つぉ")

  (its-defrule   "na"   "な")
  (its-defrule   "ni"   "に")
  (its-defrule   "nu"   "ぬ")
  (its-defrule   "ne"   "ね")
  (its-defrule   "no"   "の")
  (its-defrule   "nya"  "にゃ")
  (its-defrule   "nyu"  "にゅ")
  (its-defrule   "nye"  "にぇ")
  (its-defrule   "nyo"  "にょ")

  (its-defrule   "xka"  "ヵ")
  (its-defrule   "xke"  "ヶ")
  (its-defrule   "xti"  "てぃ")
  (its-defrule   "xdi"  "でぃ")
  (its-defrule   "xdu"  "どぅ")
  (its-defrule   "xde"  "でぇ")
  (its-defrule   "xdo"  "どぉ")
  (its-defrule   "xwi"  "うぃ")
  (its-defrule   "xwe"  "うぇ")
  (its-defrule   "xwo"  "うぉ")

;;;
;;; Zenkaku inputs
;;;

  (its-defrule (concat its-zenkaku-escape "0") "０")
  (its-defrule (concat its-zenkaku-escape "1") "１")
  (its-defrule (concat its-zenkaku-escape "2") "２")
  (its-defrule (concat its-zenkaku-escape "3") "３")
  (its-defrule (concat its-zenkaku-escape "4") "４")
  (its-defrule (concat its-zenkaku-escape "5") "５")
  (its-defrule (concat its-zenkaku-escape "6") "６")
  (its-defrule (concat its-zenkaku-escape "7") "７")
  (its-defrule (concat its-zenkaku-escape "8") "８")
  (its-defrule (concat its-zenkaku-escape "9") "９")

  (its-defrule (concat its-zenkaku-escape "A") "Ａ")
  (its-defrule (concat its-zenkaku-escape "B") "Ｂ")
  (its-defrule (concat its-zenkaku-escape "C") "Ｃ")
  (its-defrule (concat its-zenkaku-escape "D") "Ｄ")
  (its-defrule (concat its-zenkaku-escape "E") "Ｅ")
  (its-defrule (concat its-zenkaku-escape "F") "Ｆ")
  (its-defrule (concat its-zenkaku-escape "G") "Ｇ")
  (its-defrule (concat its-zenkaku-escape "H") "Ｈ")
  (its-defrule (concat its-zenkaku-escape "I") "Ｉ")
  (its-defrule (concat its-zenkaku-escape "J") "Ｊ")
  (its-defrule (concat its-zenkaku-escape "K") "Ｋ")
  (its-defrule (concat its-zenkaku-escape "L") "Ｌ")
  (its-defrule (concat its-zenkaku-escape "M") "Ｍ")
  (its-defrule (concat its-zenkaku-escape "N") "Ｎ")
  (its-defrule (concat its-zenkaku-escape "O") "Ｏ")
  (its-defrule (concat its-zenkaku-escape "P") "Ｐ")
  (its-defrule (concat its-zenkaku-escape "Q") "Ｑ")
  (its-defrule (concat its-zenkaku-escape "R") "Ｒ")
  (its-defrule (concat its-zenkaku-escape "S") "Ｓ")
  (its-defrule (concat its-zenkaku-escape "T") "Ｔ")
  (its-defrule (concat its-zenkaku-escape "U") "Ｕ")
  (its-defrule (concat its-zenkaku-escape "V") "Ｖ")
  (its-defrule (concat its-zenkaku-escape "W") "Ｗ")
  (its-defrule (concat its-zenkaku-escape "X") "Ｘ")
  (its-defrule (concat its-zenkaku-escape "Y") "Ｙ")
  (its-defrule (concat its-zenkaku-escape "Z") "Ｚ")

  (its-defrule (concat its-zenkaku-escape "a") "ａ")
  (its-defrule (concat its-zenkaku-escape "b") "ｂ")
  (its-defrule (concat its-zenkaku-escape "c") "ｃ")
  (its-defrule (concat its-zenkaku-escape "d") "ｄ")
  (its-defrule (concat its-zenkaku-escape "e") "ｅ")
  (its-defrule (concat its-zenkaku-escape "f") "ｆ")
  (its-defrule (concat its-zenkaku-escape "g") "ｇ")
  (its-defrule (concat its-zenkaku-escape "h") "ｈ")
  (its-defrule (concat its-zenkaku-escape "i") "ｉ")
  (its-defrule (concat its-zenkaku-escape "j") "ｊ")
  (its-defrule (concat its-zenkaku-escape "k") "ｋ")
  (its-defrule (concat its-zenkaku-escape "l") "ｌ")
  (its-defrule (concat its-zenkaku-escape "m") "ｍ")
  (its-defrule (concat its-zenkaku-escape "n") "ｎ")
  (its-defrule (concat its-zenkaku-escape "o") "ｏ")
  (its-defrule (concat its-zenkaku-escape "p") "ｐ")
  (its-defrule (concat its-zenkaku-escape "q") "ｑ")
  (its-defrule (concat its-zenkaku-escape "r") "ｒ")
  (its-defrule (concat its-zenkaku-escape "s") "ｓ")
  (its-defrule (concat its-zenkaku-escape "t") "ｔ")
  (its-defrule (concat its-zenkaku-escape "u") "ｕ")
  (its-defrule (concat its-zenkaku-escape "v") "ｖ")
  (its-defrule (concat its-zenkaku-escape "w") "ｗ")
  (its-defrule (concat its-zenkaku-escape "x") "ｘ")
  (its-defrule (concat its-zenkaku-escape "y") "ｙ")
  (its-defrule (concat its-zenkaku-escape "z") "ｚ")

  (its-defrule (concat its-zenkaku-escape " ")  "　")
  (its-defrule (concat its-zenkaku-escape "!")  "！")
  (its-defrule (concat its-zenkaku-escape "@")  "＠")
  (its-defrule (concat its-zenkaku-escape "#")  "＃")
  (its-defrule (concat its-zenkaku-escape "$")  "＄")
  (its-defrule (concat its-zenkaku-escape "%")  "％")
  (its-defrule (concat its-zenkaku-escape "^")  "＾")
  (its-defrule (concat its-zenkaku-escape "&")  "＆")
  (its-defrule (concat its-zenkaku-escape "*")  "＊")
  (its-defrule (concat its-zenkaku-escape "(")  "（")
  (its-defrule (concat its-zenkaku-escape ")")  "）")
  (its-defrule (concat its-zenkaku-escape "-")  "−")
  (its-defrule (concat its-zenkaku-escape "=")  "＝")
  (its-defrule (concat its-zenkaku-escape "`")  "｀")
  (its-defrule (concat its-zenkaku-escape "\\") "￥")
  (its-defrule (concat its-zenkaku-escape "|")  "｜")
  (its-defrule (concat its-zenkaku-escape "_")  "＿")
  (its-defrule (concat its-zenkaku-escape "+")  "＋")
  (its-defrule (concat its-zenkaku-escape "~")  "￣")
  (its-defrule (concat its-zenkaku-escape "[")  "［")
  (its-defrule (concat its-zenkaku-escape "]")  "］")
  (its-defrule (concat its-zenkaku-escape "{")  "｛")
  (its-defrule (concat its-zenkaku-escape "}")  "｝")
  (its-defrule (concat its-zenkaku-escape ":")  "：")
  (its-defrule (concat its-zenkaku-escape ";")  "；")
  (its-defrule (concat its-zenkaku-escape "\"") "”")
  (its-defrule (concat its-zenkaku-escape "'")  "’")
  (its-defrule (concat its-zenkaku-escape "<")  "＜")
  (its-defrule (concat its-zenkaku-escape ">")  "＞")
  (its-defrule (concat its-zenkaku-escape "?")  "？")
  (its-defrule (concat its-zenkaku-escape "/")  "／")
  (its-defrule (concat its-zenkaku-escape ",")  "，")
  (its-defrule (concat its-zenkaku-escape ".")  "．")

;;;
;;; Hankaku inputs
;;;

  (dolist (digit '( "1"  "2"  "3"  "4" "5"  "6"  "7"  "8"  "9"  "0" ))
    (its-defrule (concat its-hankaku-escape digit)  digit))

  (dolist (symbol '( " "  "!"  "@"  "#"  "$"  "%"  "^"  "&"  "*"  "("  ")"
		     "-"  "="  "`"  "\\" "|"  "_"  "+"  "~" "["  "]"  "{"  "}"
		     ":"  ";"  "\"" "'"  "<"  ">"  "?"  "/"  ","  "." ))
    (its-defrule (concat its-hankaku-escape symbol) symbol))

  (dolist (downcase '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n"
		      "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
    (its-defrule (concat its-hankaku-escape downcase) downcase))

  (dolist (upcase    '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N"
		       "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
    (its-defrule (concat its-hankaku-escape upcase) upcase))

;; SYMBOL Input
  (its-defrule   "z1"   "○")	(its-defrule   "z!"   "●")
  (its-defrule   "z2"   "▽")	(its-defrule   "z@"   "▼")
  (its-defrule   "z3"   "△")	(its-defrule   "z#"   "▲")
  (its-defrule   "z4"   "□")	(its-defrule   "z$"   "■")
  (its-defrule   "z5"   "◇")	(its-defrule   "z%"   "◆")
  (its-defrule   "z6"   "☆")	(its-defrule   "z^"   "★")
  (its-defrule   "z7"   "◎")	(its-defrule   "z&"   "£")
  (its-defrule   "z8"   "¢")	(its-defrule   "z*"   "×")
  (its-defrule   "z9"   "♂")	(its-defrule   "z("   "【")
  (its-defrule   "z0"   "♀")	(its-defrule   "z)"   "】")
  (its-defrule   "z-"   "〜")	(its-defrule   "z_"   "∴")
  (its-defrule   "z="   "≠")	(its-defrule   "z+"   "±")
  (its-defrule   "z\\"  "＼")	(its-defrule   "z|"   "‖")
  (its-defrule   "z`"   "´")	(its-defrule   "z~"   "¨")

  (its-defrule   "zq"   "《")	(its-defrule   "zQ"   "〈")
  (its-defrule   "zw"   "》")	(its-defrule   "zW"   "〉")
					; e
  (its-defrule   "zr"   "々")	(its-defrule   "zR"   "仝")
  (its-defrule   "zt"   "〆")	(its-defrule   "zT"   "§")
					; y u i o
  (its-defrule   "zp"   "〒")	(its-defrule   "zP"   "↑")
  (its-defrule   "z["   "『")	(its-defrule   "z{"   "〔")
  (its-defrule   "z]"   "』")	(its-defrule   "z}"   "〕")

					; a
  (its-defrule   "zs"   "ヽ")	(its-defrule   "zS"   "ヾ")
  (its-defrule   "zd"   "ゝ")	(its-defrule   "zD"   "ゞ")
  (its-defrule   "zf"   "〃")	(its-defrule   "zF"   "→")
  (its-defrule   "zg"   "‐")	(its-defrule   "zG"   "―")
  (its-defrule   "zh"   "←")
  (its-defrule   "zj"   "↓")
  (its-defrule   "zk"   "↑")
  (its-defrule   "zl"   "→")
  (its-defrule   "z;"   "゛")	(its-defrule   "z:"   "゜")
  (its-defrule   "z\'"  "‘")	(its-defrule   "z\""  "“")

					; z
  (its-defrule   "zx"   ":-")	(its-defrule   "zX"   ":-)")
  (its-defrule   "zc"   "〇")	(its-defrule   "zC"   "℃")
  (its-defrule   "zv"   "※")	(its-defrule   "zV"   "÷")
  (its-defrule   "zb"   "°")	(its-defrule   "zB"   "←")
  (its-defrule   "zn"   "′")	(its-defrule   "zN"   "↓")
  (its-defrule   "zm"   "″")	(its-defrule   "zM"   "〓")
  (its-defrule   "z,"   "‥")	(its-defrule   "z<"   "≦")
  (its-defrule   "z."   "…")	(its-defrule   "z>"   "≧")
  (its-defrule   "z/"   "・")	(its-defrule   "z?"   "∞")
  )

(define-its-state-machine-append its-hira-map
  (if its-hira-enable-double-n
      (its-defrule "nn" "ん"))

  (its-defrule "-" its-hira-horizontal)
  (its-defrule "[" its-hira-open-bracket)
  (its-defrule "]" its-hira-close-bracket)
  (its-defrule "." its-hira-period)
  (its-defrule "," its-hira-comma)

  (if its-hira-enable-zenkaku-alphabet
      (progn
	(its-defrule   "1"   "１")  (its-defrule   "2"   "２")
	(its-defrule   "3"   "３")  (its-defrule   "4"   "４")
	(its-defrule   "5"   "５")  (its-defrule   "6"   "６")
	(its-defrule   "7"   "７")  (its-defrule   "8"   "８")
	(its-defrule   "9"   "９")  (its-defrule   "0"   "０")
	(its-defrule   "!"   "！")  (its-defrule   "@"   "＠")
	(its-defrule   "#"   "＃")  (its-defrule   "$"   "＄")
	(its-defrule   "%"   "％")  (its-defrule   "^"   "＾")
	(its-defrule   "&"   "＆")  (its-defrule   "*"   "＊")
	(its-defrule   "("   "（")  (its-defrule   ")"   "）")
	(its-defrule   "="   "＝")  (its-defrule   "`"   "｀")
	(its-defrule   "\\"  "￥")  (its-defrule   "|"   "｜")
	(its-defrule   "_"   "＿")  (its-defrule   "+"   "＋")
	(its-defrule   "{"   "｛")  (its-defrule   "}"   "｝")
	(its-defrule   ":"   "：")  (its-defrule   ";"   "；")
	(its-defrule   "\""  "”")  (its-defrule   "'"   "’")
	(its-defrule   "<"   "＜")  (its-defrule   ">"   "＞")
	(its-defrule   "?"   "？")  (its-defrule   "/"   "／"))
    (progn
      (its-defrule   "1"   "1")  (its-defrule   "2"   "2")
      (its-defrule   "3"   "3")  (its-defrule   "4"   "4")
      (its-defrule   "5"   "5")  (its-defrule   "6"   "6")
      (its-defrule   "7"   "7")  (its-defrule   "8"   "8")
      (its-defrule   "9"   "9")  (its-defrule   "0"   "0")
      (its-defrule   "!"   "!")  (its-defrule   "@"   "@")
      (its-defrule   "#"   "#")  (its-defrule   "$"   "$")
      (its-defrule   "%"   "%")  (its-defrule   "^"   "^")
      (its-defrule   "&"   "&")  (its-defrule   "*"   "*")
      (its-defrule   "("   "(")  (its-defrule   ")"   ")")
      (its-defrule   "="   "=")  (its-defrule   "`"   "`")
      (its-defrule   "\\"  "\\") (its-defrule   "|"   "|")
      (its-defrule   "_"   "_")  (its-defrule   "+"   "+")
      (its-defrule   "{"   "{")  (its-defrule   "}"   "}")
      (its-defrule   ":"   ":")  (its-defrule   ";"   ";")
      (its-defrule   "\""  "\"") (its-defrule   "'"   "'")
      (its-defrule   "<"   "<")  (its-defrule   ">"   ">")
      (its-defrule   "?"   "?")  (its-defrule   "/"   "/")))
  )

(provide 'its/hira)
;;; its/hira.el ends here.

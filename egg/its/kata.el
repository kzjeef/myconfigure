;;; its/kata.el --- Katakana Input in Egg Input Method Architecture


;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Author: Satoru Tomura <tomura@etl.go.jp>

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

(defvar its-kata-enable-zenkaku-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Zenkaku alphabet")

(defvar its-kata-enable-double-n nil "*Enable \"nn\" input for \"ン\" ")
(defvar its-kata-period "。" "*ピリオド")  ; ". " "．"
(defvar its-kata-comma  "、" "*コンマ")    ; ", " "，"
(defvar its-kata-open-bracket  "「" "*[")  ; "［"
(defvar its-kata-close-bracket  "」" "*]") ; "］"
(defvar its-kata-horizontal  "ー" "*-")    ; "−"

(define-its-state-machine its-kata-map
  "roma-kata" "ア" Japanese
  "Map for Romaji-Katakana translation. (Japanese)"

  (defconst its-zenkaku-escape "Z")  ;; Escape character to Zenkaku inputs
  (defconst its-hankaku-escape "~")  ;; Escape character to Hankaku inputs

  (its-defrule-select-mode-temporally "q" downcase)
  (its-defrule-select-mode-temporally "Q" zenkaku-downcase)

;;; k      k
;;; kk     ッk
;;; kka    ッカ
;;;
;;; kkk    ッk DING!

  (its-defrule "tch"  "ッ" -2)

;;; 「ン」の入力

  (dolist (q1 '("b" "m" "p"))
    (its-defrule (concat "m" q1) "ン" -1))

  (its-defrule* "n"  "ン")
  (its-defrule  "n'" "ン")
  (its-defrule  "N"  "ン")

  (let ((small '"x" ))
    (its-defrule (concat small "a") "ァ")
    (its-defrule (concat small "i") "ィ")
    (its-defrule (concat small "u") "ゥ")
    (its-defrule (concat small "e") "ェ")
    (its-defrule (concat small "o") "ォ")
    (its-defrule (concat small "ya") "ャ")
    (its-defrule (concat small "yu") "ュ")
    (its-defrule (concat small "yo") "ョ")
    (its-defrule (concat small "tu") "ッ")
    (its-defrule (concat small "tsu") "ッ")
    (its-defrule (concat small "wa") "ヮ")
    )

  (its-defrule   "a"    "ア")
  (its-defrule   "i"    "イ")
  (its-defrule   "u"    "ウ")
  (its-defrule   "e"    "エ")
  (its-defrule   "o"    "オ")

  (dolist (k '(("ka"  "カ") ("ki"  "キ") ("ku"  "ク") ("ke"  "ケ") ("ko"  "コ")
	       ("kya" "キャ") ("kyu"  "キュ") ("kye"  "キェ") ("kyo"  "キョ")))
    (its-defrule (car k) (cadr k))
    (its-defrule (concat "k" (car k)) (concat "ッ" (cadr k))))
  (its-defoutput "kk" "ッk")
  (its-defoutput "kky" "ッky")

  (dolist (s '(("sa"  "サ") ("si"  "シ") ("su"  "ス") ("se"  "セ") ("so"  "ソ")
	       ("sya"  "シャ") ("syu"  "シュ") ("sye"  "シェ") ("syo"  "ショ")
	       ("sha"  "シャ") ("shi"  "シ") ("shu"  "シュ") ("she"  "シェ")
	       ("sho"  "ショ")))
    (its-defrule (car s) (cadr s))
    (its-defrule (concat "s" (car s)) (concat "ッ" (cadr s))))
  (its-defoutput "ss" "ッs")
  (its-defoutput "ssy" "ッsy")
  (its-defoutput "ssh" "ッsh")

  (dolist (T '(("ta"  "タ") ("ti"  "チ") ("tu"  "ツ") ("te"  "テ") ("to"  "ト")
	       ("tya"  "チャ") ("tyi"  "ティ") ("tyu"  "チュ") ("tye"  "チェ")
	       ("tyo"  "チョ") ("tsu"  "ツ")))
    (its-defrule (car T) (cadr T))
    (its-defrule (concat "t" (car T)) (concat "ッ" (cadr T))))
  (its-defoutput "tt" "ッt")
  (its-defoutput "tty" "ッty")
  (its-defoutput "tts" "ッts")

  (dolist (c '(("cha"  "チャ") ("chi"  "チ") ("chu"  "チュ")
	       ("che"  "チェ") ("cho"  "チョ")))
    (its-defrule (car c) (cadr c))
    (its-defrule (concat "c" (car c)) (concat "ッ" (cadr c))))
  (its-defoutput "cc" "ッc")
  (its-defoutput "cch" "ッch")

  (dolist (h '(("ha"  "ハ") ("hi"  "ヒ") ("hu"  "フ") ("he"  "ヘ") ("ho"  "ホ")
	       ("hya"  "ヒャ") ("hyu"  "ヒュ") ("hye"  "ヒェ") ("hyo"  "ヒョ")))
    (its-defrule (car h) (cadr h))
    (its-defrule (concat "h" (car h)) (concat "ッ" (cadr h))))
  (its-defoutput "hh" "ッh")
  (its-defoutput "hhy" "ッhy")

  (dolist (f '(("fa"  "ファ") ("fi"  "フィ") ("fu"  "フ") ("fe"  "フェ")
	       ("fo"  "フォ")))
    (its-defrule (car f) (cadr f))
    (its-defrule (concat "f" (car f)) (concat "ッ" (cadr f))))
  (its-defoutput "ff" "ッf")

  (dolist (r '(("ra"  "ラ") ("ri"  "リ") ("ru"  "ル") ("re"  "レ") ("ro"  "ロ")
	       ("rya"  "リャ") ("ryu"  "リュ") ("rye"  "リェ") ("ryo"  "リョ")))
    (its-defrule (car r) (cadr r))
    (its-defrule (concat "r" (car r)) (concat "ッ" (cadr r))))
  (its-defoutput "rr" "ッr")
  (its-defoutput "rry" "ッry")

  (dolist (l '(("la"  "ラ") ("li"  "リ") ("lu"  "ル") ("le"  "レ") ("lo"  "ロ")
	       ("lya"  "リャ") ("lyu"  "リュ") ("lye"  "リェ") ("lyo"  "リョ")))
    (its-defrule (car l) (cadr l))
    (its-defrule (concat "l" (car l)) (concat "ッ" (cadr l))))
  (its-defoutput "ll" "ッl")
  (its-defoutput "lly" "ッly")

  (dolist (g '(("ga"  "ガ") ("gi"  "ギ") ("gu"  "グ") ("ge"  "ゲ") ("go"  "ゴ")
	       ("gya"  "ギャ") ("gyu"  "ギュ") ("gye"  "ギェ") ("gyo"  "ギョ")))
    (its-defrule (car g) (cadr g))
    (its-defrule (concat "g" (car g)) (concat "ッ" (cadr g))))
  (its-defoutput "gg" "ッg")
  (its-defoutput "ggy" "ッgy")

  (dolist (z '(("za"  "ザ") ("zi"  "ジ") ("zu"  "ズ") ("ze"  "ゼ") ("zo"  "ゾ")
	       ("zya"  "ジャ") ("zyu"  "ジュ") ("zye"  "ジェ") ("zyo"  "ジョ")))
    (its-defrule (car z) (cadr z))
    (its-defrule (concat "z" (car z)) (concat "ッ" (cadr z))))
  (its-defoutput "zz" "ッz")
  (its-defoutput "zzy" "ッzy")

  (dolist (j '(("ja"  "ジャ") ("ji"  "ジ") ("ju"  "ジュ") ("je"  "ジェ")
	       ("jo"  "ジョ") ("jya"  "ジャ") ("jyu"  "ジュ") ("jye"  "ジェ")
	       ("jyo"  "ジョ")))
    (its-defrule (car j) (cadr j))
    (its-defrule (concat "j" (car j)) (concat "ッ" (cadr j))))
  (its-defoutput "jj" "ッj")
  (its-defoutput "jjy" "ッjy")

  (dolist (d '(("da"  "ダ") ("di"  "ヂ") ("du"  "ヅ") ("de"  "デ") ("do"  "ド")
	       ("dya"  "ヂャ") ("dyi"  "ディ") ("dyu"  "ヂュ") ("dye"  "ヂェ")
	       ("dyo"  "ヂョ")))
    (its-defrule (car d) (cadr d))
    (its-defrule (concat "d" (car d)) (concat "ッ" (cadr d))))
  (its-defoutput "dd" "ッd")
  (its-defoutput "ddy" "ッdy")

  (dolist (b '(("ba"  "バ") ("bi"  "ビ") ("bu"  "ブ") ("be"  "ベ") ("bo"  "ボ")
	       ("bya"  "ビャ") ("byu"  "ビュ") ("bye"  "ビェ") ("byo"  "ビョ")))
    (its-defrule (car b) (cadr b))
    (its-defrule (concat "b" (car b)) (concat "ッ" (cadr b))))
  (its-defoutput "bb" "ッb")
  (its-defoutput "bby" "ッby")

  (dolist (p '(("pa"  "パ") ("pi"  "ピ") ("pu"  "プ") ("pe"  "ペ") ("po"   "ポ")
	       ("pya"  "ピャ") ("pyu"  "ピュ") ("pye"  "ピェ") ("pyo"  "ピョ")))
    (its-defrule (car p) (cadr p))
    (its-defrule (concat "p" (car p)) (concat "ッ" (cadr p))))
  (its-defoutput "pp" "ッp")
  (its-defoutput "ppy" "ッpy")

  (dolist (v '(("va" "ヴァ") ("vi" "ヴィ") ("vu" "ヴ") ("ve" "ヴェ")
	       ("vo" "ヴォ")))
    (its-defrule (car v) (cadr v))
    (its-defrule (concat "v" (car v)) (concat "ッ" (cadr v))))
  (its-defoutput "vv" "ッv")

  (its-defrule   "ma"   "マ")
  (its-defrule   "mi"   "ミ")
  (its-defrule   "mu"   "ム")
  (its-defrule   "me"   "メ")
  (its-defrule   "mo"   "モ")
  (its-defrule   "mya"  "ミャ")
  (its-defrule   "myu"  "ミュ")
  (its-defrule   "mye"  "ミェ")
  (its-defrule   "myo"  "ミョ")
  (its-defrule   "ya"   "ヤ")
  (its-defrule   "yi"   "イ")
  (its-defrule   "yu"   "ユ")
  (its-defrule   "yo"   "ヨ")
  (its-defrule   "ye"   "イェ")
  (its-defrule   "wa"   "ワ")
  (its-defrule   "wi"   "ヰ")
  (its-defrule   "wu"   "ウ")
  (its-defrule   "we"   "ヱ")
  (its-defrule   "wo"   "ヲ")

  (its-defrule   "kwa"  "クヮ")
  (its-defrule   "kwi"  "クィ")
  (its-defrule   "kwu"  "ク")
  (its-defrule   "kwe"  "クェ")
  (its-defrule   "kwo"  "クォ")
  (its-defrule   "gwa"  "グヮ")
  (its-defrule   "gwi"  "グィ")
  (its-defrule   "gwu"  "グ")
  (its-defrule   "gwe"  "グェ")
  (its-defrule   "gwo"  "グォ")
  (its-defrule   "tsa"  "ツァ")
  (its-defrule   "tsi"  "ツィ")
  (its-defrule   "tse"  "ツェ")
  (its-defrule   "tso"  "ツォ")

  (its-defrule   "na"   "ナ")
  (its-defrule   "ni"   "ニ")
  (its-defrule   "nu"   "ヌ")
  (its-defrule   "ne"   "ネ")
  (its-defrule   "no"   "ノ")
  (its-defrule   "nya"  "ニャ")
  (its-defrule   "nyu"  "ニュ")
  (its-defrule   "nye"  "ニェ")
  (its-defrule   "nyo"  "ニョ")

  (its-defrule   "xka"  "ヵ")
  (its-defrule   "xke"  "ヶ")
  (its-defrule   "xti"  "ティ")
  (its-defrule   "xdi"  "ディ")
  (its-defrule   "xdu"  "ドゥ")
  (its-defrule   "xde"  "デェ")
  (its-defrule   "xdo"  "ドォ")
  (its-defrule   "xwi"  "ウィ")
  (its-defrule   "xwe"  "ウェ")
  (its-defrule   "xwo"  "ウォ")

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

(define-its-state-machine-append its-kata-map
  (if its-kata-enable-double-n
      (its-defrule "nn" "ン"))

  (its-defrule "-" its-kata-horizontal)
  (its-defrule "[" its-kata-open-bracket)
  (its-defrule "]" its-kata-close-bracket)
  (its-defrule "." its-kata-period)
  (its-defrule "," its-kata-comma)

  (if its-kata-enable-zenkaku-alphabet
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

(provide 'its/kata)
;;; its/kata.el ends here.

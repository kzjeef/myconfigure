;;; its/hankata.el --- Hnakaku Katakana Input in Egg Input Method Architecture

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


;;; Code:

(eval-when-compile
  (require 'its)
  (require 'cl))

(eval-when (compile)
  (defconst its-compaction-enable t))

(defvar its-han-kata-enable-double-n nil "*Enable \"nn\" input for \"ン\" ")
(defvar its-han-kata-enable-zenkaku-alphabet t "*Enable Zenkaku alphabet")
(defvar its-han-kata-period "｡" "*ピリオド")  ; ". " "．"
(defvar its-han-kata-comma  "､" "*コンマ")    ; ", " "，"
(defvar its-han-kata-open-bracket  "｢" "*[")  ; "［"
(defvar its-han-kata-close-bracket  "｣" "*]") ; "］"
(defvar its-han-kata-horizontal  "ｰ" "*-")    ; "−"

(define-its-state-machine its-han-kata-map
  "roma-han-kata" "ｱｱ" Japanese
  "Map for Romaji-Hankaku-Katakana translation. (Japanese)"

  (defconst its-hankaku-escape "~")  ;; Escape character to Hankaku inputs

  (its-defrule-select-mode-temporally "q" downcase)

;;; k      k
;;; kk     ッk
;;; kka    ッカ
;;;
;;; kkk    ッk DING!

  (its-defrule "tch"  "ｯ" -2)

;;; 「ン」の入力

  (dolist (q1 '("b" "m" "p"))
    (its-defrule (concat "m" q1) "ﾝ" -1))

  (its-defrule* "n"  "ﾝ")
  (its-defrule  "n'" "ﾝ")
  (its-defrule  "N"  "ﾝ")

  (let ((small '"x" ))
    (its-defrule (concat small "a") "ｧ")
    (its-defrule (concat small "i") "ｨ")
    (its-defrule (concat small "u") "ｩ")
    (its-defrule (concat small "e") "ｪ")
    (its-defrule (concat small "o") "ｫ")
    (its-defrule (concat small "ya") "ｬ")
    (its-defrule (concat small "yu") "ｭ")
    (its-defrule (concat small "yo") "ｮ")
    (its-defrule (concat small "tu") "ｯ")
    (its-defrule (concat small "tsu") "ｯ")
    (its-defrule (concat small "wa") "ﾜ")
    )

  (its-defrule   "a"    "ｱ")
  (its-defrule   "i"    "ｲ")
  (its-defrule   "u"    "ｳ")
  (its-defrule   "e"    "ｴ")
  (its-defrule   "o"    "ｵ")

  (dolist (k '(("ka"  "ｶ") ("ki"  "ｷ") ("ku"  "ｸ") ("ke"  "ｹ") ("ko"  "ｺ")
	       ("kya" "ｷｬ") ("kyu"  "ｷｭ") ("kye"  "ｷｪ") ("kyo"  "ｷｮ")))
    (its-defrule (car k) (cadr k))
    (its-defrule (concat "k" (car k)) (concat "ｯ" (cadr k))))
  (its-defoutput "kk" "ｯk")
  (its-defoutput "kky" "ｯky")

  (dolist (s '(("sa"  "ｻ") ("si"  "ｼ") ("su"  "ｽ") ("se"  "ｾ") ("so"  "ｿ")
	       ("sya"  "ｼｬ") ("syu"  "ｼｭ") ("sye"  "ｼｪ") ("syo"  "ｼｮ")
	       ("sha"  "ｼｬ") ("shi"  "ｼ") ("shu"  "ｼｭ") ("she"  "ｼｪ")
	       ("sho"  "ｼｮ")))
    (its-defrule (car s) (cadr s))
    (its-defrule (concat "s" (car s)) (concat "ｯ" (cadr s))))
  (its-defoutput "ss" "ｯs")
  (its-defoutput "ssy" "ｯsy")
  (its-defoutput "ssh" "ｯsh")

  (dolist (T '(("ta"  "ﾀ") ("ti"  "ﾁ") ("tu"  "ﾂ") ("te"  "ﾃ") ("to"  "ﾄ")
	       ("tya"  "ﾁｬ") ("tyi"  "ﾃｨ") ("tyu"  "ﾁｭ") ("tye"  "ﾁｪ")
	       ("tyo"  "ﾁｮ") ("tsu"  "ﾂ")))
    (its-defrule (car T) (cadr T))
    (its-defrule (concat "t" (car T)) (concat "ｯ" (cadr T))))
  (its-defoutput "tt" "ｯt")
  (its-defoutput "tty" "ｯty")
  (its-defoutput "tts" "ｯts")

  (dolist (c '(("cha"  "ﾁｬ") ("chi"  "ﾁ") ("chu"  "ﾁｭ")
	       ("che"  "ﾁｪ") ("cho"  "ﾁｮ")))
    (its-defrule (car c) (cadr c))
    (its-defrule (concat "c" (car c)) (concat "ｯ" (cadr c))))
  (its-defoutput "cc" "ｯc")
  (its-defoutput "cch" "ｯch")

  (dolist (h '(("ha"  "ﾊ") ("hi"  "ﾋ") ("hu"  "ﾌ") ("he"  "ﾍ") ("ho"  "ﾎ")
	       ("hya"  "ﾋｬ") ("hyu"  "ﾋｭ") ("hye"  "ﾋｪ") ("hyo"  "ﾋｮ")))
    (its-defrule (car h) (cadr h))
    (its-defrule (concat "h" (car h)) (concat "ｯ" (cadr h))))
  (its-defoutput "hh" "ｯh")
  (its-defoutput "hhy" "ｯhy")

  (dolist (f '(("fa"  "ﾌｧ") ("fi"  "ﾌｨ") ("fu"  "ﾌ") ("fe"  "ﾌｪ")
	       ("fo"  "ﾌｫ")))
    (its-defrule (car f) (cadr f))
    (its-defrule (concat "f" (car f)) (concat "ｯ" (cadr f))))
  (its-defoutput "ff" "ｯf")

  (dolist (r '(("ra"  "ﾗ") ("ri"  "ﾘ") ("ru"  "ﾙ") ("re"  "ﾚ") ("ro"  "ﾛ")
	       ("rya"  "ﾘｬ") ("ryu"  "ﾘｭ") ("rye"  "ﾘｪ") ("ryo"  "ﾘｮ")))
    (its-defrule (car r) (cadr r))
    (its-defrule (concat "r" (car r)) (concat "ｯ" (cadr r))))
  (its-defoutput "rr" "ｯr")
  (its-defoutput "rry" "ｯry")

  (dolist (l '(("la"  "ﾗ") ("li"  "ﾘ") ("lu"  "ﾙ") ("le"  "ﾚ") ("lo"  "ﾛ")
	       ("lya"  "ﾘｬ") ("lyu"  "ﾘｭ") ("lye"  "ﾘｪ") ("lyo"  "ﾘｮ")))
    (its-defrule (car l) (cadr l))
    (its-defrule (concat "l" (car l)) (concat "ｯ" (cadr l))))
  (its-defoutput "ll" "ｯl")
  (its-defoutput "lly" "ｯly")

  (dolist (g '(("ga"  "ｶﾞ") ("gi"  "ｷﾞ") ("gu"  "ｸﾞ") ("ge"  "ｹﾞ") ("go"  "ｺﾞ")
	       ("gya"  "ｷﾞｬ") ("gyu"  "ｷﾞｭ") ("gye"  "ｷﾞｪ") ("gyo"  "ｷﾞｮ")))
    (its-defrule (car g) (cadr g))
    (its-defrule (concat "g" (car g)) (concat "ｯ" (cadr g))))
  (its-defoutput "gg" "ｯg")
  (its-defoutput "ggy" "ｯgy")

  (dolist (z '(("za"  "ｻﾞ") ("zi"  "ｼﾞ") ("zu"  "ｽﾞ") ("ze"  "ｾﾞ") ("zo"  "ｿﾞ")
	       ("zya"  "ｼﾞｬ") ("zyu"  "ｼﾞｭ") ("zye"  "ｼﾞｪ") ("zyo"  "ｼﾞｮ")))
    (its-defrule (car z) (cadr z))
    (its-defrule (concat "z" (car z)) (concat "ｯ" (cadr z))))
  (its-defoutput "zz" "ｯz")
  (its-defoutput "zzy" "ｯzy")

  (dolist (j '(("ja"  "ｼﾞｬ") ("ji"  "ｼﾞ") ("ju"  "ｼﾞｭ") ("je"  "ｼﾞｪ")
	       ("jo"  "ｼﾞｮ") ("jya"  "ｼﾞｬ") ("jyu"  "ｼﾞｭ") ("jye"  "ｼﾞｪ")
	       ("jyo"  "ｼﾞｮ")))
    (its-defrule (car j) (cadr j))
    (its-defrule (concat "j" (car j)) (concat "ｯ" (cadr j))))
  (its-defoutput "jj" "ｯj")
  (its-defoutput "jjy" "ｯjy")

  (dolist (d '(("da"  "ﾀﾞ") ("di"  "ﾁﾞ") ("du"  "ﾂﾞ") ("de"  "ﾃﾞ") ("do"  "ﾄﾞ")
	       ("dya"  "ﾁﾞｬ") ("dyi"  "ﾃﾞｨ") ("dyu"  "ﾁﾞｭ") ("dye"  "ﾁﾞｪ")
	       ("dyo"  "ﾁﾞｮ")))
    (its-defrule (car d) (cadr d))
    (its-defrule (concat "d" (car d)) (concat "ｯ" (cadr d))))
  (its-defoutput "dd" "ｯd")
  (its-defoutput "ddy" "ｯdy")

  (dolist (b '(("ba"  "ﾊﾞ") ("bi"  "ﾋﾞ") ("bu"  "ﾌﾞ") ("be"  "ﾍﾞ") ("bo"  "ﾎﾞ")
	       ("bya"  "ﾋﾞｬ") ("byu"  "ﾋﾞｭ") ("bye"  "ﾋﾞｪ") ("byo"  "ﾋﾞｮ")))
    (its-defrule (car b) (cadr b))
    (its-defrule (concat "b" (car b)) (concat "ｯ" (cadr b))))
  (its-defoutput "bb" "ｯb")
  (its-defoutput "bby" "ｯby")

  (dolist (p '(("pa"  "ﾊﾟ") ("pi"  "ﾋﾟ") ("pu"  "ﾌﾟ") ("pe"  "ﾍﾟ") ("po"   "ﾎﾟ")
	       ("pya"  "ﾋﾟｬ") ("pyu"  "ﾋﾟｭ") ("pye"  "ﾋﾟｪ") ("pyo"  "ﾋﾟｮ")))
    (its-defrule (car p) (cadr p))
    (its-defrule (concat "p" (car p)) (concat "ｯ" (cadr p))))
  (its-defoutput "pp" "ｯp")
  (its-defoutput "ppy" "ｯpy")

  (dolist (v '(("va" "ｳﾞｧ") ("vi" "ｳﾞｨ") ("vu" "ｳﾞ") ("ve" "ｳﾞｪ")
	       ("vo" "ｳﾞｫ")))
    (its-defrule (car v) (cadr v))
    (its-defrule (concat "v" (car v)) (concat "ｯ" (cadr v))))
  (its-defoutput "vv" "ｯv")

  (its-defrule   "ma"   "ﾏ")
  (its-defrule   "mi"   "ﾐ")
  (its-defrule   "mu"   "ﾑ")
  (its-defrule   "me"   "ﾒ")
  (its-defrule   "mo"   "ﾓ")
  (its-defrule   "mya"  "ﾐｬ")
  (its-defrule   "myu"  "ﾐｭ")
  (its-defrule   "mye"  "ﾐｪ")
  (its-defrule   "myo"  "ﾐｮ")
  (its-defrule   "ya"   "ﾔ")
  (its-defrule   "yi"   "ｲ")
  (its-defrule   "yu"   "ﾕ")
  (its-defrule   "yo"   "ﾖ")
  (its-defrule   "ye"   "ｲｪ")
  (its-defrule   "wa"   "ﾜ")
  (its-defrule   "wi"   "ｨ")
  (its-defrule   "wu"   "ｳ")
  (its-defrule   "we"   "ｪ")
  (its-defrule   "wo"   "ｦ")

  (its-defrule   "kwa"  "ｸﾜ")
  (its-defrule   "kwi"  "ｸｨ")
  (its-defrule   "kwu"  "ｸ")
  (its-defrule   "kwe"  "ｸｪ")
  (its-defrule   "kwo"  "ｸｫ")
  (its-defrule   "gwa"  "ｸﾞﾜ")
  (its-defrule   "gwi"  "ｸﾞｨ")
  (its-defrule   "gwu"  "ｸﾞ")
  (its-defrule   "gwe"  "ｸﾞｪ")
  (its-defrule   "gwo"  "ｸﾞｫ")
  (its-defrule   "tsa"  "ﾂｧ")
  (its-defrule   "tsi"  "ﾂｨ")
  (its-defrule   "tse"  "ﾂｪ")
  (its-defrule   "tso"  "ﾂｫ")

  (its-defrule   "na"   "ﾅ")
  (its-defrule   "ni"   "ﾆ")
  (its-defrule   "nu"   "ﾇ")
  (its-defrule   "ne"   "ﾈ")
  (its-defrule   "no"   "ﾉ")
  (its-defrule   "nya"  "ﾆｬ")
  (its-defrule   "nyu"  "ﾆｭ")
  (its-defrule   "nye"  "ﾆｪ")
  (its-defrule   "nyo"  "ﾆｮ")

  (its-defrule   "xti"  "ﾃｨ")
  (its-defrule   "xdi"  "ﾃﾞｨ")
  (its-defrule   "xdu"  "ﾄﾞｩ")
  (its-defrule   "xde"  "ﾃﾞｪ")
  (its-defrule   "xdo"  "ﾄﾞｫ")
  (its-defrule   "xwi"  "ｳｨ")
  (its-defrule   "xwe"  "ｳｪ")
  (its-defrule   "xwo"  "ｳｫ")

;;;
;;; Symbol inputs
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
    (its-defrule (concat its-hankaku-escape upcase) upcase)))

(define-its-state-machine-append its-han-kata-map
  (if its-han-kata-enable-double-n
      (its-defrule "nn" "ﾝ"))

  (its-defrule "-" its-han-kata-horizontal)
  (its-defrule "[" its-han-kata-open-bracket)
  (its-defrule "]" its-han-kata-close-bracket)
  (its-defrule "." its-han-kata-period)
  (its-defrule "," its-han-kata-comma)
  )

(provide 'its/hankata)
;;; its/kata.el ends here.

;;; its/jiskana.el --- Hiragana Input in Egg Input Method Architecture

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc

;; Author: Kubo "Munk3" Hiroshi <hiroshi@netird.ad.jp>

;; This file is not a part of EGG.

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
;; This file is based on the rules of its/hira.el of Tamago 4
;;

;;; Code:

(eval-when-compile
  (require 'its)
  (require 'cl))

(eval-when (compile)
  (defconst its-compaction-enable t))

(defgroup jikana nil
  "JIS Keyboard kana Input Method"
  :group 'its)

(defvar its-jiskana-enable-zenkaku-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Zenkaku alphabet")

(defcustom its-jiskana-period "。" 
  "* >を入力したときの句点の文字: \"。\"  \". \" \"．\""
  :group 'jiskana :type 'string)

(defcustom its-jiskana-comma  "、"
 "* <を入力したときの読点の文字: \"、\" \", \" \"，\""
  :group 'jiskana :type 'string)

(defcustom its-jiskana-open-bracket  "「"
 "* { を入力したときのかぎ括弧開けの文字: \"「\" \"｛\""
  :group 'jiskana :type 'string)

(defcustom its-jiskana-close-bracket "」"
 "* } を入力したときのかぎ括弧閉じの文字: \"」\" \"｝\""
  :group 'jiskana :type 'string)

(defcustom its-jiskana-horizontal  "ー"
  "* - を入力したときの長音記号の文字: \"ー\" \"−\""
  :group 'jiskana :type 'string)

(define-its-state-machine its-jiskana-map
  "kana" "わ" Japanese
  "Map for JIS arrangement keyboard to hiragana translation. (Japanese)"

  (defconst its-zenkaku-escape "A")  ;; Escape character to Zenkaku inputs
  (defconst its-hankaku-escape "H")  ;; Escape character to Hankaku inputs

  (its-defrule-select-mode-temporally "D" downcase)
  (its-defrule-select-mode-temporally "Q" zenkaku-downcase)


  ;;; 清音

  ;; あ行
  (its-defrule   "3"    "あ")
  (its-defrule   "e"    "い")
  (its-defrule   "4"    "う")
  (its-defrule   "5"    "え")
  (its-defrule   "6"    "お")

  ;; か行
  (its-defrule*   "t"    "か"  "か")
  (its-defrule*   "g"    "き"  "き")
  (its-defrule*   "h"    "く"  "く")
  (its-defrule*   ":"    "け"  "け")
  (its-defrule*   "b"    "こ"  "こ")

  ;; さ行
  (its-defrule*   "x"    "さ"  "さ")
  (its-defrule*   "d"    "し"  "し")
  (its-defrule*   "r"    "す"  "す")
  (its-defrule*   "p"    "せ"  "せ")
  (its-defrule*   "c"    "そ"  "そ")

  ;; た行
  (its-defrule*   "q"    "た"  "た")
  (its-defrule*   "a"    "ち"  "ち")
  (its-defrule*   "z"    "つ"  "つ")
  (its-defrule*   "w"    "て"  "て")
  (its-defrule*   "s"    "と"  "と")

  ;; な行
  (its-defrule   "u"   "な")
  (its-defrule   "i"   "に")
  (its-defrule   "1"   "ぬ")
  (its-defrule   ","   "ね")
  (its-defrule   "k"   "の")

  ;; は行
  (its-defrule*   "f"   "は"  "は")
  (its-defrule*   "v"   "ひ"  "ひ")
  (its-defrule*   "2"   "ふ"  "ふ")
  (its-defrule*   "^"   "へ"  "へ")
  (its-defrule*   "-"   "ほ"  "ほ")

  ;; ま行
  (its-defrule   "j"   "ま")
  (its-defrule   "n"   "み")
  (its-defrule   "]"   "む")
  (its-defrule   "/"   "め")
  (its-defrule   "m"   "も")

  ;; や行
  (its-defrule   "7"   "や")
  (its-defrule   "8"   "ゆ")
  (its-defrule   "9"   "よ")

  ;; ら行
  (its-defrule   "o"   "ら")
  (its-defrule   "l"   "り")
  (its-defrule   "."   "る")
  (its-defrule   ";"   "れ")
  (its-defrule   "\\"   "ろ")

  ;; わ行
  (its-defrule   "0"   "わ")
  (its-defrule   "F"   "ゐ")
  (its-defrule   "J"   "ゑ")
  (its-defrule   "~"   "を")
  (its-defrule   "y"   "ん")

  ;;; 濁音
  ;; が行
  (its-defrule   "t@"    "が")
  (its-defrule   "g@"    "ぎ")
  (its-defrule   "h@"    "ぐ")
  (its-defrule   ":@"    "げ")
  (its-defrule   "b@"    "ご")

  ;; ざ行
  (its-defrule   "x@"    "ざ")
  (its-defrule   "d@"    "じ")
  (its-defrule   "r@"    "ず")
  (its-defrule   "p@"    "ぜ")
  (its-defrule   "c@"    "ぞ")

  ;; だ行
  (its-defrule   "q@"    "だ")
  (its-defrule   "a@"    "ぢ")
  (its-defrule   "z@"    "づ")
  (its-defrule   "w@"    "で")
  (its-defrule   "s@"    "ど")

  ;; ば行
  (its-defrule   "f@"   "ば")
  (its-defrule   "v@"   "び")
  (its-defrule   "2@"   "ぶ")
  (its-defrule   "^@"   "べ")
  (its-defrule   "-@"   "ぼ")

  ;; ぱ行
  (its-defrule   "f["   "ぱ")
  (its-defrule   "v["   "ぴ")
  (its-defrule   "2["   "ぷ")
  (its-defrule   "^["   "ぺ")
  (its-defrule   "-["   "ぽ")

  ;;; 
  ;; や行
  (its-defrule   "'"   "ゃ")
  (its-defrule   "("   "ゅ")
  (its-defrule   ")"   "ょ")

  ;; あ行
  (its-defrule "#"   "ぁ")
  (its-defrule "E"   "ぃ")
  (its-defrule "$"   "ぅ")
  (its-defrule "%"   "ぇ")
  (its-defrule "&"   "ぉ")

  (its-defrule "Z"  "っ")
  (its-defrule "W0" "ゎ")

  (its-defrule "4@" "ヴ")

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
  (its-defrule   "X1"   "○")	(its-defrule   "X!"   "●")
  (its-defrule   "X2"   "▽")	(its-defrule   "X@"   "▼")
  (its-defrule   "X3"   "△")	(its-defrule   "X#"   "▲")
  (its-defrule   "X4"   "□")	(its-defrule   "X$"   "■")
  (its-defrule   "X5"   "◇")	(its-defrule   "X%"   "◆")
  (its-defrule   "X6"   "☆")	(its-defrule   "X^"   "★")
  (its-defrule   "X7"   "◎")	(its-defrule   "X&"   "£")
  (its-defrule   "X8"   "¢")	(its-defrule   "X*"   "×")
  (its-defrule   "X9"   "♂")	(its-defrule   "X("   "【")
  (its-defrule   "X0"   "♀")	(its-defrule   "X)"   "】")
  (its-defrule   "X-"   "〜")	(its-defrule   "X_"   "∴")
  (its-defrule   "X="   "≠")	(its-defrule   "X+"   "±")
  (its-defrule   "X\\"  "＼")	(its-defrule   "X|"   "‖")
  (its-defrule   "X`"   "´")	(its-defrule   "X~"   "¨")

  (its-defrule   "Xq"   "《")	(its-defrule   "XQ"   "〈")
  (its-defrule   "Xw"   "》")	(its-defrule   "XW"   "〉")
					; e
  (its-defrule   "Xr"   "々")	(its-defrule   "XR"   "仝")
  (its-defrule   "Xt"   "〆")	(its-defrule   "XT"   "§")
					; y u i o
  (its-defrule   "Xp"   "〒")	(its-defrule   "XP"   "↑")
  (its-defrule   "X["   "『")	(its-defrule   "X{"   "〔")
  (its-defrule   "X]"   "』")	(its-defrule   "X}"   "〕")

					; a
  (its-defrule   "Xs"   "ヽ")	(its-defrule   "XS"   "ヾ")
  (its-defrule   "Xd"   "ゝ")	(its-defrule   "XD"   "ゞ")
  (its-defrule   "Xf"   "〃")	(its-defrule   "XF"   "→")
  (its-defrule   "Xg"   "‐")	(its-defrule   "XG"   "―")
  (its-defrule   "Xh"   "←")
  (its-defrule   "Xj"   "↓")
  (its-defrule   "Xk"   "↑")
  (its-defrule   "Xl"   "→")
  (its-defrule   "X;"   "゛")	(its-defrule   "X:"   "゜")
  (its-defrule   "X\'"  "‘")	(its-defrule   "X\""  "“")

					; z
  (its-defrule   "Xx"   ":-")	(its-defrule   "XX"   ":-)")
  (its-defrule   "Xc"   "〇")	(its-defrule   "XC"   "℃")
  (its-defrule   "Xv"   "※")	(its-defrule   "XV"   "÷")
  (its-defrule   "Xb"   "°")	(its-defrule   "XB"   "←")
  (its-defrule   "Xn"   "′")	(its-defrule   "XN"   "↓")
  (its-defrule   "Xm"   "″")	(its-defrule   "XM"   "〓")
  (its-defrule   "X,"   "‥")	(its-defrule   "X<"   "≦")
  (its-defrule   "X."   "…")	(its-defrule   "X>"   "≧")
  (its-defrule   "X/"   "・")	(its-defrule   "X?"   "∞")
  )

(define-its-state-machine-append its-jiskana-map

  (its-defrule "|" its-jiskana-horizontal)
  (its-defrule "{" its-jiskana-open-bracket)
  (its-defrule "}" its-jiskana-close-bracket)
  (its-defrule ">" its-jiskana-period)
  (its-defrule "<" its-jiskana-comma)

  )

(provide 'its/jiskana)
;;; its/jiskana.el ends here.

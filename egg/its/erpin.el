;;; its/erpin.el --- Erpin Input in Egg Input Method Architecture

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

(eval-when (compile)
  (defconst its-compaction-enable t))

(defvar its-erpin-cn-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-erpin-cn-open-braket  "$A!8(B" "*[") ; "$A#[(B"
(defvar its-erpin-cn-close-braket "$A!9(B" "*]") ; "$A#](B"

(defvar its-erpin-tw-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-erpin-tw-open-braket  "$(G!V(B" "*[") ; "$(G!b(B "
(defvar its-erpin-tw-close-braket "$(G!W(B" "*]") ; "$(G!c(B"

(eval-when-compile
  (defun its-define-erpin-qingsheng (shengmu yunmu &optional y)
    (let ((input (concat (car shengmu) yunmu))
	  (output (concat (cdr shengmu) (if y y yunmu) "(0@(B")))
      (prog1
	  (its-defrule input output)
	(its-defrule (concat input " ") output)
	(its-defrule (concat input "0") output))))

  (defmacro its-do-erpin-table (list)
    `(progn
       ,@(mapcar (lambda (syl)
		   `(its-define-erpin ,(car syl) ,(cdr syl)))
		 list)))

  (defmacro its-define-erpin (shengmu yunmu)
    `(let ((y1 (nth 1 ,yunmu)) (y2 (nth 2 ,yunmu)) (y3 (nth 3 ,yunmu))
	   (y4 (nth 4 ,yunmu)) (y5 (nth 5 ,yunmu)) (y (car ,yunmu))
	   (ss (list ,@shengmu)) sa sd state)
       (while ss
	 (setq sa (caar ss) sd (cdar ss) 
	       state (its-define-erpin-qingsheng (car ss) y y5))
	 (its-make-next-state state ?1 (concat sd y1 "(0@(B"))
	 (its-make-next-state state ?2 (concat sd y2 "(0@(B"))
	 (its-make-next-state state ?3 (concat sd y3 "(0@(B"))
	 (its-make-next-state state ?4 (concat sd y4 "(0@(B"))
	 (setq ss (cdr ss)))))

  (defmacro its-define-erpin-table ()
    '(let ((O '("o" . ""))  (B '("b" . "B")) (C '("c" . "C")) (D '("d" . "D"))
	   (F '("f" . "F")) (G '("g" . "G")) (H '("h" . "H")) (J '("j" . "J"))
	   (K '("k" . "K")) (L '("l" . "L")) (M '("m" . "M")) (N '("n" . "N"))
	   (P '("p" . "P")) (Q '("q" . "Q")) (R '("r" . "R")) (S '("s" . "S"))
	   (T '("t" . "T")) (W '("w" . "W")) (X '("x" . "X")) (Y '("y" . "Y"))
	   (Z '("z" . "Z"))
	   (I '("i" . "Ch")) (U '("u" . "Sh")) (V '("v" . "Zh"))

	   (a    '("a"  "(0!(B"    "(0"(B"    "(0#(B"    "(0$(B"    "a"   ))
	   (ai   '("s"  "(0!(Bi"   "(0"(Bi"   "(0#(Bi"   "(0$(Bi"   "ai"  ))
	   (an   '("f"  "(0!(Bn"   "(0"(Bn"   "(0#(Bn"   "(0$(Bn"   "an"  ))
	   (ang  '("g"  "(0!(Bng"  "(0"(Bng"  "(0#(Bng"  "(0$(Bng"  "ang" ))
	   (ao   '("d"  "(0!(Bo"   "(0"(Bo"   "(0#(Bo"   "(0$(Bo"   "ao"  ))
	   (e    '("e"  "(0%(B"    "(0&(B"    "(0'(B"    "(0((B"    "e"   ))
	   (ei   '("w"  "(0%(Bi"   "(0&(Bi"   "(0'(Bi"   "(0((Bi"   "ei"  ))
	   (en   '("r"  "(0%(Bn"   "(0&(Bn"   "(0'(Bn"   "(0((Bn"   "en"  ))
	   (eng  '("t"  "(0%(Bng"  "(0&(Bng"  "(0'(Bng"  "(0((Bng"  "eng" ))
	   (er   '("y"  "(0%(Br"   "(0&(Br"   "(0'(Br"   "(0((Br"   "er"  ))
	   (i    '("i"  "(0)(B"    "(0*(B"    "(0+(B"    "(0,(B"    "i"   ))
	   (ia   '("p"  "i(0!(B"   "i(0"(B"   "i(0#(B"   "i(0$(B"   "ia"  ))
	   (ian  '("h"  "i(0!(Bn"  "i(0"(Bn"  "i(0#(Bn"  "i(0$(Bn"  "ian" ))
	   (iang '("j"  "i(0!(Bng" "i(0"(Bng" "i(0#(Bng" "i(0$(Bng" "iang"))
	   (iao  '("k"  "i(0!(Bo"  "i(0"(Bo"  "i(0#(Bo"  "i(0$(Bo"  "iao" ))
	   (ie   '("l"  "i(0%(B"   "i(0&(B"   "i(0'(B"   "i(0((B"   "ie"  ))
	   (in   '("m"  "(0)(Bn"   "(0*(Bn"   "(0+(Bn"   "(0,(Bn"   "in"  ))
	   (ing  '("n"  "(0)(Bng"  "(0*(Bng"  "(0+(Bng"  "(0,(Bng"  "ing" ))
	   (iong '("b"  "i(0-(Bng" "i(0.(Bng" "i(0/(Bng" "i(00(Bng" "iong"))
	   (iu   '("y"  "i(01(B"   "i(02(B"   "i(03(B"   "i(04(B"   "iu"  ))
	   (o    '("o"  "(0-(B"    "(0.(B"    "(0/(B"    "(00(B"    "o"   ))
	   (ong  '("b"  "(0-(Bng"  "(0.(Bng"  "(0/(Bng"  "(00(Bng"  "ong" ))
	   (ou   '("q"  "(0-(Bu"   "(0.(Bu"   "(0/(Bu"   "(00(Bu"   "ou"  ))
	   (u    '("u"  "(01(B"    "(02(B"    "(03(B"    "(04(B"    "u"   ))
	   (v    '("v"  "(05(B"    "(06(B"    "(07(B"    "(08(B"    "(09(B"   ))
	   (ua   '("p"  "u(0!(B"   "u(0"(B"   "u(0#(B"   "u(0$(B"   "ua"  ))
	   (uai  '("k"  "u(0!(Bi"  "u(0"(Bi"  "u(0#(Bi"  "u(0$(Bi"  "uai" ))
	   (uan  '("x"  "u(0!(Bn"  "u(0"(Bn"  "u(0#(Bn"  "u(0$(Bn"  "uan" ))
	   (uang '("j"  "u(0!(Bng" "u(0"(Bng" "u(0#(Bng" "u(0$(Bng" "uang"))
	   (ue   '("c"  "u(0%(B"   "u(0&(B"   "u(0'(B"   "u(0((B"   "ue"  ))
	   (ve   '("c"  "(09%(B"   "(09&(B"   "(09'(B"   "(09((B"   "(09(Be"  ))
	   (ui   '("c"  "u(0)(B"   "u(0*(B"   "u(0+(B"   "u(0,(B"   "ui"  ))
	   (un   '("z"  "(01(Bn"   "(02(Bn"   "(03(Bn"   "(04(Bn"   "un"  ))
	   (uo   '("o"  "u(0-(B"   "u(0.(B"   "u(0/(B"   "u(00(B"   "uo"  )))

       (its-do-erpin-table
	(((O B C D F G H   K L M N P     S T W   Y Z I U V) . a)
	 ((O B C D   G H   K L M N P     S T W     Z I U V) . ai)
	 ((O B C D F G H   K L M N P   R S T W   Y Z I U V) . an)
	 ((O B C D F G H   K L M N P   R S T W   Y Z I U V) . ang)
	 ((O B C D   G H   K L M N P   R S T     Y Z I U V) . ao)
	 ((O   C D   G H   K L M N     R S T     Y Z I U V) . e)
	 ((O B C D F G H   K L M N P       T W     Z   U V) . ei)
	 ((O B C D F G H   K   M N P   R S   W     Z I U V) . en)
	 ((O B C D F G H   K L M N P   R S T W     Z I U V) . eng)
	 ((O                                              ) . er)
	 ((  B C D       J   L M N P Q R S T   X Y Z I U V) . i)
	 ((      D       J   L       Q         X          ) . ia)
	 ((  B   D       J   L M N P Q     T   X          ) . ian)
	 ((              J   L   N   Q         X          ) . iang)
	 ((  B   D       J   L M N P Q     T   X          ) . iao)
	 ((  B   D       J   L M N P Q     T   X          ) . ie)
	 ((  B           J   L M N P Q         X Y        ) . in)
	 ((  B   D       J   L M N P Q     T   X Y        ) . ing)
	 ((              J           Q         X          ) . iong)
	 ((      D       J   L M N   Q         X          ) . iu)
	 ((O B     F           M   P         W   Y        ) . o)
	 ((    C D   G H   K L   N     R S T     Y Z I   V) . ong)
	 ((O   C D F G H   K L M N P   R S T     Y Z I U V) . ou)
	 ((  B C D F G H J K L M N P Q R S T W X Y Z I U V) . u)
	 ((                  L   N                        ) . v)
	 ((          G H   K           R             I U V) . ua)
	 ((          G H   K                         I U V) . uai)
	 ((    C D   G H J K L   N   Q R S T   X Y Z I U V) . uan)
	 ((          G H   K                         I U V) . uang)
	 ((              J           Q         X Y        ) . ue)
	 ((                  L   N                        ) . ve)
	 ((    C D   G H   K           R S T       Z I U V) . ui)
	 ((    C D   G H J K L       Q R S T   X Y Z I U V) . un)
	 ((    C D   G H   K L   N     R S T       Z I U V) . uo)

	 (('("" . "")) . (cons "er" (cdr er)))

	 ((J Q X) . (cons "a" (cdr ia  )))
	 ((J Q X) . (cons "s" (cdr ia  )))
	 ((J Q X) . (cons "f" (cdr ian )))
	 ((J Q X) . (cons "g" (cdr iang)))
	 ((J Q X) . (cons "d" (cdr iao )))
	 ((J Q X) . (cons "e" (cdr ie  )))
	 ((J Q X) . (cons "w" (cdr ie  )))
	 ((J Q X) . (cons "r" (cdr in  )))
	 ((J Q X) . (cons "t" (cdr ing )))
	 ((J Q X) . (cons "q" (cdr iu  )))))

       (dolist (SHENG (list B C D F G H J K L M N P Q R S T W X Y Z I U V))
	 (its-defoutput (car SHENG) (cdr SHENG)))

       (its-define-erpin-qingsheng	H	 "m")
       (its-define-erpin-qingsheng	H	 "n"	"ng")
       (its-define-erpin-qingsheng	O	 "m")
       (its-define-erpin-qingsheng	O	 "n")

       (its-defrule	"on5"	"ng(0@(B")
       (its-defrule	"on2"	"(0=@(B")
       (its-defrule	"on3"	"(0>@(B")
       (its-defrule	"on4"	"(0?@(B"))))

(define-its-state-machine its-erpin-cn-map
  "erpin-cn" "$A6~(BG" Chinese-GB
  "Map for Erpin input. (Chinese-GB)"

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (its-define-erpin-table)
  (its-defrule	"b "	"$A2;(B")
  (its-defrule	"c "	"$A2E(B")
  (its-defrule	"ch "	"$A3v(B")
  (its-defrule	"d "	"$A5D(B")
  (its-defrule	"f "	"$A74(B")
  (its-defrule	"g "	"$A8v(B")
  (its-defrule	"h "	"$A:M(B")
  (its-defrule	"i "	"$AR;(B")
  (its-defrule	"j "	"$A>M(B")
  (its-defrule	"k "	"$A?I(B")
  (its-defrule	"l "	"$AAK(B")
  (its-defrule	"m "	"$AC?(B")
  (its-defrule	"n "	"$ADj(B")
  (its-defrule	"p "	"$AEz(B")
  (its-defrule	"q "	"$AH%(B")
  (its-defrule	"r "	"$AHU(B")
  (its-defrule	"s "	"$AJG(B")
  (its-defrule	"u "	"$AIO(B")
  (its-defrule	"t "	"$AK{(B")
  (its-defrule	"w "	"$ANR(B")
  (its-defrule	"x "	"$AOr(B")
  (its-defrule	"y "	"$ASV(B")
  (its-defrule	"z "	"$ATZ(B")
  (its-defrule	"v "	"$AWE(B")

  (dolist (ascii '(("0" . "$A#0(B")  ("1" . "$A#1(B")  ("2" . "$A#2(B")  ("3" . "$A#3(B")
		   ("4" . "$A#4(B")  ("5" . "$A#5(B")  ("6" . "$A#6(B")  ("7" . "$A#7(B")
		   ("8" . "$A#8(B")  ("9" . "$A#9(B") 
		   (" " . "$A!!(B")  ("!" . "$A#!(B")  ("@" . "$A#@(B")  ("#" . "$A##(B")
		   ("$" . "$A!g(B")  ("%" . "$A#%(B")  ("^" . "$A#^(B")  ("&" . "$A#&(B")
		   ("*" . "$A#*(B")  ("(" . "$A#((B")  (")" . "$A#)(B")
		   ("-" . "$A#-(B")  ("=" . "$A#=(B")  ("`" . "$A#`(B")  ("\\" . "$A#\(B")
		   ("|" . "$A#|(B")  ("_" . "$A#_(B")  ("+" . "$A#+(B")  ("~" . "$A!+(B")
		   ("[" . "$A#[(B")  ("]" . "$A#](B")  ("{" . "$A#{(B")  ("}" . "$A#}(B")
		   (":" . "$A#:(B")  (";" . "$A#;(B")  ("\"" . "$A#"(B") ("'" . "$A#'(B")
		   ("<" . "$A#<(B")  (">" . "$A#>(B")  ("?" . "$A#?(B")  ("/" . "$A#/(B")
		   ("," . "$A#,(B")  ("." . "$A#.(B")
		   ("a" . "$A#a(B")  ("b" . "$A#b(B")  ("c" . "$A#c(B")  ("d" . "$A#d(B")
		   ("e" . "$A#e(B")  ("f" . "$A#f(B")  ("g" . "$A#g(B")  ("h" . "$A#h(B")
		   ("i" . "$A#i(B")  ("j" . "$A#j(B")  ("k" . "$A#k(B")  ("l" . "$A#l(B")
		   ("m" . "$A#m(B")  ("n" . "$A#n(B")  ("o" . "$A#o(B")  ("p" . "$A#p(B")
		   ("q" . "$A#q(B")  ("r" . "$A#r(B")  ("s" . "$A#s(B")  ("t" . "$A#t(B")
		   ("u" . "$A#u(B")  ("v" . "$A#v(B")  ("w" . "$A#w(B")  ("x" . "$A#x(B")
		   ("y" . "$A#y(B")  ("z" . "$A#z(B")
		   ("A" . "$A#A(B")  ("B" . "$A#B(B")  ("C" . "$A#C(B")  ("D" . "$A#D(B")
		   ("E" . "$A#E(B")  ("F" . "$A#F(B")  ("G" . "$A#G(B")  ("H" . "$A#H(B")
		   ("I" . "$A#I(B")  ("J" . "$A#J(B")  ("K" . "$A#K(B")  ("L" . "$A#L(B")
		   ("M" . "$A#M(B")  ("N" . "$A#N(B")  ("O" . "$A#O(B")  ("P" . "$A#P(B")
		   ("Q" . "$A#Q(B")  ("R" . "$A#R(B")  ("S" . "$A#S(B")  ("T" . "$A#T(B")
		   ("U" . "$A#U(B")  ("V" . "$A#V(B")  ("W" . "$A#W(B")  ("X" . "$A#X(B")
		   ("Y" . "$A#Y(B")  ("Z" . "$A#Z(B")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule (concat its-banjiao-escape in) in)
      (its-defrule (concat its-quanjiao-escape in) out)))

  (its-defrule	","	"$A#,(B")
  (its-defrule	"."	"$A!#(B")
  (its-defrule	"/"	"$A!"(B")
  (its-defrule	":"	"$A#:(B")
  (its-defrule	";"	"$A#;(B")
  (its-defrule	"?"	"$A#?(B")
  (its-defrule	"!"	"$A#!(B"))

(define-its-state-machine its-erpin-tw-map
  "erpin-tw" "$(GD((BC" Chinese-CNS
  "Map for Erpin input."

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (its-define-erpin-table)
  (its-defrule	"b "	"$(GDb(B")
  (its-defrule	"c "	"$(GD_(B")
  (its-defrule	"ch "	"$(GEx(B")
  (its-defrule	"d "	"$(GN{(B")
  (its-defrule	"f "	"$(GE0(B")
  (its-defrule	"g "	"$(GT6(B")
  (its-defrule	"h "	"$(GLO(B")
  (its-defrule	"i "	"$(GD!(B")
  (its-defrule	"j "	"$(G^s(B")
  (its-defrule	"k "	"$(GF+(B")
  (its-defrule	"l "	"$(GD'(B")
  (its-defrule	"m "	"$(GJd(B")
  (its-defrule	"n "	"$(GH!(B")
  (its-defrule	"p "	"$(GJG(B")
  (its-defrule	"q "	"$(GF*(B")
  (its-defrule	"r "	"$(GEJ(B")
  (its-defrule	"s "	"$(GQR(B")
  (its-defrule	"u "	"$(GD8(B")
  (its-defrule	"t "	"$(GEl(B")
  (its-defrule	"w "	"$(GJ<(B")
  (its-defrule	"x "	"$(GGW(B")
  (its-defrule	"y "	"$(GD4(B")
  (its-defrule	"z "	"$(GGc(B")
  (its-defrule	"v "	"$(Gaa(B")

  (dolist (ascii '(("0" . "$(G$!(B")  ("1" . "$(G$"(B")  ("2" . "$(G$#(B")  ("3" . "$(G$$(B")
		   ("4" . "$(G$%(B")  ("5" . "$(G$&(B")  ("6" . "$(G$'(B")  ("7" . "$(G$((B")
		   ("8" . "$(G$)(B")  ("9" . "$(G$*(B") 
		   (" " . "$(G!!(B")  ("!" . "$(G!*(B")  ("@" . "$(G"i(B")  ("#" . "$(G!l(B")
		   ("$" . "$(G"c(B")  ("%" . "$(G"h(B")  ("^" . "$(G!T(B")  ("&" . "$(G!m(B")
		   ("*" . "$(G!n(B")  ("(" . "$(G!>(B")  (")" . "$(G!?(B")
		   ("-" . "$(G"1(B")  ("=" . "$(G"8(B")  ("`" . "$(G!j(B")  ("\\" . "$(G"`(B")
		   ("|" . "$(G"^(B")  ("_" . "$(G"%(B")  ("+" . "$(G"0(B")  ("~" . "$(G"D(B")
		   ("[" . "$(G!b(B")  ("]" . "$(G!c(B")  ("{" . "$(G!B(B")  ("}" . "$(G!C(B")
		   (":" . "$(G!((B")  (";" . "$(G!'(B")  ("\"" . "$(G!i(B") ("'" . "$(G!k(B")
		   ("<" . "$(G"6(B")  (">" . "$(G"7(B")  ("?" . "$(G!)(B")  ("/" . "$(G"_(B")
		   ("," . "$(G!"(B")  ("." . "$(G!%(B")
		   ("a" . "$(G$[(B")  ("b" . "$(G$\(B")  ("c" . "$(G$](B")  ("d" . "$(G$^(B")
		   ("e" . "$(G$_(B")  ("f" . "$(G$`(B")  ("g" . "$(G$a(B")  ("h" . "$(G$b(B")
		   ("i" . "$(G$c(B")  ("j" . "$(G$d(B")  ("k" . "$(G$e(B")  ("l" . "$(G$f(B")
		   ("m" . "$(G$g(B")  ("n" . "$(G$h(B")  ("o" . "$(G$i(B")  ("p" . "$(G$j(B")
		   ("q" . "$(G$k(B")  ("r" . "$(G$l(B")  ("s" . "$(G$m(B")  ("t" . "$(G$n(B")
		   ("u" . "$(G$o(B")  ("v" . "$(G$p(B")  ("w" . "$(G$q(B")  ("x" . "$(G$r(B")
		   ("y" . "$(G$s(B")  ("z" . "$(G$t(B")
		   ("A" . "$(G$A(B")  ("B" . "$(G$B(B")  ("C" . "$(G$C(B")  ("D" . "$(G$D(B")
		   ("E" . "$(G$E(B")  ("F" . "$(G$F(B")  ("G" . "$(G$G(B")  ("H" . "$(G$H(B")
		   ("I" . "$(G$I(B")  ("J" . "$(G$J(B")  ("K" . "$(G$K(B")  ("L" . "$(G$L(B")
		   ("M" . "$(G$M(B")  ("N" . "$(G$N(B")  ("O" . "$(G$O(B")  ("P" . "$(G$P(B")
		   ("Q" . "$(G$Q(B")  ("R" . "$(G$R(B")  ("S" . "$(G$S(B")  ("T" . "$(G$T(B")
		   ("U" . "$(G$U(B")  ("V" . "$(G$V(B")  ("W" . "$(G$W(B")  ("X" . "$(G$X(B")
		   ("Y" . "$(G$Y(B")  ("Z" . "$(G$Z(B")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule (concat its-banjiao-escape in) in)
      (its-defrule (concat its-quanjiao-escape in) out)))

  (its-defrule	","	"$(G!"(B")
  (its-defrule	"."	"$(G!$(B")
  (its-defrule	"/"	"$(G!#(B")
  (its-defrule	":"	"$(G!((B")
  (its-defrule	";"	"$(G!'(B")
  (its-defrule	"?"	"$(G!)(B")
  (its-defrule	"!"	"$(G!*(B"))

(define-its-state-machine-append its-erpin-cn-map
  (its-defrule "[" its-erpin-cn-open-braket)
  (its-defrule "]" its-erpin-cn-close-braket)

  (if its-erpin-cn-enable-quanjioao-alphabet
      (progn
	(its-defrule "1"  "$A#1(B")  (its-defrule "2"  "$A#2(B")
	(its-defrule "3"  "$A#3(B")  (its-defrule "4"  "$A#4(B")
	(its-defrule "5"  "$A#5(B")  (its-defrule "6"  "$A#6(B")
	(its-defrule "7"  "$A#7(B")  (its-defrule "8"  "$A#8(B")
	(its-defrule "9"  "$A#9(B")  (its-defrule "0"  "$A#0(B")
	(its-defrule "@"  "$A#@(B")
	(its-defrule "#"  "$A##(B")  (its-defrule "$"  "$A!g(B")
	(its-defrule "%"  "$A#%(B")  (its-defrule "^"  "$A#^(B")
	(its-defrule "&"  "$A#&(B")  (its-defrule "*"  "$A#*(B")
	(its-defrule "("  "$A#((B")  (its-defrule ")"  "$A#)(B")
	(its-defrule "-"  "$A#-(B")  (its-defrule "~"  "$A!+(B")
	(its-defrule "="  "$A#=(B")  (its-defrule "`"  "$A#`(B")
	(its-defrule "\\" "$A#\(B")  (its-defrule "|"  "$A#|(B")
	(its-defrule "_"  "$A#_(B")  (its-defrule "+"  "$A#+(B")
	(its-defrule "{"  "$A#{(B")  (its-defrule "}"  "$A#}(B")
	(its-defrule "\"" "$A#"(B")  (its-defrule "'"  "$A#'(B")
	(its-defrule "<"  "$A#<(B")  (its-defrule ">"  "$A#>(B"))
    (progn
      (its-defrule "1"  "1")  (its-defrule "2"  "2")
      (its-defrule "3"  "3")  (its-defrule "4"  "4")
      (its-defrule "5"  "5")  (its-defrule "6"  "6")
      (its-defrule "7"  "7")  (its-defrule "8"  "8")
      (its-defrule "9"  "9")  (its-defrule "0"  "0")
      (its-defrule "@"  "@")
      (its-defrule "#"  "#")  (its-defrule "$"  "$")
      (its-defrule "%"  "%")  (its-defrule "^"  "^")
      (its-defrule "&"  "&")  (its-defrule "*"  "*")
      (its-defrule "("  "(")  (its-defrule ")"  ")")
      (its-defrule "-"  "-")  (its-defrule "~"  "~")
      (its-defrule "="  "=")  (its-defrule "`"  "`")
      (its-defrule "\\" "\\") (its-defrule "|"  "|")
      (its-defrule "_"  "_")  (its-defrule "+"  "+")
      (its-defrule "{"  "{")  (its-defrule "}"  "}")
      (its-defrule "\"" "\"") (its-defrule "'"  "'")
      (its-defrule "<"  "<")  (its-defrule ">"  ">"))))

(define-its-state-machine-append its-erpin-tw-map
  (its-defrule "[" its-erpin-tw-open-braket)
  (its-defrule "]" its-erpin-tw-close-braket)

  (if its-erpin-tw-enable-quanjioao-alphabet
      (progn
	(its-defrule "1"  "$(G$"(B")  (its-defrule "2"  "$(G$#(B")
	(its-defrule "3"  "$(G$$(B")  (its-defrule "4"  "$(G$%(B")
	(its-defrule "5"  "$(G$&(B")  (its-defrule "6"  "$(G$'(B")
	(its-defrule "7"  "$(G$((B")  (its-defrule "8"  "$(G$)(B")
	(its-defrule "9"  "$(G$*(B")  (its-defrule "0"  "$(G$!(B")
	(its-defrule "@"  "$(G"i(B")
	(its-defrule "#"  "$(G!l(B")  (its-defrule "$"  "$(G"c(B")
	(its-defrule "%"  "$(G"h(B")  (its-defrule "^"  "$(G!T(B")
	(its-defrule "&"  "$(G!m(B")  (its-defrule "*"  "$(G!n(B")
	(its-defrule "("  "$(G!>(B")  (its-defrule ")"  "$(G!?(B")
	(its-defrule "-"  "$(G"1(B")  (its-defrule "~"  "$(G"D(B")
	(its-defrule "="  "$(G"8(B")  (its-defrule "`"  "$(G!j(B")
	(its-defrule "\\" "$(G"b(B")  (its-defrule "|"  "$(G"^(B")
	(its-defrule "_"  "$(G"%(B")  (its-defrule "+"  "$(G"0(B")
	(its-defrule "{"  "$(G!B(B")  (its-defrule "}"  "$(G!C(B")
	(its-defrule "\"" "$(G!i(B")  (its-defrule "'"  "$(G!k(B")
	(its-defrule "<"  "$(G"6(B")  (its-defrule ">"  "$(G"7(B"))
    (progn
      (its-defrule "1"  "1")  (its-defrule "2"  "2")
      (its-defrule "3"  "3")  (its-defrule "4"  "4")
      (its-defrule "5"  "5")  (its-defrule "6"  "6")
      (its-defrule "7"  "7")  (its-defrule "8"  "8")
      (its-defrule "9"  "9")  (its-defrule "0"  "0")
      (its-defrule "@"  "@")
      (its-defrule "#"  "#")  (its-defrule "$"  "$")
      (its-defrule "%"  "%")  (its-defrule "^"  "^")
      (its-defrule "&"  "&")  (its-defrule "*"  "*")
      (its-defrule "("  "(")  (its-defrule ")"  ")")
      (its-defrule "-"  "-")  (its-defrule "~"  "~")
      (its-defrule "="  "=")  (its-defrule "`"  "`")
      (its-defrule "\\" "\\") (its-defrule "|"  "|")
      (its-defrule "_"  "_")  (its-defrule "+"  "+")
      (its-defrule "{"  "{")  (its-defrule "}"  "}")
      (its-defrule "\"" "\"") (its-defrule "'"  "'")
      (its-defrule "<"  "<")  (its-defrule ">"  ">"))))

(provide 'its/erpin)

;;; its/pinyin.el --- Pinyin Input in Egg Input Method Architecture

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

(defvar its-pinyin-cn-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-pinyin-cn-open-braket  "$A!8(B" "*[") ; "$A#[(B"
(defvar its-pinyin-cn-close-braket "$A!9(B" "*]") ; "$A#](B"

(defvar its-pinyin-tw-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-pinyin-tw-open-braket  "$(G!V(B" "*[") ; "$(G!b(B "
(defvar its-pinyin-tw-close-braket "$(G!W(B" "*]") ; "$(G!c(B"

(eval-when-compile
  (defun its-prev-terminal-state (state-list)
    (while (and state-list
		(null (its-get-next-state (car state-list) -1)))
      (setq state-list (cdr state-list)))
    (car state-list))

  (defun its-define-qingsheng (shengmu yunmu &optional s y)
    (let ((input (concat shengmu yunmu))
	  (output (concat (if s s (capitalize shengmu)) (if y y yunmu) "(0@(B"))
	  state term-state)
      (setq state (its-defrule* input output))
      (its-make-next-state state ?  output)
      (its-make-next-state state ?0 output)
      (setq term-state (its-prev-terminal-state its-parent-states))
      (if term-state
	  (let ((back (- (length (its-get-keyseq term-state)) (length input)))
		(output (its-get-output (its-get-next-state term-state -1)))
		(parents its-parent-states))
	    (while (null (eq (car parents) term-state))
	      (its-make-next-state (car parents) -1 output (1+ back))
	      (its-defrule-otherwise (car parents) output nil back)
	      (setq back (1+ back)
		    parents (cdr parents)))))
      state))

  (defmacro its-do-sisheng-table (list)
    `(progn
       ,@(mapcar (lambda (syl)
		   `(its-define-sisheng ,@syl))
		 list)))

  (defmacro its-define-sisheng (shengmu yunmu)
    `(let ((qing (nth 5 ,yunmu)) (y (car ,yunmu))
	   (ss (list ,@shengmu)) s cs state i)
       (while ss
	 (setq s (car ss)
	       ss (cdr ss)
	       cs (capitalize s)
	       state (its-define-qingsheng s y cs qing))
	 (its-make-next-state state ?1 (concat cs (nth 1 ,yunmu) "(0@(B"))
	 (its-make-next-state state ?2 (concat cs (nth 2 ,yunmu) "(0@(B"))
	 (its-make-next-state state ?3 (concat cs (nth 3 ,yunmu) "(0@(B"))
	 (its-make-next-state state ?4 (concat cs (nth 4 ,yunmu) "(0@(B")))))

  (defmacro its-define-pinyin-table ()
    '(let ((- "")  (B "b") (C "c") (D "d") (F "f") (G "g") (H "h")
	   (J "j") (K "k") (L "l") (M "m") (N "n") (P "p") (Q "q")
	   (R "r") (S "s") (T "t") (W "w") (X "x") (Y "y") (Z "z")
	   (CH "ch") (SH "sh") (ZH "zh")

	   (A    '("a"    "(0!(B"    "(0"(B"    "(0#(B"    "(0$(B"    "a"   ))
	   (AI   '("ai"   "(0!(Bi"   "(0"(Bi"   "(0#(Bi"   "(0$(Bi"   "ai"  ))
	   (AN   '("an"   "(0!(Bn"   "(0"(Bn"   "(0#(Bn"   "(0$(Bn"   "an"  ))
	   (ANG  '("ang"  "(0!(Bng"  "(0"(Bng"  "(0#(Bng"  "(0$(Bng"  "ang" ))
	   (AO   '("ao"   "(0!(Bo"   "(0"(Bo"   "(0#(Bo"   "(0$(Bo"   "ao"  ))
	   (E    '("e"    "(0%(B"    "(0&(B"    "(0'(B"    "(0((B"    "e"   ))
	   (EI   '("ei"   "(0%(Bi"   "(0&(Bi"   "(0'(Bi"   "(0((Bi"   "ei"  ))
	   (EN   '("en"   "(0%(Bn"   "(0&(Bn"   "(0'(Bn"   "(0((Bn"   "en"  ))
	   (ENG  '("eng"  "(0%(Bng"  "(0&(Bng"  "(0'(Bng"  "(0((Bng"  "eng" ))
	   (ER   '("er"   "(0%(Br"   "(0&(Br"   "(0'(Br"   "(0((Br"   "er"  ))
	   (I    '("i"    "(0)(B"    "(0*(B"    "(0+(B"    "(0,(B"    "i"   ))
	   (IA   '("ia"   "i(0!(B"   "i(0"(B"   "i(0#(B"   "i(0$(B"   "ia"  ))
	   (IAN  '("ian"  "i(0!(Bn"  "i(0"(Bn"  "i(0#(Bn"  "i(0$(Bn"  "ian" ))
	   (IANG '("iang" "i(0!(Bng" "i(0"(Bng" "i(0#(Bng" "i(0$(Bng" "iang"))
	   (IAO  '("iao"  "i(0!(Bo"  "i(0"(Bo"  "i(0#(Bo"  "i(0$(Bo"  "iao" ))
	   (IE   '("ie"   "i(0%(B"   "i(0&(B"   "i(0'(B"   "i(0((B"   "ie"  ))
	   (IN   '("in"   "(0)(Bn"   "(0*(Bn"   "(0+(Bn"   "(0,(Bn"   "in"  ))
	   (ING  '("ing"  "(0)(Bng"  "(0*(Bng"  "(0+(Bng"  "(0,(Bng"  "ing" ))
	   (IONG '("iong" "i(0-(Bng" "i(0.(Bng" "i(0/(Bng" "i(00(Bng" "iong"))
	   (IU   '("iu"   "i(01(B"   "i(02(B"   "i(03(B"   "i(04(B"   "iu"  ))
	   (O    '("o"    "(0-(B"    "(0.(B"    "(0/(B"    "(00(B"    "o"   ))
	   (ONG  '("ong"  "(0-(Bng"  "(0.(Bng"  "(0/(Bng"  "(00(Bng"  "ong" ))
	   (OU   '("ou"   "(0-(Bu"   "(0.(Bu"   "(0/(Bu"   "(00(Bu"   "ou"  ))
	   (U    '("u"    "(01(B"    "(02(B"    "(03(B"    "(04(B"    "u"   ))
	   (V    '("v"    "(05(B"    "(06(B"    "(07(B"    "(08(B"    "(09(B"   ))
	   (UA   '("ua"   "u(0!(B"   "u(0"(B"   "u(0#(B"   "u(0$(B"   "ua"  ))
	   (UAI  '("uai"  "u(0!(Bi"  "u(0"(Bi"  "u(0#(Bi"  "u(0$(Bi"  "uai" ))
	   (UAN  '("uan"  "u(0!(Bn"  "u(0"(Bn"  "u(0#(Bn"  "u(0$(Bn"  "uan" ))
	   (UANG '("uang" "u(0!(Bng" "u(0"(Bng" "u(0#(Bng" "u(0$(Bng" "uang"))
	   (UE   '("ue"   "u(0%(B"   "u(0&(B"   "u(0'(B"   "u(0((B"   "ue"  ))
	   (VE   '("ve"   "(09%(B"   "(09&(B"   "(09'(B"   "(09((B"   "(09(Be"  ))
	   (UI   '("ui"   "u(0)(B"   "u(0*(B"   "u(0+(B"   "u(0,(B"   "ui"  ))
	   (UN   '("un"   "(01(Bn"   "(02(Bn"   "(03(Bn"   "(04(Bn"   "un"  ))
	   (UO   '("uo"   "u(0-(B"   "u(0.(B"   "u(0/(B"   "u(00(B"   "uo"  )))

       (its-define-qingsheng	"hm"	"")
       (its-define-qingsheng	"hng"	"")
       (its-defrule*		"m"	"m(0@(B")
       (its-defrule		"m0"	"m(0@(B")
       (its-defrule*		"n"	"n(0@(B")
       (its-defrule		"n0"	"n(0@(B")
       (its-defrule		"n2"	"(0=@(B")
       (its-defrule		"n3"	"(0>@(B")
       (its-defrule		"n4"	"(0?@(B")
       (its-define-qingsheng	""	"ng")

       (its-do-sisheng-table
	(((- B C D F G H   K L M N P     S T W   Y Z CH SH ZH ) A)
	 ((- B C D   G H   K L M N P     S T W     Z CH SH ZH ) AI)
	 ((- B C D F G H   K L M N P   R S T W   Y Z CH SH ZH ) AN)
	 ((- B C D F G H   K L M N P   R S T W   Y Z CH SH ZH ) ANG)
	 ((- B C D   G H   K L M N P   R S T     Y Z CH SH ZH ) AO)
	 ((-   C D   G H   K L M N     R S T     Y Z CH SH ZH ) E)
	 ((- B C D F G H   K L M N P       T W     Z    SH ZH ) EI)
	 ((- B C D F G H   K   M N P   R S   W     Z CH SH ZH ) EN)
	 ((- B C D F G H   K L M N P   R S T W     Z CH SH ZH ) ENG)
	 ((-                                                  ) ER)
	 ((  B C D       J   L M N P Q R S T   X Y Z CH SH ZH ) I)
	 ((      D       J   L       Q         X              ) IA)
	 ((  B   D       J   L M N P Q     T   X              ) IAN)
	 ((              J   L   N   Q         X              ) IANG)
	 ((  B   D       J   L M N P Q     T   X              ) IAO)
	 ((  B   D       J   L M N P Q     T   X              ) IE)
	 ((  B           J   L M N P Q         X Y            ) IN)
	 ((  B   D       J   L M N P Q     T   X Y            ) ING)
	 ((              J           Q         X              ) IONG)
	 ((      D       J   L M N   Q         X              ) IU)
	 ((- B     F         L M   P         W   Y            ) O)
	 ((    C D   G H   K L   N     R S T     Y Z CH    ZH ) ONG)
	 ((-   C D F G H   K L M N P   R S T     Y Z CH SH ZH ) OU)
	 ((  B C D F G H J K L M N P Q R S T W X Y Z CH SH ZH ) U)
	 ((                  L   N                            ) V)
	 ((          G H   K           R             CH SH ZH ) UA)
	 ((          G H   K                         CH SH ZH ) UAI)
	 ((    C D   G H J K L   N   Q R S T   X Y Z CH SH ZH ) UAN)
	 ((          G H   K                         CH SH ZH ) UANG)
	 ((              J           Q         X Y            ) UE)
	 ((                  L   N                            ) VE)
	 ((    C D   G H   K           R S T       Z CH SH ZH ) UI)
	 ((    C D   G H J K L       Q R S T   X Y Z CH SH ZH ) UN)
	 ((    C D   G H   K L   N     R S T       Z CH SH ZH ) UO)

	 ((J Q X) (cons "a"   (cdr IA  )))
	 ((J Q X) (cons "ai"  (cdr IA  )))
	 ((J Q X) (cons "an"  (cdr IAN )))
	 ((J Q X) (cons "ang" (cdr IANG)))
	 ((J Q X) (cons "ao"  (cdr IAO )))
	 ((J Q X) (cons "e"   (cdr IE  )))
	 ((J Q X) (cons "ei"  (cdr IE  )))
	 ((J Q X) (cons "en"  (cdr IN  )))
	 ((J Q X) (cons "eng" (cdr ING )))
	 ((J Q X) (cons "ou"  (cdr IU  ))))))))

(define-its-state-machine its-pinyin-cn-map
  "pinyin-cn" "$AF4(BG" Chinese-GB
  "Map for Pinyin input. (Chinese-GB)"

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (its-define-pinyin-table)
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
  (its-defrule	"sh "	"$AIO(B")
  (its-defrule	"t "	"$AK{(B")
  (its-defrule	"w "	"$ANR(B")
  (its-defrule	"x "	"$AOr(B")
  (its-defrule	"y "	"$ASV(B")
  (its-defrule	"z "	"$ATZ(B")
  (its-defrule	"zh "	"$AWE(B")

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

(define-its-state-machine its-pinyin-tw-map
  "pinyin-tw" "$(GQ;(BC" Chinese-CNS
  "Map for Pinyin input."

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-tw)

  (its-define-pinyin-table)
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
  (its-defrule	"sh "	"$(GD8(B")
  (its-defrule	"t "	"$(GEl(B")
  (its-defrule	"w "	"$(GJ<(B")
  (its-defrule	"x "	"$(GGW(B")
  (its-defrule	"y "	"$(GD4(B")
  (its-defrule	"z "	"$(GGc(B")
  (its-defrule	"zh "	"$(Gaa(B")

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

(define-its-state-machine-append its-pinyin-cn-map
  (its-defrule "[" its-pinyin-cn-open-braket)
  (its-defrule "]" its-pinyin-cn-close-braket)

  (if its-pinyin-cn-enable-quanjioao-alphabet
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

(define-its-state-machine-append its-pinyin-tw-map
  (its-defrule "[" its-pinyin-tw-open-braket)
  (its-defrule "]" its-pinyin-tw-close-braket)

  (if its-pinyin-tw-enable-quanjioao-alphabet
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

(provide 'its/pinyin)

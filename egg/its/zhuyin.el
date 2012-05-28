;;; its/zhuyin.el --- Zhuyin Input in Egg Input Method Architecture

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

(defvar its-zhuyin-cn-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-zhuyin-cn-open-braket  "$A!8(B" "*[") ; "$A#[(B"
(defvar its-zhuyin-cn-close-braket "$A!9(B" "*]") ; "$A#](B"

(defvar its-zhuyin-tw-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-zhuyin-tw-open-braket  "$(G!V(B" "*[") ; "$(G!b(B "
(defvar its-zhuyin-tw-close-braket "$(G!W(B" "*]") ; "$(G!c(B"

(eval-when-compile
  (defmacro its-do-zhuyin-table (list)
    `(progn
       ,@(mapcar (lambda (syl) `(its-define-zhuyin ,@syl))
		 list)))

  (defmacro its-define-zhuyin (shengmu yunmu1 &optional yunmu2 qingsheng)
    `(let ((s (list ,@shengmu))
	   (yi (concat (car ,yunmu1) (car ,yunmu2)))
	   (yo (concat (nth 1 ,yunmu1) (nth 1 ,yunmu2)))
	   (tone ,(if qingsheng "(0A(B" "(0@(B"))
	   in out out1 state)
       (while s
	 (setq in (concat (car (car s)) yi)
	       out (concat (nth 1 (car s)) yo)
	       out1 (concat out tone)
	       state (its-defrule* in out1 out)
	       s (cdr s))
	 (its-defrule (concat in " ") out1)
	 ,@(if (null qingsheng)
	       '((its-make-next-state state ?1 (concat out "(0A(B"))
		 (its-make-next-state state ?2 (concat out "(0B(B"))
		 (its-make-next-state state ?3 (concat out "(0C(B"))
		 (its-make-next-state state ?4 (concat out "(0D(B")))))))

  (defmacro its-define-zhuyin-table ()
    '(let ((-  '(""  ""))
	   (B  '("b" "(0E(B")) (P  '("p" "(0F(B")) (M  '("m" "(0G(B")) (F '("f" "(0H(B"))
	   (D  '("d" "(0I(B")) (T  '("t" "(0J(B")) (N  '("n" "(0K(B")) (L '("l" "(0L(B"))
	   (G  '("v" "(0M(B")) (K  '("k" "(0N(B")) (H  '("h" "(0O(B"))
	   (J  '("g" "(0P(B")) (Q  '("7" "(0Q(B")) (X  '("c" "(0R(B"))
	   (ZH '("," "(0S(B")) (CH '("." "(0T(B")) (SH '("/" "(0U(B")) (R '("j"  "(0V(B"))
	   (Z  '(";" "(0W(B")) (C  '(":" "(0X(B")) (S  '("s" "(0Y(B"))

	   (A   '("a" "(0Z(B")) (O   '("o" "(0[(B")) (e   '("r" "(0\(B")) (E   '("w" "(0](B"))
	   (AI  '("i" "(0^(B")) (EI  '("q" "(0_(B")) (AO  '("z" "(0`(B")) 
	   (AN  '("8" "(0b(B")) (EN  '("9" "(0c(B")) (ANG '("0" "(0d(B")) (ENG '("-" "(0e(B"))
	   (ER  '("^" "(0f(B")) (OU  '("y" "(0a(B"))
	   (I   '("e" "(0g(B")) (U   '("x" "(0h(B")) (V   '("u" "(0i(B")))

       (its-define-zhuyin (- H) M nil t)
       (its-define-zhuyin (- H) '("@" "@") nil t)
       (its-define-zhuyin (-  ) N nil t)

       (its-do-zhuyin-table
	(((                              ZH CH SH R Z C S ) -)
	 ((- B P M F D T N L G K H       ZH CH SH   Z C S ) A)
	 ((- B P M F       L                              ) O)
	 ((-     M   D T N L G K H       ZH CH SH R Z C S ) e)
	 ((- B P M   D T N L G K H       ZH CH SH   Z C S ) AI)
	 ((- B P M F D T N L G K H       ZH    SH   Z C   ) EI)
	 ((- B P M   D T N L G K H       ZH CH SH R Z C S ) AO)
	 ((- B P M F D T N L G K H       ZH CH SH R Z C S ) AN)
	 ((- B P M F D   N   G K H       ZH CH SH R Z C S ) EN)
	 ((- B P M F D T N L G K H       ZH CH SH R Z C S ) ANG)
	 ((- B P M F D T N L G K H       ZH CH SH R Z C S ) ENG)
	 ((-                                              ) ER)
	 ((-   P M F D T N L G K H       ZH CH SH R Z C S ) OU)
	 ((- B P M   D T N L       J Q X                  ) I)
	 ((-         D     L       J Q X                  ) I A)
	 ((-                                              ) I O)
	 ((- B P M   D T N L       J Q X                  ) I E)
	 ((- B P M   D T N L       J Q X                  ) I AO)
	 ((-     M   D   N L       J Q X                  ) I OU)
	 ((- B P M   D T N L       J Q X                  ) I AN)
	 ((- B P M       N L       J Q X                  ) I EN)
	 ((-             N L       J Q X                  ) I ANG)
	 ((- B P M   D T N L       J Q X                  ) I ENG)
	 ((- B P M F D T N L G K H       ZH CH SH R Z C S ) U)
	 ((-                 G K H       ZH CH SH R       ) U A)
	 ((-         D T N L G K H       ZH CH SH R Z C S ) U O)
	 ((-                 G K H       ZH CH SH         ) U AI)
	 ((-         D T     G K H       ZH CH SH R Z C S ) U EI)
	 ((-         D T N L G K H       ZH CH SH R Z C S ) U AN)
	 ((-         D T   L G K H       ZH CH SH R Z C S ) U EN)
	 ((-                 G K H       ZH CH SH         ) U ANG)
	 ((-         D T N L G K H       ZH CH    R Z C S ) U ENG)
	 ((-             N L       J Q X                  ) V)
	 ((-             N L       J Q X                  ) V E)
	 ((-                       J Q X                  ) V AN)
	 ((-                       J Q X                  ) V EN)
	 ((-                       J Q X                  ) V ENG)))

       (mapcar (lambda (s) (its-defoutput (car s) (nth 1 s)))
	       (list B P M F D T N L G K H J Q X))

       (its-defrule (concat (car N) "2") (concat (nth 1 N) "(0B(B"))
       (its-defrule (concat (car N) "3") (concat (nth 1 N) "(0C(B"))
       (its-defrule (concat (car N) "4") (concat (nth 1 N) "(0D(B")))))

(define-its-state-machine its-zhuyin-cn-map
  "zhuyin-cn" "$AW"(BG" Chinese-GB
  "Map for Zhuyin input. (Chinese-GB)"

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (its-define-zhuyin-table)
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

    (its-defrule "<" "$A#,(B")
    (its-defrule ">" "$A!#(B")
    (its-defrule "?" "$A!"(B"))

(define-its-state-machine its-zhuyin-tw-map
  "zhuyin-tw" "$(GNC(BC" Chinese-CNS
  "Map for Zhuyin input."

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-tw)

  (its-define-zhuyin-table)
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

    (its-defrule "<" "$(G!"(B")
    (its-defrule ">" "$(G!$(B")
    (its-defrule "?" "$(G!#(B"))

(define-its-state-machine-append its-zhuyin-cn-map
  (its-defrule "[" its-zhuyin-cn-open-braket)
  (its-defrule "]" its-zhuyin-cn-close-braket)

(if its-zhuyin-cn-enable-quanjioao-alphabet
      (progn
	(its-defrule "#"  "$A##(B")  (its-defrule "$"  "$A!g(B")
	(its-defrule "%"  "$A#%(B")
	(its-defrule "&"  "$A#&(B")  (its-defrule "*"  "$A#*(B")
	(its-defrule "("  "$A#((B")  (its-defrule ")"  "$A#)(B")
	(its-defrule "~"  "$A!+(B")
	(its-defrule "="  "$A#=(B")  (its-defrule "`"  "$A#`(B")
	(its-defrule "\\" "$A#\(B")  (its-defrule "|"  "$A#|(B")
	(its-defrule "_"  "$A#_(B")  (its-defrule "+"  "$A#+(B")
	(its-defrule "{"  "$A#{(B")  (its-defrule "}"  "$A#}(B")
	(its-defrule "\"" "$A#"(B")  (its-defrule "'"  "$A#'(B"))
    (progn
      (its-defrule "#"  "#")  (its-defrule "$"  "$")
      (its-defrule "%"  "%")
      (its-defrule "&"  "&")  (its-defrule "*"  "*")
      (its-defrule "("  "(")  (its-defrule ")"  ")")
      (its-defrule "~"  "~")
      (its-defrule "="  "=")  (its-defrule "`"  "`")
      (its-defrule "\\" "\\") (its-defrule "|"  "|")
      (its-defrule "_"  "_")  (its-defrule "+"  "+")
      (its-defrule "{"  "{")  (its-defrule "}"  "}")
      (its-defrule "\"" "\"") (its-defrule "'"  "'"))))

(define-its-state-machine-append its-zhuyin-tw-map
  (its-defrule "[" its-zhuyin-tw-open-braket)
  (its-defrule "]" its-zhuyin-tw-close-braket)

  (if its-zhuyin-tw-enable-quanjioao-alphabet
      (progn
	(its-defrule "#"  "$(G!l(B")  (its-defrule "$"  "$(G"c(B")
	(its-defrule "%"  "$(G"h(B")
	(its-defrule "&"  "$(G!m(B")  (its-defrule "*"  "$(G!n(B")
	(its-defrule "("  "$(G!>(B")  (its-defrule ")"  "$(G!?(B")
	(its-defrule "~"  "$(G"D(B")
	(its-defrule "="  "$(G"8(B")  (its-defrule "`"  "$(G!j(B")
	(its-defrule "\\" "$(G"`(B")  (its-defrule "|"  "$(G"^(B")
	(its-defrule "_"  "$(G"%(B")  (its-defrule "+"  "$(G"0(B")
	(its-defrule "{"  "$(G!B(B")  (its-defrule "}"  "$(G!C(B")
	(its-defrule "\"" "$(G!i(B")  (its-defrule "'"  "$(G!k(B"))
    (progn
      (its-defrule "#"  "#")  (its-defrule "$"  "$")
      (its-defrule "%"  "%")
      (its-defrule "&"  "&")  (its-defrule "*"  "*")
      (its-defrule "("  "(")  (its-defrule ")"  ")")
      (its-defrule "~"  "~")
      (its-defrule "="  "=")  (its-defrule "`"  "`")
      (its-defrule "\\" "\\") (its-defrule "|"  "|")
      (its-defrule "_"  "_")  (its-defrule "+"  "+")
      (its-defrule "{"  "{")  (its-defrule "}"  "}")
      (its-defrule "\"" "\"") (its-defrule "'"  "'"))))

(provide 'its/zhuyin)

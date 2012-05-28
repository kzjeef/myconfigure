;;; its/quanjiao.el --- Quanjiao ASCII Input in Egg Input Method Architecture

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

(define-its-state-machine its-quanjiao-up-cn-map
  "quanjiao-upcase-cn" "$A#A(B" Chinese-GB
  "Map for quanjiao-upcase input. (Chinese-GB)"

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
		   ("a" . "$A#A(B")  ("b" . "$A#B(B")  ("c" . "$A#C(B")  ("d" . "$A#D(B")
		   ("e" . "$A#E(B")  ("f" . "$A#F(B")  ("g" . "$A#G(B")  ("h" . "$A#H(B")
		   ("i" . "$A#I(B")  ("j" . "$A#J(B")  ("k" . "$A#K(B")  ("l" . "$A#L(B")
		   ("m" . "$A#M(B")  ("n" . "$A#N(B")  ("o" . "$A#O(B")  ("p" . "$A#P(B")
		   ("q" . "$A#Q(B")  ("r" . "$A#R(B")  ("s" . "$A#S(B")  ("t" . "$A#T(B")
		   ("u" . "$A#U(B")  ("v" . "$A#V(B")  ("w" . "$A#W(B")  ("x" . "$A#X(B")
		   ("y" . "$A#Y(B")  ("z" . "$A#Z(B")
		   ("A" . "$A#A(B")  ("B" . "$A#B(B")  ("C" . "$A#C(B")  ("D" . "$A#D(B")
		   ("E" . "$A#E(B")  ("F" . "$A#F(B")  ("G" . "$A#G(B")  ("H" . "$A#H(B")
		   ("I" . "$A#I(B")  ("J" . "$A#J(B")  ("K" . "$A#K(B")  ("L" . "$A#L(B")
		   ("M" . "$A#M(B")  ("N" . "$A#N(B")  ("O" . "$A#O(B")  ("P" . "$A#P(B")
		   ("Q" . "$A#Q(B")  ("R" . "$A#R(B")  ("S" . "$A#S(B")  ("T" . "$A#T(B")
		   ("U" . "$A#U(B")  ("V" . "$A#V(B")  ("W" . "$A#W(B")  ("X" . "$A#X(B")
		   ("Y" . "$A#Y(B")  ("Z" . "$A#Z(B")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule in out))))

(define-its-state-machine-append its-quanjiao-up-cn-map)

(define-its-state-machine its-quanjiao-down-cn-map
  "quanjiao-downcase-cn" "$A#a(B" Chinese-GB
  "Map for quanjiao-downcase input. (Chinese-GB)"

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
      (its-defrule in out))))

(define-its-state-machine-append its-quanjiao-down-cn-map)

(define-its-state-machine its-quanjiao-up-tw-map
  "quanjiao-upcase-tw" "$(G$A(B" Chinese-CNS
  "Map for quanjiao-upcase input. (Chinese-CNS)"

  (dolist (ascii '(("0" . "$(G$!(B")  ("1" . "$(G$"(B")  ("2" . "$(G$#(B")  ("3" . "$(G$$(B")
		   ("4" . "$(G$%(B")  ("5" . "$(G$&(B")  ("6" . "$(G$'(B")  ("7" . "$(G$((B")
		   ("8" . "$(G$)(B")  ("9" . "$(G$*(B") 
		   (" " . "$(G!!(B")  ("!" . "$(G!*(B")  ("@" . "$(G"i(B")  ("#" . "$(G!l(B")
		   ("$" . "$(G"c(B")  ("%" . "$(G"h(B")  ("^" . "$(G!T(B")  ("&" . "$(G!m(B")
		   ("*" . "$(G!n(B")  ("(" . "$(G!>(B")  (")" . "$(G!?(B")
		   ("-" . "$(G"1(B")  ("=" . "$(G"8(B")  ("`" . "$(G!j(B")  ("\\" . "$(G"b(B")
		   ("|" . "$(G"^(B")  ("_" . "$(G"%(B")  ("+" . "$(G"0(B")  ("~" . "$(G"D(B")
		   ("[" . "$(G!b(B")  ("]" . "$(G!c(B")  ("{" . "$A#{(B")  ("}" . "$(G!a(B")
		   (":" . "$(G!((B")  (";" . "$(G!'(B")  ("\"" . "$(G!i(B") ("'" . "$(G!k(B")
		   ("<" . "$(G"6(B")  (">" . "$(G"7(B")  ("?" . "$(G!)(B")  ("/" . "$(G"a(B")
		   ("," . "$(G!"(B")  ("." . "$(G!%(B")
		   ("a" . "$(G$A(B")  ("b" . "$(G$B(B")  ("c" . "$(G$C(B")  ("d" . "$(G$D(B")
		   ("e" . "$(G$E(B")  ("f" . "$(G$F(B")  ("g" . "$(G$G(B")  ("h" . "$(G$H(B")
		   ("i" . "$(G$I(B")  ("j" . "$(G$J(B")  ("k" . "$(G$K(B")  ("l" . "$(G$L(B")
		   ("m" . "$(G$M(B")  ("n" . "$(G$N(B")  ("o" . "$(G$O(B")  ("p" . "$(G$P(B")
		   ("q" . "$(G$Q(B")  ("r" . "$(G$R(B")  ("s" . "$(G$S(B")  ("t" . "$(G$T(B")
		   ("u" . "$(G$U(B")  ("v" . "$(G$V(B")  ("w" . "$(G$W(B")  ("x" . "$(G$X(B")
		   ("y" . "$(G$Y(B")  ("z" . "$(G$Z(B")
		   ("A" . "$(G$A(B")  ("B" . "$(G$B(B")  ("C" . "$(G$C(B")  ("D" . "$(G$D(B")
		   ("E" . "$(G$E(B")  ("F" . "$(G$F(B")  ("G" . "$(G$G(B")  ("H" . "$(G$H(B")
		   ("I" . "$(G$I(B")  ("J" . "$(G$J(B")  ("K" . "$(G$K(B")  ("L" . "$(G$L(B")
		   ("M" . "$(G$M(B")  ("N" . "$(G$N(B")  ("O" . "$(G$O(B")  ("P" . "$(G$P(B")
		   ("Q" . "$(G$Q(B")  ("R" . "$(G$R(B")  ("S" . "$(G$S(B")  ("T" . "$(G$T(B")
		   ("U" . "$(G$U(B")  ("V" . "$(G$V(B")  ("W" . "$(G$W(B")  ("X" . "$(G$X(B")
		   ("Y" . "$(G$Y(B")  ("Z" . "$(G$Z(B")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule in out))))

(define-its-state-machine-append its-quanjiao-up-tw-map)

(define-its-state-machine its-quanjiao-down-tw-map
  "quanjiao-downcase-tw" "$(G$[(B" Chinese-CNS
  "Map for quanjiao-downcase input. (Chinese-CNS)"

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
      (its-defrule in out))))

(define-its-state-machine-append its-quanjiao-down-tw-map)

(provide 'its/quanjiao)

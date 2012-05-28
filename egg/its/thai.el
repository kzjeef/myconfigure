;;; its/thai.el --- Inputting Thai characters in Egg Input Method Architecture

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

(eval-when-compile
  (defmacro define-its-thai-keymap (&rest rule)
    (let (input output type list)
      (while rule
	(setq input  (car (car rule))
	      output (nth 1 (car rule))
	      type   (nth 2 (car rule))
	      rule   (cdr rule)
	      list   (cons `(its-defrule ,input ,output) list))
	(if type
	    (setq list (cons `(setq ,type (cons (cons ,input ,output) ,type))
			     list))))
      `(let (consonant vowel tone)
	 ,@list
	 (its-thai-composit consonant vowel tone))))

  (defun its-thai-composit (consonant vowel tone)
    (let (keyseq output)
      (while consonant
	(setq keyseq (car (car consonant))
              output (cdr (car consonant))
	      consonant (cdr consonant))
	(its-thai-add-vowel keyseq output vowel tone)
	(its-thai-add-tone keyseq output tone))))

  (defun its-thai-add-vowel (keyseq output vowel tone)
    (let (next-keyseq next-output)
      (while vowel
	(setq next-keyseq (concat keyseq (car (car vowel)))
	      next-output (concat output (cdr (car vowel)))
	      vowel (cdr vowel))
        (its-defrule next-keyseq (compose-string next-output))
	(its-thai-add-tone next-keyseq next-output tone))))

  (defun its-thai-add-tone (keyseq output tone)
    (let (next-keyseq next-output)
      (while tone
	(setq next-keyseq (concat keyseq (car (car tone)))
	      next-output (concat output (cdr (car tone)))
              tone (cdr tone))
        (its-defrule next-keyseq (compose-string next-output))))))

;; Thai Kesmanee keyboard support.

(define-its-state-machine its-thai-kesmanee-map
  "kesmanee" ",T!!(B" Thai
  "Map for Thai Kesmanee input method with TIS620 keyboard. (Thai)"

  (define-its-thai-keymap
    ("1"  ",TE(B" consonant)    ("!"  "#")
    ("2"  "/")              ("@"  ",Tq(B")
    ("3"  "_")              ("#"  ",Tr(B")
    ("4"  ",T@(B" consonant)    ("$"  ",Ts(B")
    ("5"  ",T6(B" consonant)    ("%"  ",Tt(B")
    ("6"  ",TX(B" vowel)        ("^"  ",TY(B" vowel)
    ("7"  ",TV(B" vowel)        ("&"  "0,TQi(B1" vowel)
    ("8"  ",T$(B" consonant)    ("*"  ",Tu(B")
    ("9"  ",T5(B" consonant)    ("("  ",Tv(B")
    ("0"  ",T((B" consonant)    (")"  ",Tw(B")
    ("-"  ",T"(B" consonant)    ("_"  ",Tx(B")
    ("="  ",T*(B" consonant)    ("+"  ",Ty(B")
    ("\\" ",T_(B")              ("|"  ",To(B")
    ("`"  ",T#(B" consonant)    ("~"  ",T%(B" consonant)

    ("q"  ",Tf(B")              ("Q"  ",Tp(B")
    ("w"  ",Td(B")              ("W"  "\"")
    ("e"  ",TS(B")              ("E"  ",T.(B" consonant)
    ("r"  ",T>(B" consonant)    ("R"  ",T1(B" consonant)
    ("T"  ",T8(B" consonant)    ("t"  ",TP(B")
    ("y"  ",TQ(B" vowel)        ("Y"  ",Tm(B" tone)
    ("u"  ",TU(B" vowel)        ("U"  ",Tj(B" tone)
    ("i"  ",TC(B" consonant)    ("I"  ",T3(B" consonant)
    ("o"  ",T9(B" consonant)    ("O"  ",TO(B")
    ("p"  ",TB(B" consonant)    ("P"  ",T-(B" consonant)
    ("["  ",T:(B" consonant)    ("{"  ",T0(B" consonant)
    ("]"  ",TE(B" consonant)    ("}"  ",")

    ("a"  ",T?(B" consonant)    ("A"  ",TD(B")
    ("s"  ",TK(B" consonant)    ("S"  ",T&(B" consonant)
    ("d"  ",T!(B" consonant)    ("D"  ",T/(B" consonant)
    ("f"  ",T4(B" consonant)    ("F"  ",Tb(B")
    ("g"  ",T`(B")              ("G"  ",T,(B" consonant)
    ("h"  ",Ti(B" tone)         ("H"  ",Tg(B" vowel)
    ("j"  ",Th(B" tone)         ("J"  ",Tk(B" tone)
    ("k"  ",TR(B")              ("K"  ",TI(B" consonant)
    ("l"  ",TJ(B" consonant)    ("L"  ",TH(B" consonant)
    (";"  ",TG(B" consonant)    (":"  ",T+(B" consonant)
    ("'"  ",T'(B" consonant)    ("\"" ",TF(B")

    ("z"  ",T<(B" consonant)    ("Z"  "(")
    ("x"  ",T;(B" consonant)    ("X"  ")")
    ("c"  ",Ta(B")              ("C"  ",T)(B" consonant)
    ("v"  ",TM(B" consonant)    ("V"  ",TN(B" consonant)
    ("b"  ",TT(B" vowel)        ("B"  ",TZ(B" vowel)
    ("n"  ",TW(B" vowel)        ("N"  ",Tl(B" tone)
    ("m"  ",T7(B" consonant)    ("M"  ",Tn(B" vowel)
    (","  ",TA(B" consonant)    ("<"  ",T2(B" consonant)
    ("."  ",Tc(B")              (">"  ",TL(B" consonant)
    ("/"  ",T=(B" consonant)    ("?"  "?")))

(define-its-state-machine-append its-thai-kesmanee-map)

(provide 'its/thai)

;;; its/thai.el ends here

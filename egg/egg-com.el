;;; egg-com.el --- Communication Routines in Egg Input Method Architecture

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc

;; Author: Hisashi Miyashita <himi@bird.scphys.kyoto-u.ac.jp>
;;         NIIBE Yutaka <gniibe@chroot.org>
;;	   KATAYAMA Yoshio <kate@pfu.co.jp>  ; Korean, Chinese support.

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


(require 'egg-edep)

(defvar egg-fixed-euc '(fixed-euc-jp))
(make-variable-buffer-local 'egg-fixed-euc)
(put 'egg-fixed-euc 'permanent-local t)

(defvar egg-mb-euc 'euc-japan)
(make-variable-buffer-local 'egg-mb-euc)
(put 'egg-mb-euc 'permanent-local t)

;; Japanese

(defun fixed-euc-jp-pre-write-conversion (from to)
  (let ((work-buf (generate-new-buffer " *temp*"))
	ch)
    (if (stringp from)
	(encode-coding-string from 'euc-japan nil work-buf)
      (encode-coding-region from to 'euc-japan work-buf))
    (set-buffer work-buf)
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (while (not (eobp))
      (setq ch (following-char))
      (cond ((= ch #x8E)		; SS2 for JISX0201-kana
	     (delete-char 1)		; SS2 BYTE -> 0 BYTE&0x7F
	     (insert 0)
	     (forward-char 1))
	    ((= ch #x8F)		; SS3 for JISX0212
	     (delete-char 1)		; SS3 BYTE1 BYTE2 -> BYTE1 BYTE2&0x7F
	     (forward-char 1)
	     (setq ch (following-char))
	     (delete-char 1)
	     (insert (logand ch #x7F)))
	    ((>= ch #xA0)		; JISX0208
	     (forward-char 2))
	    (t				; ASCII
	     (insert 0)			; BYTE -> 0 BYTE
	     (forward-char 1))))))

(defun fixed-euc-jp-post-read-conversion (len)
  (let ((str (string-as-unibyte (buffer-substring (point) (+ (point) len))))
	(pos (point))
	i ch1 ch2)
    (delete-region (point) (+ (point) len))
    (setq i 0)
    (while (< i len)
      (setq ch1 (aref str i))
      (setq ch2 (aref str (1+ i)))
      (cond ((>= ch1 #x80)
	     (if (>= ch2 #x80)
		 (setq ch1 		; JISX0208
		       (decode-char 'japanese-jisx0208
				    (logior (lsh (logand ch1 #x7F) 8)
					    (logand ch2 #x7F))))
	       (setq ch1		; JISX0212
		     (decode-char 'japanese-jisx0212
				  (logior (lsh (logand ch1 #x7F) 8) ch2)))))
	    (t
	     (if (>= ch2 #x80)
		 (setq ch1		; JISX0201-kana
		       (decode-char 'katakana-jisx0201 (logand ch2 #x7F)))
	       (setq ch1 ch2))))
      (insert ch1)
      (setq i (+ i 2)))
    (prog1 (- (point) pos)
      (goto-char pos))))

(eval-and-compile
  (if (string< mule-version "6.0") ;; for before Emacs23
      (progn
	(define-ccl-program ccl-decode-fixed-euc-jp
	  `(2
	    ((r2 = ,(charset-id 'japanese-jisx0208))
	     (r3 = ,(charset-id 'japanese-jisx0212))
	     (r4 = ,(charset-id 'katakana-jisx0201))
	     (read r0)
	     (loop
	      (read r1)
	      (if (r0 < ?\x80)
		  ((r0 = r1)
		   (if (r1 < ?\x80)
		       (write-read-repeat r0))
		   (write r4)
		   (write-read-repeat r0))
		((if (r1 > ?\x80)
		     ((write r2 r0)
		      (r0 = r1)
		      (write-read-repeat r0))
		   ((write r3 r0)
		    (r0 = (r1 | ?\x80))
		    (write-read-repeat r0)))))))))

	(define-ccl-program ccl-encode-fixed-euc-jp
	  `(2
	    ((read r0)
	     (loop
	      (if (r0 == ,(charset-id 'latin-jisx0201))                   ; Unify
		  ((read r0)
		   (r0 &= ?\x7f)))
	      (if (r0 < ?\x80)                                            ;G0
		  ((write 0)
		   (write-read-repeat r0)))
	      (r6 = (r0 == ,(charset-id 'japanese-jisx0208)))
	      (r6 |= (r0 == ,(charset-id 'japanese-jisx0208-1978)))
	      (if r6                                                      ;G1
		  ((read r0)
		   (write r0)
		   (read r0)
		   (write-read-repeat r0)))
	      (if (r0 == ,(charset-id 'katakana-jisx0201))                ;G2
		  ((read r0)
		   (write 0)
		   (write-read-repeat r0)))
	      (if (r0 == ,(charset-id 'japanese-jisx0212))                ;G3
		  ((read r0)
		   (write r0)
		   (read r0)
		   (r0 &= ?\x7f)
		   (write-read-repeat r0)))
	      (read r0)
	      (repeat)))))
	(make-coding-system 'fixed-euc-jp 4 ?W "Coding System for fixed EUC Japanese"
			    (cons ccl-decode-fixed-euc-jp ccl-encode-fixed-euc-jp))
	)
    ;; Emacs23 or later
    ;; From Handa-san. [mule-ja : No.09414]
    (define-charset 'fixed-euc-jp
      "Fixed EUC Japanese"
      :dimension 2
      :superset '(ascii
		  (katakana-jisx0201 . #x80)
		  (japanese-jisx0208 . #x8080)
		  (japanese-jisx0212 . #x8000)))
    (define-coding-system 'fixed-euc-jp
      "Coding System for fixed EUC Japanese"
      :mnemonic ?W
      :coding-type 'charset
      :charset-list '(fixed-euc-jp))
    )
  )

;; Korean

(eval-and-compile
(define-ccl-program ccl-decode-fixed-euc-kr
  `(2
    ((r2 = ,(charset-id 'korean-ksc5601))
     (read r0)
     (loop
      (read r1)
      (if (r0 < ?\x80)
	  (r0 = r1 & ?\x7f)
	((write r2 r0)
	 (r0 = r1 | ?\x80)))
      (write-read-repeat r0)))))

(define-ccl-program ccl-encode-fixed-euc-kr
  `(2
    ((read r0)
     (loop
      (if (r0 < ?\x80)
	  ((write 0)
	   (write-read-repeat r0)))
      (if (r0 == ,(charset-id 'korean-ksc5601))
	  ((read r0)
	   (write r0)
	   (read r0)
	   (write-read-repeat r0)))
      (read r0)
      (repeat)))))
)

(make-coding-system 'fixed-euc-kr 4 ?W "Coding System for fixed EUC Korean"
		    (cons ccl-decode-fixed-euc-kr ccl-encode-fixed-euc-kr))

;; Chinese

(defconst egg-pinyin-shengmu
  '((""  . 0)  ("B" . 1)  ("C"  . 2)  ("Ch" . 3)  ("D" . 4)
    ("F" . 5)  ("G" . 6)  ("H"  . 7)  ("J"  . 8)  ("K" . 9)
    ("L" . 10) ("M" . 11) ("N"  . 12) ("P"  . 13) ("Q" . 14)
    ("R" . 15) ("S" . 16) ("Sh" . 17) ("T"  . 18) ("W" . 19)
    ("X" . 20) ("Y" . 21) ("Z"  . 22) ("Zh" . 23)))

(defconst egg-pinyin-yunmu
  '(("(0@(B"      0 0) ("(0@(B"      0 1) ("(0@(B"      0 3) ("(0@(B"      0 5) ("(0@(B"      0 7)
    ("a(0@(B"     1 0) ("(0!@(B"     1 1) ("(0"@(B"     1 3) ("(0#@(B"     1 5) ("(0$@(B"     1 7)
    ("ai(0@(B"    2 0) ("(0!(Bi(0@(B"    2 1) ("(0"(Bi(0@(B"    2 3) ("(0#(Bi(0@(B"    2 5) ("(0$(Bi(0@(B"    2 7)
    ("an(0@(B"    3 0) ("(0!(Bn(0@(B"    3 1) ("(0"(Bn(0@(B"    3 3) ("(0#(Bn(0@(B"    3 5) ("(0$(Bn(0@(B"    3 7)
    ("ang(0@(B"   4 0) ("(0!(Bng(0@(B"   4 1) ("(0"(Bng(0@(B"   4 3) ("(0#(Bng(0@(B"   4 5) ("(0$(Bng(0@(B"   4 7)
    ("ao(0@(B"    5 0) ("(0!(Bo(0@(B"    5 1) ("(0"(Bo(0@(B"    5 3) ("(0#(Bo(0@(B"    5 5) ("(0$(Bo(0@(B"    5 7)
    ("e(0@(B"     6 0) ("(0%@(B"     6 1) ("(0&@(B"     6 3) ("(0'@(B"     6 5) ("(0(@(B"     6 7)
    ("ei(0@(B"    7 0) ("(0%(Bi(0@(B"    7 1) ("(0&(Bi(0@(B"    7 3) ("(0'(Bi(0@(B"    7 5) ("(0((Bi(0@(B"    7 7)
    ("en(0@(B"    8 0) ("(0%(Bn(0@(B"    8 1) ("(0&(Bn(0@(B"    8 3) ("(0'(Bn(0@(B"    8 5) ("(0((Bn(0@(B"    8 7)
    ("eng(0@(B"   9 0) ("(0%(Bng(0@(B"   9 1) ("(0&(Bng(0@(B"   9 3) ("(0'(Bng(0@(B"   9 5) ("(0((Bng(0@(B"   9 7)
    ("er(0@(B"   10 0) ("(0%(Br(0@(B"   10 1) ("(0&(Br(0@(B"   10 3) ("(0'(Br(0@(B"   10 5) ("(0((Br(0@(B"   10 7)
    ("i(0@(B"    11 0) ("(0)@(B"    11 1) ("(0*@(B"    11 3) ("(0+@(B"    11 5) ("(0,@(B"    11 7)
    ("ia(0@(B"   12 0) ("i(0!@(B"   12 1) ("i(0"@(B"   12 3) ("i(0#@(B"   12 5) ("i(0$@(B"   12 7)
    ("ian(0@(B"  13 0) ("i(0!(Bn(0@(B"  13 1) ("i(0"(Bn(0@(B"  13 3) ("i(0#(Bn(0@(B"  13 5) ("i(0$(Bn(0@(B"  13 7)
    ("iang(0@(B" 14 0) ("i(0!(Bng(0@(B" 14 1) ("i(0"(Bng(0@(B" 14 3) ("i(0#(Bng(0@(B" 14 5) ("i(0$(Bng(0@(B" 14 7)
    ("iao(0@(B"  15 0) ("i(0!(Bo(0@(B"  15 1) ("i(0"(Bo(0@(B"  15 3) ("i(0#(Bo(0@(B"  15 5) ("i(0$(Bo(0@(B"  15 7)
    ("ie(0@(B"   16 0) ("i(0%@(B"   16 1) ("i(0&@(B"   16 3) ("i(0'@(B"   16 5) ("i(0(@(B"   16 7)
    ("in(0@(B"   17 0) ("(0)(Bn(0@(B"   17 1) ("(0*(Bn(0@(B"   17 3) ("(0+(Bn(0@(B"   17 5) ("(0,(Bn(0@(B"   17 7)
    ("ing(0@(B"  18 0) ("(0)(Bng(0@(B"  18 1) ("(0*(Bng(0@(B"  18 3) ("(0+(Bng(0@(B"  18 5) ("(0,(Bng(0@(B"  18 7)
    ("iong(0@(B" 19 0) ("i(0-(Bng(0@(B" 19 1) ("i(0.(Bng(0@(B" 19 3) ("i(0/(Bng(0@(B" 19 5) ("i(00(Bng(0@(B" 19 7)
    ("iu(0@(B"   20 0) ("i(01@(B"   20 1) ("i(02@(B"   20 3) ("i(03@(B"   20 5) ("i(04@(B"   20 7)
    ("m(0@(B"    21 0) ("m(0@(B"    21 1) ("m(0@(B"    21 3) ("m(0@(B"    21 5) ("m(0@(B"    21 7)
    ("n(0@(B"    22 0) ("n(0@(B"    22 1) ("(0=@(B"    22 3) ("(0>@(B"    22 5) ("(0?@(B"    22 7)
    ("ng(0@(B"   23 0) ("ng(0@(B"   23 1) ("ng(0@(B"   23 3) ("ng(0@(B"   23 5) ("ng(0@(B"   23 7)
    ("o(0@(B"    24 0) ("(0-@(B"    24 1) ("(0.@(B"    24 3) ("(0/@(B"    24 5) ("(00@(B"    24 7)
    ("ong(0@(B"  25 0) ("(0-(Bng(0@(B"  25 1) ("(0.(Bng(0@(B"  25 3) ("(0/(Bng(0@(B"  25 5) ("(00(Bng(0@(B"  25 7)
    ("ou(0@(B"   26 0) ("(0-(Bu(0@(B"   26 1) ("(0.(Bu(0@(B"   26 3) ("(0/(Bu(0@(B"   26 5) ("(00(Bu(0@(B"   26 7)
    ("u(0@(B"    27 0) ("(01@(B"    27 1) ("(02@(B"    27 3) ("(03@(B"    27 5) ("(04@(B"    27 7)
    ("ua(0@(B"   28 0) ("u(0!@(B"   28 1) ("u(0"@(B"   28 3) ("u(0#@(B"   28 5) ("u(0$@(B"   28 7)
    ("uai(0@(B"  29 0) ("u(0!(Bi(0@(B"  29 1) ("u(0"(Bi(0@(B"  29 3) ("u(0#(Bi(0@(B"  29 5) ("u(0$(Bi(0@(B"  29 7)
    ("uan(0@(B"  30 0) ("u(0!(Bn(0@(B"  30 1) ("u(0"(Bn(0@(B"  30 3) ("u(0#(Bn(0@(B"  30 5) ("u(0$(Bn(0@(B"  30 7)
    ("uang(0@(B" 31 0) ("u(0!(Bng(0@(B" 31 1) ("u(0"(Bng(0@(B" 31 3) ("u(0#(Bng(0@(B" 31 5) ("u(0$(Bng(0@(B" 31 7)
    ("ue(0@(B"   32 0) ("u(0%@(B"   32 1) ("u(0&@(B"   32 3) ("u(0'@(B"   32 5) ("u(0(@(B"   32 7)
    ("ui(0@(B"   33 0) ("u(0)@(B"   33 1) ("u(0*@(B"   33 3) ("u(0+@(B"   33 5) ("u(0,@(B"   33 7)
    ("un(0@(B"   34 0) ("(01(Bn(0@(B"   34 1) ("(02(Bn(0@(B"   34 3) ("(03(Bn(0@(B"   34 5) ("(04(Bn(0@(B"   34 7)
    ("uo(0@(B"   35 0) ("u(0-@(B"   35 1) ("u(0.@(B"   35 3) ("u(0/@(B"   35 5) ("u(00@(B"   35 7)
    ("(09@(B"    36 0) ("(05@(B"    36 1) ("(06@(B"    36 3) ("(07@(B"    36 5) ("(08@(B"    36 7)
    ("(09(Be(0@(B"   37 0) ("(09%@(B"   37 1) ("(09&@(B"   37 3) ("(09'@(B"   37 5) ("(09(@(B"   37 7)
    ("0(0@(B"    38 0) ("1(0@(B"    38 1) ("2(0@(B"    38 3) ("3(0@(B"    38 5) ("4(0@(B"    38 7)))

(defconst egg-pinyin-table
  [
   0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0
   0 1 1 1 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 1 1 0 1 1 0 1 0 1 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1
   0 1 0 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1
   0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 0 1 0 0 0 1 1 1 1 0 0 1 0 0 0 1 1 1 1 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 0 1 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 0 1 0 0 0 0 1 1 1 0 0 1 0 0 0 0 1 1 1 1
   0 1 1 1 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1 0 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 1
   0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 0 0 1
   0 0 0 1 1 1 1 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 0 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 0 1 0 0 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1
   0 1 1 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 1
   0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 0 0 1
   0 1 0 1 1 1 1 0 0 0 0 1 0 0 0 0 0 1 1 0 0 0 0 0 1 1 1 1 0 0 1 0 1 0 1 0 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1
   ])

(defconst egg-zhuyin-shengmu
  '((""  .  0) ("(0E(B" .  1) ("(0X(B" .  2) ("(0T(B" .  3) ("(0I(B" .  4)
    ("(0H(B" .  5) ("(0M(B" .  6) ("(0O(B" .  7) ("(0P(B" .  8) ("(0N(B" .  9)
    ("(0L(B" . 10) ("(0G(B" . 11) ("(0K(B" . 12) ("(0F(B" . 13) ("(0Q(B" . 14)
    ("(0V(B" . 15) ("(0Y(B" . 16) ("(0U(B" . 17) ("(0J(B" . 18) ("(0h(B" . 19)
    ("(0R(B" . 20) ("(0g(B" . 21) ("(0W(B" . 22) ("(0S(B" . 23)))

(defconst egg-zhuyin-yunmu
  '(("(0@(B"    0 0) ("(0A(B"    0 1) ("(0B(B"    0 3) ("(0C(B"    0 5) ("(0D(B"    0 7) ; i
    ("(0Z@(B"   1 0) ("(0ZA(B"   1 1) ("(0ZB(B"   1 3) ("(0ZC(B"   1 5) ("(0ZD(B"   1 7) ; a
    ("(0^@(B"   2 0) ("(0^A(B"   2 1) ("(0^B(B"   2 3) ("(0^C(B"   2 5) ("(0^D(B"   2 7) ; ai
    ("(0b@(B"   3 0) ("(0bA(B"   3 1) ("(0bB(B"   3 3) ("(0bC(B"   3 5) ("(0bD(B"   3 7) ; an
    ("(0d@(B"   4 0) ("(0dA(B"   4 1) ("(0dB(B"   4 3) ("(0dC(B"   4 5) ("(0dD(B"   4 7) ; ang
    ("(0`@(B"   5 0) ("(0`A(B"   5 1) ("(0`B(B"   5 3) ("(0`C(B"   5 5) ("(0`D(B"   5 7) ; ao
    ("(0\@(B"   6 0) ("(0\A(B"   6 1) ("(0\B(B"   6 3) ("(0\C(B"   6 5) ("(0\D(B"   6 7) ; e
    ("(0_@(B"   7 0) ("(0_A(B"   7 1) ("(0_B(B"   7 3) ("(0_C(B"   7 5) ("(0_D(B"   7 7) ; ei
    ("(0c@(B"   8 0) ("(0cA(B"   8 1) ("(0cB(B"   8 3) ("(0cC(B"   8 5) ("(0cD(B"   8 7) ; en
    ("(0e@(B"   9 0) ("(0eA(B"   9 1) ("(0eB(B"   9 3) ("(0eC(B"   9 5) ("(0eD(B"   9 7) ; eng
    ("(0f@(B"  10 0) ("(0fA(B"  10 1) ("(0fB(B"  10 3) ("(0fC(B"  10 5) ("(0fD(B"  10 7) ; er
    ("(0g@(B"  11 0) ("(0gA(B"  11 1) ("(0gB(B"  11 3) ("(0gC(B"  11 5) ("(0gD(B"  11 7) ; i
    ("(0gZ@(B" 12 0) ("(0gZA(B" 12 1) ("(0gZB(B" 12 3) ("(0gZC(B" 12 5) ("(0gZD(B" 12 7) ; ia
    ("(0gb@(B" 13 0) ("(0gbA(B" 13 1) ("(0gbB(B" 13 3) ("(0gbC(B" 13 5) ("(0gbD(B" 13 7) ; ian
    ("(0gd@(B" 14 0) ("(0gdA(B" 14 1) ("(0gdB(B" 14 3) ("(0gdC(B" 14 5) ("(0gdD(B" 14 7) ; iang
    ("(0g`@(B" 15 0) ("(0g`A(B" 15 1) ("(0g`B(B" 15 3) ("(0g`C(B" 15 5) ("(0g`D(B" 15 7) ; iao
    ("(0g]@(B" 16 0) ("(0g]A(B" 16 1) ("(0g]B(B" 16 3) ("(0g]C(B" 16 5) ("(0g]D(B" 16 7) ; ie
    ("(0gc@(B" 17 0) ("(0gcA(B" 17 1) ("(0gcB(B" 17 3) ("(0gcC(B" 17 5) ("(0gcD(B" 17 7) ; in
    ("(0ge@(B" 18 0) ("(0geA(B" 18 1) ("(0geB(B" 18 3) ("(0geC(B" 18 5) ("(0geD(B" 18 7) ; ing
    ("(0ie@(B" 19 0) ("(0ieA(B" 19 1) ("(0ieB(B" 19 3) ("(0ieC(B" 19 5) ("(0ieD(B" 19 7) ; iong
    ("(0ga@(B" 20 0) ("(0gaA(B" 20 1) ("(0gaB(B" 20 3) ("(0gaC(B" 20 5) ("(0gaD(B" 20 7) ; iu
    ("(0G@(B"  21 0) ("(0GA(B"  21 1) ("(0GB(B"  21 3) ("(0GC(B"  21 5) ("(0GD(B"  21 7) ; m
    ("(0K@(B"  22 0) ("(0KA(B"  22 1) ("(0KB(B"  22 3) ("(0KC(B"  22 5) ("(0KD(B"  22 7) ; n
    ("@(0@(B"  23 0) ("@(0A(B"  23 1) ("@(0B(B"  23 3) ("@(0C(B"  23 5) ("@(0D(B"  23 7) ; ng
    ("(0[@(B"  24 0) ("(0[A(B"  24 1) ("(0[B(B"  24 3) ("(0[C(B"  24 5) ("(0[D(B"  24 7) ; o
    ("(0he@(B" 25 0) ("(0heA(B" 25 1) ("(0heB(B" 25 3) ("(0heC(B" 25 5) ("(0heD(B" 25 7) ; ong
    ("(0a@(B"  26 0) ("(0aA(B"  26 1) ("(0aB(B"  26 3) ("(0aC(B"  26 5) ("(0aD(B"  26 7) ; ou
    ("(0h@(B"  27 0) ("(0hA(B"  27 1) ("(0hB(B"  27 3) ("(0hC(B"  27 5) ("(0hD(B"  27 7) ; u
    ("(0hZ@(B" 28 0) ("(0hZA(B" 28 1) ("(0hZB(B" 28 3) ("(0hZC(B" 28 5) ("(0hZD(B" 28 7) ; ua
    ("(0h^@(B" 29 0) ("(0h^A(B" 29 1) ("(0h^B(B" 29 3) ("(0h^C(B" 29 5) ("(0h^D(B" 29 7) ; uai
    ("(0hb@(B" 30 0) ("(0hbA(B" 30 1) ("(0hbB(B" 30 3) ("(0hbC(B" 30 5) ("(0hbD(B" 30 7) ; uan
    ("(0hd@(B" 31 0) ("(0hdA(B" 31 1) ("(0hdB(B" 31 3) ("(0hdC(B" 31 5) ("(0hdD(B" 31 7) ; uang
    ("(0i]@(B" 37 0) ("(0i]A(B" 37 1) ("(0i]B(B" 37 3) ("(0i]C(B" 37 5) ("(0i]D(B" 37 7) ; ue
    ("(0h_@(B" 33 0) ("(0h_A(B" 33 1) ("(0h_B(B" 33 3) ("(0h_C(B" 33 5) ("(0h_D(B" 33 7) ; ui
    ("(0hc@(B" 34 0) ("(0hcA(B" 34 1) ("(0hcB(B" 34 3) ("(0hcC(B" 34 5) ("(0hcD(B" 34 7) ; un
    ("(0h[@(B" 35 0) ("(0h[A(B" 35 1) ("(0h[B(B" 35 3) ("(0h[C(B" 35 5) ("(0h[D(B" 35 7) ; uo
    ("(0i@(B"  36 0) ("(0iA(B"  36 1) ("(0iB(B"  36 3) ("(0iC(B"  36 5) ("(0iD(B"  36 7) ; (09(B
    ("(0i]@(B" 37 0) ("(0i]A(B" 37 1) ("(0i]B(B" 37 3) ("(0i]C(B" 37 5) ("(0i]D(B" 37 7) ; (09(Be
    ("0(0@(B"  38 0) ("1(0A(B"  38 1) ("2(0B(B"  38 3) ("3(0C(B"  38 5) ("4(0D(B"  38 7) ; undefined
    ("(0ib@(B" 39 0) ("(0ibA(B" 39 1) ("(0ibB(B" 39 3) ("(0ibC(B" 39 5) ("(0ibD(B" 39 7) ; (09(Ban
    ("(0ic@(B" 40 0) ("(0icA(B" 40 1) ("(0icB(B" 40 3) ("(0icC(B" 40 5) ("(0icD(B" 40 7) ; (09(Bn
    ))

(defconst egg-zhuyin-table
  [
   ;; empty ShengMu
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x9586 ?\x0000 ?\x9592 ?\x9599
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x959b ?\x95a0 ?\x0000 ?\x959e
   ?\x95a2
   ;; ShengMu B
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu C
   ?\x828b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0280 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu Ch
   ?\x838b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000
   ?\x0000 ?\x0380 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu D
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu F
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu G
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu H
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu J
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x08a4 ?\x0000 ?\x0000
   ?\x08a7 ?\x0000 ?\x08a5 ?\x0000 ?\x08a8 ?\x0000 ?\x889b ?\x88a0 ?\x8000 ?\x889e
   ?\x88a2
   ;; ShengMu K
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu L
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu M
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu N
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu P
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000 
   ;; ShengMu Q
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0ea4 ?\x0000 ?\x0000
   ?\x0ea7 ?\x0000 ?\x0ea5 ?\x0000 ?\x0ea8 ?\x0000 ?\x8e9b ?\x8ea0 ?\x8000 ?\x8e9e
   ?\x8ea2
   ;; ShengMu R
   ?\x8f8b ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000
   ?\x0000 ?\x0f80 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu S
   ?\x908b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000
   ?\x0000 ?\x1080 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu Sh
   ?\x918b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x1180 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu T
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu W
   ?\x939b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x0000 ?\x1380 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu X
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x14a4 ?\x0000 ?\x0000
   ?\x14a7 ?\x0000 ?\x14a5 ?\x0000 ?\x14a8 ?\x0000 ?\x949b ?\x94a0 ?\x8000 ?\x949e
   ?\x94a2
   ;; ShengMu Y 
   ?\x958b ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0090 ?\x0000 ?\x9591 ?\x9592
   ?\x0000 ?\x1580 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x1588 ?\x1589 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0093 ?\x8000 ?\x00a4 ?\x0000 ?\x0000
   ?\x00a7 ?\x0000 ?\x00a5 ?\x0000 ?\x00a8 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu Z
   ?\x968b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x1680 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu Zh 
   ?\x978b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x1780 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ])

(defconst egg-chinese-syllable-max-len
  (max (length "Zhu(0!(Bng(0@(B") (length "(0ShdA(B")))

(defun egg-chinese-syllable (str pos)
  (setq str (substring str pos (min (length str)
				    (+ pos egg-chinese-syllable-max-len))))
  (or (car (egg-pinyin-syllable str))
      (car (egg-zhuyin-syllable str))))

(defsubst egg-make-fixed-euc-china-code (s y)
  (cons
   (+ (* 2 (nth 1 y)) (logand (nth 2 y) 1) 32)
   (+ (* 4 (if (= s 0) 20 s)) (lsh (nth 2 y) -1) 156)))

(defun egg-pinyin-syllable (str)
  (if (eq (string-match "^[A-Za-z(0!(B-(0?(B]+(0@(B" str) 0)
      (let (s y end)
	(setq end (match-end 0))
	(cond
	 ((setq s (cdr (assoc (substring str 0 2) egg-pinyin-shengmu)))
	  (setq y (substring str 2 end)))
	 ((setq s (cdr (assoc (substring str 0 1) egg-pinyin-shengmu)))
	  (setq y (substring str 1 end)))
	 (t
	  (setq s 0 y (substring str 0 end))))
	(if (and (setq y (assoc y egg-pinyin-yunmu))
		 (= (aref egg-pinyin-table (+ (* 39 s) (nth 1 y))) 1))
	    (cons end (egg-make-fixed-euc-china-code s y))))))

(defun egg-zhuyin-syllable (str)
  (if (eq (string-match "^[(0E(B-(0i(B@0-4]+[(0@ABCD(B]" str) 0)
      (let (end s y c z (zhuyin-len (length "(0E(B")))
	(setq end (match-end 0)
	      c (substring str 0 zhuyin-len)
	      s (cdr (assoc c egg-zhuyin-shengmu))
	      y (assoc (substring str zhuyin-len end) egg-zhuyin-yunmu))
	(if (or (null (and s y))
		(and (or (eq s 11) (eq s 12)) (eq (nth 1 y) 0))) ; [(0GK(B][(0@ABCD(B]
	    (setq s 0
		  y (assoc (substring str 0 end) egg-zhuyin-yunmu)))
	(if (and y
		 (setq z (aref egg-zhuyin-table (+ (* 41 s) (nth 1 y))))
		 (/= (logand z ?\x8000) 0))
	    (if (/= (logand z ?\x80) 0)
		(cons end (egg-make-fixed-euc-china-code
			   (logand (lsh z -8) ?\x7f)
			   (list nil (logand z ?\x7f) (nth 2 y))))
	      (cons end (egg-make-fixed-euc-china-code s y)))))))

(defun encode-fixed-euc-china-region (beg end type)
  "Encode the text in the region to EUC-CN/TW."
  (let (s syl c cset)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (< (point) (point-max))
	  (setq s (buffer-substring
		   (point)
		   (min (point-max) (+ (point) egg-chinese-syllable-max-len))))
	  (cond
	   ((setq syl (egg-pinyin-syllable s))
	    (delete-region (point) (+ (point) (car syl)))
	    (insert (car (cdr syl)) (cdr (cdr syl))))
	   ((setq syl (egg-zhuyin-syllable s))
	    (delete-region (point) (+ (point) (car syl)))
	    (insert (car (cdr syl)) (cdr (cdr syl))))
	   (t
	    (setq c (split-char (following-char))
		  cset (car c))
	    (cond
	     ((or (and (eq cset 'chinese-gb2312) (eq type 'cn))
		  (and (eq cset 'chinese-cns11643-1) (eq type 'tw)))
	      (delete-char 1)
	      (insert (+ (nth 1 c) 128) (+ (nth 2 c) 128)))
	     ((and (eq cset 'chinese-cns11643-2) (eq type 'tw))
	      (delete-char 1)
	      (insert (+ (nth 1 c) 128) (nth 2 c)))
	     ((eq cset 'chinese-sisheng)
	      (delete-char 1)
	      (insert 0 (+ (nth 1 c) 128)))
	     ((eq cset 'ascii)
	      (delete-char 1)
	      (insert 0 (nth 1 c)))
	     (t
	      (delete-char 1))))))
	(- (point-max) (point-min))))))

(defun pre-write-encode-fixed-euc-china (from to type)
  (let ((buf (current-buffer))
	(work (get-buffer-create " *pre-write-encoding-work*")))
    (set-buffer work)
    (erase-buffer)
    (if (null (stringp from))
	(save-excursion
	  (set-buffer buf)
	  (setq from (buffer-substring from to))))
    (insert (string-as-multibyte from))
    (encode-fixed-euc-china-region 1 (point-max) type)
    nil))

(defun pre-write-encode-euc-cn (from to)
  (pre-write-encode-fixed-euc-china from to 'cn))

(defun pre-write-encode-euc-tw (from to)
  (pre-write-encode-fixed-euc-china from to 'tw))

(defun decode-fixed-euc-china-region (beg end type zhuyin)
  "Decode EUC-CN/TW encoded text in the region.
Return the length of resulting text."
  (let ((str (string-as-unibyte (buffer-substring beg end)))
	(i 0)
	(char (make-string 3 0))
	l c0 c1 s y ss)
    (delete-region beg end)
    (setq l (1- (length str)))
    (while (< i l)
      (setq c0 (aref str i)
	    c1 (aref str (1+ i))
	    i  (+ i 2))
      (cond
       ((eq c0 0)
	(if (<= c1 ?\xa0)
	    (insert c1)
	  (aset char 0 leading-code-private-11)
	  (aset char 1 (charset-id 'chinese-sisheng))
	  (aset char 2 c1)
	  (insert (string-as-multibyte char))))
       ((>= c0 ?\x80)
	(cond
	 ((eq type 'cn)
	  (aset char 0 (charset-id 'chinese-gb2312))
	  (aset char 1 c0)
	  (aset char 2 (logior c1 ?\x80)))
	 ((>= c1 ?\x80)
	  (aset char 0 (charset-id 'chinese-cns11643-1))
	  (aset char 1 c0)
	  (aset char 2 c1))
	 (t
	  (aset char 0 (charset-id 'chinese-cns11643-2))
	  (aset char 1 c0)
	  (aset char 2 (+ c1 ?\x80))))
	(insert (string-as-multibyte char)))
       (t
	(setq c1 (logand c1 ?\x7f))
	(setq s (- (lsh c1 -2) 7);;(+ (lsh (- c1 32) -2) 1)
	      y (- (lsh c0 -1) 16);;(lsh (- c0 32) -1)
	      ss (+ (logand c0 1) (logand c1 3)))
	(if (and (eq s 20)
		 (eq (aref egg-pinyin-table (+ (* 39 20) y)) 0))
	    (setq s 0))
	(if (null zhuyin)
	    (setq s (car (nth s egg-pinyin-shengmu))
		  y (car (nth (+ (* 5 y) ss) egg-pinyin-yunmu)))
	  (setq c0 (aref egg-zhuyin-table (+ (* 41 s) y)))
	  (if (eq (logand c0 ?\x8080) ?\x80)
	      (setq s (lsh c0 -8)
		    y (logand c0 ?\x7f)))
	  (setq s (car (nth s egg-zhuyin-shengmu))
		y (car (nth (+ (* 5 y) ss) egg-zhuyin-yunmu))))
	(if enable-multibyte-characters
	    (insert s y)
	  (insert (string-as-unibyte s) (string-as-unibyte y))))))
    (- (point) beg)))

(defun post-read-decode-fixed-euc-china (len type zhuyin)
  (let ((pos (point))
	(buffer-modified-p (buffer-modified-p)))
    (prog1
	(decode-fixed-euc-china-region pos (+ pos len) type zhuyin)
      (set-buffer-modified-p buffer-modified-p))))

(defun post-read-decode-euc-py-cn (len)
  (post-read-decode-fixed-euc-china len 'cn nil))

(defun post-read-decode-euc-zy-cn (len)
  (post-read-decode-fixed-euc-china len 'cn t))

(defun post-read-decode-euc-py-tw (len)
  (post-read-decode-fixed-euc-china len 'tw nil))

(defun post-read-decode-euc-zy-tw (len)
  (post-read-decode-fixed-euc-china len 'tw t))

(make-coding-system 'fixed-euc-py-cn 0 ?W
		    "Coding System for fixed EUC Chinese-gb2312")
(coding-system-put 'fixed-euc-py-cn
		   'pre-write-conversion 'pre-write-encode-euc-cn)
(coding-system-put 'fixed-euc-py-cn
		   'post-read-conversion 'post-read-decode-euc-py-cn)

(make-coding-system 'fixed-euc-zy-cn 0 ?W
		    "Coding System for fixed EUC Chinese-gb2312")
(coding-system-put 'fixed-euc-zy-cn
		   'pre-write-conversion 'pre-write-encode-euc-cn)
(coding-system-put 'fixed-euc-zy-cn
		   'post-read-conversion 'post-read-decode-euc-zy-cn)

(make-coding-system 'fixed-euc-py-tw 0 ?W
		    "Coding System for fixed EUC Chinese-cns11643")
(coding-system-put 'fixed-euc-py-tw
		   'pre-write-conversion 'pre-write-encode-euc-tw)
(coding-system-put 'fixed-euc-py-tw
		   'post-read-conversion 'post-read-decode-euc-py-tw)

(make-coding-system 'fixed-euc-zy-tw 0 ?W
		    "Coding System for fixed EUC Chinese-cns11643")
(coding-system-put 'fixed-euc-zy-tw
		   'pre-write-conversion 'pre-write-encode-euc-tw)
(coding-system-put 'fixed-euc-zy-tw
		   'post-read-conversion 'post-read-decode-euc-zy-tw)

;; Binary data

(eval-and-compile
(define-ccl-program ccl-decode-egg-binary
  `(1
    ((read r0)
     (loop
      (if (r0 == ?\xff)
	  (read r1))			; toss out
      (write-read-repeat r0)))))

(define-ccl-program ccl-encode-egg-binary
  `(2
    ((read r0)
     (loop
      (if (r0 == ?\xff)
	  ((write r0)
	   (r0 = 0)))
      (write-read-repeat r0))))))

(make-coding-system 'egg-binary 4 ?W "Coding System for binary data"
		    (cons ccl-decode-egg-binary ccl-encode-egg-binary))


(defun comm-format-u32c (uint32c)
  (insert-char (logand (lsh (car uint32c) -8) 255) 1)
  (insert-char (logand (car uint32c) 255) 1)
  (insert-char (logand (lsh (nth 1 uint32c) -8) 255) 1)
  (insert-char (logand (nth 1 uint32c) 255) 1))

(defun comm-format-u32 (uint32)
  (insert-char (logand (lsh uint32 -24) 255) 1)
  (insert-char (logand (lsh uint32 -16) 255) 1)
  (insert-char (logand (lsh uint32 -8) 255) 1)
  (insert-char (logand uint32 255) 1))

(defun comm-format-i32 (int32)
  (insert-char (logand (ash int32 -24) 255) 1)
  (insert-char (logand (ash int32 -16) 255) 1)
  (insert-char (logand (ash int32 -8) 255) 1)
  (insert-char (logand int32 255) 1))

(defun comm-format-u16 (uint16)
  (insert-char (logand (lsh uint16 -8) 255) 1)
  (insert-char (logand uint16 255) 1))

(defun comm-format-u8 (uint8)
  (insert-char (logand uint8 255) 1))

(defun comm-format-truncate-after-null (s)
  (if (string-match "\0" s)
      (substring s 0 (match-beginning 0))
    s))

(defun comm-format-u16-string (s)
  (insert (encode-coding-string (comm-format-truncate-after-null s)
				egg-fixed-euc))
  (insert-char 0 2))

(defun comm-format-mb-string (s)
  (insert (encode-coding-string  (comm-format-truncate-after-null s)
				 egg-mb-euc))
  (insert-char 0 1))

(defun comm-format-u8-string (s)
  (insert (comm-format-truncate-after-null s))
  (insert-char 0 1))

(defun comm-format-binary-data (s)
  (insert (encode-coding-string s 'egg-binary))
  (insert-char ?\377 2))

(defun comm-format-fixlen-string (s len)
  (setq s (comm-format-truncate-after-null s))
  (insert (if (< (length s) len) s (substring s 0 (1- len))))
  (insert-char 0 (max (- len (length s)) 1)))

(defun comm-format-vector (s len)
  (setq s (concat s))
  (insert (if (<= (length s) len) s (substring s 0 len)))
  (insert-char 0 (- len (length s))))

(defmacro comm-format (format &rest args)
  "Format a string out of a control-list and arguments into the buffer.
The formated datas are network byte oder (i.e. big endian)..
U: 32-bit integer.  The argument is 2 element 16-bit unsigned integer list.
u: 32-bit integer.  The argument is treat as unsigned integer.
   (Note:  Elisp's integer may be less than 32 bits)
i: 32-bit integer.
   (Note:  Elisp's integer may be greater than 32 bits)
w: 16-bit integer.
b: 8-bit integer.
S: 16-bit wide-character EUC string (0x0000 terminated).
E: Multibyte EUC string (0x00 terminated).
s: 8-bit string (0x00 terminated).
B: Binary data (0xff terminated).
v: 8-bit vector (no terminator).  This takes 2 args (data length).
V: Fixed length string (0x00 terminated).  This takes 2 args (data length)."
  (let ((p args)
	(form format)
	(result (list 'progn))
	f arg)
    (while (and form p)
      (setq f (car form)
	    arg (car p))
      (nconc result
	     (list
	      (cond ((eq f 'U) (list 'comm-format-u32c arg))
		    ((eq f 'u) (list 'comm-format-u32 arg))
		    ((eq f 'i) (list 'comm-format-i32 arg))
		    ((eq f 'w) (list 'comm-format-u16 arg))
		    ((eq f 'b) (list 'comm-format-u8 arg))
		    ((eq f 'S) (list 'comm-format-u16-string arg))
		    ((eq f 'E) (list 'comm-format-mb-string arg))
		    ((eq f 's) (list 'comm-format-u8-string arg))
		    ((eq f 'B) (list 'comm-format-binary-data arg))
		    ((eq f 'V) (setq p (cdr p))
			       (list 'comm-format-fixlen-string arg (car p)))
		    ((eq f 'v) (setq p (cdr p))
			       (list 'comm-format-vector arg (car p))))))
      (setq form (cdr form)
	    p (cdr p)))
    (if (or form p)
	(error "comm-format %s: arguments mismatch" format))
    result))

(defvar comm-accept-timeout nil)

;; Assume PROC is bound to the process of current buffer
;; Do not move the point, leave it where it was.
(defmacro comm-accept-process-output ()
  `(let ((p (point)))
     (if (null (accept-process-output proc comm-accept-timeout))
	 (egg-error "backend timeout (comm-accept-process-output)"))
     (goto-char p)))

(defmacro comm-require-process-output (n)
  `(if (< (point-max) (+ (point) ,n))
       (comm-wait-for-space proc ,n)))

(defun comm-wait-for-space (proc n)
  (let ((p (point))
	(r (+ (point) n)))
    (while (< (point-max) r)
      (if (null (accept-process-output proc comm-accept-timeout))
	  (egg-error "backend timeout (comm-wait-for-space)"))
      (goto-char p))))

(defmacro comm-following+forward-char ()
  `(prog1
       (following-char)
     (forward-char 1)))

(defun comm-unpack-u32c ()
  (progn
    (comm-require-process-output 4)
    (list (+ (lsh (comm-following+forward-char) 8)
	     (comm-following+forward-char))
	  (+ (lsh (comm-following+forward-char) 8)
	     (comm-following+forward-char)))))

(defun comm-unpack-i32 ()
  (progn
    (comm-require-process-output 4)
    (+ (lsh (- (logxor (comm-following+forward-char) 128) 128) 24)
       (lsh (comm-following+forward-char) 16)
       (lsh (comm-following+forward-char) 8)
       (comm-following+forward-char))))

(defun comm-unpack-u32 ()
  (progn
    (comm-require-process-output 4)
    (+ (lsh (comm-following+forward-char) 24)
       (lsh (comm-following+forward-char) 16)
       (lsh (comm-following+forward-char) 8)
       (comm-following+forward-char))))

(defun comm-unpack-u16 ()
  (progn
    (comm-require-process-output 2)
    (+ (lsh (comm-following+forward-char) 8)
       (comm-following+forward-char))))

(defun comm-unpack-u8 ()
  (progn
    (comm-require-process-output 1)
    (comm-following+forward-char)))

(defun comm-unpack-u16-string ()
  (let ((start (point)))
    (while (not (search-forward "\0\0" nil t))
      (comm-accept-process-output))
    (decode-coding-string (buffer-substring start (- (point) 2))
			  egg-fixed-euc)))

(defun comm-unpack-mb-string ()
  (let ((start (point)))
    (while (not (search-forward "\0" nil t))
      (comm-accept-process-output))
    (decode-coding-string (buffer-substring start (1- (point)))
			  egg-mb-euc)))

(defun comm-unpack-u8-string ()
  (let ((start (point)))
    (while (not (search-forward "\0" nil 1))
      (comm-accept-process-output))
    (buffer-substring start (1- (point)))))

(defun comm-unpack-binary-data ()
  (let ((start (point)))
    (while (not (search-forward "\377\377" nil 1))
      (comm-accept-process-output))
    (string-as-unibyte
     (decode-coding-string (buffer-substring start (- (point) 2))
			   'egg-binary))))

(defun comm-unpack-fixlen-string (len)
  (let (s)
    (comm-require-process-output len)
    (goto-char (+ (point) len))
    (setq s (buffer-substring (- (point) len) (point)))
    (if (string-match "\0" s)
	(setq s (substring s 0 (match-beginning 0))))
    s))

(defun comm-unpack-vector (len)
  (progn
    (comm-require-process-output len)
    (goto-char (+ (point) len))
    (buffer-substring (- (point) len) (point))))

(defmacro comm-unpack (format &rest args)
  "Unpack a string out of a control-string and set arguments.
See `comm-format' for FORMAT."
  (let ((p args)
	(form format)
	(result (list 'progn))
	arg f)
    (while (and form p)
      (setq f (car form)
	    arg (car p))
      (nconc result
	     (list
	      (cond ((eq f 'U) `(setq ,arg (comm-unpack-u32c)))
		    ((eq f 'u) `(setq ,arg (comm-unpack-u32)))
		    ((eq f 'i) `(setq ,arg (comm-unpack-i32)))
		    ((eq f 'w) `(setq ,arg (comm-unpack-u16)))
		    ((eq f 'b) `(setq ,arg (comm-unpack-u8)))
		    ((eq f 'S) `(setq ,arg (comm-unpack-u16-string)))
		    ((eq f 'E) `(setq ,arg (comm-unpack-mb-string)))
		    ((eq f 's) `(setq ,arg (comm-unpack-u8-string)))
		    ((eq f 'B) `(setq ,arg (comm-unpack-binary-data)))
		    ((eq f 'V) (setq p (cdr p))
			       `(setq ,arg (comm-unpack-fixlen-string ,(car p))))
		    ((eq f 'v) (setq p (cdr p))
			       `(setq ,arg (comm-unpack-vector ,(car p)))))))
      (setq form (cdr form)
	    p (cdr p)))
    (if (or form p)
	(error "comm-unpack %s: arguments mismatch" format))
    result))

(defmacro comm-call-with-proc (proc vlist send-expr &rest receive-exprs)
  (let ((euc-select
	 (and (eq (car-safe (car vlist)) 'zhuyin)
	      '((egg-fixed-euc (nth (if zhuyin 1 0) egg-fixed-euc))))))
  `(let* ((proc ,proc)
	  (buffer (process-buffer proc))
	  ,@vlist)
     (if (and (memq (process-status proc) '(open run))
	      (buffer-live-p buffer))
	 (save-excursion
	   (set-buffer buffer)
	   (let ,euc-select
	     (erase-buffer)
	     ,send-expr
	     (goto-char (point-max))
	     (process-send-region proc (point-min) (point-max))
	     ,@receive-exprs))
       (egg-error "process %s was killed" proc)))))

(defmacro comm-call-with-proc-1 (proc vlist send-expr &rest receive-exprs)
  `(let ,vlist
     (erase-buffer)
     ,send-expr
     (goto-char (point-max))
     (process-send-region proc (point-min) (point-max))
     ,@receive-exprs))

(provide 'egg-com)
;;; egg-com.el ends here.

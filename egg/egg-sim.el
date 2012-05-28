;;; -*- coding: iso-2022-7bit -*-
;;; egg-sim.el --- EGG Simple Input Method

;; Copyright (C) 2000 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2000 TOMURA Satoru <tomura@etl.go.jp>


;; Author: TOMURA Satoru <tomura@etl.go.jp>

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

;;; This code is based on egg-jsymbol.el of Egg V3.

;;; 92.10.18 modified for Mule Ver.0.9.6 by K.Handa <handa@etl.go.jp>
;;;	Moved from egg.el
;;; 92.12.26 modified for Mule Ver.0.9.7 by T.Shingu <shingu@cpr.canon.co.jp>
;;;	JIS Hojo Kanji support.

(require 'menudiag)

(provide 'egg-sim)

(defun make-char-list (charset &optional from to)
  (let ((result nil)
	(chars (charset-chars charset))
	min max)
    (setq min (if (= chars 96) 32 33)
	  max (if (= chars 96) 127 126))
    (setq from (if from (+ min (1- from)) min)
	  to   (if to (+ min (1- to)) max))
    (and (<= min from)
	 (<= to max)
	 (cond ((= (charset-dimension charset) 1)
		(while (<= from to)
		  (setq result (cons (char-to-string
				      (make-char charset to))
				     result)
			to (1- to)))
		result)
	       ((= (charset-dimension charset) 2)
		(while (<= from to)
		  (let ((code max))
		    (while (<= min code)
		      (setq result (cons (char-to-string
					  (make-char charset to code))
					 result)
			    code (1- code))))
		  (setq to (1- to)))
		result)))))

(defvar egg-sim-ascii-menu
  `(menu "ASCII:" ,(make-char-list 'ascii)))

(defvar egg-sim-latin-1-menu
  `(menu "ISO 8859-1:" ,(make-char-list 'latin-iso8859-1)))

(defvar egg-sim-latin-2-menu
  `(menu "ISO 8859-2:" ,(make-char-list 'latin-iso8859-2)))

(defvar egg-sim-latin-3-menu
  `(menu "ISO 8859-3:" ,(make-char-list 'latin-iso8859-3)))

(defvar egg-sim-latin-4-menu
  `(menu "ISO 8859-4:" ,(make-char-list 'latin-iso8859-4)))

(defvar egg-sim-latin-5-menu
  `(menu "ISO 8859-9:" ,(make-char-list 'latin-iso8859-9)))

(defvar egg-sim-cyrillic-menu
  `(menu "ISO 8859-5:" ,(make-char-list 'cyrillic-iso8859-5)))

(defvar egg-sim-arabic-menu
  `(menu "ISO 8859-6:" ,(make-char-list 'arabic-iso8859-6)))

(defvar egg-sim-greek-menu
  `(menu "ISO 8859-7:" ,(make-char-list 'greek-iso8859-7)))

(defvar egg-sim-hebrew-menu
  `(menu "ISO 8859-8:" ,(make-char-list 'hebrew-iso8859-8)))

(defvar egg-sim-thai-menu
  `(menu "TIS620.2529:" ,(make-char-list 'thai-tis620)))

(defvar egg-sim-lao-menu
  `(menu "lao:"         ,(make-char-list 'lao)))

(defvar egg-sim-vietnamese-menu
  `(menu "Vietnamese:"
	 (("VISCII" .
	   (menu "VISCII:"
		 (
		  "a" ",1`(B" ",1d(B" ",1c(B" ",1a(B" ",1U"
		  ",1e(B" ",1"(B" ",1F" ",1G" ",1!(B" ",1#(B"
		  ",1b(B" ",1%(B" ",1&(B" ",1g(B" ",1$(B" ",1'(B"
		  "e" ",1i(B" ",1k(B" ",1((B" ",1h(B" ",1)(B"
		  ",1j(B" ",1*(B" ",1,(B" ",1-(B" ",1+(B" ",1.(B" 
		  "i" ",1m(B" ",1o(B" ",1n(B" ",1l(B" ",18(B"
		  "o" ",1s(B" ",1v(B" ",1u(B" ",1r(B" ",1w(B"
		  ",1t(B" ",1/(B" ",11(B" ",12(B" ",10(B" ",15(B"
		  ",1=(B" ",1>(B" ",17(B" ",1^(B" ",16(B" ",1~(B"
		  "u" ",1z(B" ",1|(B" ",1{(B" ",1y(B" ",1x(B"
		  ",1_(B" ",1Q" ",1X" ",1f(B" ",1W" ",1q(B"
		  "y" ",1}(B" ",1V" ",1[(B" ",1O" ",1\(B"

		  "A" ",2`(B" ",2d(B" ",2c(B" ",2a(B" ",2U"
		  ",2e(B" ",2"(B" ",2F" ",2G" ",2!(B" ",2#(B" 
		  ",2b(B" ",2%(B" ",2&(B" ",2g(B" ",2$(B" ",2'(B" 
		  "E" ",2h(B" ",2k(B" ",2((B" ",2i(B" ",2)(B" 
		  ",2j(B" ",2+(B" ",2,(B" ",2-(B" ",2*(B" ",2.(B" 
		  "I" ",2l(B" ",2o(B" ",2n(B" ",2m(B" ",28(B"
		  "O" ",2r(B" ",2v(B" ",2u(B" ",2s(B" ",2w(B"
		  ",2t(B" ",20(B" ",21(B" ",22(B" ",2/(B" ",25(B"
		  ",2=(B" ",26(B" ",27(B" ",2^(B" ",2>(B" ",2~(B" 
		  "U" ",2y(B" ",2|(B" ",2{(B" ",2z(B" ",2x(B" 
		  ",2_(B" ",2W" ",2X" ",2f(B" ",2Q" ",2q(B"
		  "Y" ",2O" ",2V" ",2[(B" ",2}(B" ",2\(B" 

		  ",2p(B" ",1p(B"
		  )))

	  ("VISCII1.1(lower-case)" .
	   (menu "VISCII1.1 lower-case:" 
		 ,(make-char-list 'vietnamese-viscii-lower)))
	  ("VISCII1.1(upper-case)" . 
	   (menu "VISCII1.1 upper-case:"
		 ,(make-char-list 'vietnamese-viscii-upper))))))

(defvar egg-sim-chinese-big5-menu
  `(menu "Big5:"
	 (("Level1" .
	   (menu "Big 5 Level1:" , (make-char-list 'chinese-big5-1)))
	  ("Level2" .
	   (menu "Big 5 Level2:" , (make-char-list 'chinese-big5-2))))))

(defvar egg-sim-chinese-cns-menu
  `(menu "CNS 11643:"
	 (("Plane-1" .
	   (menu "CNS 11643-1:" ,(make-char-list 'chinese-cns11643-1)))
	  ("Plane- 2" .
	   (menu "CNS 11643-2:" ,(make-char-list 'chinese-cns11643-2)))
	  ("Plane-3" .
	   (menu "CNS 11643-3:" ,(make-char-list 'chinese-cns11643-3)))
	  ("Plane-4" .
	   (menu "CNS 11643-4:" ,(make-char-list 'chinese-cns11643-4)))
	  ("Plane-5" .
	   (menu "CNS 11643-5:" ,(make-char-list 'chinese-cns11643-5)))
	  ("Plane-6" .
	   (menu "CNS 11643-6:" ,(make-char-list 'chinese-cns11643-6)))
	  ("Plane-7" .
	   (menu "CNS 11643-7:" ,(make-char-list 'chinese-cns11643-7))))))

(defvar egg-sim-chinese-gb-menu
  `(menu "GB 2312:" 
	 (("All" . 
	   (menu "GB 2312:" ,(make-char-list 'chinese-gb2312)))
	  ("Symbols" . 
	   (menu "GB2312/1:" ,(make-char-list 'chinese-gb2312 1 1)))
	  ("Numbers" . 
	   (menu "GB2312/2:" ,(make-char-list 'chinese-gb2312 2 2)))
	  ("Fullwidth ASCII" . 
	   (menu "GB2312/3:" ,(make-char-list 'chinese-gb2312 3 3)))
	  ("Hiragana" .
	   (menu "GB2312/4:" ,(make-char-list 'chinese-gb2312 4 4)))
	  ("Katanaka" . 
	   (menu "GB2312/5:" ,(make-char-list 'chinese-gb2312 5 5)))
	  ("Greek" . 
	   (menu "GB2312/6:" ,(make-char-list 'chinese-gb2312 6 6)))
	  ("Cyrillic" . 
	   (menu "GB2312/7:" ,(make-char-list 'chinese-gb2312 7 7)))
	  ("Pinyin/Bopomofo" . 
	   (menu "GB2312/8:" ,(make-char-list 'chinese-gb2312 8 8)))
	  ("Box Drawings" . 
	   (menu "GB2312/9:" ,(make-char-list 'chinese-gb2312 9 9)))
	  )))

(defvar egg-sim-chinese-menu
  `(menu "Chinese:"
	 (("GB2312"  . , egg-sim-chinese-gb-menu)
	  ("CNS11643" . , egg-sim-chinese-cns-menu)
	  ("Big5" . , egg-sim-chinese-big5-menu))))

(defvar egg-sim-korean-menu
  `(menu "Korean:"
	 (("KSC5601"  .
	   (menu "KSC 5601:" ,(make-char-list 'korean-ksc5601)))
	  ("Symbol" .
	   (menu "KSC 5601/1-2:" ,(make-char-list 'korean-ksc5601 1 2)))
	  ("Fullwidth ASCII" .
	   (menu "KSC 5601/3:" , (make-char-list 'korean-ksc5601 3 3)))
	  ("Jamo" .
	   (menu "KSC 5601/4:" , (make-char-list 'korean-ksc5601 4 4)))
	  ("Roman Number/Greek" .
	   (menu "KSC 5601/5:" , (make-char-list 'korean-ksc5601 5 5)))
	  ("Box Drawings" .
	   (menu "KSC 5601/6:" , (make-char-list 'korean-ksc5601 6 6)))
	  ("Unit" .
	   (menu "KSC 5601/7:" , (make-char-list 'korean-ksc5601 7 7)))
	  ("Misc." .
	   (menu "KSC 5601/8-9:" , (make-char-list 'korean-ksc5601 8 9)))
	  ("Hiragana" .
	   (menu "KSC 5601/10:" , (make-char-list 'korean-ksc5601 10 10)))
	  ("Katakana" .
	   (menu "KSC 5601/11:" , (make-char-list 'korean-ksc5601 11 11)))
	  ("Cyrillic" .
	   (menu "KSC 5601/12:" , (make-char-list 'korean-ksc5601 12 12)))
	  ("Hangul" .
	   (menu "KSC 5601/16-40:" , (make-char-list 'korean-ksc5601 16 40)))
	  ("Hanja" .
	   (menu "KSC 5601/42-93:" , (make-char-list 'korean-ksc5601 42 93))))))

(defvar egg-sim-japanese-menu 
  `(menu "Japanese:"
	 (("JISX0201" .
	   (menu "JIS X 0201:" 
	   ,(append (make-char-list 'latin-jisx0201)
		    (make-char-list 'katakana-jisx0201))))
	  ("JISX0208" .
	   (menu "JIS X 0208:" ,(make-char-list 'japanese-jisx0208)))
	  ("JISX0212" .
	   (menu "JIS X 0212:" ,(make-char-list 'japanese-jisx0212)))
	  ("JISX0208/0212" .
	   (menu "記号入力:"
		 (("JIS入力" . japanese-jisx0208)
		  ("記号"     . 
		   (menu "記号:"     , (make-char-list 'japanese-jisx0208 1 2)))
		  ("英数字"   . 
		   (menu "英数字:"   , (make-char-list 'japanese-jisx0208 3 3)))
		  ("ひらがな" . 
		   (menu "ひらがな:" , (make-char-list 'japanese-jisx0208 4 4)))
		  ("カタカナ" . 
		   (menu "カタカナ:" , (make-char-list 'japanese-jisx0208 5 5)))
		  ("ギリシャ文字" . 
		   (menu "ギリシャ文字:" , (make-char-list 'japanese-jisx0208 6 6)))
		  ("キリル文字" . 
		   (menu "キリル文字:" , (make-char-list 'japanese-jisx0208 7 7)))
		  ("罫線" . 
		   (menu "罫線:" , (make-char-list 'japanese-jisx0208 8 8)))
			  ;;;"部首入力"  (bushyu-input)
                          ;;; "画数入力" (kakusuu-input)
		  ("第一水準" . 
		   (menu "第一水準:" , (make-char-list 'japanese-jisx0208 16 47)))
		  ("第二水準" . 
		   (menu "第二水準:" , (make-char-list 'japanese-jisx0208 48 84)))
		  ("補助漢字" . 
		   (menu "補助漢字:" , (make-char-list 'japanese-jisx0212 2 77)))))))))

(defvar egg-sim-ipa-menu
  `(menu "IPA:" ,(make-char-list 'ipa)))

(defvar egg-sisheng-menu
  `(menu "SiSheng characters" ,(make-char-list 'chinese-sisheng)))

(defvar egg-sim-code-input-menu
  `(menu "Charset:"
	 (("JISX0208" . japanese-jisx0208)
	  ("JISX0212" . japanese-jisx0212)
	  ("CNS11643-1" . chinese-cns11634-1)
	  ("CNS11643-2" . chinese-cns11634-2)
	  ("CNS11643-3" . chinese-cns11634-3)
	  ("CNS11643-4" . chinese-cns11634-4)
	  ("CNS11643-5" . chinese-cns11634-5)
	  ("CNS11643-6" . chinese-cns11634-6)
	  ("CNS11643-7" . chinese-cns11634-7)
	  ("Big5-1" . chinese-big5-1)
	  ("Big5-2" . chinese-big5-2)
	  ("GB2312" . chinese-gb2312)
	  ("KSC5601" . korean-ksc5601))))

(defvar egg-simple-input-method-menu-item-list
  `(("Code Input" . ,egg-sim-code-input-menu)
    ("Arabic"   . , egg-sim-arabic-menu)
    ("ASCII"    . , egg-sim-ascii-menu)
    ("Chinese"  . , egg-sim-chinese-menu)
    ("Cyrillic" . , egg-sim-cyrillic-menu)
    ("Greek"    . , egg-sim-greek-menu)
    ("Hebrew"   . , egg-sim-hebrew-menu)
    ("Japanese" . , egg-sim-japanese-menu)
    ("Korean"   . , egg-sim-korean-menu)
    ("Latin" . 
     (menu "Latin:"
	   (("Latin-1" . , egg-sim-latin-1-menu)
	    ("Latin-2" . , egg-sim-latin-2-menu)
	    ("Latin-3" . , egg-sim-latin-3-menu)
	    ("Latin-4" . , egg-sim-latin-4-menu)
	    ("Latin-5" . , egg-sim-latin-5-menu))))
    ("Thai/Lao" . 
     (menu "Thai/Lao:"
	   (("Thai" . , egg-sim-thai-menu)
	    ("Lao"  . , egg-sim-lao-menu))))
    ("Vietnamese" . , egg-sim-vietnamese-menu)
    ("Phonetic code" . 
     (menu "Phonetic code:"
	   (("SISHENG" . , egg-sisheng-menu)
	    ("IPA" .  , egg-sim-ipa-menu))))
    ))

(defvar egg-language-environment-alist 
  `(("ASCII"         . , egg-sim-ascii-menu)
    ("Chinese-BIG5"  . , egg-sim-chinese-big5-menu)
    ("Chinese-CNS"   . , egg-sim-chinese-cns-menu)
    ("Chinese-GB"    . , egg-sim-chinese-gb-menu)
    ("Cyrillic-ISO"  . , egg-sim-cyrillic-menu)
    ("Cyrillic-KOI8" . , egg-sim-cyrillic-menu)
    ("Cyrillic-ALT"  . , egg-sim-cyrillic-menu)
    ("Czech"         . , egg-sim-latin-2-menu)
    ("Devanagari")
    ("English"       . , egg-sim-ascii-menu)
    ("Ethiopic")
    ("German"        . , egg-sim-latin-1-menu)
    ("Greek"         . , egg-sim-greek-menu)
    ("Hebrew"        . , egg-sim-hebrew-menu)
    ("Hindi")
    ("IPA"           . , egg-sim-ipa-menu)
    ("Japanese"      . , egg-sim-japanese-menu)
    ("Korean"        . , egg-sim-korean-menu)
    ("Lao"           . , egg-sim-lao-menu)
    ("Latin-1"       . , egg-sim-latin-1-menu)
    ("Latin-2"       . , egg-sim-latin-2-menu)
    ("Latin-3"       . , egg-sim-latin-3-menu)
    ("Latin-4"       . , egg-sim-latin-4-menu)
    ("Latin-5"       . , egg-sim-latin-5-menu)
    ("Romaian"       . , egg-sim-latin-2-menu)
    ("Slovenian"     . , egg-sim-latin-2-menu)
    ("Slovak"        . , egg-sim-latin-2-menu)
    ("Thai"          . , egg-sim-thai-menu)
    ("Tibetan")
    ("Turkish"       . , egg-sim-latin-5-menu)
    ("Vietnamese"    . , egg-sim-vietnamese-menu)))

(defvar egg-simple-input-method-menu
  `(menu "Character set:" , egg-simple-input-method-menu-item-list))

;;;;###autoload
(defun egg-simple-input-method ()
  (interactive)
  (let ((result (egg-simple-input-menu)))
    (cond((stringp result)
	  (insert result))
	 ((symbolp result)
	  (egg-character-code-input result
				    (format "%s/Character Code in Hexadecimal:"
					    (charset-description result)))))))

(defun egg-simple-input-menu ()
  (let ((menu (cdr-safe (assoc current-language-environment 
			       egg-language-environment-alist))))
    (if menu
	(menudiag-select
	 `(menu "Character set:" ,(cons (cons current-language-environment
					      menu)
					egg-simple-input-method-menu-item-list)))
      (menudiag-select egg-simple-input-method-menu))))

(defun egg-character-code-input (charset prompt)
  (egg-insert-character-code-from-minibuffer charset prompt))

(defun egg-insert-character-code-from-minibuffer (charset prompt)
  (let ((str (read-from-minibuffer prompt)) val)
    (while (null (setq val (egg-read-character-code-from-string str charset)))
      (beep)
      (setq str (read-from-minibuffer prompt str)))
    (insert (make-char charset (car val) (cdr val)))))

(defun egg-hexadigit-value (ch)
  (cond((and (<= ?0 ch) (<= ch ?9))
	(- ch ?0))
       ((and (<= ?a ch) (<= ch ?f))
	(+ (- ch ?a) 10))
       ((and (<= ?A ch) (<= ch ?F))
	(+ (- ch ?A) 10))))

(defun egg-read-character-code-from-string (str charset)
  (if (and (= (length str) 4)
	   (<= 2 (egg-hexadigit-value (aref str 0)))
	   (egg-hexadigit-value (aref str 1))
	   (<= 2 (egg-hexadigit-value (aref str 2)))
	   (egg-hexadigit-value (aref str 3)))
      (let ((code1 (+ (* 16 (egg-hexadigit-value (aref str 0)))
		      (egg-hexadigit-value (aref str 1))))
	    (code2 (+ (* 16 (egg-hexadigit-value (aref str 2)))
		      (egg-hexadigit-value (aref str 3))))
	    (min (if (= (charset-chars charset) 94)
		     33 32))
	    (max (if (= (charset-chars charset) 94)
		     126 127)))
	(and (<= min code1)
	     (<= code1 max)
	     (<= min code2)
	     (<= code2 max)
	     (cons code1 code2)))))

;;;
;;;
;;;

(defun make-non-iso2022-code-table-file (name)
  (with-temp-file name
    (set-buffer-multibyte nil)
    (insert ";;; -*- coding: -*-\n\n")
    (insert " |")

    (let ((i 0))
      (while (< i 16)
	(insert (format "  %X " i))
	(setq i (1+ i))))
    (insert "\n")

    (insert "-+")
    (let ((i 0))
      (while (< i 16)
	(insert (format "----" i))
	(setq i (1+ i))))
    (insert "\n")

    (let ((i 0))
      (while (< i 16)
	(insert (format "%X|" i))
	(let ((j 0) (c i))
	  (while (< j 16)
	    (insert (format " \"%c\"" c))
	    (setq j (1+ j)
		  c (+ c 16)))
	  (insert (format "\n")))
	(setq i (1+ i))))))

(defun make-iso2022-94char-code-table-file (name)
  (with-temp-file name
    (set-buffer-multibyte nil)
    (insert ";;; -*- coding: -*-\n\n")
    (insert " |")
    (let ((i 0))
      (while (< i 16)
	(insert (format "  %X " i))
	(setq i (1+ i))))
    (insert "\n")

    (insert "-+")
    (let ((i 0))
      (while (< i 16)
	(insert (format "----" i))
	(setq i (1+ i))))
    (insert "\n")

    (let ((i 0))
      (while (< i 16)
	(insert (format "%X|" i))
	(let ((j 0) (c i))
	  (while (< j 16)
	    (if (or (<= c 31)
		    (= c 127)
		    (and (<= 128 c)
			 (<= c 160))
		    (= c 255))
		(insert "    ")
	      (insert (format " \"%c\"" c)))
	    (setq j (1+ j)
		  c (+ c 16)))
	  (insert (format "\n")))
	(setq i (1+ i))))))
  
(defun make-iso2022-96char-code-table-file (name)
  (with-temp-file name
    (set-buffer-multibyte nil)
    (insert ";;; -*- coding: -*-\n\n")
    (insert " |")
    (let ((i 0))
      (while (< i 16)
	(insert (format "  %X " i))
	(setq i (1+ i))))
    (insert "\n")

    (insert "-+")
    (let ((i 0))
      (while (< i 16)
	(insert (format "----" i))
	(setq i (1+ i))))
    (insert "\n")

    (let ((i 0))
      (while (< i 16)
	(insert (format "%X|" i))
	(let ((j 0) (c i))
	  (while (< j 16)
	    (if (or (<= c 31)
		    (= c 127)
		    (and (<= 128 c)
			 (< c 160)))
		(insert "    ")
	      (insert (format " \"%c\"" c)))
	    (setq j (1+ j)
		  c (+ c 16)))
	  (insert (format "\n")))
	(setq i (1+ i))))))

(defun make-euc-code-table-file (name)
  (with-temp-file name
    (set-buffer-multibyte nil)
    (insert ";;; -*- coding: -*-\n\n")
    (insert "  |")
    (let ((i 1))
      (while (<= i 94)
        ;;                "XX"
	(insert (format "  %02d " i))
	(setq i (1+ i))))
    (insert "\n")

    (insert "-+")
    (let ((i 1))
      (while (<= i 94)
	(insert (format "-----" i))
	(setq i (1+ i))))
    (insert "\n")

    (let ((i 1))
      (while (<= i 94)
	(insert (format "%02d|" i))
	(let ((j 1))
	  (while (<= j 94)
	    (insert (format " \"%c%c\""
			    (+ i 32 128)
			    (+ j 32 128)))
	    (setq j (1+ j)))
	  (insert (format "\n")))
	(setq i (1+ i))))))

;;; jisx0213.el --- Charset Definition for JIS X 0213

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

;; This module defines JIS X 0213 character sets if these character
;; sets are not defined.  This module also defines fixed-euc-jisx0213
;; coding systems if these coding systems are not defined and can be
;; defined.

;;; Code:

(if (not (charsetp 'japanese-jisx0213-1))
    (define-charset 151 'japanese-jisx0213-1
      [2 94 2 0 ?O 0 "JISX0213-1" "JISX0213-1" "JISX0213-1 (Japanese)"]))

(if (not (charsetp 'japanese-jisx0213-2))
    (define-charset 254 'japanese-jisx0213-2
      [2 94 2 0 ?P 0 "JISX0213-2" "JISX0213-2" "JISX0213-2 (Japanese)"]))

;; Mule-UCS is required to adopt fixed-euc-jisx0213.
(or 
 noninteractive ;; unnecessary in non-interactive mode.
 (unless (or (require 'jisx0213)
             (get 'jisx0213-to-jisx0208/0212 'translation-table))
   (message "Mule-UCS not installed."))
 (coding-system-p 'fixed-euc-jisx0213)
 (progn

   (define-ccl-program ccl-decode-fixed-euc-jisx0213
     `(2
       ((r3 = ,(charset-id 'katakana-jisx0201))
        (loop ;;      ascii kana 212 208
         (read r0) ;; r0 -   0    0    h   h
         (read r1) ;; r1 -   l    h    l   h
         (if (r0 < ?\x80)
             ((if (r1 < ?\x80)
                  (write-repeat r1))
              (write r3)
              (write-repeat r1))
           ((r0 &= 127)
            (r0 <<= 7)
            (if (r1 > ?\x80)
                ((r1 &= 127)
                 (r2 = ,(charset-id 'japanese-jisx0213-1)))
              (r2 = ,(charset-id 'japanese-jisx0213-2)))
            (r0 += r1)
            (translate-character jisx0213-to-jisx0208/0212 r2 r0)
            (write-multibyte-character r2 r0)
            (repeat)
            ))))))

   (define-ccl-program ccl-encode-fixed-euc-jisx0213
     `(2
       ((loop
         (read-multibyte-character r0 r1)
         (r6 = (r0 == ,(charset-id 'ascii))) ;G0
         (r6 |= (r0 == ,(charset-id 'latin-jisx0201)))
         (if r6
             ((write 0)
              (write-repeat r1)))
         (r6 = (r0 == ,(charset-id 'japanese-jisx0208)))
         (r6 |= (r0 == ,(charset-id 'japanese-jisx0208-1978)))
         (r6 |= (r0 == ,(charset-id 'japanese-jisx0213-1)))
         (if r6                         ;G1
             ((r2 = (r1 >> 7))
              (write (r2 | ?\x80))
              (write ((r1 & ?\x7f) | ?\x80))
              (repeat)))
         (if (r0 == ,(charset-id 'katakana-jisx0201)) ;G2
             ((write 0)
              (write (r1 | ?\x80))
              (repeat)))
         (r6 = (r0 == ,(charset-id 'japanese-jisx0212))) ;G3
         (r6 |= (r0 == ,(charset-id 'japanese-jisx0213-2)))
         (if r6
             ((r2 = (r1 >> 7))
              (write (r2 | ?\x80))
              (write (r1 & ?\x7f))
              (repeat)))
         (repeat)))))

   (make-coding-system 
    'fixed-euc-jisx0213 4 ?W "Coding System for fixed EUC Japanese"
    (cons ccl-decode-fixed-euc-jisx0213 ccl-encode-fixed-euc-jisx0213))))

(provide 'egg-x0213)

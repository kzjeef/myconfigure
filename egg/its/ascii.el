;;; its/ascii.el --- ASCII Input in Egg Input Method Architecture

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

(define-its-state-machine its-up-map
  "upcase" "aA" nil
  "Map for upcase input."

  (let ((i ? ))
    (while (<= i ?~)
      (its-defrule (char-to-string i) (upcase (char-to-string i)))
      (setq i (1+ i)))))

(define-its-state-machine-append its-up-map)

(define-its-state-machine its-down-map
  "downcase" "aa" nil
  "Map for downcase input."

  (let ((i ? ))
    (while (<= i ?~)
      (its-defrule (char-to-string i) (char-to-string i))
      (setq i (1+ i)))))

(define-its-state-machine-append its-down-map)

(provide 'its/ascii)

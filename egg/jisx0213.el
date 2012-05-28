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


;;; Code:

(unless (charsetp 'japanese-jisx0213-1)
  (define-charset 151 'japanese-jisx0213-1
    [2 94 2 0 ?O 0 "JISX0213-1" "JISX0213-1" "JISX0213-1 (Japanese)"]))

(unless (charsetp 'japanese-jisx0213-2)
  (define-charset 254 'japanese-jisx0213-2
    [2 94 2 0 ?P 0 "JISX0213-2" "JISX0213-2" "JISX0213-2 (Japanese)"]))

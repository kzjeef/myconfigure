;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;

(load! "google-c-style")
(load! "+prog")
(load! "misc")
(load! "+bindings")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jiejing Zhang"
      user-mail-address "jiejing.zjj@alibaba-inc.com")
(require 'cl)
(setq doom-font (font-spec :family "Source Code Pro" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
(defvar system-type-as-string (prin1-to-string system-type))
(defvar on_windows_nt (string-match "windows-nt" system-type-as-string)
)
(defvar on_darwin     (string-match "darwin" system-type-as-string))
(defvar on_gnu_linux  (string-match "gnu/linux" system-type-as-string))
(defvar on_cygwin     (string-match "cygwin" system-type-as-string))
(defvar on_solaris    (string-match "usg-unix-v" system-type-as-string))


(defun is-in-spacemacs()
  (boundp 'spacemacs-emacs-min-version))

(defun not-in-spacemacs()
  (not (is-in-spacemacs)))


(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))


(setq pyim-dicts
   (quote
    ((:name "greatdict" :file "~/myconfigure/input/pyim-greatdict.pyim.gz"))))


(cond (on_darwin
         (setq ccls-executable "/usr/local/bin/ccls")))

(cond (on_gnu_linux
       (setq ccls-executable "/var/lib/snapd/snap/ccls/current/bin/ccls")))

(with-eval-after-load 'org
    ;; here goes your Org config :)
    ;; ....
    (require 'ox-md nil t)

    (setq org-agenda-files '("~/Dropbox/org/"))


    (defun do-org-show-all-inline-images ()
      (interactive)
      (org-display-inline-images t t))

    (global-set-key (kbd "C-c C-x C v")
                    'do-org-show-all-inline-images)

    (cond (on_darwin
           (setq org-plantuml-jar-path
                 (expand-file-name "/usr/local/Cellar/plantuml/8037/plantuml.8037.jar"))
           ))

    )


(setq large-file-warning-threshold 100000000) ;dont' remove this line, otherwise vlf will crash.


  ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (and (equal major-mode 'c++-mode)
                   (ignore-errors
                     (save-excursion
                       (goto-char (c-langelem-pos langelem))
                       ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                       ;;   and with unclosed brace.
                       (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
              0                           ; no additional indent
            ad-do-it)))                   ; default behavior





;; common c++ config.
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                 ;(ggtags-mode 1)
;;                 ;(setq ggtags-highlight-tag nil)

;;                 ;; this make sure fly check pop up error message.
;;                 (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)

;;                 ;; 自动把focus窗口调到黄金比例
;;                 ;(spacemacs/toggle-golden-ratio-on)
;;                 (spacemacs/toggle-hungry-delete-on)
;;                 ;;(spacemacs/toggle-indent-guide-on)
;; ;;                (irony-mode  t)
;; ;;                (irony-eldoc t)   ;; 可以推算C++类型语义的类型提示， 在状态栏提示变量的类型。 ,, 这个功能会提示return 到js函数
;;                 ;; google c style.
;;                 (google-set-c-style)
;;                 (google-make-newline-indent)
;;                 (setq-default c-basic-offset 4)
;;                 (setq-default tab-width 8)

;;                 (flycheck-pos-tip-mode 1)

;;                 ;;(spacemacs/toggle-fill-column-indicator-on)
;;                 (global-set-key "\M-n" 'helm-gtags-dwim)
;;                 (global-set-key "\M-r" 'helm-gtags-find-rtag)
;;                 )))


  (global-set-key (kbd "C-c ;") 'iedit-mode)

  ;; Disable eldoc mode, which is very slow on big proj.
;  (global-eldoc-mode -1)

  (show-paren-mode t)

    (setq puml-plantuml-jar-path "/usr/local/Cellar/plantuml/8037/plantuml.8037.jar")
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))
  ;; end pinyin input setup.

  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

  ;; disable bold font effect.
  (mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal :underline nil))
   (face-list))


(setq-default evil-escape-key-sequence "jk")

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))

                                        ;(global-hl-line-mode -1) ;; enable hightlight current line.

(auto-compression-mode 1) ;; 打开压缩文件时自动解压缩

(setq visiable-bell t)	 ;; 把嘟的声音去掉
(setq ring-bell-function 'ignore)	;; 不要让那个DIDI的响

(setq transient-mark-mode nil)	 ;; 两次按C－space以后高亮显示区域

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)
(display-time-mode 1)


(setq suggest-key-bindings 1)	 ;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。

(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/emacs.bak")))
(setq backup-by-copying t)
(ansi-color-for-comint-mode-on)	 ;; 消除shell中的乱码
(fset 'yes-or-no-p 'y-or-n-p)	 ;; 把Yes或者用y代替

(when (display-graphic-p)
  (set-clipboard-coding-system 'chinese-iso-8bit) ;; 剪切板，用于和其他程序之间复制内容
  (set-clipboard-coding-system 'ctext) ;;解决firefox有时候复制文件有乱马

  (set-keyboard-coding-system 'chinese-iso-8bit) ;; 键盘输入，用于输入法。
  (set-terminal-coding-system 'chinese-iso-8bit) ;; 终端显示的编码方式。
  )
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt) ;; 密码的相关的提示密码
;;  (setq kill-emacs-query-functions
;;       (lambda() (y-or-n-p "Do you really want to quit?")))

(cond (on_darwin
       (require 'dired)

       (define-key dired-mode-map "o" 'dired-open-mac)
       (defun dired-open-mac ()
         (interactive)
         (let ((file-name (dired-get-file-for-visit)))
           (if (file-exists-p file-name)
               (shell-command (concat "open '" file-name "'" nil )))))
       ))

(cond (on_darwin
       ;; need find-file to do this
       (add-to-list 'load-path "/opt/local/share/emacs/site-lisp")

       ;; let meta key also become a command(M) key.
       (setq mac-option-modifier 'meta)
       (setq mac-control-modifier 'control)
                                        ;(setq mac-right-option-modifier nil)
       ;;(setq exec-path (append exec-path '("/opt/local/bin")) )
       (setenv "LC_ALL" "en_US.UTF-8")
       (setenv "LANG" "en_US.UTF-8")
       ;; Change control and meta key under mac, make less pain...

       ;;(exec-path-from-shell-initialize)
       ))


(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'ff-find-related-file) ;; Find header file.


(setq vc-follow-symlinks t)


(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-transformers nil)
  (setq company-show-numbers t)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-j") 'company-select-previous-or-abort))


(use-package! helm-tramp
  :config
  (setq tramp-default-method "ssh")
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  (setq helm-tramp-custom-connections '(/ssh:gtrun@10.220.170.112:/home/gtrun /ssh:test@10.220.170.113:/home/test/http.log))
  (add-hook 'helm-tramp-pre-command-hook '(lambda () ;;(global-aggressive-indent-mode 0)
                                            (projectile-mode 0)
                                            ;;(editorconfig-mode 0)
                                            ))
  (add-hook 'helm-tramp-quit-hook '(lambda () ;;(global-aggressive-indent-mode 1)
                                     (projectile-mode 1)
                                     ;;(editorconfig-mode 1)
                                     ))
  )

;; (evil-set-initial-state 'ccls-tree-mode 'emacs)



;; (set-company-backend! '(c-mode
;;                         c++-mode
;;                         ess-mode
;;                         haskell-mode
;;                         ;;emacs-lisp-mode
;;                         lisp-mode
;;                         sh-mode
;;                         php-mode
;;                         python-mode
;;                         go-mode
;;                         ruby-mode
;;                         rust-mode
;;                         js-mode
;;                         css-mode
;;                         web-mode
;;                         )
;;   '(:separate company-tabnine
;;               company-files
;;               company-yasnippet))

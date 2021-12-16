;;; ~/myconfigure/doom_config/+prog.el -*- lexical-binding: t; -*-


;; A good refeence:
;; https://github.com/ztlevi/doom-config
;;
(after! company
  (setq company-idle-delay 0.2))

 (use-package! theme-changer
   :defer t
   :config
 (setq calendar-location-name "Shanghai")
 (setq calendar-latitude +31.11)
 (setq calendar-longitude +121.29)
; (change-theme 'doom-one-light  'doom-one)
                                        ;
;      (change-theme 'doom-one 'doom-one)
 (if (display-graphic-p)
    (progn
    ;; if graphic
 (change-theme 'doom-one' doom-one)
      )
    ;; else (optional)
    (message "no color theme under console.")
;;  (change-theme 'doom-molokai 'doom-molokai)
    )
)
(setq py-python-command "python3")
(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; 关闭在 tramp 下面的自动补全
(defun company-files--connected-p (file)
  (not (file-remote-p file)))

(use-package! bazel-mode
  :defer t
  :commands bazel-mode
  :init
  (add-to-list 'auto-mode-alist '("BUILD\\(\\.bazel\\)?\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode))
  :config
  ;; disable format-all becuase it doesn't sort BUILD list variables
  (setq bazel-mode-buildifier-before-save t)
  (appendq! +format-on-save-enabled-modes '(bazel-mode)))

(add-to-list 'auto-mode-alist '("\\.inl\\'" . +cc-c-c++-objc-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . +cc-c-c++-objc-mode))

;; 精细的undo, 不然会和默认的undo行为不同.
(setq evil-want-fine-undo 't)

;; 长行换行
(set-default 'truncate-lines t)



(add-hook 'c-mode-common-hook
          (lambda()
            ;(hl-line-mode -1)
            ;(global-hl-line-mode -1)
            ;
               (setq lsp-enable-file-watchers nil)
               (setq lsp-auto-guess-root nil)

            )
          't
          )
;; will apply clang-format iff there is a .clang-format file under project.
;; will only format the modify line,  really handy.
;; too much change!!!!
;;(add-hook 'c-mode-common-hook #'clang-format+-mode)

(add-hook 'c-mode-common-hook 'google-set-c-style)

;; setup doxymacs
(add-to-list 'load-path "~/myconfigure/doxyemacs")
(require 'doxymacs)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(add-hook 'c-mode-common-hook'doxymacs-mode)



(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "build_101")
  )


(when IS-MAC
;; method0 是英文输入法，method1是中文输入法
(setq input-switch-method0 "com.apple.keylayout.ABC")
(setq input-switch-method1 "com.sogou.inputmethod.sogou.pinyin")
(setq input-switch-is-on nil)

;; 通过运行命令切换输入法
(defun input-switch-use-method (method)
  (when input-switch-is-on
    (shell-command (replace-regexp-in-string "method" method "swim use method"))))

;; 开启或关闭输入法切换
(defun input-switch-enable () (interactive) (setq input-switch-is-on t))
(defun input-switch-disable () (interactive) (setq input-switch-is-on nil))

;; 进入insert mode切换第二输入法（中文）
(add-hook 'evil-insert-state-entry-hook
          (lambda () (input-switch-use-method input-switch-method1)))
;; 退出insert mode切换第一输入法（英文）
(add-hook 'evil-insert-state-exit-hook
          (lambda () (input-switch-use-method input-switch-method0)))

;; 在org mode 下面自动打开.
(add-hook 'org-mode-hook (lambda()
                           (input-switch-enable)
                           ))

)

;(with-eval-after-load 'valign
;(setq valign-fancy-bar non-nil))


;; 让中文表格对齐.
(setq valign-fancy-bar 1)
(add-hook 'org-mode-hook 'valign-mode)


(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

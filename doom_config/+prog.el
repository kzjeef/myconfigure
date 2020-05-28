;;; ~/myconfigure/doom_config/+prog.el -*- lexical-binding: t; -*-


;; A good refeence:
;; https://github.com/ztlevi/doom-config
;;
(after! company
  (setq company-idle-delay 0.2))


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


(add-hook 'c-mode-common-hook
          (lambda()
            ;(hl-line-mode -1)
            ;(global-hl-line-mode -1)
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

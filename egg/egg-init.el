;;
;; egg-init.el
;;
;; Takuo KITAME <kitame@northeye.org>
;; ISHIKAWA Mutsumi <ishikawa@debian.org>
;;

(setq load-path
      (cons
       (concat "/usr/share/" (symbol-name debian-emacs-flavor)
              "/site-lisp/egg")
       load-path))

(setq egg-canna-helper-path "/usr/lib/egg/egg-helper")

;; (set-language-info "Japanese"    'input-method "japanese-egg-wnn")

;;; Try M-x customize-group hira

;;(set-language-info "Chinese-GB"  'input-method "chinese-gb-egg-wnn-py")
;;(set-language-info "Chinese-CNS" 'input-method "chinese-cns-egg-wnn-py")
;;(set-language-info "Korean"      'input-method "korean-egg-wnn")

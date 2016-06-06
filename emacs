  ;; -*- mode: emacs-lisp -*-
;; .emacs
;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)
;; Time-stamp:

;;________________________________________________________________
;;    Determine where we are
;;________________________________________________________________



;;
(defvar system-type-as-string (prin1-to-string system-type))

(defvar on_windows_nt (string-match "windows-nt" system-type-as-string))
(defvar on_darwin     (string-match "darwin" system-type-as-string))
(defvar on_gnu_linux  (string-match "gnu/linux" system-type-as-string))
(defvar on_cygwin     (string-match "cygwin" system-type-as-string))
(defvar on_solaris    (string-match "usg-unix-v" system-type-as-string))

(defun is-in-spacemacs()
  (boundp 'spacemacs-emacs-min-version))

(defun not-in-spacemacs()
  (not (is-in-spacemacs)))

(setq dotspacemacs-delete-orphan-packages nil)


(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(
                           ("elpa" . "http://elpa.gnu.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                                        ;			   ;;("melpa" . "http://melpa.milkbox.net/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	 编程相关的配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; wrap to some feature don't really block the boot.
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

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/doxymacs")
(add-to-list 'load-path "~/.emacs.d/site-lisp/multiple-cursors/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/ecb/")


(defun dotspacemacs/layers ()

  (setq-default
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     csv
     windows-scripts
     html
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-help-tooltip nil
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.3
                      auto-completion-private-snippets-directory t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-enable-smart-eshell t
            shell-default-term-shell (getenv "SHELL"))
     gtags
     org
     git
     javascript
     markdown
     cscope
     dash
     python
     chrome
     themes-megapack
     asm
     plantuml
     search-engine ;; M-m a /
     (c-c++ :variables
            c-c++-enable-clang-support nil
            c-c++-default-mode-for-headers 'c++-mode)
   ;; semantic ;; sematic is too slow...
    syntax-checking
    version-control
    logcat-mode
    )

   ;; disable helm-gtags because it have key conflict with ggtags-mode, which is more powerful.
   dotspacemacs-excluded-packages '(auto-complete-clang
                                    helm-gtags
                                    tern-mode
                                    adaptive-wrap)
   dotspacemacs-additional-packages '(fold-dwim
                                      irony company-irony flycheck-irony company-irony-c-headers
                                      iedit
                                      ace-jump-mode
                                      android-mode
                                      plantuml-mode
                                      js-comint
                                      )
   dotspacemacs-delete-orphan-packages nil))


(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-editing-style 'emacs
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(
                         spacemacs-dark
                         default
                         whiteboard
                         spacemacs-light
                         zenburn
                         monokai
                         tsdh-light
                         )

   dotspacemacs-colorize-cursor-according-to-state t
   ;; dotspacemacs-default-font '("Source Code Pro"
   ;;                             :size 13
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 0.8)

   dotspacemacs-default-font '("Monaco"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 0.9)

   dotspacemacs-maximized-at-startup t
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-search-tools '("ag" "grep")
   dotspacemacs-whitespace-cleanup "nil"
   ))

(defun dotspacemacs/user-init ()
  (setq gc-cons-threshold 100000000)
  )

(defun dotspacemacs/user-config ()
  ;; disable ac-mode but enable company mode.
;  (auto-complete-mode -1)
;  (global-auto-complete-mode -1)
  (global-company-mode -1)

  (spacemacs|diminish helm-gtags-mode "G" "g")

  (use-package fold-dwim)
  (global-set-key (kbd "<f7>")      'fold-dwim-toggle)
  (global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
  (global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)


  (use-package clang-format)
  (global-set-key [C-M-tab] 'clang-format-region)

  (safe-wrap (myirony-mode-setup))
  (menu-bar-mode 1)

  (add-hook 'edit-server-done-hook (lambda () (shell-command "open -a \"Google Chrome\"")))

;  (global-set-key (kbd "M-.") 'helm-gtags-dwim)
                                        ;  (global-set-key (kbd "M-,") 'pop-tag-mark)



  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)
                (setq ggtags-highlight-tag nil)
                (local-set-key "\M-." 'ggtags-find-tag-dwim)
                (global-set-key "\M-," 'tags-loop-continue)
                (local-set-key "\M-*" 'pop-tag-mark)
                )))

  (global-set-key (kbd "C-c ;") 'iedit-mode)

  (setq
     company-dabbrev-ignore-case nil
     company-dabbrev-code-ignore-case nil
     company-dabbrev-downcase nil
     company-idle-delay 0.3
     company-show-numbers t
     company-dabbrev-downcase nil
     company-dabbrev-code-everywhere t
     company-minimum-prefix-length 2
     )

  ;; Disable eldoc mode, which is very slow on big proj.
  (global-eldoc-mode -1)


    ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)

  (use-package undo-tree
    :diminish undo-tree-mode
    :config (global-undo-tree-mode))



  (defun do-org-show-all-inline-images ()
    (interactive)
    (org-display-inline-images t t))

  (global-set-key (kbd "C-c C-x C v")
                  'do-org-show-all-inline-images)

  (setq org-plantuml-jar-path
        (expand-file-name "/usr/local/Cellar/plantuml/8037/plantuml.8037.jar"))

  (spacemacs/toggle-truncate-lines-on)
  ;; Visual line navigation for textual modes
  (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-off)

  (visual-line-mode  -1)


  ;; pinyin input setup.
  ;; This pinyin dict is downlowad by:
  ;; http://tumashu.github.io/chinese-pyim-bigdict/pyim-bigdict.pyim
  (setq pyim-dicts
    (quote
     ((:name "pinyin" :file "/Users/jiejing/myconfigure/pyim-bigdict.pyim" :coding utf-8-unix :dict-type pinyin-dict))))
  ;; (require 'chinese-pyim-company)
  ;; (setq pyim-company-max-length 6) ;; seems  it was very slow.
  (setq pyim-company-complete-chinese-enable nil)

  (setq puml-plantuml-jar-path "/usr/local/Cellar/plantuml/8037/plantuml.8037.jar")
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))

  ;; end pinyin input setup.


  (add-hook 'js2-mode-hook (lambda()
                             (custom-set-variables
                              '(js2-basic-offset 2)
                              '(js2-bounce-indent-p nil)
                              (hungry-delete-mode 1)
                              )
                             (smartparens-mode 1)
                             (company-mode 1)
                             ))

  (eval-after-load 'js2-mode
    '(progn
       (define-key js2-mode-map (kbd "TAB") (lambda()
                                              (interactive)
                                              (let ((yas/fallback-behavior 'return-nil))
                                                (unless (yas/expand)
                                                  (indent-for-tab-command)
                                                  (if (looking-back "^\s*")
                                                      (back-to-indentation))))))))

  ;; disable bold font effect.
  (mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal :underline nil))
   (face-list))



  ;; do yas-setup again.
  (safe-wrap (yas-setup))


  (safe-wrap (js-comint-setup))



); end user-config;

(defun js-comint-setup()
  (require 'js-comint)
  ;; Use node as our repl
  (setq inferior-js-program-command "node")
  
  (setq inferior-js-mode-hook
        (lambda ()
          ;; We like nice colors
          (ansi-color-for-comint-mode-on)
          ;; Deal with some prompt nonsense
          (add-to-list 'comint-preoutput-filter-functions
                       (lambda (output)
                         (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                                                   (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))

  ;; Doc:
  ;; This function can send some content to REPL to see the result.
  ;; start REPL: M-x run-js
  ;; send region: M-x js-send-region
  ;; send whole buffer:  M-x js-send-buffer
  )



(defun cedet-init()
  ;; Load CEDET.
  ;; See cedet/common/cedet.info for configuration details.
  ;; IMPORTANT: Tou must place this *before* any CEDET component (including
  ;; EIEIO) gets activated by another package (Gnus, auth-source, ...).
  (load-file "~/.emacs.d/site-lisp/cedet/cedet-devel-load.el")

  ;; Add further minor-modes to be enabled by semantic-mode.
  ;; See doc-string of `semantic-default-submodes' for other things
  ;; you can use here.
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
  (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

  ;; Enable Semantic
  (semantic-mode 1)

  ;; Enable EDE (Project Management) features
  (global-ede-mode 1))


(defun ecb-init()
  (require 'ecb)
  (setq ecb-tip-of-the-day nil)
  (setq ecb-is-active nil))

(defun ace-jump-init()
  (autoload
    'ace-jump-mode
    "ace-jump-mode"
    "Emacs quick move minor mode"
    t)
  ;; you can select the key you prefer to
  (define-key global-map (kbd "C-c j") 'ace-jump-mode)
  ;;
  ;; enable a more powerful jump back function from ace jump mode
  ;;
  (autoload
    'ace-jump-mode-pop-mark
    "ace-jump-mode"
    "Ace jump back:-)"
    t)
  (eval-after-load "ace-jump-mode"
    '(ace-jump-mode-enable-mark-sync))
  (define-key global-map (kbd "C-c u") 'ace-jump-mode-pop-mark)
  )

(defun rtags-setup()
  ;; indexing operation.

  ;; rdm &   # start daemon
  ;; rc -J . # indexing
  ;; cmake ../ -DCMAKE_EXPORT_COMPILE_COMMANDS=1  it will generate compile-command.json

  (load "rtags/rtags")
  ;;  (require 'flycheck-rtags) ;; seem it will cause flycheck invalid.


  (require 'rtags)
  (rtags-enable-standard-keybindings)

  ;; in company mode.
					;  (setq rtags-autostart-diagnostics t)
  (global-set-key (kbd "M-,") 'rtags-find-references-at-point)
  (global-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
  (global-set-key (kbd "M-[") 'rtags-location-stack-back)
  (global-set-key (kbd "M-]") 'rtags-location-stack-forward)
  ;;  (rtags-imenu)

  (push 'company-rtags company-backends)
  (setq rtags-completion-enabled t)




;  (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
;  (add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)

  )

;;--------------------------------------------------------------------------------
;;                               Copmany mode init
;;--------------------------------------------------------------------------------
(defun company-mode-init()
  (global-company-mode)
  (add-hook 'after-init-hook 'global-company-mode)
 (global-set-key [backtab] 'company-complate-common) ;; [shift] + [tab]
;  (global-set-key [tab] 'tab-indent-or-complete) ;; just keep defualt, [tab] is for the yas and indent.
  (setq company-idle-delay 1) ;; don't pop compnay by timeout, default 0.7 seconds.
  (dolist (hook (list
		 'emacs-lisp-mode-hook
		 'lisp-mode-hook
		 'lisp-interaction-mode-hook
		 'scheme-mode-hook
		 'c-mode-common-hook
		 'python-mode-hook
		 'haskell-mode-hook
		 'asm-mode-hook
		 'emms-tag-editor-mode-hook
		 'sh-mode-hook))

    (add-hook hook 'company-mode))
  )




(defun flex-bison-init()
  (autoload 'flex-mode "flex-mode" nil t)
  (autoload 'bison-mode "bison-mode" nil t)
  (setq auto-mode-alist
	(cons '("\\.flex" . flex-mode) auto-mode-alist))
  (setq auto-mode-alist
	(cons '("\\.y" . bison-mode) auto-mode-alist)))

(defun ac-complete-func-init()

(defcustom mycustom-system-include-paths '("./include/" "/opt/local/include" "/usr/include" )
  "This is a list of include paths that are used by the clang auto completion."
  :group 'mycustom
  :type '(repeat directory)
  )
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/") 

(set-default 'ac-sources
             '(ac-source-abbrev
               ac-source-dictionary
               ac-source-yasnippet
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic))

(setq ac-sources (append '(ac-source-clang) ac-sources))

(ac-config-default)

(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(add-to-list 'ac-modes 'objc-mode)
;; (global-auto-complete-mode t)

(add-hook 'slime-mode-hook 'set-up-slime-ac)
 (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
 (eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'slime-repl-mode))

(dolist (m '(c-mode c++-mode java-mode objc-mode))
  (add-to-list 'ac-modes m))

;;(global-auto-complete-mode t)
)


(defun myirony-mode-setup()

  (require 'irony)

  (defun my:irony-enable()
      (irony-mode 1)
    )

  (add-hook 'c++-mode-hook 'my:irony-enable)
  (add-hook 'c-mode-hook 'my:irony-enable)
  (add-hook 'objc-mode-hook 'my:irony-enable)

  (add-hook 'irony-mode-hook
            (lambda ()
              (define-key irony-mode-map [remap completion-at-point]
                'irony-completion-at-point-async)
              (define-key irony-mode-map [remap complete-symbol]
                'irony-completion-at-point-async)))

  (spacemacs|diminish irony-mode " Ⓘ" " I")

  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-hook 'irony-mode-hook 'company-mode)

  ;; (eval-after-load 'flycheck
  ;;   '(add-to-list 'flycheck-checkers 'irony))
  (add-hook 'irony-mode-hook 'flycheck-mode))



(defun nxml-setup()
  (defun nxml-custom-keybindings ()
    (define-key nxml-mode-map "\C-c\C-c" 'nxml-complete))

  (add-hook 'nxml-mode-hook 'nxml-custom-keybindings))


(defun yas-setup()
  (require 'yasnippet)

  (setq yas-snippet-dirs '())
  (add-to-list 'yas-snippet-dirs "~/myconfigure/yasnippet-snippets")
  (add-to-list 'yas-snippet-dirs "~/myconfigure/mysnippets")


  (yas-global-mode 1)

  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  ;; Alternatively use Control-c + tab
  (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)
  )


(defun flycheck-setup()
(require 'flycheck)
(global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq flycheck-standard-error-navigation nil)


(global-set-key [\M-f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [\M-f4] 'kmacro-end-or-call-macro)

(global-set-key [f3] 'flycheck-previous-error)
(global-set-key [f4] 'flycheck-next-error)
            ;; flycheck errors on a tooltip (doesnt work on console)
;; (when (display-graphic-p (selected-frame))
;;   (eval-after-load 'flycheck
;;     '(custom-set-variables
;;       '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
(add-hook 'after-init-hook 'global-flycheck-mode))

(defun fic-mode-setup()
;;; highlight TODO, etc mode.
  (require 'fic-mode)
  (add-hook 'c++-mode-hook 'turn-on-fic-mode)
  (add-hook 'c-mode-hook 'turn-on-fic-mode)
  (add-hook 'python-mode-hook 'turn-on-fic-mode)
  (add-hook 'js2-mode-hook 'turn-on-fic-mode)
  (add-hook 'java-mode-hook 'turn-on-fic-mode)
  (add-hook 'objc-mode-hook 'turn-on-fic-mode)
  (add-hook 'ruby-mode-hook 'turn-on-fic-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-fic-mode)
  )

(defun dash-setup ()
  (autoload 'dash-at-point "dash-at-point"
    "Search the word at point with Dash." t nil)
  (global-set-key "\C-cd" 'dash-at-point)
;;  (global-set-key [f1] 'dash-at-point)
  (global-set-key "\C-ce" 'dash-at-point-with-docset))

(defun markdown-setup()
  (autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
)

(defun git-setup ()
  (featurep 'git)
  nil
  ((lambda ()
     (require 'git)
     (require 'git-blame)
     (autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)
     (require 'magit)
     (global-set-key (kbd "C-x g") 'magit-status)

     )))

(defun generic-programming-realted-config ()

					; diable doxymacs for conflict of cedet.
   (safe-wrap ((lambda ()
		 (require 'doxymacs)
		 (defun doxymacs-font-lock ()
		   (interactive)
		   (font-lock-add-keywords nil doxymacs-doxygen-keywords))
		 (doxymacs-font-lock)
		 )))
  ;; Auto enable whitespace mode in diff mode

  ;;  (global-hl-line-mode t) ;; Highlight current line, seems easier to find the cursor.

  (setq wsd-style "roundgreen")

  (hs-minor-mode)
  (global-set-key (kbd "M-+") 'hs-show-block)
  (global-set-key (kbd "M-_") 'hs-hide-block)

  (global-hi-lock-mode 1)
  ;; enable hightlight, C-x w h word <RET> <RET> to hightlight

  (add-hook 'diff-mode-hook
	    (lambda ()
	      (whitespace-mode -1)))
  ;; Remeber artist-mode can draw picutre !!!
					; (define-key c-mode-base-map [(return)] 'newline-and-indent)
					;(c-set-offset 'inextern-lang '0)
  (setq comment-multi-line t)	 ;; 大段注释的时候， 每行的开头都是*
  (c-toggle-hungry-state t)	 ;; hungry delete

;;  (require 'flyspell)
  ;;  (flyspell-prog-mode)             ;; 会对程序中的注释做拼写检查


  (setq puml-plantuml-jar-path "/Users/jiejing/myconfigure/plantuml.jar")
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))
                                        ;(hightlight-change-mode)	 ;; 会对做的修改做Hight light
  (which-func-mode t)	 ;; 在状态栏显示当前函数
  ;; (set-variable 'show-trailing-whitespace 1) ;;有多余空格的时候高亮
  ;; (add-hook 'before-save-hook 'whitespace-cleanup) ;;在保存之前清除空字符

  ;; FFAP, find the related file.
  ;; (ffap-bindings)
  ;; 设定搜索的路径 ffap-c-path
  ;; (setq ffap-c-path
  ;;     '("/usr/include" "/usr/local/include"))
  ;; 如果是新文件要确认
  ;; (setq ffap-newfile-prompt t)
  ;; ffap-kpathsea-expand-path 展开路径的深度
  ;; (setq ffap-kpathsea-depth 5)

  ;; Hide & Show minor mode, usually good when looking big source file.
					;(hs-minor-mode)
  ;;  (safe-wrap (flex-bison-init)) ; cause editor hang, remove it.

                                        ;(electric-layout-mode) ;; good control of space line.



  )
;; end generic programming config.


(defun term-init()
  (require 'multi-term)
  (setq multi-term-program "/bin/bash"))

;;; Theme setting part... Really important.

;; Auto disable theme setup before...
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

(defun reset-theme-list()
  (setq all-themes '(solarized-light adwaita wombat twilight-bright solarized-light monokai))
  (setq valid-themes all-themes))

(defun color-init()
  (reset-theme-list)
  (when (not (boundp 'current-theme))
    (looping-select-theme)))



(defun looping-select-theme()
  (interactive)
  (if valid-themes
      ((lambda()
	 (setq current-theme (car valid-themes))
	 (setq valid-themes (cdr valid-themes))
	 (load-theme current-theme t)
;	 (message "Current Theme is: %s" current-theme)
	 ))
    (reset-theme-list)
    (disable-theme current-theme)))

(defun config-in-tty-mode ()
  ;; don't load color in tty mode.
					;(color-init)
  )

(defun load-python-env()
  (require 'python-mode)
  (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
  (autoload 'python-mode "python-mode" "Python editing mode." t)

  (add-hook 'python-mode-hook (function cscope:hook))
  (add-hook 'python-mode-hook
            (lambda()
              (ac-stop) ;; ac mode 会造成输入的时候闪烁
;              (setq dash-at-point-docset "django")
	      ;; JEDI document: http://tkf.github.io/emacs-jedi/latest/#jedi:key-complete
              (jedi:setup)
              ;; jedi have a bug will not running in acquamcs.
	      (setq jedi:complete-on-dot t)
	      (highlight-indentation-current-column-mode)
	      ;; also can complete by C-TAB
	      (setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
	      (setq default-tab-width 4)))
  (add-to-list 'load-path "~/.emacs.d/site-lisp/python/")
  (setq py-install-directory "~/.emacs.d/site-lisp/python/")
  (add-to-list 'auto-mode-alist '("\\.py?$" . python-mode))
  )

(defun load-squirrel-env()
  (setq auto-mode-alist (cons '("\\.ni$" . squirrel-mode) auto-mode-alist))
  (autoload 'squirrel-mode "squirrel-mode" "Squirrel Mode." t))
  


(defun load-ruby-env()
  ;;  (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

  ;;   (add-to-list 'auto-mode-alist
  ;;               '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . enh-ruby-mode))
  ;;  (add-to-list 'auto-mode-alist
  ;;               '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))

  (add-to-list 'auto-mode-alist
               '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
  
  ;; optional
  ;;  (setq enh-ruby-program "(path-to-ruby1.9)/bin/ruby") ; so that still works if ruby points to ruby1.8

  ;; Enhanced Ruby Mode defines its own specific faces with the hook erm-define-faces. If your theme is already defining those faces, to not overwrite them, just remove the hook with:
  (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)
  (add-hook 'ruby-mode-hook
	    (lambda () (highlight-indentation-current-column-mode)))
  (add-hook 'enh-ruby-mode-hook
	    (lambda () (highlight-indentation-current-column-mode)))
  (add-hook 'ruby-mode-hook 'projectile-on)
  (add-hook 'coffee-mode-hook
	    (lambda () (highlight-indentation-current-column-mode)))
  (rvm-use-default)
  (add-hook 'coffee-mode-hook
	    (lambda () (highlight-indentation-current-column-mode)))
					;(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

  (require 'rinari)
;  (add-hook 'ruby-mode-hook(lambda() (setq dash-at-point-docset "rails")))
  (add-hook 'ruby-mode-hook 'hs-minor-mode)

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(enh-ruby-op-face ((t (:foreground "#d9045a")))))



  )

;; (load-ruby-env)

(defun android-setup()

  ;; android create project Notes:
  ;; 1. if no build.xml, create it by
  ;; $ android update project --name $(NAME) --target $(TARGET) --path ~/development/PowerShark/
  ;; 2. The target parameter can find by 
  ;; $ android list targets
  ;; 3. then you can use android mode
  (require 'android-mode)

  (setq android-mode-sdk-dir "~/sdk" )

  
;  (add-hook 'gud-mode-hook
;            (lambda ()
;	      (add-to-list 'gud-jdb-classpath "/home/gregj/work/android-sdk-linux_86/platforms/android-7/android.jar")
;;	      ))
  )


(defun load-web-env()
  (autoload 'js2-mode "js2-mode" nil t)

  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
  (add-hook 'js2-mode-hook (lambda()
			     (custom-set-variables
			      '(js2-basic-offset 2)
			      '(js2-bounce-indent-p nil)
			      )))
  ;; Css mode indent
  (add-hook 'css-mode-hook (lambda() 
			     (setq cssm-indent-function #'cssm-c-style-indenter)
			     (setq cssm-indent-level 2)))

					;  (require 'hl-tags-mode)
  
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

  (add-hook 'web-mode-hook
	    (lambda () (flyspell-prog-mode)))
  )

(defun load-java-relate-lib ()
  (generic-programming-realted-config)
  (add-hook 'java-mode-hook (function cscope:hook))
;  (cscope-minor-mode)
  (message "load java")
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
  (setq c-basic-offset 4
	tab-width 4
	indent-tabs-mode nil)
  ;;(glasses-mode nil) ;; ThisIsAVarInJava

  )

(defun load-c-relate-lib ()
  (generic-programming-realted-config)
;  (cscope-minor-mode)

  )

(defun if-in-tty()
  (if (equal (frame-parameter nil 'font) "tty")
      t
    nil))


(defun font-configing()
  (cond (on_gnu_linux
	 (if (>= (x-display-pixel-height) 1080)
	     (set-default-font "DejaVu Sans Mono-10.5")
					;       (set-default-font "Ubuntu mono-13")
					;	    (set-default-font "Lucida Console-13")
					;	    (set-default-font "Monaco-10")
	   (set-default-font "DejaVu Sans Mono-10.5")
					;	    (set-default-font "-apple-Monaco-normal-normal-normal-*-12-*-*-*-*-0-iso10646-1")
					;	    (set-default-font "Inconsolata-15")
	   )))

  (cond (on_darwin
					;(set-face-attribute 'default nil
					;		:family "Inconsolata" :height 165 :weight 'normal)

					;(set-face-attribute 'default nil
					;		:family "Ubuntu mono" :height 160 :weight 'normal)

					;	 (set-default-font "DejaVuSansMono-12")
	 ;;	 (set-default-font "AkkurantMonoMono-13")
					;	 (set-default-font "BitstreamVeraSansMono-set")
					;	 (12-default-font "-apple-Monaco-normal-normal-normal-*-12-*-*-*-*-0-iso10646-1")
;;as	 (set-default-font "Monaco-12")
	 ))
  )

(defun config-not-in-tty-mode ()
  (font-configing)
  (global-visual-line-mode t)        ;; Auto truncate line  
  (mouse-avoidance-mode 'animate)	;; 光标靠近鼠标的时候，　鼠标自己就跑了
  (setq x-select-enable-clipboard t)	;;让X的剪切板和EMACS联系起来
  (tool-bar-mode -1) ;; 不要工具按钮
  (scroll-bar-mode -1) ;; 不要缩放条
  (safe-wrap (color-init))
  )

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

(defun cscope-setup ()
					;  (print "cscope setup")
  (require 'xcscope)
;;  (setq cscope-do-not-update-database t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; start configure work here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
;;; if no cscope installed, ignore it.


;; ;; Config for Mac
(cond (on_darwin
       ;; need find-file to do this
       (add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
       (setq mac-option-key-is-meta t)
					;(setq mac-right-option-modifier nil)
       (setq exec-path (append exec-path '("/opt/local/bin")) )
       (setenv "LC_ALL" "en_US.UTF-8")
       (setenv "LANG" "en_US.UTF-8")
       ;; Change control and meta key under mac, make less pain...
       (setq mac-command-modifier 'meta)
       (setq mac-control-modifier 'control)

       (exec-path-from-shell-initialize)
       ))

(defun toggle-control-position ()
  "toggle the control position bewteen alt or contorl under mac."
  (if (eq mac-command-modifier 'meta) ((lambda()
					 (setq mac-command-modifier 'control)
					 (setq mac-control-modifier 'meta)
					 (setq mac-option-modifier 'control)
					 (setq mac-option-key-is-meta t)))
    ((lambda()
       (setq mac-command-modifier 'meta)
       (setq mac-control-modifier 'control)
       (setq mac-option-key-is-meta t))))
  )


(require 'find-file) ;; for the "cc-other-file-alist" variable
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))
(defadvice ff-get-file-name (around ff-get-file-name-framework
				    (search-dirs 
				     fname-stub 
				     &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
	      (header (match-string 2 fname-stub))
	      (fname-stub (concat framework ".framework/Headers/" header)))
	 ad-do-it))
   ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)
(setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
			      "/System/Library/Frameworks" "/Library/Frameworks"))

;;todo should enhance in c-c++
;; Objective C settings.
(add-to-list 'auto-mode-alist '("\\.m?$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))
					;(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))




(safe-wrap (flycheck-setup))
; (safe-wrap (stock-init))

(when (not-in-spacemacs)
  (message "not in spacemacs")
  (safe-wrap (company-mode-init))
  (safe-wrap (yas-setup))
  (safe-wrap (cedet-init))
  (safe-wrap (ecb-init))
  (safe-wrap (cscope-setup))
  (safe-wrap (myirony-mode-setup))
  (safe-wrap  (android-setup))
  (safe-wrap (fic-mode-setup))
  (safe-wrap (scrolling-setup))
  (safe-wrap (misc-config-no-nessary))
  (safe-wrap (load-web-env))

  ;(safe-wrap (ac-complete-func-init))

  (setq hippie-expand-try-functions-list
        '(
          ;;	senator-try-expand-sematic
          ;; try-expand-line
          ;; try-expand-line-all-buffers
          try-expand-dabbrev
          try-expand-list
          try-expand-list-all-buffers
          try-expand-dabbrev-visible
          try-expand-dabbrev-all-buffers
          ;; 	try-expand-dabbrev-from-kill ;; don't want to comple some typo words.
          try-complete-file-name
          try-complete-file-name-partially
          try-complete-lisp-symbol
          try-complete-lisp-symbol-partially
          ;;	try-expand-whole-kill
          ))


  ); end not in spacemacs config.
  
  
(safe-wrap (ace-jump-init))
(safe-wrap (nxml-setup))
(safe-wrap (dash-setup))
(safe-wrap (term-init))


(safe-wrap (git-setup))
;;(safe-wrap (load-python-env))
;;(safe-wrap (load-ruby-env))






(setq-default kill-whole-line t)	;; 在行首 C-k 时，同时删除该行。
(defalias 'qrr 'query-replace-regexp)   ;; regexp query.




;; (global-set-key [kp-insert] 'overwrite-mode) ; [Ins]
(global-set-key [f2] 'git-grep)	 ;; Git grep.
;; F3 start micro
;; F4 reply micro
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'ff-find-related-file) ;; Find header file.

(if (not-in-spacemacs)
    (global-set-key [f8] 'compile)
  (global-set-key [f8] 'projectile-compile-project))

(global-set-key [f9] 'spacemacs/jump-in-buffer)
(global-set-key [f10] 'neotree-toggle)
; (global-set-key [f12] 'org-todo-list)
;;(global-set-key [\M-f12] 'org-todo-list) ;; mac use

(global-set-key[\M-f9] 'looping-select-theme)

(global-set-key [\M-f11] 'toggle-control-position)

(global-set-key [\M-f11] 'toggle-control-position)

(global-set-key [\C-\M-f9] 'looping-alpha)

(global-set-key (kbd "M-z")  'undo-tree)  ;; undo tree is really better.
(global-set-key (kbd "M-s")  'occur)

(global-set-key (kbd "M-1")  'delete-other-windows)
(global-set-key (kbd "M-3")  'split-window-right)
(global-set-key (kbd "M-2")  'split-window-below)
(global-set-key (kbd "M-0")  'delete-window)
(global-set-key (kbd "M-o")  'other-window)

(global-set-key (kbd "M-e") 'move-end-of-line)
;; use some vi key move around...
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-j") 'next-line)

(global-set-key (kbd "M-SPC") 'set-mark-command)

;; switch frame.
(global-set-key [\M-f12] 'other-frame)

;; C-M-q will indent whole region, such as a function, or a code block. 
(global-set-key (kbd "C-h") 'help)
;; (global-set-key (kbd "M-k") 'kill-line)
(global-set-key (kbd "C-l")  'recenter-top-bottom)
;; Use these two mapping to avoid right hand small finger pinky
;; Ctrl-M is return key in default.
(global-set-key "\M-b"  'backward-char)
;;(global-set-key "\M-m"  [return])
(global-set-key "\C-q" 'backward-kill-word)


(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

					;(global-set-key "\C-xl" 'goto-line)	;; used to goto line

(put 'upcase-region 'disabled nil)	;; 打开C－x c－u把区域变成大写的功能
;; 自动补全的尝试列表
;; (global-set-key [(meta ?/)] 'hippie-expand)
;; (autoload 'senator-try-expand-sematic "senator")

(setq speedbar-use-images nil)  ;; don't use image in  speedbar.
(make-face 'speedbar-face)
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))

;(require 'smartparens-config)
;(require 'smartparens-ruby)
;(smartparens-global-mode)
;(show-smartparens-global-mode)
;;(sp-with-modes '(rhtml-mode)
;;	       (sp-local-pair "<" ">")
;;	       (sp-local-pair "<%" "%>"))

;; for object-c.
(add-hook 'objc-mode-hook
          (lambda ()
	    ;;          (message "objc modeb hook start")
            (setq cscope-do-not-update-database nil)
            (load-c-relate-lib)

	    (setq dash-at-point-docset nil)
      (turn-on-auto-revert-mode)
            (setq indent-tabs-mode nil)
	    ;;          (flymode-init)
            (c-set-style "cc-mode")
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)

	    ;;	    (highlight-80+-mode)
	    ;;	    (setq highlight-80+-columns 200) ;; hight light 100+ colums
;;            (cscope-minor-mode)
))


;; For linux kernel
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))



(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c C-g") 'go-goto-imports)
			  (local-set-key (kbd "C-c C-f") 'gofmt)
			  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (setq tab-width 4)
                          (setq indent-tabs-mode 1)))
	  


(add-hook 'c-mode-common-hook
	  (lambda ()
            (message "c common hook")
	    ;; Add kernel style
	    (c-set-offset 'inextern-lang 0)
	    ;; This cc style disable the name space indent.
	    (defconst my-cc-style
	      '("cc-mode"
		(c-offsets-alist . ((innamespace . [0])))
		(c-electric-pound-behavior     . 'alignleft)
		;; Some more cc mode's cleanup settings.
		(c-cleanup-list . (brace-else-brace
				   brace-elseif-brace
				   brace-catch-brace
				   empty-defun-braces
				   defun-close-semi
				   list-close-comma
				   scope-operator))))
            (c-add-style "my-cc-style" my-cc-style)

	    (c-add-style
	     "linux-tabs-only"
	     '("linux" (c-offsets-alist
			(arglist-cont-nonempty
			 c-lineup-gcc-asm-reg
			 c-lineup-arglist-tabs-only))
               (setq-default indent-tabs-mode t)))
	    ))

(add-hook 'c-mode-hook
	  (lambda ()
	    (load-c-relate-lib)
	    (setq c-basic-offset 4
		  tab-width 4
		  indent-tabs-mode nil)
	    (let ((filename (buffer-file-name)))
	      ;; Enable kernel mode for the appropriate files
	      (when (and filename
			 (or (string-match "linux" filename)
			     (string-match "kernel" filename)))
		;; or like this: (string-match (expand-file-name "~/src/linux-trees")
		(c-set-style "linux-tabs-only")
		;; for kernel, hightlight 80 chars more line.
		;;                 (highlight-80+-mode)
		))))


;; --------- Begin Fix enum class in c++11
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)
;; ----------- End Fix enum class in c++11

(add-hook 'c++-mode-hook
	  (lambda ()
	    (load-c-relate-lib)
      (setq comment-start "/* " comment-end   " */") ; like traditional comment style.
	    (c-set-offset 'innamespace 0)
	    (setq c-basic-offset 4
		  tab-width 4
		  indent-tabs-mode nil)
	    (c-set-style "my-cc-style")
	    ))

(add-hook 'java-mode-hook
	  (lambda ()
					;   (message "with java mode hook")
      (turn-on-auto-revert-mode) ; Auto reload file, if want to enable this global, use (global-auto-revert-mode 1)
	    (load-java-relate-lib)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(remove-hook 'find-file-hooks 'vc-find-file-hook)

(defun misc-config-no-nessary()
  (require 'ido)
  (require 'ibuffer)
                                        ;(setq ido-auto-merge-work-directories-length -1)
  (ido-mode)                             ;Ido mode really good.
  (setq ido-save-directory-list-file "~/.emacs.d/ido.last")
  (icomplete-mode t)	 ;; 给出用 M-x foo-bar-COMMAND 输入命令的提示。
  
  )

;;-------------------------------------------------------------------------------- 
;;	 日常的配置
;;--------------------------------------------------------------------------------

;; The following key-binding iconifies a window -- we disable it:
(global-unset-key "\C-x\C-z")

(set-frame-parameter nil 'alpha '(100 100))
;; following is transparency setting.
(defun toggle-transparency ()
   (interactive)
   (if (/=
        (cadr (frame-parameter nil 'alpha))
        100)
       (set-frame-parameter nil 'alpha '(100 100))
     (set-frame-parameter nil 'alpha '(90 65))))
;; (global-set-key (kbd "C-c t") 'toggle-transparency)
(global-set-key [f12] 'toggle-transparency)


;; C-x C-n invokes set-goal-column; disable it.
(global-unset-key "\C-x\C-n")

;; We use C-c C-z to invoke a shell inside emacs.
;; The benefits of running a shell from emacs (rather than in an xterm) are:
;;   - It is possible to cycle through the previous commands
;;     and edit them using the usual emacs commands
;;   - One completely avoids using the mouse
;;   - The commands are fontified, which makes it easy to identify, for example,
;;     the command line options.
;;   - There is no upper bound on the output size: Everything is saved.
;;     This of course requires caution, but has the benefit of allowing
;;     the usual search commands in the shell window.
(global-set-key "\C-c\C-z" 'eshell)

;; prevent down-arrow from adding empty lines to the bottom of the buffer
;; (which is the default behaviour)
(setq next-line-add-newlines nil)

;; Highlight the marked region.
(setq-default transient-mark-mode t)

;; We set a key-binding for this often-used command:
(global-set-key "\M-C" 'compile)

(setq mail-signature "Jiejing")

;(setq org-log-done 'time)
(setq org-log-done 'note)

(require 'cl)
;; filter not exist files, otherwise agenda mode will report error
(require 'cl)
(setq org-agenda-files (remove-if 'nil (mapcar (lambda (x)
						 (if (file-exists-p x)
						     x
						   nil))
					       (list "~/org/app.org"
						     "~/Google 云端硬盘/Nvidia Notes/nvidia_notes.org"
						     "~/org/todo.org"
						     "~/SyncDrive/Nvidia Notes/nvidia_notes.org"))))

(global-set-key "\C-ca" 'org-agenda)
(prefer-coding-system 'utf-8)
;; Navigate between windows using Alt-1, Alt-2, Shift-left, shift-up, shift-right
(windmove-default-keybindings)

;; http://www.emacswiki.org/emacs/MultiTerm



;(require 'projectile)
;(projectile-global-mode t)   ;; project mode, https://github.com/bbatsov/projectile
;(setq projectile-enable-caching nil)
;; Press Command-p for fuzzy find in project





(setq-default truncate-lines nil) ;; 自动折行
(auto-compression-mode 1) ;; 打开压缩文件时自动解压缩
(auto-image-file-mode)	 ;; 自动打开图片模式
(column-number-mode 1) ;; 显示列号
(blink-cursor-mode 1) ;; 光标不要闪烁
(show-paren-mode 1) ;; 高亮显示匹配的括号

(defalias 'list-buffers 'ibuffer)
;; (menu-bar-mode -1)	 ;; 不要 menu-bar。
;;(autoload 'big5togb-region "big5togb" "Big5 to GB2312" t)
;;【big5togb.el】ZSH 写的将 big5 文本转为 gb2312 的包。

(setq transient-mark-mode nil)	 ;; 两次按C－space以后高亮显示区域
(setq inhibit-startup-message t)	;; 不显示 Emacs 的开始画面
(setq default-major-mode 'text-mode)	;; 任意的打开一个新文件时，缺省使用 text-mode。
(setq visiable-bell t)	 ;; 把嘟的声音去掉
(setq ring-bell-function 'ignore)	;; 不要让那个DIDI的响
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)	 
;; (display-time)  ;; 在 mode-line 上显示时间。
(setq require-final-newline t)
(setq track-eol t)
(setq suggest-key-bindings 1)	 ;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq line-number-display-limit 30000);; 当行数超过一定数值，不再显示行号。
(setq kill-ring-max 200)	 ;; kill-ring 最多的记录个数。
(setq bookmark-save-flag 1)
;; 每当设置书签的时候都保存书签文件，否则只在你退出 Emacs 时保存。
					; (setq bookmark-default-file "~/.emacs.d/.emacs.bmk")
;; 缺省书签文件的路径及文件名。
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/emacs.bak")))
(setq backup-by-copying t)
;; Emacs 中，改变文件时，默认都会产生备份文件(以 ~ 结尾的文件)。可以完全去掉
;; (并不可取)，也可以制定备份的方式。这里采用的是，把所有的文件备份都放在一
;; 个固定的地方("~/var/tmp")。对于每个备份文件，保留最原始的两个版本和最新的
;; 五个版本。并且备份的时候，备份文件是复本，而不是原件。
(ansi-color-for-comint-mode-on)	 ;; 消除shell中的乱码
(fset 'yes-or-no-p 'y-or-n-p)	 ;; 把Yes或者用y代替
;; (cua-mode t)	 ;; something like CTRL-V is copy
(set-keyboard-coding-system 'chinese-iso-8bit) ;; 键盘输入，用于输入法。
(set-terminal-coding-system 'chinese-iso-8bit) ;; 终端显示的编码方式。
(set-clipboard-coding-system 'chinese-iso-8bit) ;; 剪切板，用于和其他程序之间复制内容
(set-clipboard-coding-system 'ctext) ;;解决firefox有时候复制文件有乱马

(if (eq system-type 'darwin)
    (setq ispell-dictionary "english")
  (setq ispell-dictionary "american"))
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt) ;; 密码的相关的提示密码

;; Full screen settings.
;; Needs Mac configure of full screen
(global-set-key (kbd "C-M-RET")		'toggle-fullscreen)
(global-set-key (kbd "C-M-<return>")	'toggle-fullscreen)


(setq mode-require-final-newline nil)


;; Set a visible bell function...
;(setq visible-bell nil)
;(setq ring-bell-function `(lambda ()
;                            (set-face-background 'default "DodgerBlue")
;                            (set-face-background 'default "black")))

;; remove the startup message.
(setq inhibit-splash-screen t)

;;(load "desktop")
;;(desktop-load-default)
;;(desktop-read)

(setq search_highlight t)

;; Ask question when C-x, C-c.
(setq kill-emacs-query-functions
      (lambda() (y-or-n-p "Do you really want to quit?")))

;; For daemon mode
(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (with-selected-frame frame
	      (when window-system
		(config-not-in-tty-mode)
		(config-in-tty-mode)))))

;; Non-deamon mode config
					;(if (equal (daemonp) nil)
(if (equal (if-in-tty) nil)
    (config-not-in-tty-mode)
  (config-in-tty-mode))
					;)

;; tramp is a remote file access mode, default enable.
(require 'tramp)
(setq tramp-chunksize 500) ;;; maybe help on large trunk..
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(defun sudo-edit-current-file ()
  (interactive)
  (let ((position (point)))
    (find-alternate-file
     (if (file-remote-p (buffer-file-name))
         (let ((vec (tramp-dissect-file-name (buffer-file-name))))
           (tramp-make-tramp-file-name
            "sudo"
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            (tramp-file-name-localname vec)))
       (concat "/sudo:root@localhost:" (buffer-file-name))))
    (goto-char position)))

;; copy from :https://gist.github.com/offby1/1240799
;; There's something similar (but fancier) in vc-git.el: vc-git-grep
;; -I means don't search through binary files
;; --no-color, oddly enough, is required to allow emacs to colorize the output

(defcustom git-grep-switches "--extended-regexp -I -n --ignore-case --no-color"
  "Switches to pass to `git grep'."
  :type 'string)

(defcustom git-grep-default-work-tree (expand-file-name "~/work/adtrack")
  "Top of your favorite git working tree.  \\[git-grep] will search from here if it cannot figure out where else to look."
  :type 'directory
  )

(when (require 'vc-git nil t)

  ;; Uncomment this to try out the built-in-to-Emacs function.
  ;;(defalias 'git-grep 'vc-git-grep)


  (defun git-grep (command-args)
    (interactive
     (let ((root (vc-git-root default-directory)))
       (when (not root)
         (setq root git-grep-default-work-tree)
         (message "git-grep: %s doesn't look like a git working tree; searching from %s instead" default-directory root))
       (list (read-shell-command "Run git-grep (like this): "
                                 (format (concat
                                          "cd %s && "
                                          "git --no-pager grep %s -e %s")
                                         root
                                         git-grep-switches
                                         (let ((thing (and

                                        ; don't snarf stuff from the
                                        ; buffer if we're not looking
                                        ; at a file.  Perhaps we
                                        ; should also check to see if
                                        ; the file is part of a git
                                        ; repo.
                                                       buffer-file-name
                                                       (thing-at-point 'symbol))))
                                           (or (and thing (progn
                                                            (set-text-properties 0 (length thing) nil thing)
                                                            (shell-quote-argument (regexp-quote thing))))
                                               "")))
                                 'git-grep-history))))
    (let ((grep-use-null-device nil))
      (grep command-args))))





(defun dpi  (xres yres inch)
  "return  screen dpi from resolution and inch of screen."
  (/ (sqrt (+ (expt (float xres) 2)
	      (expt (float yres) 2)))
     (float inch)))



(defun long-edge (a b)
  (sqrt (+ (* a a) (* b b))))

(defun average-ration(x y)
  (/ (abs (- y x))  (/ (+ x y) 2)))

(defun page2mb (page-number)
  "Define a function conv page number to MB"
  (/ (* page-number 4) 1024))
(put 'set-goal-column 'disabled nil)


;;;  Tips Section ;;; 

;;; M-^  -->   delete-indention: make current line connect with last line,
;;; very usuful when change multi line to one line, which I often did.

;;; hightlight-change-mode: it will height light the changes you make in whole process.

;;; M-x re-builder   --> this function can test the regex in live.

;;; M-\             --> will delete all space around

;;; save-buffers-kill-emacs’  --> without process-killing query, save all buffers and quit emacs directly.


;;; use tramp to transpsete access a file in remote.
;;; access file like this:
;;; 
;;; C-x C-f /remotehost:filename  RET (or /method:user@remotehost:filename)
;;;
;;; if you want sudo open some file, 
;;; C-x C-f /sudo::/etc/host  RET
;;;
;;; will auto download the file, and save the file...
;;; Wiki for full tricks: http://www.emacswiki.org/emacs/TrampMode

;;; Q: emacs init too slow ?
;;; A: use this command to profile:
;;;    emacs -Q -l ~/myconfigure/profile-dotemacs.el -f profile-dotemacs

;; Notes about useful tricks I should remember.
;; 1. adjust case status of just inputed words, (save the caps key.)
;;	M-- M-u : upper case just inputed words. some thing like THIS
;;	M-- M-c : Capital just input Word

;; quick moving in one buffer, use ace mode
;; C-c j ; start ace-jump mode.
;; C-c u ; pop ace jump stack, jump back.


(defun package-install-refrash-package()
  (require 'package)
  (interactive)
  (package-refresh-contents)
  (package-install 'ggtags)
;;  (package-install 'wsd-mode)
  (package-install 'dash-at-point)
  (package-install 'rinari)
  (package-install 'yasnippet)
  (package-install 'yasnippet-bundle)
  (package-install 'web-mode)
  (package-install 'js2-mode)
  ;(package-install 'multi-term)
					;  (package-install 'python-mode)
  (package-install 'jedi)
  (package-install '2048-game)
  (jedi:install-server)
  (package-install 'highlight-indentation)
  ;(package-install 'rvm)
  (package-install 'grizzl)
  (package-install 'projectile)
  (package-install 'smartparens)
  (package-install 'yaml-mode)
  (package-install 'ag)
  (package-install 'magit)
  (package-install 'markdown-mode)
;  (package-install 'doxymacs)
  
					;  (package-install 'enh-ruby-mode) ; this package will disable yasnnipe, no need.
;  (package-install 'flymake)
;  (package-install 'flymake-ruby)
  (package-install 'sr-speedbar)
					;  (package-install 'emacs-rails-reloaded)
  (package-install 'rspec-mode)
  (package-install 'yari)
  (package-install 'exec-path-from-shell)
  (package-install 'zenburn-theme)
  (package-install 'twilight-theme)
  (package-install 'twilight-bright-theme)
  (package-install 'solarized-theme)
  (package-install 'monokai-theme)
  (package-install 'company)
  (package-install 'auto-complete-nxml)

;  (package-install 'auto-complete-clang-async)
;  (package-install 'auto-complete-clang)
;  (package-install 'ac-slime)
;  (package-install 'slime)
  (package-install 'go-mode)
					;  (package-install 'ergoemacs-mode)
;  (package-install 'emacs-eclim)
  (package-install 'erlang)
;  (package-install 'stock-ticker)
  (package-install 'android-mode)
  (package-install 'highlight-symbol)
  ;;  (package-install 'squirrel-mode)
  (package-install 'let-alist)
  (package-install 'flycheck)
  (package-install 'puml-mode)

  (package-install 'cpputils-cmake)
  (package-install 'fold-dwim)
  (package-install 'android-mode)

  )



;;________________________________________________________________
;;    Scrolling
;;________________________________________________________________


;; We also map scroll wheel and trackpad events to scrolling.
;; The mouse wheel on windows generates few events.
;; Scroll by 3 unless shifted.

(defun scrolling-setup()
  (defun up-slow () (interactive) (scroll-up 1))
  (defun down-slow () (interactive) (scroll-down 1))

  (defun up-semi-slow () (interactive) (scroll-up 2))
  (defun down-semi-slow () (interactive) (scroll-down 2))

  (defun up-medium () (interactive) (scroll-up 3))
  (defun down-medium () (interactive) (scroll-down 3))



  (cond (on_gnu_linux
         (global-set-key [mouse-4] 'down-medium)
         (global-set-key [mouse-5] 'up-medium)
         ;;    Scrolling
         ;;________________________________________________________________

         ;; We also map scroll wheel and trackpad events to scrolling.
         ;; The mouse wheel on windows generates few events.
         ;; Scroll by 3 unless shifted.

         (defun up-slow () (interactive) (scroll-up 1))
         (defun down-slow () (interactive) (scroll-down 1))

         (defun up-semi-slow () (interactive) (scroll-up 2))
         (defun down-semi-slow () (interactive) (scroll-down 2))

         (defun up-medium () (interactive) (scroll-up 3))
         (defun down-medium () (interactive) (scroll-down 3)))

        (cond (on_windows_nt
               ;; xemacs won't like the following:
               (global-set-key [mouse-4] 'down-medium)
               (global-set-key [mouse-5] 'up-medium)

               (global-set-key [S-mouse-4] 'down-slow)
               (global-set-key [S-mouse-5] 'up-slow)
               ))

        ;; The trackpad on Mac OSX generates too many events.
        ;; Scroll by 1 unless shifted.
        (cond (on_darwin
               (global-set-key [mouse-4] 'down-slow)
               (global-set-key [mouse-5] 'up-slow)

               (global-set-key [S-mouse-4] 'down-medium)
               (global-set-key [S-mouse-5] 'up-medium)
               ))

        (cond (on_gnu_linux
               (global-set-key [mouse-4] 'down-medium)
               (global-set-key [mouse-5] 'up-medium)

               (global-set-key [S-mouse-4] 'down-slow)
               (global-set-key [S-mouse-5] 'up-slow)
               ))

        (defun up-fast () (interactive) (scroll-up 8))
        (defun down-fast () (interactive) (scroll-down 8))
        (global-set-key [C-mouse-4] 'down-fast)
        (global-set-key [C-mouse-5] 'up-fast)
        (global-set-key [S-mouse-4] 'down-slow)
        (global-set-key [S-mouse-5] 'up-slow)
        )

  (defun up-fast () (interactive) (scroll-up 8))
  (defun down-fast () (interactive) (scroll-down 8))
  (global-set-key [C-mouse-4] 'down-fast)
  (global-set-key [C-mouse-5] 'up-fast)

  ;; Ordinarily emacs jumps by half a page when scrolling -- reduce:
  (setq scroll-step 1)

  ;; The default value is 5, which is too fast on a MacBook or a trackpad; reduce:
  (cond (on_darwin
         (mouse-wheel-mode 1)
         (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
         (setq mouse-wheel-progressive-speed 'f)
         ))

  ;; And finally, the most useful addition to .emacs: the ability to
  ;; scroll from the keyboard (what is everyone else using!?)
  (global-set-key "m-N" 'up-semi-slow)
  (global-set-key "m-P" 'down-semi-slow)

  )

;;________________________________________________________________
;;    Some dired settings
;;________________________________________________________________
(cond (on_darwin
       (require 'dired)

       (define-key dired-mode-map "o" 'dired-open-mac)
       (defun dired-open-mac ()
	 (interactive)
	 (let ((file-name (dired-get-file-for-visit)))
	   (if (file-exists-p file-name)
	       (shell-command (concat "open '" file-name "'" nil )))))
       ))


(defun replace_const_char()
  (interactive)
  (search-forward "L\"");
  (backward-char 2)
  (insert "const_cast<WCHAR *>(")
  (search-forward "\"")
  (search-forward "\"")
  (insert ")"))

(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP.

See `flush-lines' or `keep-lines' for behavior of this command.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; Delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(large-file-warning-threshold nil)
 '(package-selected-packages
   (quote
    (js-comint tide typescript-mode logcat bison-mode mediawiki plantuml-mode chinese-pyim zonokai-theme zenburn-theme zen-and-art-theme xterm-color x86-lookup ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme quelpa pyvenv pytest pyenv-mode purple-haze-theme puml-mode professional-theme powershell popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox page-break-lines orgit organic-green-theme org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree nasm-mode naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode live-py-mode linum-relative link-hint light-soap-theme leuven-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-cscope helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md ggtags gandalf-theme fold-dwim flycheck-pos-tip flycheck-irony flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator fic-mode farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help engine-mode emmet-mode elisp-slime-nav edit-server dracula-theme django-theme disaster diff-hl define-word dash-at-point darktooth-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode company-web company-tern company-statistics company-quickhelp company-irony-c-headers company-irony company-c-headers company-anaconda column-enforce-mode colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmake-mode clues-theme clean-aindent-mode clang-format cherry-blossom-theme busybee-theme buffer-move bubbleberry-theme bracketed-paste birds-of-paradise-plus-theme bbdb-android badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme android-mode ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-mode ace-jump-helm-line ac-ispell)))
 '(pyim-dicts
   (quote
    ((:name "pinyin" :file "/Users/jiejing/myconfigure/pyim-bigdict.pyim" :coding utf-8-unix :dict-type pinyin-dict))) t)
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-cmd . "cd /Users/jiejing/project/kalaok/libDirectAudio/; ./build.sh")
     (projectile-project-compilation-cmd . "make -C mybuild -j4")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.


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



(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ruby
     yaml
     swift
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
;;     ivy // ivy really slow on long line files.
     helm
     ;; auto-completion
     ;; better-defaults
     emacs-lisp
     ;; git
     ;; markdown
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     scala
     sql
     octave
     csv
     windows-scripts
     html
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-help-tooltip nil
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.3
                      )
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
     android-logcat
     ;;    logcat-mode
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.

   dotspacemacs-additional-packages '(fold-dwim
                                        ;                                      irony company-irony flycheck-irony company-irony-c-headers
                                      iedit
                                      groovy-mode
                                      ;;                                      vlf ;
                                      ag
                                      protobuf-mode
                                      log4j-mode
                                      ace-jump-mode
                                      android-mode
                                      plantuml-mode
                                      js-comint
                                      nlinum  ;; line number mode, have perfomrnace with big file.
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   ;; disable helm-gtags because it have key conflict with ggtags-mode, which is more powerful.
   dotspacemacs-excluded-packages '(auto-complete-clang
                                    helm-gtags
                                    ws-butler
                                    tern
                                    adaptive-wrap)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         professional
                         monokai
                         spacemacs-dark
                         default
                         zenburn
                         whiteboard
                         solarized-light
                         adwaita
                         spacemacs-light
                         wombat
                         twilight-bright
                         tsdh-light
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Monaco"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 0.9)


   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
;;   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq gc-cons-threshold 100000000)

  (setq-default line-spacing 2)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (global-company-mode -1)

  (add-hook 'compilation-mode-hook (lambda() (font-lock-mode -1)))

  (setq-default dotspacemacs-line-numbers nil)


    (spacemacs|diminish helm-gtags-mode "G" "g")
  (spacemacs|diminish doxymacs-mode "☱" "☱")
  (spacemacs|diminish hide-ifdef-mode "♺" "♺")
  (spacemacs|diminish ggtags-mode "♕" "♕")
  (spacemacs|diminish flycheck-mode "☂" "☂")
  (setq ggtags-global-ignore-case t)

  (use-package fold-dwim)
  (global-set-key (kbd "<f7>")      'fold-dwim-toggle)
  (global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
  (global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)

  (global-set-key "\M-*" 'pop-tag-mark)


  (use-package clang-format)
  (global-set-key [C-M-tab] 'clang-format-region)

;  (safe-wrap (myirony-mode-setup))
  (menu-bar-mode 1)

  (setq large-file-warning-threshold 100000000) ;dont' remove this line, otherwise vlf will crash.
  ;; (require 'vlf)
  ;; (require 'vlf-setup)
  ;; (eval-after-load "vlf"
  ;;   '(define-key vlf-prefix-map "\C-xv" vlf-mode-map))


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

  ;; (defvar zenburn-override-colors-alist
  ;;   '(("zenburn-bg+05" . "#282828")
  ;;     ("zenburn-bg+1"  . "#2F2F2F")
  ;;     ("zenburn-bg+2"  . "#3F3F3F")
  ;;     ("zenburn-bg+3"  . "#4F4F4F")))
;;  (load-theme 'zenburn t)

  ;; Disable eldoc mode, which is very slow on big proj.
  (global-eldoc-mode -1)

;;  (global-nlinum-mode t)

  (global-set-key[\M-f9] 'spacemacs/cycle-spacemacs-theme)


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


    (setq puml-plantuml-jar-path "/usr/local/Cellar/plantuml/8037/plantuml.8037.jar")
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))
  ;; end pinyin input setup.

  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))


  (add-hook 'js2-mode-hook (lambda()

                              (setq js2-basic-offset 2)
                              (setq js2-bounce-indent-p nil)
                              (hungry-delete-mode 1)
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

  (setq-default evil-escape-key-sequence "jk")


  ;; do yas-setup again.
  ;;(safe-wrap (yas-setup))
  (safe-wrap (js-comint-setup))

  (safe-wrap (hide-if-0))

  (global-hl-line-mode -1)

  (auto-compression-mode 1) ;; 打开压缩文件时自动解压缩

  (setq visiable-bell t)	 ;; 把嘟的声音去掉
  (setq ring-bell-function 'ignore)	;; 不要让那个DIDI的响

  (setq transient-mark-mode nil)	 ;; 两次按C－space以后高亮显示区域


  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  (setq display-time-interval 10)

  (setq suggest-key-bindings 1)	 ;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。


  (setq kept-old-versions 2)
  (setq kept-new-versions 5)
  (setq delete-old-versions t)
  (setq backup-directory-alist '(("." . "~/emacs.bak")))
  (setq backup-by-copying t)

  (ansi-color-for-comint-mode-on)	 ;; 消除shell中的乱码

  (fset 'yes-or-no-p 'y-or-n-p)	 ;; 把Yes或者用y代替

  (set-clipboard-coding-system 'chinese-iso-8bit) ;; 剪切板，用于和其他程序之间复制内容
  (set-clipboard-coding-system 'ctext) ;;解决firefox有时候复制文件有乱马

  (set-keyboard-coding-system 'chinese-iso-8bit) ;; 键盘输入，用于输入法。
  (set-terminal-coding-system 'chinese-iso-8bit) ;; 终端显示的编码方式。


  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt) ;; 密码的相关的提示密码


  ;; Full screen settings.
  ;; Needs Mac configure of full screen
  (global-set-key (kbd "C-M-RET")		'toggle-fullscreen)
  
  (global-set-key (kbd "C-M-<return>")	'toggle-fullscreen)

  (setq kill-emacs-query-functions
        (lambda() (y-or-n-p "Do you really want to quit?")))

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



  (cond (on_darwin
         ;; need find-file to do this
         (add-to-list 'load-path "/opt/local/share/emacs/site-lisp")

         ;; let meta key also become a command(M) key.
         (setq mac-option-modifier 'meta)

                                        ;(setq mac-right-option-modifier nil)
         (setq exec-path (append exec-path '("/opt/local/bin")) )
         (setenv "LC_ALL" "en_US.UTF-8")
         (setenv "LANG" "en_US.UTF-8")
         ;; Change control and meta key under mac, make less pain...
;;         (setq mac-command-modifier 'meta)
;;         (setq mac-control-modifier 'control)

         (exec-path-from-shell-initialize)
         ))

  (global-set-key [f5] 'revert-buffer)
  (global-set-key [f6] 'ff-find-related-file) ;; Find header file.

  (global-set-key (kbd "M-1")  'delete-other-windows)
  (global-set-key (kbd "M-3")  'split-window-right)
  (global-set-key (kbd "M-2")  'split-window-below)
  (global-set-key (kbd "M-0")  'delete-window)
  (global-set-key (kbd "M-o")  'other-window)

  (setq vc-follow-symlinks t)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (protobuf-mode zonokai-theme zenburn-theme zen-and-art-theme yapfify xterm-color x86-lookup web-mode web-beautify underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode powershell plantuml-mode planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme orgit organic-green-theme org-projectile org-present org-pomodoro alert log4e gntp org-download omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme nlinum niflheim-theme nasm-mode naquadah-theme mustang-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme lush-theme livid-mode skewer-mode simple-httpd live-py-mode light-soap-theme less-css-mode json-snatcher json-reformat js2-refactor multiple-cursors js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme htmlize heroku-theme hemisu-theme helm-pydoc helm-gitignore helm-dash helm-css-scss helm-cscope xcscope helm-company helm-c-yasnippet hc-zenburn-theme gruber-darker-theme grandshell-theme gotham-theme gnuplot gmail-message-mode ham-mode html-to-markdown gitignore-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md ggtags gandalf-theme fuzzy fold-dwim flymd flycheck-pos-tip pos-tip flatui-theme flatland-theme firebelly-theme farmhouse-theme evil-magit git-commit with-editor espresso-theme eshell-z eshell-prompt-extras esh-help engine-mode emmet-mode edit-server dracula-theme django-theme disaster dash-at-point autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-c-headers company-anaconda color-theme-sanityinc-solarized coffee-mode cmake-mode clues-theme clang-format cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet apropospriate-theme anti-zenburn-theme android-mode anaconda-mode pythonic ample-zen-theme ample-theme alect-themes ag afternoon-theme ace-jump-mode ac-ispell auto-complete logview log4j-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby yaml-mode tao-theme noflet json-mode js-comint hy-mode gruvbox-theme groovy-mode gitconfig-mode ensime sbt-mode scala-mode diff-hl darktooth-theme color-theme-sanityinc-tomorrow company flycheck yasnippet markdown-mode magit magit-popup haml-mode js2-mode swift-mode elogcat logcat ws-butler winum which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy)))
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-cmd . "make -C mybuild -j4")
     (projectile-project-compilation-cmd . "cd /Users/jiejing/project/kalaok/libDirectAudio/; ./build.sh")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#000000" :background "#FFFFDD")))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (white-sand-theme symon string-inflection rebecca-theme realgud test-simple loc-changes load-relative password-generator org-brain ivy-purpose window-purpose imenu-list evil-org evil-lion editorconfig browse-at-remote elogcat logcat ws-butler winum which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#000000" :background "#FFFFDD")))))
)

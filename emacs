;;-*- mode: emacs-lisp -*-
;; .emacs
;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)
;; Time-stamp: 

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

(setq load-path
      (remove (concat "/usr/share/emacs/"
		      (substring emacs-version 0 -2) "/lisp/cedet")
	      load-path))

(setq stack-trace-on-error nil)

(defun ecb-init()
  (add-to-list 'load-path "~/.emacs.d/site-lisp/ecb/")
  (require 'ecb)
  (require 'ecb-autoloads)
  (setq ecb-tip-of-the-day nil)
  ;; set up cscope windown in ecb.
  (ecb-layout-define "my-cscope-layout" left nil
                   (ecb-set-methods-buffer)
                   (ecb-split-ver 0.5 t)
                   (other-window 1)
                   (ecb-set-history-buffer)
                   (ecb-split-ver 0.25 t)
                   (other-window 1)
                   (ecb-set-cscope-buffer))
  
  (defecb-window-dedicator ecb-set-cscope-buffer " *ECB cscope-buf*"
    (switch-to-buffer "*cscope*"))
  (setq ecb-layout-name "my-cscope-layout")
  
  ;; Disable buckets so that history buffer can display more entries
  (setq ecb-history-make-buckets 'never)
  (setq-default my-ecb-already-active nil)

  (custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
    '(ecb-options-version "2.40")
   '(ecb-layout-window-sizes (quote (("my-cscope-layout"
                                      (0.2559241706161137 . 0.4888888888888889)
                                      (0.2559241706161137 . 0.1111111111111111)))))))



(defun toggle-ecb-activate()
  (interactive)
  (if (eq my-ecb-already-active t)
      (ecb-deactivate)
    (ecb-activate))
  (if (eq my-ecb-already-active t)
      (setq my-ecb-already-active nil)
    (setq my-ecb-already-active t)))



(defun complete-func-init()
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/ac-dict")
(ac-config-default))

;; my git setup codes.
(defun cedet-configure()
; load once
 (if (featurep 'cedet)
     nil
   ((lambda ()
(load-file "~/.emacs.d/site-lisp/cedet/common/cedet.el")
(setq semanticdb-default-save-directory "~/.emacs.d/semanticdb")
(semantic-load-enable-code-helpers)
(global-set-key [(control tab)] 'semantic-ia-complete-symbol-menu) 
;(setq semanticdb-project-roots
;(list (expand-file-name "/")))
(local-set-key "." 'semantic-complete-self-insert)
(local-set-key ">" 'semantic-complete-self-insert)
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)
;;(setq sematicdb-project-roots "~/jb")
;;(global-ede-mode 1)
;;(ede-enable-generic-projects)

;; This Book Mark use F2 as navigater, F2 set/clear a bookmark, 
;; Shift F2 pervious book mark, 
;; C-F2 next bookmark
;; S-C-F2 clear all bookmark.
(enable-visual-studio-bookmarks)
))))


;;(safe-wrap (cedet-configure))
(safe-wrap (complete-func-init))
;;(safe-wrap (ecb-init))

(defun cedet-not-configure()
  (require 'semantic/analyze/refs)
  (require 'semantic/bovine/c)
  (require 'semantic/bovine/gcc)
  (require 'semantic/bovine/clang)
  (require 'semantic/ia)
  (require 'semantic/decorate/include)
  (require 'semantic/lex-spp)
  (require 'eassist)

  (add-to-list  'Info-directory-list "~/projects/cedet-git/doc/info")

;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

  (semantic-mode 1)
  (global-semantic-highlight-edits-mode (if window-system 1 -1))
;  (global-semantic-show-unmatched-syntax-mode 1)
  (semantic-complete-analyze-inline-idle t)
  (global-semantic-show-parser-state-mode 1)

;  (globad-sematicdb-minor-mode t)
  (setq semantic-idle-completions-mode t)
  (setq semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-tooltip)
  (global-semantic-idle-completions-mode t)

  (semantic-mode 1))

(defun elscreen-setup()
;;; The tabbar.
  (load "elscreen" "ElScreen" t)
  (elscreen-start)
  (global-set-key (kbd "C-c t a b e") 'elscreen-create)
  (global-set-key (kbd "C-c t a b d") 'elscreen-kill)

  (global-set-key (kbd "C-M-_") 'elscreen-previous)
  (global-set-key (kbd "C-M-+") 'elscreen-next))

(defun fic-mode-setup()
;;; highlight TODO, etc mode.
  (require 'fic-mode)
  (add-hook 'c++-mode-hook 'turn-on-fic-mode)
  (add-hook 'c-mode-hook 'turn-on-fic-mode)
  (add-hook 'java-mode-hook 'turn-on-fic-mode)
  (add-hook 'objc-mode-hook 'turn-on-fic-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-fic-mode)
)


(defun git-setup ()
 (featurep 'git)
    nil
    ((lambda ()
    (require 'git)
    (require 'git-blame)
    (autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)
    (require 'magit)
    ;; add signed off by;

    (defun signed-off-by-me ()
      (interactive)		
      (insert "Signed-off-by Zhang Jiejing \<jiejing.zhang@freescale.com\>")
      )

    (global-set-key (kbd "C-c C-s s") 'signed-off-by-me)
    )))

(defun generic-programming-realted-config ()

; diable doxymacs for conflict of cedet.  
;(safe-wrap ((lambda ()
;              (require 'doxymacs)
;	     (doxymacs-font-lock)
;	     )))

;; Auto enable whitespace mode in diff mode
(add-hook 'diff-mode-hook 
          '(lambda () 
            (whitespace-mode 1)))
;; Remeber artist-mode can draw picutre !!!
(define-key c-mode-base-map [(return)] 'newline-and-indent)
(c-set-offset 'inextern-lang '0)
(setq comment-multi-line t)	 ;; 大段注释的时候， 每行的开头都是*
(c-toggle-hungry-state t)	 ;; hungry delete
(which-func-mode t)	 ;; 在状态栏显示当前函数
  ;; (set-variable 'show-trailing-whitespace 1) ;;有多余空格的时候高亮
(font-lock-add-keywords 'python-mode
			  '(("\\&lt;\\(FIXME\\|HACK\\|XXX\\|TODO\\)" 1 font-lock-warning-face prepend)))
  ;; (add-hook 'before-save-hook 'whitespace-cleanup) ;;在保存之前清除空字符
  ;; (setq-default indent-tabs-mode t)	;; 在kernel模式下默认用table

(ffap-bindings)
;; 设定搜索的路径 ffap-c-path
;; (setq ffap-c-path
;;     '("/usr/include" "/usr/local/include"))
;; 如果是新文件要确认
(setq ffap-newfile-prompt t)
;; ffap-kpathsea-expand-path 展开路径的深度
(setq ffap-kpathsea-depth 5)


;; This cc style disable the name space indent.
(defconst my-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "cc-mode-nonamespace-indent" my-cc-style)

;; Hide & Show minor mode, usually good when looking big source file.
(hs-minor-mode)
)
;; end generic programming config.

(defun flymode-init()
" init flymode related things."

(require 'flymake)
(defvar xcode:gccver "4.2")
(defvar xcode:sdkver "5.1")
(defvar xcode:sdkpath "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/")
(defvar xcode:sdk (concat xcode:sdkpath "/SDKs/iPhoneSimulator" xcode:sdkver ".sdk"))
;(defvar flymake-objc-compiler (concat xcode:sdkpath "/usr/bin/llvm-gcc-" xcode:gccver))
(defvar flymake-objc-compiler (concat xcode:sdkpath "/usr/bin/i686-apple-darwin11-llvm-gcc-" xcode:gccver))
(defvar flymake-objc-compile-default-options (list "-Wall" "-Wextra" "-fsyntax-only" "-ObjC" "-std=c99" "-isysroot" xcode:sdk))
(defvar flymake-last-position nil)
(defvar flymake-objc-compile-options '("-I."))
(defun flymake-objc-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                    'flymake-create-temp-inplace))
         (local-file (file-relative-name
                     temp-file
                     (file-name-directory buffer-file-name))))
    (list flymake-objc-compiler (append flymake-objc-compile-default-options flymake-objc-compile-options (list local-file)))))

(add-hook 'objc-mode-hook
         (lambda ()
           (push '("\\.m$" flymake-objc-init) flymake-allowed-file-name-masks)
           (push '("\\.h$" flymake-objc-init) flymake-allowed-file-name-masks)
           (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
               (flymake-mode t))
         ))
(defun flymake-display-err-minibuffer ()
  "改行有 error 或 warinig 显示在 minibuffer"
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "下一个错误"
  (flymake-display-err-minibuffer))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "前一个错误"
  (flymake-display-err-minibuffer))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "为了将问题行自动显示到 minibuffer 中，添加 post command hook "
  (set (make-local-variable 'post-command-hook)
       (add-hook 'post-command-hook 'flymake-display-err-minibuffer)))

;; post-command-hook 与 anything.el 有冲突时使用
(define-key global-map (kbd "C-c d") 'flymake-display-err-minibuffer)
)

(defun color-init()
"init the color theme"
(require 'color-theme)
(color-theme-initialize)
;(color-theme-clarity)
;(color-theme-calm-forest)
;(color-theme-blue-mood)
(if (eq system-type 'darwin)
    (color-theme-classic)
  (color-theme-xemacs))

(defun toggle-night-color-theme ()
  "Switch to/from night color scheme."
  (interactive)
  (require 'color-theme)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (color-theme-snapshot) ; restore default (light) colors
    ;; create the snapshot if necessary
    (when (not (commandp 'color-theme-snapshot))
      (fset 'color-theme-snapshot (color-theme-make-snapshot)))
        (color-theme-clarity)))

(global-set-key [f10] 'toggle-night-color-theme)
)

(defun config-in-tty-mode ()
;; don't load color in tty mode.
;(color-init)
)


(defun load-java-relate-lib ()
(generic-programming-realted-config)
(add-hook 'java-mode-hook (function cscope:hook))
(cscope-minor-mode)
(setq-default indent-tabs-mode nil) ;; 使用空格代替tab
;;(glasses-mode nil) ;; ThisIsAVarInJava
)

(defun load-c-relate-lib ()
(generic-programming-realted-config)
(cscope-minor-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)
)

(defun if-in-tty()
(if (equal (frame-parameter nil 'font) "tty")
t
nil))

(defun config-not-in-tty-mode ()
(if (not (eq system-type 'darwin))
    ((lambda ()
;; use monaco fonts default, want to switch to Lucida, change this to nil
      (if t
	  (if (>= (x-display-pixel-width) 1920)
	      (set-default-font "Monaco-12")
	    (set-default-font "Monaco-12"))
	(if (>= (x-display-pixel-width) 1920)
	    (set-default-font "Lucida Sans Typewriter-13")
	  (set-default-font "Lucida Sans Typewriter-12"))))) nil)

(mouse-avoidance-mode 'animate)	;; 光标靠近鼠标的时候，　鼠标自己就跑了
(setq x-select-enable-clipboard t)	;;让X的剪切板和EMACS联系起来
(tool-bar-mode -1) ;; 不要工具按钮
(scroll-bar-mode -1) ;; 不要缩放条
(color-init))

(defun page2mb (page-number)
  "Define a function conv page number to MB"
 (/ (* page-number 4) 1024))
(put 'set-goal-column 'disabled nil)



(defun toggle-fullscreen-nonmac (&optional f)
      (interactive)
      (let ((current-value (frame-parameter nil 'fullscreen)))
	(set-frame-parameter nil 'fullscreen
			     (if (equal 'fullboth current-value)
				 (if (boundp 'old-fullscreen) old-fullscreen nil)
			       (progn (setq old-fullscreen current-value)
				      'fullboth)))))
;; Put cscope windows into ecb windows.
(defun ecb-cscope-window()
  (require 'ecb)
(ecb-layout-define "my-cscope-layout" left nil
(ecb-set-methods-buffer)
(ecb-split-ver 0.5 t)
(other-window 1)
;(ecb-set-history-buffer)       
;(ecb-split-ver 0.5 t)		
;(other-window 1)	
(ecb-set-cscope-buffer))
(defecb-window-dedicator ecb-set-cscope-buffer " *ECB cscope-buf*"
(switch-to-buffer "*cscope*"))
(setq ecb-layout-name "my-cscope-layout"))

(defun cscope-setup ()
;  (print "cscope setup")
  (require 'xcscope)
  (setq cscope-do-not-update-database t)
  (ecb-cscope-window)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; start configure work here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
;;; if no cscope installed, ignore it.


;; Config for Mac
(if (eq system-type 'darwin)
((lambda ()
;; 为.h文件选择合适的Mode， 根据.h文件的内容来选择是什么mode
;; need find-file to do this
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)
(setq exec-path (append exec-path '("/opt/local/bin")) )
(set-face-attribute 'default nil
                :family "Monaco" :height 130 :weight 'normal)
t
)) nil)

;; Objective C settings.
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))
;(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

(defun xcode:buildandrun ()
 (interactive)
 (do-applescript
  (format
   (concat
    "tell application \"Xcode\" to activate \r"
    "tell application \"System Events\" \r"
    "     tell process \"Xcode\" \r"
    "          key code 36 using {command down, command shift} \r"
    "    end tell \r"
    "end tell \r"
    ))))


(defun google-style()
  (require 'google-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style))

(defun cflow-configure()
  ;; For Flow graph draw
  (autoload 'cflow-mode "cflow-mode")
  (setq auto-mode-alist (append auto-mode-alist
                                '(("\\.cflow$" . cflow-mode))))

  (require 'cflow-mode)
  (defvar cmd nil nil)
  (defvar cflow-buf nil nil)
  (defvar cflow-buf-name nil nil)
  
  (defun yyc/cflow-function (function-name)
    "Get call graph of inputed function. "
                                        ;(interactive "sFunction name:\n")
    (interactive (list (car (senator-jump-interactive "Function name: "
                                                      nil nil nil))))
    (setq cmd (format "cflow  -b --main=%s %s" function-name buffer-file-name))
    (setq cflow-buf-name (format "**cflow-%s:%s**"
                                 (file-name-nondirectory buffer-file-name)
                                 function-name))
    (setq cflow-buf (get-buffer-create cflow-buf-name))
    (set-buffer cflow-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (shell-command-to-string cmd))
    (pop-to-buffer cflow-buf)
    (goto-char (point-min))
    (cflow-mode)
    )
  )

(safe-wrap (cscope-setup))
(safe-wrap (git-setup))
(safe-wrap (google-style))
(safe-wrap (elscreen-setup))
(safe-wrap (fic-mode-setup))
(safe-wrap (cflow-configure))
(setq Man-notify-method 'pushy)
(setq-default kill-whole-line t)	;; 在行首 C-k 时，同时删除该行。
(defalias 'qrr 'query-replace-regexp)   ;; regexp query.

(global-set-key [(f1)] (lambda() 
                 (interactive) 
                 (let ((woman-topic-at-point t))
                 (woman))))
(global-set-key [f5] 'revert-buffer)	;; 恢复文件
(global-set-key [f6] 'ff-find-related-file) ;; 找到对应的头文件
(global-set-key [f7] 'grep-find)
(global-set-key [f8] 'compile)	 ;; 在 Emacs 中编译
(global-set-key [f9] 'gdb)	 ;; 在 Emacs 中调试
(global-set-key [f12] 'toggle-ecb-activate)
;(global-set-key [f12] 'todo-show)
(global-set-key "\C-z" 'undo)	 ;; 撤销命令
;(global-set-key "\C-xl" 'goto-line)	;; used to goto line
(global-set-key "\C-xj" [?\C-x ?b return]) ;; 跳到前一个buffer
(put 'upcase-region 'disabled nil)	;; 打开C－x c－u把区域变成大写的功能
;; 自动补全的尝试列表
;; (global-set-key [(meta ?/)] 'hippie-expand)
;(autoload 'senator-try-expand-sematic "senator")
(setq hippie-expand-try-functions-list
'(
senator-try-expand-sematic
;; try-expand-line
;; try-expand-line-all-buffers
try-expand-list
try-expand-list-all-buffers
try-expand-dabbrev
try-expand-dabbrev-visible
try-expand-dabbrev-all-buffers
try-expand-dabbrev-from-kill
try-complete-file-name
try-complete-file-name-partially
try-complete-lisp-symbol
try-complete-lisp-symbol-partially
;;	try-expand-whole-kill
))

;; for object-c.
(add-hook 'objc-mode-hook
'(lambda ()
;   (message "objc modeb hook start")
   (setq cscope-do-not-update-database nil)
   (load-c-relate-lib)
   (setq-default indent-tabs-mode nil) ;; 不用table
;;   (glasses-mode nil) ;; ThisIsAVarInJava
   (c-set-style "cc-mode")
   (define-key objc-mode-map (kbd "C-c C-r") 'xcode:buildandrun)
;;   (flymode-init)
   ))

;; lazy evaluate accelerate boot speed
(add-hook 'c-mode-hook
'(lambda ()
;;(setq commento-style 'mutil-line)
; (message "with c mode hook")
 (load-c-relate-lib)
 (setq-default indent-tabs-mode nil)
 (c-set-style "linux")
; (message "c mode finished")
 (setq c-mode-hook-loaded t)
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

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-set-offset 'inextern-lang 0)
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                          (string-match "kernel_imx" filename))
;; or like this: (string-match (expand-file-name "~/src/linux-trees")
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))


(add-hook 'c++-mode-hook
'(lambda ()
;   (message "with cpp mode hook")
;;	(setq comment-style 'mutil-line)
(load-c-relate-lib)
(setq-default indent-tabs-mode nil) ;; 在kernel模式下默认用table
(c-set-style "cc-mode-nonamespace-indent")))

(add-hook 'java-mode-hook
'(lambda ()
;   (message "with java mode hook")
   (load-java-relate-lib)))

(remove-hook 'find-file-hooks 'vc-find-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	 日常的配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mail-signature "
--
Zhang Jiejing")

(setq-default truncate-lines nil) ;; 自动折行
(auto-compression-mode 1) ;; 打开压缩文件时自动解压缩
(auto-image-file-mode)	 ;; 自动打开图片模式
(column-number-mode 1) ;; 显示列号
(blink-cursor-mode -1) ;; 光标不要闪烁
(show-paren-mode 1) ;; 高亮显示匹配的括号
(icomplete-mode t)	 ;; 给出用 M-x foo-bar-COMMAND 输入命令的提示。
;(iswitchb-mode t)	 ;; buffer切换的时候有字符匹配
;; (menu-bar-mode -1)	 ;; 不要 menu-bar。
;;(autoload 'big5togb-region "big5togb" "Big5 to GB2312" t)
;;【big5togb.el】ZSH 写的将 big5 文本转为 gb2312 的包。
(global-font-lock-mode t)	 ;; open font lock
(setq font-lock-maximum-decoration t)
(setq font-lock-verbose t)
(setq font-lock-maximum-size '((t . 1048576)
(vm-mode . 5250000)))
(setq-default line-spacing 1)	 ;; 行距设置， 中文行距更大一些

(setq transient-mark-mode nil)	 ;; 两次按C－space以后高亮显示区域
(setq inhibit-startup-message t)	;; 不显示 Emacs 的开始画面
(setq default-major-mode 'text-mode)	;; 任意的打开一个新文件时，缺省使用 text-mode。
(setq visiable-bell t)	 ;; 把嘟的声音去掉
(setq ring-bell-function 'ignore)	;; 不要让那个DIDI的响
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)	 ;; 在 mode-line 上显示时间。
(setq require-final-newline t)
(setq track-eol t)
(setq suggest-key-bindings 1)	 ;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq line-number-display-limit 100000000);; 当行数超过一定数值，不再显示行号。
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
(set-language-environment "UTF-8")
(setq ispell-dictionary "en")
(add-hook 'comint-output-filter-functions
'comint-watch-for-password-prompt) ;; 密码的相关的提示密码

(safe-wrap (display-battery-mode)) ;; 显示电池容量， 全屏的时候很需要

;; Full screen settings.
(if (eq system-type 'darwin)
    ;; Needs Mac configure of full screen
    ((lambda ()
       (global-set-key (kbd "C-M-RET")  'ns-toggle-fullscreen)
       (global-set-key (kbd "C-M-<return>") 'ns-toggle-fullscreen)))
    ((lambda () 
      (global-set-key (kbd "C-M-RET") 'toggle-fullscreen-nonmac)
      (global-set-key (kbd "C-M-<return>") 'toggle-fullscreen-nonmac))))


;;(load "desktop")
;;(desktop-load-default)
;;(desktop-read)

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

;; 开启服务器模式
;(server-force-delete)
;(server-start)
;; 用Daemon替代


;; Start useful functions.

(defun dpi  (xres yres inch)
  "return  screen dpi from resolution and inch of screen."
   (/ (sqrt (+ (expt (float xres) 2)
               (expt (float yres) 2)))
      (float inch)))



(defun long-edge (a b)
  (sqrt (+ (* a a) (* b b))))

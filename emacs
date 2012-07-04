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

;; my git setup codes.
(defun cedet-configure()
; load once
 (if (featurep 'cedet)
     nil
   ((lambda ()
   (load-file "~/.emacs.d/site-lisp/cedet-1.1/common/cedet.el")
   (setq sematicdb-project-roots
	 (list
	  (expand-file-name "/")))
   ;; enable source code folding
   (global-semantic-tag-folding-mode 1)))))

(defun git-setup ()
 (featurep 'git)
    nil
    ((lambda ()
    (require 'git)
    (require 'git-blame)
    (autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)
    (require 'magit)
    ;; add signed off by;

    (defun signed-off-by-me n()
      (interactive)		
      (insert "Signed-off-by Zhang Jiejing \<jiejing.zhang@freescale.com\>")
      )

    (global-set-key (kbd "C-c s") 'signed-off-by-me)
    )))

(defun cedet-configure()
; load once
 (if (featurep 'cedet)
     nil
   ((lambda ()
   (load-file "~/.emacs.d/site-lisp/cedet-1.1/common/cedet.el")
   (setq sematicdb-project-roots
	 (list
	  (expand-file-name "/")))
   ;; enable source code folding
   (global-semantic-tag-folding-mode 1)))))


(defun generic-programming-realted-config ()
(safe-wrap ((lambda () (require 'doxymacs)
	     (doxymacs-font-lock))))

;; Remeber artist-mode can draw picutre !!!
(define-key c-mode-base-map [(return)] 'newline-and-indent)
(setq comment-multi-line t)	 ;; 大段注释的时候， 每行的开头都是*
(c-toggle-hungry-state t)	 ;; hungry delete
(which-func-mode t)	 ;; 在状态栏显示当前函数
(safe-wrap (cedet-configure))
  ;; (set-variable 'show-trailing-whitespace 1) ;;有多余空格的时候高亮
(font-lock-add-keywords 'python-mode
			  '(("\\&lt;\\(FIXME\\|HACK\\|XXX\\|TODO\\)" 1 font-lock-warning-face prepend)))
  ;; (add-hook 'before-save-hook 'whitespace-cleanup) ;;在保存之前清除空字符
  ;; (setq-default indent-tabs-mode t)	;; 在kernel模式下默认用table
)

(defun color-init()
"init the color theme"
(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)
;(color-theme-calm-forest)

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
(glasses-mode t) ;; ThisIsAVarInJava
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

(defun cscope-setup ()
  (print "cscope setup")
  (require 'xcscope)
  (setq cscope-do-not-update-database t))
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

(safe-wrap (cscope-setup))
(safe-wrap (git-setup))
(setq Man-notify-method 'pushy)
(setq-default kill-whole-line t)	;; 在行首 C-k 时，同时删除该行。

(global-set-key [(f1)] (lambda() 
                 (interactive) 
                 (let ((woman-topic-at-point t))
                 (woman))))
(global-set-key [f5] 'revert-buffer)	;; 恢复文件
(global-set-key [f6] 'ff-find-related-file) ;; 找到对应的头文件
(global-set-key [f7] 'grep-find)
(global-set-key [f8] 'compile)	 ;; 在 Emacs 中编译
(global-set-key [f9] 'gdb)	 ;; 在 Emacs 中调试
(global-set-key [f12] 'speedbar)
;(global-set-key [f12] 'todo-show)
(global-set-key "\C-z" 'undo)	 ;; 撤销命令
;(global-set-key "\C-xl" 'goto-line)	;; used to goto line
(global-set-key "\C-xj" [?\C-x ?b return]) ;; 跳到前一个buffer
(put 'upcase-region 'disabled nil)	;; 打开C－x c－u把区域变成大写的功能
;; 自动补全的尝试列表
;; (global-set-key [(meta ?/)] 'hippie-expand)
(autoload 'senator-try-expand-sematic "senator")
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
   (message "objc mode hook start")
   (setq cscope-do-not-update-database nil)
   (load-c-relate-lib)
   (setq-default indent-tabs-mode nil) ;; 不用table
   (glasses-mode t) ;; ThisIsAVarInJava
   (c-set-style "cc-mode")
   (message "objc mode hook finish")))

;; lazy evaluate accelerate boot speed
(add-hook 'c-mode-hook
'(lambda ()
;;(setq commento-style 'mutil-line)
(message "with c mode hook")
(load-c-relate-lib)
(setq-default indent-tabs-mode t) ;; 在kernel模式下默认用table
(c-set-style "linux")
(message "c mode finished")
))

(add-hook 'c++-mode-hook
'(lambda ()
   (message "with cpp mode hook")
;;	(setq comment-style 'mutil-line)
(load-c-relate-lib)
(setq-default indent-tabs-mode nil) ;; 在kernel模式下默认用table
(c-set-style "cc-mode")))

(add-hook 'java-mode-hook
'(lambda ()
   (message "with java mode hook")
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
(setq line-number-display-limit 1000000);; 当行数超过一定数值，不再显示行号。
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
(display-battery-mode) ;; 显示电池容量， 全屏的时候很需要

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





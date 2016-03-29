;;; stock-ticker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "stock-ticker" "stock-ticker.el" (21747 63707
;;;;;;  0 0))
;;; Generated autoloads from stock-ticker.el

(let ((loads (get 'stock-ticker 'custom-loads))) (if (member '"stock-ticker" loads) nil (put 'stock-ticker 'custom-loads (cons '"stock-ticker" loads))))

(defvar stock-ticker-symbols '("^gspc" "^dji" "^ixic" "^tnx" "^nya" "XAUUSD=X" "EURUSD=X") "\
List of ticker symbols that the mode line will cycle through.")

(custom-autoload 'stock-ticker-symbols "stock-ticker" t)

(defvar stock-ticker-update-interval 300 "\
Number of seconds between rest calls to fetch data.")

(custom-autoload 'stock-ticker-update-interval "stock-ticker" t)

(defvar stock-ticker-display-interval 10 "\
Number of seconds between refreshing the mode line.")

(custom-autoload 'stock-ticker-display-interval "stock-ticker" t)

(defvar stock-ticker-global-mode nil "\
Non-nil if Stock-Ticker-Global mode is enabled.
See the command `stock-ticker-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `stock-ticker-global-mode'.")

(custom-autoload 'stock-ticker-global-mode "stock-ticker" nil)

(autoload 'stock-ticker-global-mode "stock-ticker" "\
Add stock ticker info to the mode line.

Enabeling stock ticker global mode will add stock information in the form
SYMBOL: PRICE CHANGE (PERCENT CHANGE) to the mode line for each stock symbol
listed in 'stock-ticker-symbols'. Only one symbol is displayed at a time and
the mode cycles through the requested symbols at a configurable interval.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; stock-ticker-autoloads.el ends here

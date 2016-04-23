(setq load-path (cons "/home/edison/apps/emacs" load-path))
(setq load-path (cons "/usr/local/lib/emacs/site-lisp/subversion" load-path))
(setq load-path (cons "/usr/local/lib/emacs/site-lisp/" load-path))

;(require 'wdired)
;(autoload 'wdired-change-to-wdired-mode "wdired")

;; (if (not window-system)
;;     (progn
;;       (setq baud-rate 38400)
;;       ;; Fix some colors in non graphical terminals:
;;       ; (set-face-foreground 'font-lock-comment-face "red")
;;       )
;;   )
(if window-system
    (progn
;;       (setq baud-rate 153600) ;; Seems to speed up things?
;       (set-foreground-color "black")
;       (set-background-color "white")
      (set-default-font "6x10")
      (set-border-color "green")
      (set-cursor-color "red")
      (set-mouse-color "NavyBlue")
      (split-window-horizontally)
      (set-frame-size (selected-frame) 166 58)
      )
  (mwheel-install)
  )
;; remove the menu and the toolbar (not needed for vip):
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(auto-fill-mode -1)
(server-start)
(display-time)

(defun tiny-size ()
  "Change to tiny font size"
  (interactive)
  (progn
    (set-default-font "5x7")
    )
  )
(defun small-size ()
  "Change to small font size"
  (interactive)
  (progn
    (set-default-font "6x10")
    )
  )
(defun mini-size ()
  "Change to mini font size"
  (interactive)
  (progn
    (set-default-font "8x13")
    )
  )
(defun normal-size ()
  "Change to normal font size"
  (interactive)
  (progn
    (set-default-font "9x15")
    )
  )
(defun big-size ()
  "Change to big font size"
  (interactive)
  (progn
    (set-default-font "10x20")
;    (set-frame-size (selected-frame) 80 35)
    )
  )
(defun huge-size ()
  "Change to huge font size"
  (interactive)
  (progn
    (set-default-font "12x24")
;    (set-frame-size (selected-frame) 80 35)
    )
  )

(defun goto-previous-window ()
  "Select the window above or to the left of the window now selected.
>From the upper left corner window, select the one at the lower right."
  (interactive)
  (select-window (previous-window)))

(global-set-key "\C-xp" 'goto-previous-window)

;; (load-file "/home/edison/.emacs.d/plindent.el")

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(load "/home/edison/apps/emacs/tagging.el")

(tagging-set-mode-params
 'ciao-mode 
 '((padding-char . ?-) 
   (left-delim . "{")
   (right-delim . "}-")
   (tagline-indicator . "^\\(%\\s-*\\)*>+-")
   (implicit . ( ( "^:-\\s-*module"
		   "module" "decl" "use" "import" "export")
		 ( "^:-\\s-*export"
		   "export" "decl" )
		 ( "^:-\\s-*use_module"
		   "use" "decl")
		 ( "^:-\\s-*use_package"
		   "use" "decl")
		 ( "^:-\\s-*regtype"
		   "regtype" "decl" )
		 ( "^:-\\s-*comment"
		   "comment" "doc")
		 ( "^:-\\s-*doc"
		   "comment" "doc")
		 ( "^:-\\s-*[a-z][a-zA-Z0-9_]*" 
		   "decl")
		 ( "^[a-z]" 
		   "pred" "code" "def" )
		 ( "^'" 
		   "pred" "code" "def" )
		 )
	     )))

(tagging-set-mode-params
 'latex-mode 
 '((padding-char . ?-) 
   (left-delim . "~={")
   (right-delim . "}=~")
   (tagline-indicator . "^\\(%\\s-*\\)*!+")))

(tagging-set-mode-params
 'emacs-lisp-mode
 '((padding-char . ?=)
   (left-delim . "(")
   (right-delim . ")===")
   (tagline-indicator . "^\\(;\\s-*\\)*>")))

; (zone-leave-me-alone)

(add-hook 'LaTeX-mode-hook 'tagging-minor-mode)
(add-hook 'ciao-mode-hook 'tagging-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'tagging-minor-mode)

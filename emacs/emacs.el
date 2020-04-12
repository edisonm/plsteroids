
(setq-default indent-tabs-mode nil)
; (setq load-path (cons "/home/edison/apps/emacs" load-path))
; (setq load-path (cons "/usr/local/lib/emacs/site-lisp/subversion" load-path))
; (setq load-path (cons "/usr/local/lib/emacs/site-lisp/" load-path))

(if (file-exists-p "/home/edison/apps/plsteroids/emacs/lookatmessage.el")
    (load-file "/home/edison/apps/plsteroids/emacs/lookatmessage.el")
  )

;(require 'wdired)
;(autoload 'wdired-change-to-wdired-mode "wdired")

;; (if (not window-system)
;;     (progn
;;       (setq baud-rate 38400)
;;       ;; Fix some colors in non graphical terminals:
;;       ; (set-face-foreground 'font-lock-comment-face "red")
;;       )
;;   )
(if (display-graphic-p)
    (progn
;;     (setq baud-rate 153600) ;; Seems to speed up things?
;      (set-foreground-color "black")
;      (set-background-color "white")
;      (set-default-font "6x10")
      (set-border-color "green")
      (set-cursor-color "red")
      (set-mouse-color "NavyBlue")
      (add-to-list 'default-frame-alist '(width . 166))
      (add-to-list 'default-frame-alist '(height . 77))
      (split-window-horizontally)
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

(defun goto-previous-window ()
  "Select the window above or to the left of the window now selected.
>From the upper left corner window, select the one at the lower right."
  (interactive)
  (select-window (previous-window)))

(global-set-key "\C-xp" 'goto-previous-window)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Fixes in emacs colors, they are ugly and different in command-line and
;; graphic versions without justified reason:

(defvar myred "Red color")


(if (not window-system) 
    (progn
      (setq myred "red")
      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(comint-highlight-prompt ((t (:inherit minibuffer-prompt :foreground "brightyellow"))))
       '(custom-variable-tag ((t (:foreground "brightyellow" :weight bold))))
       '(diff-added ((t (:inherit diff-changed :background "color-237" :foreground "color-229"))))
       '(diff-file-header ((t (:background "color-235" :weight bold))))
       '(diff-header ((t (:background "color-235"))))
       '(diff-removed ((t (:inherit diff-changed :background "color-237" :foreground "color-225"))))
       '(font-lock-builtin-face ((t (:foreground "color-45"))))
       '(font-lock-comment-face ((t (:foreground "color-243"))))
       '(font-lock-function-name-face ((t (:foreground "brightcyan"))))
       '(font-lock-string-face ((t (:foreground "brightcyan"))))
       '(font-lock-variable-name-face ((t (:foreground "brightyellow"))))
       '(minibuffer-prompt ((t (:foreground "brightyellow"))))
       )
      )
  
  (progn
    (setq myred "DarkRed")
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     ; '(background "blue")
     '(comint-highlight-prompt ((t (:inherit minibuffer-prompt :foreground "yellow"))))
     '(custom-variable-tag ((t (:foreground "yellow" :weight bold))))
     '(diff-added ((t (:inherit diff-changed :background "gray23" :foreground "yellow1"))))
     '(diff-file-header ((t (:background "DimGray" :weight bold))))
     '(diff-header ((t (:background "DimGray"))))
     '(diff-removed ((t (:inherit diff-changed :background "gray23" :foreground "wheat"))))
     '(font-lock-builtin-face ((t (:foreground "LightSeaGreen"))))
     '(font-lock-comment-face ((t (:foreground "gray47"))))
     '(font-lock-function-name-face ((t (:foreground "cyan"))))
     '(font-lock-string-face ((t (:foreground "cyan"))))
     '(font-lock-variable-name-face ((t (:foreground "yellow"))))
     '(minibuffer-prompt ((t (:foreground "yellow"))))
     ;; '(ciao-face-comment ((t (:foreground "gray50"))))
     ;; '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
     ;; '(font-lock-doc-string-face ((t (:foreground "green2"))))
     ;; '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
     ;; '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
     ;; '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
     ;; '(font-lock-type-face ((t (:foreground "#9290ff"))))
     ;; '(font-lock-warning-face ((((class color) (background dark)) (:foreground "yellow" :background "red"))))
     ;; '(highlight ((t (:background "CornflowerBlue"))))
     ;; '(list-mode-item-selected ((t (:background "gold"))))
     ;; '(makefile-space-face ((t (:background "wheat"))) t)
     ;; '(mode-line ((t (:background "green"))))
     ;; '(paren-match ((t (:background "darkseagreen4"))))
     ;; '(region ((t (:background "DarkSlateBlue"))))
     ;; '(show-paren-match ((t (:foreground "black" :background "wheat"))))
     ;; '(show-paren-mismatch ((((class color)) (:foreground "white" :background "red"))))
     ;; '(speedbar-button-face ((((class color) (background dark)) (:foreground "green4"))))
     ;; '(speedbar-directory-face ((((class color) (background dark)) (:foreground "khaki"))))
     ;; '(speedbar-file-face ((((class color) (background dark)) (:foreground "cyan"))))
     ;; '(speedbar-tag-face ((((class color) (background dark)) (:foreground "Springgreen"))))
     ;; '(vhdl-speedbar-architecture-selected-face ((((class color) (background dark)) (:underline t :foreground "Blue"))))
     ;; '(vhdl-speedbar-entity-face ((((class color) (background dark)) (:foreground "darkGreen"))))
     ;; '(vhdl-speedbar-entity-selected-face ((((class color) (background dark)) (:underline t :foreground "darkGreen"))))
     ;; '(vhdl-speedbar-package-face ((((class color) (background dark)) (:foreground "black"))))
     ;; '(vhdl-speedbar-package-selected-face ((((class color) (background dark)) (:underline t :foreground "black"))))
     ;; '(widget-field ((((class grayscale color) (background light)) (:background "DarkBlue"))))
     )
    )
  )

; (zone-leave-me-alone)

;(add-hook 'LaTeX-mode-hook 'tagging-minor-mode)
;(add-hook 'ciao-mode-hook 'tagging-minor-mode)
;(add-hook 'emacs-lisp-mode-hook 'tagging-minor-mode)

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
    )
  )
(defun huge-size ()
  "Change to huge font size"
  (interactive)
  (progn
    ;(set-default-font "12x24")
     (set-default-font "-adobe-courier-medium-r-normal--34-*-100-100-m-200-iso8859-1")
    )
  )


(if (string= "root" (getenv "USER"))
    ;; run-at-time is a kludge to fix a problem with non-x execution (-nw)
    (run-at-time "0.1 sec" nil #'set-background-color myred)
  (set-background-color "black")
  )

(set-foreground-color "white")
; (message (number-to-string (display-pixel-width)))
;; (if (< (display-pixel-width) 3000)
;;     (set-default-font "-adobe-courier-medium-r-normal--14-*-100-100-m-90-iso8859-1")
;;   (set-default-font "-adobe-courier-medium-r-normal--20-*-100-100-m-150-iso8859-1")
;;   )

; Just one character comment, otherwise in SWI it will be confused with
; predicate documentation:
; (setq comment-add 0)
(add-hook 'prolog-mode-hook (lambda () (setq-local comment-add 0)))
(xterm-mouse-mode)

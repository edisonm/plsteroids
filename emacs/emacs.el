(setq-default indent-tabs-mode nil)

(if (file-exists-p "/home/edison/apps/plsteroids/emacs/lookatmessage.el")
    (load-file "/home/edison/apps/plsteroids/emacs/lookatmessage.el")
  )

(if (display-graphic-p)
    (progn
      (set-border-color "green")
      (set-cursor-color "red")
      (set-mouse-color "NavyBlue")
      (add-to-list 'default-frame-alist '(width . 166))
      (add-to-list 'default-frame-alist '(height . 64))
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

; Just one character comment, otherwise in SWI it will be confused with
; predicate documentation:
; (setq comment-add 0)
(add-hook 'prolog-mode-hook (lambda () (setq-local comment-add 0)))
(set-background-color "black")

; global preferences - Edison Mera
; sudo cp 60mypreferences.el /etc/emacs/site-start.d/

(defvar myred "Red color")

(if (not window-system) 
    (progn
      (setq myred "color-52")
      (setq myblack "color-16")
      )
  
  (progn
    (setq myred "DarkRed")
    (setq myblack "black")
    )
  )

(run-at-time "0.1 sec" nil #'set-foreground-color "white")

(if (string= "root" (getenv "USER"))
    ;; run-at-time is a kludge to fix a race condition with -nw option
    (run-at-time "0.2 sec" nil #'set-background-color myred)
  (run-at-time "0.2 sec" nil #'set-background-color myblack)
  )

(xterm-mouse-mode)

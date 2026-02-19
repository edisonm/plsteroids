;; Author: Edison Mera
;; Hook to point at the given ERROR using one keystroke

(defun look-at-message--base-directory (buf)
  "Directory used to resolve relative file names for BUF."
  (with-current-buffer buf
    (or (and buffer-file-name (file-name-directory buffer-file-name))
        default-directory)))

(defun look-at-message (origin-buffer &optional cd-to-base-dir)
  "Open the file at point and jump to line/column.
If file name is relative, resolve it against ORIGIN-BUFFER and also set
the visited buffer's `default-directory' to that base directory."
  (save-excursion
    (let* ((origin-buffer (or origin-buffer (current-buffer)))
           (base-dir (look-at-message--base-directory origin-buffer))
           error-filename error-line error-column
           (was-relative nil))

      ;; --- original filename extraction ---
      (search-backward-regexp "[^a-ZA-Z0-9\\/\._-]")
      (forward-char 1)
      (let ((beg (point)))
        (search-forward-regexp "[^a-ZA-Z0-9\\/\._-]")
        (backward-char 1)
        (setq error-filename (buffer-substring-no-properties beg (point))))

      ;; --- original line/column parsing (kept) ---
      (when (search-forward-regexp "[ :]" nil t nil)
        (let ((beg (point)))
          (when (search-forward-regexp "[^0-9]" nil t nil)
            (backward-char 1)
            (setq error-line
                  (string-to-number
                   (buffer-substring-no-properties beg (point))))))

        (when (search-forward-regexp "[ :]" nil t nil)
          (let ((beg (point)))
            (when (search-forward-regexp "[^0-9]" nil t nil)
              (backward-char 1)
              (setq error-column
                    (string-to-number
                     (buffer-substring-no-properties beg (point))))
              (forward-char 1)))))

      ;; Remember whether it was relative *before* expanding
      (setq was-relative (and error-filename
                              (not (file-name-absolute-p error-filename))))

      ;; Expand relative -> absolute so `file-exists-p` and open work
      (when was-relative
        (setq error-filename (expand-file-name error-filename base-dir)))

      ;; --- Open and jump (as in your original) ---
      (if (and error-filename (file-exists-p error-filename))
          (progn
            (find-file-other-window error-filename)  ;; open buffer [1]

            ;; This is the equivalent of doing: M-x cd /home/user/project
            ;; but only for the visited file buffer, and only when it was relative.
            (if cd-to-base-dir
              (when was-relative
                (setq-local default-directory (file-name-as-directory base-dir))))

            (when error-line (goto-line error-line))       ;; [1]
            (when error-column (move-to-column error-column))) ;; [1]
        (message (concat "File " (or error-filename "<nil>") " not found"))))))

(defun look-at-message-show ()
  (interactive)
  (let ((oldbuf (current-buffer)))
    ;; Pass origin buffer so relative paths resolve from where you invoked it
    (look-at-message oldbuf nil)
    (switch-to-buffer-other-window oldbuf)))

(defun look-at-message-show-in-dir ()
  (interactive)
  (let ((oldbuf (current-buffer)))
    ;; Pass origin buffer so relative paths resolve from where you invoked it
    (look-at-message oldbuf 'true)
    (switch-to-buffer-other-window oldbuf)))

(defun look-at-message-go ()
  (interactive)
  (look-at-message (current-buffer) nil)
  )

(global-set-key "\C-cs" 'look-at-message-show)
(global-set-key "\C-cd" 'look-at-message-show-in-dir)
(global-set-key "\C-cg" 'look-at-message-go)

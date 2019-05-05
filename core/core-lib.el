;;; -*- lexical-binding: t; -*-
(defun projectile-directory-path ()
  "Retrieve the directory path relative to project root.

  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'.

  Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
				(file-name-directory file-name)
			      list-buffers-directory))
    (file-relative-name
     (file-truename directory-name)
     (projectile-project-root))))

(defun projectile-file-path ()
  "Retrieve the file path relative to project root.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-name (buffer-file-name))
    (file-relative-name (file-truename file-name) (projectile-project-root))))

(defun projectile-file-path-with-line ()
  "Retrieve the file path relative to project root, including line number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-path (projectile-file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun projectile-file-path-with-line-column ()
  "Retrieve the file path relative to project root, including line and column number.

  This function respects the value of the `column-number-indicator-zero-based'
  variable.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-path (projectile-file-path-with-line))
    (concat
     file-path
     ":"
     (number-to-string (if (and
			    ;; Emacs 26 introduced this variable.
			    ;; Remove this check once 26 becomes the minimum version.
			    (boundp column-number-indicator-zero-based)
			    (not column-number-indicator-zero-based))
			   (1+ (current-column))
			 (current-column))))))

(defun projectile-copy-directory-path ()
  "Copy and show the directory path relative to project root.

  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (projectile-directory-path))
      (message "%s" (kill-new directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun projectile-copy-file-path ()
  "Copy and show the file path relative to project root."
  (interactive)
  (if-let (file-path (projectile-file-path))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun projectile-copy-file-path-with-line ()
  "Copy and show the file path relative to project root, including line number."
  (interactive)
  (if-let (file-path (projectile-file-path-with-line))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun projectile-copy-file-path-with-line-column ()
  "Copy and show the file path relative to project root, including line and column number.

  This function respects the value of the `column-number-indicator-zero-based'
  variable."
  (interactive)
  (if-let (file-path (projectile-file-path-with-line-column))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun directory-path ()
  "Retrieve the directory path of the current buffer.

  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'.

  Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
				(file-name-directory file-name)
			      list-buffers-directory))
    (file-truename directory-name)))

(defun file-path ()
  "Retrieve the file path of the current buffer.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun file-path-with-line ()
  "Retrieve the file path of the current buffer, including line number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun file-path-with-line-column ()
  "Retrieve the file path of the current buffer, including line and column number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (file-path-with-line))
    (concat
     file-path
     ":"
     (number-to-string (if (and
			    ;; Emacs 26 introduced this variable.
			    ;; Remove this check once 26 becomes the minimum version.
			    (boundp column-number-indicator-zero-based)
			    (not column-number-indicator-zero-based))
			   (1+ (current-column))
			 (current-column))))))

(defun copy-directory-path ()
  "Copy and show the directory path of the current buffer.

  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (directory-path))
      (message "%s" (kill-new directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (file-path))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (file-path)))
      (message "%s" (kill-new file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun copy-file-name-base ()
  "Copy and show the file name without its final extension of the current
  buffer."
  (interactive)
  (if-let (file-name (file-name-base (file-path)))
      (message "%s" (kill-new file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun copy-file-path-with-line ()
  "Copy and show the file path of the current buffer, including line number."
  (interactive)
  (if-let (file-path (file-path-with-line))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun copy-file-path-with-line-column ()
  "Copy and show the file path of the current buffer, including line and column number.

  This function respects the value of the `column-number-indicator-zero-based'
  variable."
  (interactive)
  (if-let (file-path (file-path-with-line-column))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
	     (assoc ?_ register-alist))
	(jump-to-register ?_)
      (progn
	(window-configuration-to-register ?_)
	(delete-other-windows)))))

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun activate-capture-frame ()
  "run org-capture in capture frame"
  (select-frame-by-name "capture")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture))

(defun activate-capture-notes ()
  "run org-capture in capture frame"
  (switch-to-buffer (get-buffer-create "CAPTURE-notes.org")))

(defun counsel-rg-from-current-directory ()
  (interactive)
  (counsel-rg "" (file-name-directory buffer-file-name)))

(defun counsel-rg-thing-at-point ()
  (interactive)
  (counsel-rg (ivy-thing-at-point)))

(defun counsel-find-models ()
  (interactive "P")
  (counsel-projectile-find-file))

(defun open-config-file ()
  (interactive)
  (find-file "~/.suon-emacs-v2/init.el"))

(defun open-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun list-processes-other-window ()
  (interactive)
  (list-processes)
  (switch-to-buffer-other-window "*Process List*"))

(defun prodigy-awesome-tab-group ()
  "Jump to project by awesome tab group"
  (interactive)
  (-when-let (service (prodigy-service-at-pos))
    (let* ((project-name (f-expand (prodigy-service-cwd service)))
	   (group-to-switch (format "Project: %s/" project-name))
	   (result (awesome-tab-switch-group group-to-switch)))
      (when (listp result) (counsel-projectile-find-file)))))

(defun copy-this-file ()
  "Copy this file"
  (interactive)
  (dired-jump)
  (dired-do-copy)
  (dired-find-file))

(defun mouse-dumb-jump-go ()
  ""
  (dumb-jump-go))

(defun split-window-vertically-2 ()
  "Split screen to 2 windows"
  (interactive)
  (let ((w (selected-window)))
    (delete-other-windows)
    (split-window-right)
    (select-window (next-window))
    (awesome-tab-backward-tab)
    (select-window w)))

(defun split-window-vertically-3 ()
  "Split screen to 2 windows"
  (interactive)
  (let ((w (selected-window)))
    (delete-other-windows)
    (split-window-right)
    (select-window (next-window))
    (awesome-tab-backward-tab)
    (split-window-right)
    (select-window (next-window))
    (awesome-tab-backward-tab)
    (balance-windows-area)
    (select-window w)))

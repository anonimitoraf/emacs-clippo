;;; clippo.el --- Clipboard manager with Emacs   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Rafael Nicdao

;; Author: Rafael Nicdao <nicdaoraf@gmail.com>
;; Keywords: clipboard, 'clipboard manager'

;;; Commentary:

;;; Code:

(defun clippo--paste-to-os-clipboard (text)
  "Puts TEXT into the OS's clipboard."
  (let ((process-connection-type nil))
    (let ((proc (pcase system-type
                  ('darwin (start-process "clippo-pbcopy" "*Messages*" "pbcopy"))
                  ('gnu/linux (start-process "clippo-xclip" "*Messages*" "xclip" "-sel" "clip"))
                  (s (user-error "Unsupported OS: %s" s)))))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun clippo (&optional callback)
  "Pops out the `kill-ring' which you can copy from.
Optionally executes CALLBACK afterwards"
  (condition-case nil
      (let ((new-frame (make-frame '((name . "emacs-clippo")
                                     (minibuffer . only)))))
        (select-frame new-frame)
        (raise-frame new-frame)
        (let ((yanked (read-from-kill-ring "Copy to clipboard: ")))
          (clippo--paste-to-os-clipboard yanked)
          (when callback (funcall callback))
          (delete-frame)
          yanked))
    ;; Cancelled
    (quit (delete-frame))))

(defun clippo-yabai (window-id)
  "Just like the fn `clippo' but specifically for yabai.
Focuses WINDOW-ID as a callback."
  (clippo (lambda () (shell-command (format "yabai -m window --focus %s" window-id)))))

(provide 'clippo)
;;; clippo.el ends here

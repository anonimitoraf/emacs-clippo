;;; clippo.el --- Clipboard manager with Emacs   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Rafael Nicdao

;; Author: Rafael Nicdao <nicdaoraf@gmail.com>
;; Keywords: clipboard, 'clipboard manager'

;;; Commentary:

;;; Code:

(defun clippo--paste-to-osx (text)
  "Puts TEXT into OSX's clipboard."
  (let ((process-connection-type nil)
        (proc (start-process "pbcopy" "*Messages*" "pbcopy")))
    (process-send-string proc text)
    (process-send-eof proc)))

(defun clippo (&optional callback)
  "Pops out the `kill-ring' which you can copy from.
Optionally executes CALLBACK afterwards"
  (let ((new-frame (make-frame '((name . "emacs-clippo")
                                 (minibuffer . only)))))
    (select-frame new-frame)
    (raise-frame new-frame)
    (let ((yanked (read-from-kill-ring "Copy to clipboard: ")))
      (run-at-time 0 nil (lambda () (delete-frame new-frame)))
      (clippo--paste-to-osx yanked)
      (when callback (funcall callback))
      yanked)))

(defun clippo-yabai (window-id)
  "Just like the fn `clippo' but specifically for yabai.
Focuses WINDOW-ID as a callback."
  (clippo (lambda () (shell-command (format "yabai -m window --focus %s" window-id)))))

(provide 'clippo)
;;; clippo.el ends here

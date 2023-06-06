;;; clippo.el --- Clipboard manager   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Rafael Nicdao

;; Author: Rafael Nicdao <nicdaoraf@gmail.com>
;; Keywords: clipboard, 'clipboard manager'
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (clipmon "20160925"))
;; Homepage: https://github.com/anonimitoraf/emacs-clippo

;;; Commentary:

;; Clippo is a simple clipboard manager meant to be used outside of Emacs.

;;; Code:

;; TODO Add for the clipmon requirement
(require 'clipmon)

(defun clippo-start-listen ()
  "Start listening for any copies to the system clipboard"
  (interactive)
  (clipmon-mode-start))

(defun clippo-stop-listen ()
  "Stop listening for any copies to the system clipboard"
  (interactive)
  (clipmon-mode-stop))

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
                                     (minibuffer . only)
                                     (undecorated . nil)
                                     (width . 0.5)))))
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

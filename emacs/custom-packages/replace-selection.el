;;; replace-selection.el --- Replace selections, just like Vim's "r" command.  -*- lexical-binding: t; -*-
;; Author: Daniel Brackenbury <daniel.brackenbury@gmail.com>
;; Package-Version: 1.0.0
;; Keywords: editing replacing substitution

;;; Commentary:
;; Replace entire selections with characters or phrases.

;;; Code:
(defun replace-selection-with-char (char)
  "Replace the currently selected text with a single CHAR.

Similar in functionality to the command typically bound to Vim's \"r\" key."
  (interactive "cPress the character to replace selection with.")
  (if (use-region-p)
      (let ((replacement-length (- (region-end) (region-beginning)))
            (original-point (point)))
        (goto-char (region-beginning))
        (delete-char replacement-length)
        (dotimes (_ replacement-length)
          (insert-char char))
        (goto-char original-point))
    (progn (delete-char 1)
           (save-excursion (insert-char char)))))

(defun replace-selection-with-phrase (phrase)
  "Replace the currently selected text with the given PHRASE.

Similar in functionality to the command typically bound to Vim's \"r\" key."
  (interactive "sReplace selection with: ")
  (if (use-region-p)
      (let ((original-point (point)))
        (delete-region (region-beginning) (region-end))
        (goto-char (region-beginning))
        (insert phrase)
        (goto-char original-point))
    (progn (beep)
           (message "Please select a region to replace."))))

(provide 'replace-selection)
;;; replace-selection.el ends here

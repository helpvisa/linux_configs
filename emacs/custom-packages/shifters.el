;;; shifters.el --- Shift characters, lines, and words around.  -*- lexical-binding: t; -*-
;; Author: Daniel Brackenbury <daniel.brackenbury@gmail.com>
;; Package-Version: 1.0.0
;; Keywords: editing shifting sliding shift slide

;;; Commentary:
;; Move words, characters, and lines around the buffer.
;; Select regions to shift them left or right.
;; Having no mark active shifts around the character in front of the cursor.
;; Shifting left / right beyond line limits will add whitespace.
;; Shifting up / down will shift the current line.
;; Activate your mark to shift all lines touches by the active mark.
;; Use repeat-mode for maximum fun!

;;; Code:
(defun shift-line (&optional repeats direction)
  "Shift the current line(s) REPEATS # of times in the given DIRECTION (up / down).

Not meant for interactive binding.

Taken on its own, this function will *not* create new lines when at the beginning
or end of a buffer.  See ‘shift-line-down’ and ‘shift-line-up’ for this behaviour."
  (unless repeats (setq repeats 1))
  (unless direction (setq direction 1))
  (with-current-buffer (current-buffer)
    (undo-boundary)
    (dotimes (_number repeats)
      (let ((shift-beginning (if (use-region-p)
                                 (region-beginning)
                               (line-beginning-position)))
            (shift-end (+ (line-end-position) 1)))
        (let ((deactivate-mark nil)
              (region-was-active (use-region-p))
              (current-line (buffer-substring shift-beginning
                                              shift-end))
              (column-at-start (current-column)))
          (delete-region shift-beginning shift-end)
          (forward-line direction)
          (let ((new-last-position (point)))
            (insert current-line)
            (when region-was-active
                (set-mark new-last-position))
            (forward-line -1)
            (move-to-column column-at-start)))))))

(defun shift-line-down (count)
  "Shift the current line(s) down by COUNT times.

If at end of buffer, create a new empty line below the current line and shift
the current line onto that line.

Due to the way Emacs handles files which do not end with a newline character,
you may notice inconsistent behaviour when shifting past the final line in any
buffer which does not end with a newline.  This is because ‘eobp’ returns nil if
said newline character is missing from the end of the buffer."
  (interactive "p")
  ;; check to see if at end of buffer
  (when (save-excursion (forward-line)
                        (eobp))
    (if (use-region-p)
        (let ((deactivate-mark nil))
          (save-excursion (goto-char (region-beginning))
                          (newline)
                          (set-mark (point))))
      (newline)))
  (shift-line count))

(defun shift-line-up (count)
  "Shift the current line(s) up by COUNT times.

If at beginning of buffer, create a new empty line below the current line."
  (interactive "p")
  (let ((point-at-start (if (use-region-p)
                            (region-beginning)
                          (point))))
    (shift-line count -1)
    (let ((deactivate-mark nil))
      (when (save-excursion (when (use-region-p)
                              (goto-char (region-beginning)))
                            (and (eq point-at-start (point))
                                 (bobp)))
        (save-excursion (end-of-line)
                        (newline))))))

(defun shift-selection (&optional repeats direction)
  "Shift char or region REPEATS # of times in the given DIRECTION (left / right).

Not meant for interactive binding.

Taken on its own, this function may wrap unpredictably at line boundaries.
See ‘shift-selection-left’ and ‘shift-selection-right’ for this behaviour.

See also ‘shift-selection-word-left’ and ‘shift-selection-word-right’"
  (unless repeats (setq repeats 1))
  (unless direction (setq direction 1))
  (with-current-buffer (current-buffer)
    (undo-boundary)
    (dotimes (_number repeats)
      (let ((shift-beginning (if (use-region-p)
                                 (region-beginning)
                               (point)))
            (shift-end (if (use-region-p)
                           (region-end)
                         (+ (point) 1)))
            (deactivate-mark nil))
        (let ((region-was-active (use-region-p))
              (initial-line-number (line-number-at-pos))
              (current-selection (buffer-substring shift-beginning
                                              shift-end)))
          (delete-region shift-beginning shift-end)
          (forward-char direction)
          (when (not (eq initial-line-number (line-number-at-pos)))
            (forward-line (* direction -1))
                     (if (< 0 direction)
                (progn (end-of-line)
                       (insert " "))
              (progn (beginning-of-line)
                     (insert " ")
                     (beginning-of-line))))
          (let ((new-last-position (point)))
            (insert current-selection)
            (delete-trailing-whitespace)
            (if region-was-active
                (set-mark new-last-position)
            (backward-char))))))))

(defun shift-selection-left (count)
  "Shift the current char(s) left by COUNT times.

Attemping to shift past the beginning of a line will add a space after the
region being shifted, effectively creating extra space between the selection and
the rest of the line."
  (interactive "p")
  ;; safety case to prevent disappearing characters at beginning of buffer
  (when (if (use-region-p)
            (save-excursion (goto-char (region-beginning))
                            (bobp))
          (bobp))
    (insert " "))
  (shift-selection count -1))

(defun shift-selection-right (count)
  "Shift the current char(s) right by COUNT times.

Attemping to shift past the end of a line will add additional spaces between the
region being shifted and the end of the line, effectively increasing the gap
between the selection and the rest of the line."
  (interactive "p")
  ;; safety case to prevent disappearing characters at end of buffer
  (let ((deactivate-mark nil))
    (when (if (use-region-p)
              (save-excursion (goto-char (region-end))
                              (forward-char)
                              (eobp))
            (save-excursion (forward-char)
                            (eobp)))
      (if (use-region-p)
          (insert "  ")
        (progn (insert "  ")
               (forward-char -1)))))
  (shift-selection count))

(defun shift-selection-word-left (count)
  "Shift the current char(s) a word left by COUNT times.

Attemping to shift past the beginning of a line will add a space after the
region being shifted, effectively creating extra space between the selection and
the rest of the line."
  (interactive "p")
  ;; make sure cursor starts from beginning of region in all cases to get
  ;; accurate measurement of how far back we need to travel
  (let ((shift-position (- (save-excursion (if (use-region-p)
                                               (goto-char (region-beginning)))
                                           (point))
                           (save-excursion (if (use-region-p)
                                               (goto-char (region-beginning)))
                                           (dotimes (_number count)
                                             (backward-word))
                                           (point)))))
    (shift-selection shift-position -1)))

(defun shift-selection-word-right (count)
  "Shift the current char(s) a word right by COUNT times.

Attemping to shift past the end of a line will add additional spaces between the
region being shifted and the end of the line, effectively increasing the gap
between the selection and the rest of the line."
  (interactive "p")
  ;; make sure cursor starts from end of region in all cases to get
  ;; accurate measurement of how far forward we need to travel
  (let ((shift-position (- (save-excursion (if (use-region-p)
                                               (goto-char (region-end)))
                                           (dotimes (_number count)
                                             (forward-word))
                                           (point))
                           (save-excursion (if (use-region-p)
                                               (goto-char (region-end)))
                                           (point)))))
    (shift-selection shift-position 1)))

(provide 'shifters)
;;; shifters.el ends here

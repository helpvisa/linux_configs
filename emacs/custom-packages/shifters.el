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
Not meant for interactive binding."
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
          (if (save-excursion (end-of-line)
                              (eobp))
              (save-excursion (end-of-line)
                              (newline)))
          (let ((new-last-position (point)))
            (insert current-line)
            (if region-was-active
                (set-mark new-last-position))
            (forward-line -1)
            (move-to-column column-at-start)))))))

(defun shift-line-down (count)
  "Shift the current line(s) down by COUNT times."
  (interactive "p")
  (shift-line count))

(defun shift-line-up (count)
  "Shift the current line(s) up by COUNT times."
  (interactive "p")
  (shift-line count -1))

(defun shift-selection (&optional repeats direction)
  "Shift char or region REPEATS # of times in the given DIRECTION (left / right).
Not meant for interactive binding."
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
              (current-word (buffer-substring shift-beginning
                                              shift-end)))
          (delete-region shift-beginning shift-end)
          (forward-char direction)
          (if (not (eq initial-line-number (line-number-at-pos)))
              (progn (forward-line (* direction -1))
                     (if (< 0 direction)
                         (end-of-line)
                       (beginning-of-line))
                     (insert " ")))
          (let ((new-last-position (point)))
            (insert current-word)
            (delete-trailing-whitespace)
            (if region-was-active
                (set-mark new-last-position)
            (backward-char))))))))

(defun shift-selection-left (count)
  "Shift the current char(s) left by COUNT times."
  (interactive "p")
  (shift-selection count -1))

(defun shift-selection-right (count)
  "Shift the current char(s) right by COUNT times."
  (interactive "p")
  (shift-selection count))

(defun shift-selection-word-left (count)
  "Shift the current char(s) a word left by COUNT times."
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
  "Shift the current char(s) a word right by COUNT times."
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

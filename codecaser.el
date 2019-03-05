;;; codecaser.el --- Change the casing of names cycling CamelCase, snake_case, pascalCase

;;; Commentary:

;;; Code:
(require 'thingatpt)

(defvar codecaser-thing-at-point-default-by-mode '((nxml-mode . word)
                                                   (emacs-lisp-mode . symbol)
                                                   (t . symbol))
  "Alist, mapping modes to thing at point categories. The modes
  are given by their symbols, like e.g. `nxml-mode'. They must
  match the buffers value of `major-mode'. For the possible thing
  at point categories see `thing-at-point'. There is one special
  element `(t . <thing-at-point>)'. If present, it is used as
  default for all modes that do not appear explicitly in this
  alist.")

(defun codecaser--camel-case-to-snake-case (str)
  "Convert CamelCase string STR to snake_case (i.e. underscore
separated all lower case)."
  (let* ((case-fold-search nil)
         (trimmed-string (string-trim str))
         (position-max (length trimmed-string))
         (normalized-string (if (> (length trimmed-string) 0)
                                (concat (upcase (substring trimmed-string 0 1)) (substring trimmed-string 1 position-max))
                              ""))
         (next-start-position 0)
         (current-match-end 0)
         (current-match-string "")
         (parts nil))
    (while (> position-max next-start-position)
      (if (not (string-match "\\([A-Z]+[a-z]+\\|[A-Z]*[0-9]*[a-z]*\\)" normalized-string next-start-position))
          (setq next-start-position position-max)
        (setq next-start-position (match-end 1))
        (setq current-match-string (match-string 1 str))
        (if (string-match "^\\([A-Z]*\\)\\([A-Z][a-z]+\\)" current-match-string)
            (setq parts (cons (match-string 1 current-match-string) (cons (match-string 2 current-match-string) parts)))
          (if (string-match "^\\([A-Z]*\\)\\([0-9]*\\)\\([a-z]*\\)" current-match-string)
              (setq parts (cons (match-string 3 current-match-string) (cons (match-string 2 current-match-string) (cons (match-string 1 current-match-string) parts))))
            (setq parts (cons current-match-string parts))))))
    (mapconcat 'downcase (reverse (delete "" parts)) "_")))

(defun codecaser--get-default-thing-at-point-for-mode (mode)
  "Get default thing at point for major MODE.

MODE must be a major mode symbol as given by a buffers
`major-mode' value. The default thing at point for MODE is
retrieved from `codecaser-thing-at-point-default-by-mode'. If
`codecaser-thing-at-point-default-by-mode' does not contain an
entry for MODE, this function returns `symbol'."
  (let ((association (or (assq mode codecaser-thing-at-point-default-by-mode) (assq 't codecaser-thing-at-point-default-by-mode))))
    (if association
        (cdr association)
      'symbol)))

(defun codecaser-thing-at-point-camel-case-to-snake-case-dwim (start end)
  "Convert the buffer substring between START and END position
from CamelCase to snake_case and replace the original substring
with the conversion result.

If region is active start and end default to the lower and upper
bound of the active region. Otherwise use the default thing at
point for the current major mode (see
`codecaser--get-default-thing-at-point-for-mode')."
  (interactive
   (let* ((use-empty-active-region nil)
          (bounds (if (use-region-p)
                      '((region-beginning) (region-end))
                    (bounds-of-thing-at-point (codecaser--get-default-thing-at-point-for-mode major-mode)))))
     `(,(car bounds) ,(cdr bounds))))
  (let* ((text (buffer-substring-no-properties start end))
         (replacement (codecaser--camel-case-to-snake-case text)))
    (delete-region start end)
    (goto-char start)
    (save-excursion
      (insert replacement))))

(provide 'codecaser)
;;; codecaser.el ends here

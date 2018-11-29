;;; Package --- Summary
;;; General custom functions
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(defun my/update-all-autoloads ()
  (interactive)
  (cd emacs-d)
  (let ((generated-autoload-file
         (expand-file-name "loaddefs.el")))
    (when (not (file-exists-p generated-autoload-file))
      (with-current-buffer (find-file-noselect
                            generated-autoload-file)
        (insert ";;")
        (save-buffer)))
    (mapcar #'update-directory-autoloads
            '("" "config" "config/modes"))))

;; ——

;; Byte compile the current file
(defun my/byte-compile-this-file ()
  "Compile the file the buffer is visiting."
  (interactive)
  (byte-compile-file (expand-file-name buffer-file-name)))


(defun my/align-repeat (start end regexp)
  "Between START and END, repeat alignment with respect to the given regular expression REGEXP."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun my/align-decimal (start end)
    "Between START and END, align a table of numbers on (optional) decimal points."
    (interactive "r")
    (align-regexp start end "\\([:space:]*[+-]?[0-9]+\\)\\.?\\([0-9]*\\(?:[eE][+-][0-9]+\\)?\\)" -1 0 t))


;; M-w saves the current line if no region is selected
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))


;; C-w deletes and saves the current line if no region is selected
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; ——

(defvar my/comment-line-last-col nil)

(defun my/toggle-comment-line (n again)
  "Comment or uncomment the next N line(s).
If AGAIN is true, use the same mode as the last call."
  (if comment-start
      (let*(
            (end    (cond ((or (not comment-end) (equal comment-end "")) "")
                          ((string-match "^ " comment-end) comment-end)
                          (t (concat " " comment-end))))
            (start  (cond ((string-match " $" comment-start) comment-start)
                          ((and (= (length comment-start) 1) (equal end ""))
                           (concat comment-start " "))
                          (t (concat comment-start " "))))

            (end    (concat comment-end))
            (start  (concat comment-start))

            (qstart (regexp-quote start))
            (qend   (regexp-quote end))

            (col    (and again my/comment-line-last-col))
            (mode   (and again (if col 'comment 'uncomment)))
            (direction (if (< 0 n) 1 -1))
            (n  (abs n)))

        (catch 'done
          (beginning-of-line)
          (if (< direction 0) (forward-line -1))

          (while (>= (setq n (1- n)) 0)
            (when (eobp) (throw 'done nil))

            (skip-chars-forward "\t ")
            (unless (eolp)
              (unless mode (setq mode (if (looking-at (concat qstart "\\(.*\\)" qend "$")) 'uncomment 'comment)))

              (let ((cur (current-column)))
                (cond ((and col (< col cur))
                       (move-to-column col t))
                      ((eq mode 'comment)
                       (setq col cur))))

              (cond ((eq mode 'comment)
                     (insert start) (end-of-line) (insert end))

                    ((eq mode 'uncomment)
                     (when (looking-at (concat qstart "\\(.*\\)" qend "$"))
                       (replace-match "\\1" t)))))

            (forward-line direction))

          (if (< direction 0)
              (forward-line 1)))

        (setq my/comment-line-last-col col))
    (message "Comments not available for this mode")))


(defun my/comment-line-and-go-down (n)
  "Toggle a comment on current N line(s) (disable line by line)."
  (interactive "p")
  (my/toggle-comment-line (+ n) (eq last-command 'my/comment-line-and-go-down)))


(defun my/go-up-and-comment-line (n)
  "Toggle a comment on current N line(s) (disable line by line)."
  (interactive "p")
  (my/toggle-comment-line (- n) (eq last-command 'my/go-up-and-comment-line)))

;; ——

(defun my/sort-words (reverse beg end)
  "Sort words alphabetically in the selected region.

   If prefixed with negative \\[universal-argument], sorts in REVERSE.
   The region is delimited by BEG and END.

   The variable `sort-fold-case' determines whether alphabetic case
   affects the sort order.

   See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun my/sort-symbols (reverse beg end)
  "Sort symbols alphabetically in the selected region .

   If prefixed with negative \\[universal-argument], sorts in REVERSE.
   The region is delimited by BEG and END.

   See `my/sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun my/reverse-words (beg end)
  "Reverse the order of words in region delimited by BEG and END."
  (interactive "*r")
  (apply 'insert
   (reverse
    (split-string
     (delete-and-extract-region beg end) "\\b"))))

;; ——

(defun my/xml-formatter ()
  "Format the region delimited by BEG and END using xmllint."
  (interactive)
  (save-excursion
    (catch 'catcher
      (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t))
    (nxml-mode)
    (indent-region (point-min) (point-max))))

(provide 'functions)

;;; ————————————————————————————————————————————————————————
;;; functions.el ends here

;;; Package --- Summary
;;; markdown configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:25:18 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package markdown-mode
  :defer t

  :mode
  ("\\.md$" . markdown-mode)
  ("\\.markdown$" . markdown-mode)
  ("README\\.md$" . gfm-mode)

  :config
  (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files." t)
  (autoload 'gfm-mode      "markdown-mode" "Major mode for editing GitHub Flavored Markdown files" t))

(provide 'cfg-markdown)

;;; ————————————————————————————————————————————————————————
;;; cfg-markdown.el ends here

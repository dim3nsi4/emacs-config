;;; Package --- Summary
;;; latex configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package latex
  :defer t
  :commands (font-latex-add-keywords)

  :config
  ;; Changes the default fontification for the given keywords
  (eval-after-load "font-latex"
    '(font-latex-add-keywords
      '(("newenvironment" "*{[[")
        ("renewenvironment" "*{[[")
        ("newcommand" "*|{\\[[")
        ("renewcommand" "*|{\\[[")
        ("providecommand" "*|{\\[[")
        ("fbox" "")
        ("mbox" "")
        ("sbox" ""))
      'function)))

;; ——

(use-package auctex
  :defines
  (TeX-auto-save
   TeX-parse-self
   reftex-plug-into-AUCTeX)

  :hook
  ((LaTeX-mode . visual-line-mode)
   (LaTeX-mode . flyspell-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . turn-on-reftex))

  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        reftex-plug-into-AUCTeX t)
  (setq-default TeX-master nil))

;; ——

(use-package cdlatex
  :hook
  ((LaTeX-mode-hook . turn-on-cdlatex)
   (latex-mode-hook . turn-on-cdlatex)
   (org-mode-hook   . turn-on-org-cdlatex)))

;; ——

(provide 'cfg-latex)

;;; ————————————————————————————————————————————————————————
;;; cfg-latex.el ends here

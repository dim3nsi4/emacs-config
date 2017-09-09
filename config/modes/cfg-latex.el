;;; Package --- Summary
;;; latex configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:44:04 seimandp>
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
  :defer t
  :defines (TeX-auto-save
            TeX-parse-self
            reftex-plug-into-AUCTeX)
  :init
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        reftex-plug-into-AUCTeX t)
  (setq-default TeX-master nil))

;; ——

(use-package cdlatex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'latex-mode-hook 'turn-on-cdlatex)
  (add-hook 'org-mode-hook   'turn-on-org-cdlatex))

;; ——

(provide 'cfg-latex)

;;; ————————————————————————————————————————————————————————
;;; cfg-latex.el ends here

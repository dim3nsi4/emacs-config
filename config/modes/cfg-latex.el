;;; Package --- Summary
;;; latex configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package tex
  :defer t
  :ensure auctex

  :hook
  ((LaTeX-mode . flyspell-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . reftex-mode))

  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        reftex-plug-into-AUCTeX t)

  (setq-default TeX-master t
                TeX-PDF-mode t
                TeX-engine 'luatex))

;; ——

(use-package cdlatex)

;; ——

(provide 'cfg-latex)

;;; ————————————————————————————————————————————————————————
;;; cfg-latex.el ends here

;;; Package --- Summary
;;; org configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package org
  :defer t

  :defines
  (org-agenda-files
   org-agenda-restore-windows-after-quit
   org-agenda-window-setup
   org-table-convert-region-max-lines
   org-table-automatic-realign
   org-export-with-sub-superscripts
   org-clock-persist
   org-latex-compiler
   org-latex-pdf-process)

  :commands
  (org-link-set-parameters
   org-clock-persistence-insinuate
   org-babel-do-load-languages)

  :config
  (setq org-directory "~/.org-mode.d"
        org-agenda-files (list "~/.org-mode.d/agenda/")
        org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window
        org-default-notes-file "notes.org"
        org-support-shift-select t
        org-src-fontify-natively t
        org-startup-folded nil
        org-table-convert-region-max-lines 9999
        org-table-automatic-realign t
        org-image-actual-width nil
        org-highlight-latex-and-related '(latex script entities)
        org-export-with-sub-superscripts (quote {})
        org-log-done t)

  (setq org-link-frame-setup '((vm      . vm-visit-folder-other-frame)
                               (vm-imap . vm-visit-imap-folder-other-frame)
                               (gnus    . org-gnus-no-new-news)
                               (file    . find-file)
                               (wl      . wl-other-frame)))

  ;; Org clock
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Change the way footnote are defined
  (setq org-footnote-definition-re "^\\[fn:[-_[:word:]]+\\]"
        org-footnote-re            (concat "\\[\\(?:fn:\\([-_[:word:]]+\\)?:"
                                           "\\|"
                                           "\\(fn:[-_[:word:]]+\\)\\)"))

  ;; Default LaTeX compiler
  (setq org-latex-compiler "lualatex")

  ;; Command for latex compilation
  (setq org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "%latex -interaction nonstopmode -output-directory %o %f"
          "%latex -interaction nonstopmode -output-directory %o %f"))

  ;; Fontify broken links (require org 9.0.0 or above)
  (org-link-set-parameters
   "file"
   :face (lambda (path) (if (file-exists-p path) 'org-link 'org-warning)))

  ;; Default packages
  (setq org-latex-default-packages-alist '(("AUTO"     "inputenc"  t ("pdflatex")) ;; Accept different input encodings
                                           ("T1"       "fontenc"   t ("pdflatex")) ;; Standard package for selecting font encodings
                                           (""         "graphicx"  t)              ;; Enhanced support for graphics
                                           (""         "grffile"   t)              ;; Extended file name support for graphics
                                           (""         "longtable" nil)            ;; Allow tables to flow over page boundaries
                                           (""         "wrapfig"   nil)            ;; Produces figures which text can flow around
                                           (""         "rotating"  nil)            ;; Rotation tools, including rotated full-page floats
                                           (""         "float"     nil)            ;; Improved interface for floating objects
                                           ("normalem" "ulem"      t)              ;; Package for underlining
                                           (""         "amsmath"   t)              ;; AMS mathematical facilities for LaTeX
                                           (""         "amsfonts"  t)              ;; TeX fonts from the American Mathematical Society
                                           (""         "marvosym"  t)              ;; Martin Vogel's Symbols (marvosym) font
                                           (""         "wasysym"   t)              ;; LaTeX support file to use the WASY2 fonts
                                           (""         "hyperref"  nil)))          ;; Extensive support for hypertext in LaTeX

  ;; Add custom classes
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes '("new-aiaa"
                                      "\\documentclass[11pt]{new-aiaa}"
                                      ("\\section{%s}"       . "\\section*{%s}")
                                      ("\\subsection{%s}"    . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))

  ;; Files association
  (setq org-file-apps (append '(("\\.png\\'"  . "feh -g +0+0 %s")) org-file-apps))
  (setq org-file-apps (append '(("\\.jpg\\'"  . "feh -g +0+0 %s")) org-file-apps))
  (setq org-file-apps (append '(("\\.jpeg\\'" . "feh -g +0+0 %s")) org-file-apps))
  (setq org-file-apps (append '(("\\.tif\\'"  . "feh -g +0+0 %s")) org-file-apps))
  (setq org-file-apps (append '(("\\.tiff\\'" . "feh -g +0+0 %s")) org-file-apps))

  (setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                   ("/" italic "<i>" "</i>")
                                   ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                   ("=" org-code "<code>" "</code>" verbatim)
                                   ("~" (or )rg-verbatim "<code>" "</code>" verbatim))))

  ;; Activate support for languages for org-babel
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (gnuplot    . t)
                                                           (python     . t)
                                                           (latex      . t)
                                                           (sh         . t)
                                                           (sqlite     . t))))

;; ——

(use-package org-ref
  :after org
  :defer t

  :defines
  (reftex-default-bibliography
   org-ref-bibliography-notes
   org-ref-default-bibliography
   org-ref-pdf-directory
   bibtex-completion-pdf-open-function)

  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)

  :config
  (setq reftex-default-bibliography '("~/DATA/90_BIBLIOGRAPHY/references.bib"))

  (setq org-latex-prefer-user-labels t)

  (setq org-ref-bibliography-notes     "~/DATA/90_BIBLIOGRAPHY/notes.org"
        org-ref-default-bibliography '("~/DATA/90_BIBLIOGRAPHY/references.bib")
        org-ref-pdf-directory          "~/DATA/90_BIBLIOGRAPHY/bibtex-pdfs/")

  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "open" "*open*" "open" fpath))))

;; ——

(use-package org-bullets
  :commands
  (org-bullets-mode)

  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ——

;; (use-package org-wiki
;;   :config
;;   (setq org-wiki-location "~/.org-mode.d/wiki/"))

;; ——

;; (use-package ox-wk)

;; ——

(provide 'cfg-org)

;;; ————————————————————————————————————————————————————————
;;; cfg-org.el ends here

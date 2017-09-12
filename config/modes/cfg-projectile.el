;;; Package --- Summary
;;; projectile configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package projectile
  :demand
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))

  :config

  (dolist (item '(".meghanad" "__pycache__"))
    (add-to-list 'projectile-globally-ignored-directories item))
  (dolist (item '("GTAGS" "GRTAGS" "GPATH" "*.elc" "*.class"))
    (add-to-list 'projectile-globally-ignored-files item))

  (defvar projectile-ag-command
    (concat "\\ag"                ; used unaliased version of `ag': \ag
            " -i"                 ; case insensitive
            " -f"                 ; follow symbolic links
            " --skip-vcs-ignores" ; Ignore files/dirs ONLY from `.ignore',
            " -0"                 ; output null separated results
            " -g ''")             ; get file names matching the regex ''
    "Ag command to be used by projectile to generate file cache.")

  (when (executable-find "ag")
    (defun projectile-get-ext-command ()
      "Override the projectile-defined function so that `ag' is
      always used for getting a list of all files in a project."
      projectile-ag-command))

  (projectile-mode))

;; ——

(req-package ivy
  :after projectile
  :require projectile
  :config
  (setq projectile-completion-system 'ivy))

;; ——

(req-package hydra
  :defer t
  :after projectile

  :bind
  (:map projectile-mode-map
        ("C-c p" . my/hydra-projectile/body))

  :config
  (defhydra my/hydra-projectile (:color teal :hint nil :idle 0.25)
    "
Projectile

Projects: [_p_]  switch-project            Files: [_d_]  find-dir                  Compilation: [_c_]  compile-project
          [_q_]  switch-open-project              [_f_]  find-file                              [_P_]  test-project
          [_V_]  browse-dirty-project             [_g_]  find-file-dwim                         [_u_]  run-project
          [_y_]  remove from known project        [_T_]  find-test-file                         [_t_]  toggle-between-implementation-and-test
           ^ ^                                    [_e_]  recentf
 Buffers: [_b_]  switch-to-buffer                 [_l_]  file-in-directory                Misc: [_E_]  edit-dir-locals
          [_k_]  kill-buffers                     [_F_]  file-in-known-projects                 [_v_]  version-control
          [_I_]  ibuffer                          [_D_]  dired                                  [_m_]  commander
          [_S_]  save-project-buffers
           ^ ^                             Cache: [_z_]  cache-current-file           Terminal: [_&_]  run-async-shell-command-in-root
  Search: [_o_]  multi-occur                      [_i_]  invalidate-cache                       [_!_]  run-shell-command-in-root
          [_r_]  replace                           ^ ^                                          [_xe_] run-eshell
          [_sg_] grep                       Tags: [_j_]  find-tag                               [_xs_] run-shell
          [_ss_] ag                               [_R_]  regenerate-tags                        [_xt_] run-term
      "
    ("SPC" counsel-projectile)
    ("<escape>" nil :exit t)

    ;; Projects
    ("p"  counsel-projectile-switch-project)
    ("q"  projectile-switch-open-project)
    ("V"  projectile-browse-dirty-projects)
    ("y"  projectile-remove-known-project)


    ;; Files in project(s)
    ("f"  counsel-projectile-find-file)
    ("g"  projectile-find-file-dwim)
    ("l"  projectile-find-file-in-directory)
    ("e"  projectile-recentf)
    ("d"  counsel-projectile-find-dir)
    ("D"  projectile-dired)
    ("F"  projectile-find-file-in-known-projects)
    ("T"  projectile-find-test-file)
    ("a"  projectile-find-other-file)

    ;; Cache
    ("z"  projectile-cache-current-file)
    ("i"  projectile-invalidate-cache)

    ;; Buffers
    ("b"  counsel-projectile-switch-to-buffer)
    ("k"  projectile-kill-buffers)
    ("I"  projectile-ibuffer)
    ("S"  projectile-save-project-buffers)

    ;; Search/Replace
    ("o"  projectile-multi-occur)
    ("r"  projectile-replace)
    ("sg" projectile-grep)
    ("ss" counsel-projectile-ag)

    ;; Tags
    ("j"  projectile-find-tag)
    ("R"  projectile-regenerate-tags)

    ;; Complitation
    ("c"  projectile-compile-project)
    ("P"  projectile-test-project)
    ("u"  projectile-run-project)
    ("t"  projectile-toggle-between-implementation-and-test)

    ;; Terminal
    ("&"  projectile-run-async-shell-command-in-root)
    ("!"  projectile-run-shell-command-in-root)
    ("xe" projectile-run-eshell)
    ("xs" projectile-run-shell)
    ("xt" projectile-run-term)

    ;; Other window
    ("wv"  projectile-display-buffer)
    ("wa"  projectile-find-other-file-other-window)
    ("wb"  projectile-switch-to-buffer-other-window)
    ("wd"  projectile-find-dir-other-window)
    ("wf"  projectile-find-file-other-window)
    ("wg"  projectile-find-file-dwim-other-window)
    ("wt"  projectile-find-implementation-or-test-other-window)

    ;; Misc
    ("E"  projectile-edit-dir-locals)
    ("v"  projectile-vc)
    ("m"  projectile-commander)))

;; ——

(provide 'cfg-projectile)

;;; ————————————————————————————————————————————————————————
;;; cfg-projectile.el ends here

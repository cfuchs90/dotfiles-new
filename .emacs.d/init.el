(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'load-path "~/emacs.d/offline-packages")

(setq package-enable-at-startup nil)
(package-initialize)

;; Instruct Emacs to use use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'custom-theme-load-path (expand-file-name "themes"
                                                       user-emacs-directory))

;; require use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Themes
; atom-one-dark theme as main theme for emacs
(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))

(use-package doom-themes)

(use-package vscode-dark-plus-theme)
  
  
(use-package org-beautify-theme
  :ensure t
  :config
  (load-theme 'org-beautify t))

(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

(use-package smart-mode-line
  :config
  (setq sml/theme 'atom-one-dark)
  (sml/setup))


;; general nice editing configs
(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(use-package perspective
  :ensure t  ; use `:straight t` if using straight.el!
  :bind (("C-x k" . persp-kill-buffer*))
  :custom (setq persp-mode-prefix-key (kbd "C-ä"))
  :init
  (persp-mode))

(use-package key-chord
  :ensure t
  :init
  (require 'key-chord)
  (key-chord-mode 1)
  (key-chord-define-global "qq" 'view-mode)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state))

(use-package evil)

(use-package view
  :ensure t
  :config
  (add-hook 'view-mode-hook
          (lambda ()
            (setq cursor-type (if view-mode t 'bar))))
  :bind(:map view-mode-map
	     ("n" . forward-line)
	     ("p" . previous-line)))

(use-package paredit
   :ensure t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-complection-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Schreibtisch/Projects")
    (setq projectile-project-search-path '("~/Schreibtisch/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flycheck
  :ensure t)


(use-package expand-region
  :ensure t)

(use-package dot-mode
  :ensure t
  :config
  (dot-mode-on))

;; All treemacs configuration
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-width                           35
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-ö"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)
	("C-ö" . treemacs)))




(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; 
;; General Development
(defun efs/lsp-mode-setup()
  (setq lsp-headerline-breadcrumb-segmets '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode .lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)


(use-package dap-mode)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))



(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package docker-compose-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package highlight-indentation
  :ensure t)

;; WebDev Packages
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook
  (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))


(use-package js2-mode
  :ensure t
  :mode
  (("\\.jsx?\\'" . js2-mode))
  :init
  (setq js-basic-indent 2)
  (setq-default js2-basic-indent 4
                js2-basic-offset 4
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$")))

;; (define-key js2-mode-map (kbd "M-.") nil) 

(use-package js2-refactor
  :ensure t
  :mode
  (("\\.jsx?\\'" . js2-mode))
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c ."))



;; (use-package xref-js2
;;   :ensure t
;;   :config
;;   (add-hook 'js2-mode-hook (lambda ()
;;   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))


;; Python Development
(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :config
  (setq virtualenv-workon-home "~/.local/share/virtualenvs")
  :custom
  (python-shell-interpreter "python3")
  (py-shell-name "/usr/bin/python3"))

(use-package pyvenv)

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

;; R Development
(use-package ess
  :ensure t
  :init (require 'ess-site))

;; Org Mode
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org
  :config
  (setq org-agenda-files '("~/Dokumente/Orgfiles/tasks.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "HOLD(h)" "|" "DONE(d!)" "CANCELED(k)")
	;  (sequence ("BACKLOG(b)" "PLAN(p)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "|" "COMPLETED(c)" ("CANCLED(k@)")))))
  
	  ))
(setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
	'((:startgroup)
					; Put mutually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("planning" . ?p)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)))

  (setq org-capture-templates
	'(
	  ("t" "Todo" entry (file+headline "~/Dokumente/Orgfiles/tasks.org" "Active") "%T %^G")
	("b" "Backlog" entry (file+headline "~/Dokumente/Orgfiles/tasks.org" "Backlog"))))

  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+@work")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  )

;; ORG ROAM
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dokumente/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))


;; Global Key Bindings
(global-set-key (kbd "M-ü") 'er/expand-region)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-a") 'back-to-indentation)
;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x b") 'persp-counsel-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "C-c c") 'counsel-org-capture)
(global-set-key (kbd "C-x a") 'org-agenda)
;(global-set-key (kbd "C-ä") 'toggle-transparency)
(global-set-key (kbd "C-x C-m") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x r b") 'counsel-bookmark)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "C-ü") 'counsel-load-theme)



;; General Emacs Config
(use-package popper
  :ensure t ; or :straight t
  :bind (("C-´"   . popper-toggle-latest)
         ("M-´"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))  

;; disable line numbering for certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode
		eshell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq delete-old-versions -1) ;; delete excess backup versions silently
(setq version-control t) ; use version control
(setq vc-make-backup-files t) ;; make backup files even when in version controlled dir
(setq backup-directory-alist'(("." . "~/.emacs.d/backups"))) ; which directory to put backup files
(setq vc-follow-symlinks t) ; Follow sym link
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))) ;; transform backups file name
(setq inhibit-startup-screen t) ;; don't show startup screen
(setq ring-bell-function 'ignore) ; silent bell, loud bell is annoying
(setq coding-system-for-read 'utf-8) ; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil) ;; sentence should end with only a point.
(setq default-fill-column 80) ;; toggle wrapping after 80 chars
(setq-default cursor-type 'bar) ;; set the cursor to a bar
(setq global-display-fill-column-indicator-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq-default major-mode 'text-mode)
(global-display-line-numbers-mode)
(electric-pair-mode)
(global-subword-mode 1)
(show-paren-mode 1)
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
					; fonts
(set-frame-font "DejaVu Sans 10")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9685cefcb4efd32520b899a34925c476e7920725c8d1f660e7336f37d6d95764" "e29a6c66d4c383dbda21f48effe83a1c2a1058a17ac506d60889aba36685ed94" default))
 '(exwm-floating-border-color "#1c1e24")
 '(highlight-tail-colors ((("#2e343d") . 0) (("#353340") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#222228" "#EB64B9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#222228" "#74DFC4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#222228" "#4E415C"))
 '(objed-cursor-color "#964C7B")
 '(package-selected-packages
   '(popper vscode-dark-plus-theme doom-themes perspective org-roam pipenv evil-mode evil-visual-mark-mode helpful ivy-rich counsel-projectile dap-mode lsp-ivy lsp-treemacs lsp-ui company-box company ## lsp-mode highlight-indentation docker-compose-mode dockerfile-mode docker-file docker-file-mode treemacs-projectile treemacs use-package-hydra yasnippet-snippets which-key web-mode visual-regexp-steroids use-package undo-tree synosaurus smart-mode-line-atom-one-dark-theme shell-pop sass-mode rainbow-delimiters projectile perspeen paredit org-super-agenda org-ref org-bullets org-beautify-theme openwith magit julia-mode helm-css-scss flycheck expand-region evil-commentary ess emmet-mode elpy elfeed-goodies dot-mode counsel auctex atom-one-dark-theme all-the-icons-ivy all-the-icons-dired ace-window academic-phrases))
 '(pdf-view-midnight-colors (cons "#FFFFFF" "#27212E"))
 '(persp-mode-prefix-key [67109092])
 '(py-python-command "python3")
 '(rustic-ansi-faces
   ["#27212E" "#964C7B" "#74DFC4" "#FFE261" "#40B4C4" "#EB64B9" "#B4DCE7" "#FFFFFF"])
 '(vc-annotate-background "#27212E")
 '(vc-annotate-color-map
   (list
    (cons 20 "#74DFC4")
    (cons 40 "#a2e0a3")
    (cons 60 "#d0e182")
    (cons 80 "#FFE261")
    (cons 100 "#ffd35f")
    (cons 120 "#ffc55d")
    (cons 140 "#FFB85B")
    (cons 160 "#f89c7a")
    (cons 180 "#f18099")
    (cons 200 "#EB64B9")
    (cons 220 "#ce5ca4")
    (cons 240 "#b2548f")
    (cons 260 "#964C7B")
    (cons 280 "#834973")
    (cons 300 "#72466b")
    (cons 320 "#604363")
    (cons 340 "#544863")
    (cons 360 "#544863")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

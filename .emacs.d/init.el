(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'load-path "~/emacs.d/offline-packages")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

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

;; mu4e
(require 'mu4e)

;;(set-frame-font "JetBrains Mono")
(set-frame-font "Fira Mono")

(setq mu4e-maildir (expand-file-name "~/.mail/gmail"))
(setq mu4e-drafts-folder "/gmail/Entw&APw-rfe")
(setq mu4e-sent-folder "/gmail/Gesendet")
(setq mu4e-trash-folder "/gmail/Papierkorb")
(setq mu4e-user-email-address-list "fuchs.christian90@gmail.com")
(setq mu4e-view-show-addresses t)
(setq message-kill-buffer-on-exit t)
(setq mu4e-context-policy 'pick-first)
(setq mu4e-conform-quit nil)
(setq mu4e-attachment-dir "~/Downloads/MailAttachments")
(setq mu4e-vie-show-images t)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(setq mu4e-sent-messages-behaviour 'delete)
(setq user-mail-address "fuchs.christian90@gmail.com")
(setq user-full-name "Christian Fuchs")

(setq mu4e-maildir-shortcuts
      '(
	("/gmail/Inbox" . ?i)
	("/gmail/Services" . ?s)
	("/gmail/Gesendet" . ?g)))
;; mu4e Shortcuts
;; (setq mu4e-maildir-shortcuts
;;       '(
;; 	("Inbox" . ?i)
;; 	("Gesendet" . ?g)
;; 	("Papierkorb" . ?t)
;; 	("Services" . ?s)))

;; Themes

;; atom-one-dark theme as main theme for emacs
(when (window-system)(use-package atom-one-dark-theme
  :ensure t)
(use-package org-beautify-theme
  :ensure t)
(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t)
(use-package xresources-theme
  :ensure t))






;; transparency
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

  

(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t)
  ;; (setq sml/theme 'atom-one-dark)
  ;; (sml/setup))


;; general nice editing configs
(use-package ivy
  :ensure t)

(use-package counsel
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package perspeen
  :ensure t
  :init
  (setq perspeen-use-tab t)
  :config
  (perspeen-mode t))

(use-package key-chord
  :ensure t
  :init
  (require 'key-chord)
  (key-chord-mode 1)
  (key-chord-define-global "qq" 'view-mode))

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
  :ensure t)

(use-package rainbow-delimiters
   :ensure t)

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
        ("M-ä"       . treemacs-select-window)
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

;; Hydra Bindings
(use-package use-package-hydra
  :ensure t)

(use-package hydra
  :bind
  :hydra (hydra-tab (global-map "C-t")
  "Perspeen Tab"
  ("c" perspeen-tab-create-tab)
  ("n" perspeen-tab-next)
  ("d" perspeen-tab-del)
  ("p" perspeen-tab-prev))

  :hydra (hydra-workspace (global-map "C-ü")
  "Perspeen Workspace"
  ("c" perspeen-create-ws)
  ("n" perspeen-next-ws)
  ("p" perspeen-previous-ws)
  ("d" perspeen-delete-ws)
  ("r" perspeen-rename-ws)))

;;   (defhydra( hydra-tab (global-map "C-t")
;;   "Perspeen Tab"
;;   ("c" perspeen-tab-create-tab)
;;   ("n" perspeen-tab-next)
;;   ("d" perspeen-tab-del)
;;   ("p" perspeen-tab-prev))

;; (defhydra hydra-workspace (global-map "C-ü")
;;   "Perspeen Workspace"
;;   ("c" perspeen-create-ws)
;;   ("n" perspeen-next-ws)
;;   ("p" perspeen-previous-ws)
;;   ("d" perspeen-delete-ws)
;;   ("r" perspeen-rename-ws)))


;; General Development
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
(use-package js2-mode
  :ensure t
  :mode
  (("\\.js\\'" . js2-mode))
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
  (("\\.js\\'" . js2-mode))
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c ."))

;; (use-package xref-js2
;;   :ensure t
;;   :config
;;   (add-hook 'js2-mode-hook (lambda ()
;;   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))


;; R Development
(use-package ess
  :ensure t
  :init (require 'ess-site))



;; Global Key Bindings
(global-set-key (kbd "M-ü") 'er/expand-region)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-x a") 'org-agenda)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-ä") 'toggle-transparency)
(global-set-key "\C-x\C-m" 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c m") 'mu4e)


;; General Emacs Config
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

(when (window-system)
  ;;(load-theme 'atom-one-dark t)
  (load-theme 'xresources t)
  (load-theme 'org-beautify t))



(add-hook 'text-mode-hook #'auto-fill-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(custom-safe-themes
   '("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "e5dc5b39fecbeeb027c13e8bfbf57a865be6e0ed703ac1ffa96476b62d1fae84" "e29a6c66d4c383dbda21f48effe83a1c2a1058a17ac506d60889aba36685ed94" default))
 '(fci-rule-color "#3E4451")
 '(package-selected-packages
   '(xresources-theme highlight-indentation docker-compose-mode dockerfile-mode docker-file docker-file-mode treemacs-projectile treemacs use-package-hydra yasnippet-snippets which-key web-mode visual-regexp-steroids use-package undo-tree synosaurus smart-mode-line-atom-one-dark-theme shell-pop sass-mode rainbow-delimiters projectile perspeen paredit org-super-agenda org-ref org-bullets org-beautify-theme openwith magit julia-mode helm-css-scss flycheck expand-region evil-commentary ess emmet-mode elpy elfeed-goodies dot-mode counsel auctex atom-one-dark-theme all-the-icons-ivy all-the-icons-dired ace-window academic-phrases))
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(window-divider-default-right-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

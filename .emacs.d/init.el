;; El startup viene de Witchmacs
;; https://github.com/snackon/Witchmacs/blob/master/init.el
;; Make emacs startup faster
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;; quitar ese sonido irritante
(setq ring-bell-function 'ignore)

;;(server-start)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)	;; Disable visible scrollbar
(tool-bar-mode -1)	;; Disable toolbar
(menu-bar-mode -1)     ;; Quitar la barra de menú

;;opacity
;;(set-frame-parameter (selected-frame) 'alpha '(95 . 90))
;; hace que la línea actual se vea bien.
(global-hl-line-mode t)
;; quita los backups que terminan con ~
(setq make-backup-files nil)

;; Tener lineas y numeros
(column-number-mode)
;;(global-display-line-numbers-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(global-subword-mode 1);; move cursor by camelCase

;; (use-package ido
;;   :init
;;   (ido-mode 1)
;;   :config
;;   (setq ido-enable-flex-matching nil)
;;   (setq ido-create-new-buffer 'always)
;;   (setq ido-everywhere t))

;; (use-package icomplete
;;   :init
;;   (icomplete-mode 1)
;;   :config
;;   (setq icomplete-separator "\n")
;;   (setq icomplete-hide-common-prefix nil)
;;   (setq icomplete-in-buffer t))
;;  (define-key icomplete-minibuffer-map (kbd "TAB") 'icomplete-forward-completions)
;;  (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)

;; Quitar el buffer de *Completions*
(setq completion-auto-help nil)

(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
			          (?\' . ?\')
				  (?\¿ . ?\?)
			          ))
(electric-pair-mode t)

(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)

(require 'zone)
(zone-when-idle 600)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Instala todos los paquetes si no están en la computadora
(require 'use-package)
(setq use-package-always-ensure t)

;; El startup que hace que se vea lindo
;; Es casi el mismo de su página.
;; (use-package dashboard
;;   :ensure t
;;   :init
;;   (setq dashboard-banner-logo-title "Welcome to Emacs")
;;   (setq dashboard-startup-banner 'logo) 
;;   (setq dashboard-center-content t)
;;   :config
;;   (dashboard-setup-startup-hook))

;; defer packages
(setq use-package-always-defer t)

;; El buen look del prot
(use-package modus-themes
  :init (load-theme 'modus-operandi t))

;; Iconos lindos
(use-package all-the-icons)

;; La barra linda de DOOM
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Que se vean lindos los {},(),[]
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; La función de system-crafters para poder tener org-mode en el centro
(defun efs/visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/visual-fill)
  (newsticker-treeview-mode . efs/visual-fill)
  (info-mode . efs/visual-fill)
     ;; (dired-mode . efs/visual-fill)
	)
(use-package ido-vertical-mode
  :ensure t)

;; Te da la posibilidad de ver que onda con cada uno de los comandos
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package yasnippet-snippets
  :ensure t)

;; Snippets pal dia a dia
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook
  ((c-mode c++-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package vertico
  :ensure t)
(vertico-mode)

(use-package pdf-tools
  :ensure t)
;; quitar los números en los pdf

(use-package switch-window
       :ensure t
       :config
       (setq switch-window-input-style 'minibuffer)
       (setq switch-window-increase 4)
       (setq switch-window-threshold 2)
       (setq switch-window-shortcut-style 'qwerty)
       (setq switch-window-qwerty-shortcuts
		 '("a" "s" "d" "f" "j" "k" "l"))
       :bind
       ([remap other-window] . switch-window))

(use-package magit
  :ensure t)
;; Te complementa cuando escribes
(use-package company
  :ensure t
  :hook (cc-mode . company-mode)
        (svelte-mode . company-mode))

;; Parte de programacion
(use-package lsp-mode
  :ensure t
  :hook (c++-mode . lsp)
  (clojure-mode . lsp))
;; (use-package treemacs
;;   :ensure t)
(use-package svelte-mode
  :ensure t)

;; IDE para 
(use-package slime
  :ensure t)

(setq inferior-lisp-program "sbcl")

(use-package clojure-mode) 
(use-package cider)

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

;;(define-key org-mode (kbd "C-M-return") 'org-insert-item)

;; usar c++ con org-mode
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
                             (js . t)
			         (R . t)))
(org-indent-mode)
(org-latex-preview)

(use-package org-tree-slide)
(with-eval-after-load "org-tree-slide"
  (define-key org-tree-slide-mode-map (kbd "<left>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<right>") 'org-tree-slide-move-next-tree)
  )

;; Org Roam porque es más lindo tomar apuntes en esa cosa
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(use-package org-roam-ui
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package auctex)
(use-package cdlatex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; Me gusta que dired funcione en parte como evil
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
	     ;; ("j" . dired-next-line)
	     ;; ("k" . dired-previews-line)
	     ;; ("h" . dired-up-directory)
	     ;; ("/" . dired-goto-file)
	     ;; ("K" . dired-do-kill-lines)
	 ))

;; Quiero que se vea limpio por default, pero puedas tener la opción de
;; ver muchas cosas.
(use-package dired-hide-details
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-single)

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "feh")
				    ("mp4" . "mpv")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

;; (defun efs/exwm-update-class ()
;;   (exwm-workspace-rename-buffer exwm-class-name))

;; (use-package exwm
;;   :config
;;   ;; Set the default number of workspaces
;;   (setq exwm-workspace-number 10)

;;   ;; When window "class" updates, use it to set the buffer name
;;   ;; (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

;;   ;; These keys should always pass through to Emacs
;;   (setq exwm-input-prefix-keys
;; 	'(?\C-x
;; 	  ?\C-u
;; 	  ?\C-h
;; 	  ?\M-x
;; 	  ?\M-`
;; 	  ?\M-&
;; 	  ?\M-:
;; 	  ?\C-\M-j  ;; Buffer list
;; 	  ?\C-\ ))  ;; Ctrl+Space

;;   ;; Ctrl+Q will enable the next key to be sent directly
;;   (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;;   ;; Set up global key bindings.  These always work, no matter the input state!
;;   ;; Keep in mind that changing this list after EXWM initializes has no effect.
;;   (setq exwm-input-global-keys
;;         `(
;;           ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
;;           ([?\s-r] . exwm-reset)

;;           ;; Move between windows
;;           ([s-left] . windmove-left)
;;           ([s-right] . windmove-right)
;;           ([s-up] . windmove-up)
;;           ([s-down] . windmove-down)

;;           ;; Launch applications via shell command
;;           ([?\s-&] . (lambda (command)
;;                        (interactive (list (read-shell-command "$ ")))
;;                        (start-process-shell-command command nil command)))

;;           ;; Switch workspace
;;           ([?\s-w] . exwm-workspace-switch)

;;           ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
;;           ,@(mapcar (lambda (i)
;;                       `(,(kbd (format "s-%d" i)) .
;;                         (lambda ()
;;                           (interactive)
;;                           (exwm-workspace-switch-create ,i))))
;;                     (number-sequence 0 9))))

;;   (exwm-enable))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(defun line-breaker ()
  "Move a word to next line and be
at the end of the newly created line"
  (interactive)
  (backward-word)
  (open-line 1)
  (next-line)
  (move-end-of-line 1))

(toggle-truncate-lines t)
(global-set-key (kbd "C-ñ") 'line-breaker)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)

;;(setq compile-command "g++ ")

(setq newsticker-url-list
  '(("Mental Outlaw" "https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" nil nil nil)
    ("Hardcore History" "https://feeds.feedburner.com/dancarlin/history?format=xml" nil nil nil)
    ("Project Euler" "https://projecteuler.net/rss2_euler.xml" nil nil nil)
    ("Darknet Diaries" "https://feeds.megaphone.fm/darknetdiaries" nil nil nil)
    ("Sacha Chua" "https://sachachua.com/blog/category/emacs-news/feed/" nil nil nil)
    ("Prot CodeBlog" "https://protesilaos.com/codelog.xml" nil nil nil)
    ("Prot Commentary" "https://protesilaos.com/commentary.xml " nil nil nil)
    ("News Prot" "https://protesilaos.com/news.xml" nil nil nil)
    ("System Crafters" "https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" nil nil nil)
    ("LWN" "https://lwn.net/headlines/newrss" nil nil nil)
    ("Ars technica" "http://feeds.arstechnica.com/arstechnica/index/" nil nil nil)
    ("Hacker News" "https://news.ycombinator.com/rss" nil nil nil)
    ("DW" " https://rss.dw.com/rdf/rss-en-all" nil nil nil)
    ("Times" "https://feeds.feedburner.com/time/world" nil nil nil)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eterm-256color dired-hide-dotfiles dired-open dired-single all-the-icons-dired cdlatex auctex org-roam-ui yasnippet-snippets which-key visual-fill-column vertico use-package switch-window svelte-mode slime rainbow-delimiters pdf-tools org-tree-slide org-roam modus-themes magit lsp-mode ido-vertical-mode doom-modeline company cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
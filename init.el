;; Minimal UI
;;(scroll-bar-mode -1)
(tool-bar-mode   -1)
(setq-default fill-column 80)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-linum-mode t)
(column-number-mode 1)
(setq kill-whole-line t)
(global-hl-line-mode +1)
(setq-default indent-tabs-mode nil)

;;Disable the bell! Disable the bell! Disable the bell! Disable the bell! Disable the bell! Disable the bell! Disable the bell! Disable the bell! Disable the bell!
(setq visible-bell 1)

;; global todo list
(setq org-agenda-files (list "~/todo.org"))



;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)


;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))


(use-package snakemake-mode
  :ensure t)

(use-package google-this)
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package multi-term
  :ensure t
  :init
  (setq multi-term-program "/usr/bin/zsh"))

;; package does not exist?
;; (use-package bookmark+
;;   :ensure t)

(use-package lsp-mode
  :ensure t)
(use-package lsp-ui
  :ensure t)
(use-package lsp-haskell
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  )



;;Helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50)
  :config
  (helm-mode 1))


;; (use-package ranger
;;   :ensure t)


;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))


; open in browser
(use-package flymd
  :ensure t)





;; projectile, some kind of project system ( looks at git directories)
(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/src/"))
  )



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (lsp-docker lsp-haskell lsp-ui lsp-mode multi-term elm-mode helm-dash dash-docs magit ox-reveal helm-ag emamux-ghci- emamux-ghci ghc yaml-mode helm-projectile general hydra indent-tools helm-swoop emamux haskell-mode projectile-speedbar sr-speedbar snakemake-mode dockerfile-mode ein transpose-frame py-autopep8 elpy flycheck which-key use-package projectile helm doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; scroll one line at a time (less "jumpy" than defaults)    
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
    
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    
    (setq scroll-step 1) ;; keyboard scroll one line at a time

;;auto code folding
   (defun toggle-selective-display (column)
      (interactive "P")
      (set-selective-display
       (or column
           (unless selective-display
             (1+ (current-column))))))


(defun toggle-hiding (column)
      (interactive "P")
      (if hs-minor-mode
          (if (condition-case nil
                  (hs-toggle-hiding)
                (error t))
              (hs-show-all))
        (toggle-selective-display column)))


(load-library "hideshow")
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'yaml-mode-hook       'hs-minor-mode)


;; flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  )



;; python ide stuff
(use-package elpy
  :ensure t
  :defer t
  :init
  (defalias 'workon 'pyvenv-workon)
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (workon "default")
  (setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")
  (setq elpy-rpc-timeout 100
        elpy-disable-backend-error-display nil
        elpy-rpc-error-timeout 30)

)

(setq elpy-rpc-python-command "python3")




(use-package ein
    :ensure t)

;; (use-package py-autopep8
;;   :ensure t
;;   :init
;;   (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


;; frame swapping
(use-package transpose-frame :ensure t)


(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))




(use-package general
  :ensure t
  :config (general-define-key
  :prefix "C-c"
  "b"  '(helm-buffers-list :which-key "buffers list")
  "v" '(flymd-flyit :flymd) 
  "l"  'shrink-window-horizontally
  "k" 'backward-kill-line
  "f" 'elpy-format-code
  "C-z" 'elpy-shell-switch-to-shell
  )
  (general-define-key
   "M-x" 'helm-M-x
   "C-x b" '(helm-buffers-list :which-key "buffers list")
   "C-x C-f" 'helm-find-files
   "C-x r b" 'helm-filtered-bookmarks
   "C-c +" 'text-scale-increase
   "C-c -" 'text-scale-decrease
   "M-g M-f" 'first-error
   )
  )

;;  (global-set-key (kbd "M-x") 

(put 'downcase-region 'disabled nil)



(use-package sr-speedbar
  :ensure t
  :config
  (setq
   sr-speedbar-width 10
   speedbar-use-images nil)
  (when window-system
    (sr-speedbar-open))
  )

(use-package windmove
  ;; :defer 4
  :ensure t
  ;; :init
  ;; (when (fboundp 'windmove-default-keybindings)
  ;; (windmove-default-keybindings))
  :config
  ;; use command key on Mac
  ;;(windmove-default-keybindings 'control)
  ;; wrap around at edges
  (setq windmove-wrap-around t)
  (general-define-key
   "<left>" 'windmove-left
   "<right>" 'windmove-right
   "<up>" 'windmove-up
   "<down>" 'windmove-down
   )
  )

;; Make windmove work in Org mode:
(use-package org
  :ensure t
  :config
  (setq org-replace-disputed-keys t)
  (setq org-log-done 'time)
  )
;; (add-hook 'org-shiftup-final-hook 'windmove-up)
;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
;; (add-hook 'org-shiftright-final-hook 'windmove-right)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )


(use-package projectile-speedbar
  :ensure t
  )


(use-package haskell-mode
  :ensure t
  :pin melpa-stable
  )



(use-package ghc
  :ensure t
  ;; :config
  ;; (autoload 'ghc-init "ghc" nil t)
  ;; (autoload 'ghc-debug "ghc" nil t)
  ;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  )


 (use-package emamux
   :ensure t
   )


;; (start-process
;;    "unused"
;;    nil
;;    "xterm"
;;    "-e" "tmux" "new-session" "-n" "ghci" "-s" "haskell" "cabal repl"
;;    )
;; (setq emamux-ghci:tmux-address "haskell:ghci")

;; not in package library
;; (use-package emamux-ghci
;;   :ensure t
;;    )


(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(face lines-tail)
	whitespace-line-column 800
	show-trailing-whitespace t
	indicate-empty-lines t)
  (global-whitespace-mode t)
  )


(use-package helm-swoop
  :ensure t
  )

(use-package helm-projectile
  :ensure t
  :pin melpa-stable
  :config
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  )

(use-package yaml-mode
 :ensure t)


(use-package indent-tools
  :ensure t
  :config
  (add-hook 'python-mode-hook
 (lambda () (define-key python-mode-map (kbd "C-c >") 'indent-tools-hydra/body))
 )
  (add-hook 'yaml-mode-hook
 (lambda () (define-key python-mode-map (kbd "C-c >") 'indent-tools-hydra/body))
)
  )

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  )
  )

(setq hydra-examples-verbatim t)
;;** Example 2: move window splitter
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-splitter (global-map "C-M-;")
    "splitter"
    ("h" hydra-move-splitter-left)
    ("j" hydra-move-splitter-down)
    ("k" hydra-move-splitter-up)
    ("l" hydra-move-splitter-right)))

;;** Example 3: jump to error
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-error (global-map "M-g")
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit")))


(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))


  
(use-package sql
  :ensure t
  :config
  (add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
  )

(load "~/.emacs.d/database_setup.el")
(put 'scroll-left 'disabled nil)


(use-package magit
  :ensure t
  :pin melpa-stable
  )


(use-package helm-ag
  :ensure t
  )

(defun sqlparse-region (beg end)
  (interactive "r")
  (shell-command-on-region
   beg end
   "python -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), reindent=True))'"
   t t))


(add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))

;; Minimal UI
;;(scroll-bar-mode -1)
(tool-bar-mode   -1)
;;(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-linum-mode t)


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
  (load-theme 'doom-one-light t))


;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50)
  :config
  (helm-mode 1))


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
;;  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/src/")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (projectile-speedbar sr-speedbar py-autopep8 elpy flycheck which-key use-package projectile helm doom-themes))))
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


;; flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; python ide stuff
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
)


;(use-package ein
;  :ensure t)

(use-package py-autopep8
  :ensure t
  :init
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


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
  "h" 'helm-M-x
  )
  (general-define-key
   "M-x" 'helm-M-x
   "C-x C-f" 'helm-find-files)
   "C-x r b" 'helm-filtered-bookmarks
  )

;;  (global-set-key (kbd "M-x") 

(use-package sr-speedbar
  :ensure t
  :config
  (setq
   sr-speedbar-width 20
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
  (windmove-default-keybindings 'control)
  ;; wrap around at edges
  (setq windmove-wrap-around t))

;; Make windmove work in Org mode:
(use-package org
  :ensure t
  :config
  (setq org-replace-disputed-keys t)
  )
;; (add-hook 'org-shiftup-final-hook 'windmove-up)
;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
;; (add-hook 'org-shiftright-final-hook 'windmove-right)


(use-package projectile-speedbar
  :ensure t
  )

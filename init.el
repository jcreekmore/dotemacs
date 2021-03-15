;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(let* ((package--builtins nil)
       (packages
        '(company
          counsel-projectile   ;
          dash-at-point        ; 
          ebib                 ; Manage bibtex databases
          ivy                  ; Ivy Mode
          groovy-mode          ; Gimme Jenkinsfile support
          ledger-mode          ; Emacs Major mode for Ledger
          magit                ; control Git from Emacs
          markdown-mode        ; Emacs Major mode for Markdown-formatted files
          org                  ; Outline-based notes management and organizer
          ripgrep              ; Ripgrep mode
          rust-mode            ; Emacs Major mode for Rust
          pdf-tools            ; Emacs support library for PDF files
          projectile           ; Manage and navigate projects in Emacs easily
          swiper               ; Fancy iSearch interface
          terraform-mode       ; Emacs Major mode for Terraform files
          wgrep
          yaml-mode
          which-key)))         ; Display available keybindings in popup
  (ignore-errors ;; This package is only relevant for Mac OS X.
    (when (memq window-system '(mac ns))
      (push 'exec-path-from-shell packages)
      (push 'reveal-in-osx-finder packages))
    (let ((packages (remove-if 'package-installed-p packages)))
      (when packages
        ;; Install uninstalled packages
        (package-refresh-contents)
        (mapc 'package-install packages))))) 

(exec-path-from-shell-initialize)

;; Get my visuals all pretty
(load-theme 'deeper-blue t)
(cond ((member "Hack" (font-family-list))
       (set-face-attribute 'default nil :font "Hack-14"))
      ((member "Inconsolata" (font-family-list))
       (set-face-attribute 'default nil :font "Inconsolata-14")))

;; Get rid of crufty nonsense
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 79                    ; Maximum line width
              truncate-lines t                  ; Don't fold lines
              indent-tabs-mode nil              ; Use spaces instead of tabs
              split-width-threshold 160         ; Split verticly by default
              split-height-threshold nil        ; Split verticly by default
              auto-fill-function 'do-auto-fill) ; Auto-fill-mode everywhere

(setq auto-revert-interval 1            ; Refresh buffers fast
      default-input-method "TeX"        ; Use TeX when toggling input method
      echo-keystrokes 0.1               ; Show keystrokes asap
      inhibit-startup-message t         ; No splash screen please
      initial-scratch-message nil       ; Clean scratch buffer
      recentf-max-saved-items 100       ; Show more recent files
      ring-bell-function 'ignore        ; Quiet
      sentence-end-double-space nil)    ; No double space

;; Some mac-bindings interfere with Emacs bindings.
(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

(setq org-agenda-files '("~/docs"))

(define-key global-map "\C-cc"
  (lambda () (interactive) (org-capture)))

(define-key global-map (quote [f12])
  (lambda () (interactive) (magit-status)))

(define-key global-map "\M-o" 'occur)


(setq make-backup-files nil)
(setq auto-save-default nil)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/docs/inbox.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("w" "Work Log" entry (file+datetree "~/docs/work.org")
         "* %?\n   Entered on %T\n   %i\n")))

(require 'yaml-mode)
(require 'company)
(require 'terraform-mode)

(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'rust-mode-hook 'company-mode)
(add-hook 'rust-mode-hook 'display-fill-column-indicator-mode)


(add-hook 'python-mode-hook
          (lambda () (setq show-trailing-whitespace t)))

(global-display-line-numbers-mode)
(ivy-mode t)
(server-start)
(desktop-save-mode t)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(counsel-mode t)
(counsel-projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(wgrep dash-at-point groovy-mode yaml-mode which-key terraform-mode rust-mode ripgrep reveal-in-osx-finder pdf-tools ox-jira markdown-mode magit ledger-mode evil ein ebib counsel-projectile company))
 '(projectile-completion-system 'ivy)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "__pycache__" "build" "target" ".venv" ".mypy_cache"))
 '(projectile-ignored-projects '("~/work/taskerv2"))
 '(python-shell-completion-native-enable nil)
 '(python-shell-interpreter "python3.7"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'wgrep)

(global-set-key "\C-cd" 'dash-at-point)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper-backward)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

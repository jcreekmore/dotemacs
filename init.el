;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(let* ((package--builtins nil)
       (packages
        '(magit                ; control Git from Emacs
          markdown-mode        ; Emacs Major mode for Markdown-formatted files
          org                  ; Outline-based notes management and organizer
          rust-mode            ; Emacs Major mode for Rust
          pdf-tools            ; Emacs support library for PDF files
          projectile           ; Manage and navigate projects in Emacs easily
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
      custom-file (make-temp-file "")   ; Discard customization's
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

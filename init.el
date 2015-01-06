(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(package-initialize)

(ensure-package-installed
 'ace-jump-mode
 'ace-window
 'auctex
 'color-theme
 'elpy
 'expand-region
 'flymake-cursor
 'flymake-jslint
 'gist
 'haskell-mode
 'helm
 'helm-projectile
 'js2-mode
 'ledger-mode
 'linum-relative
 'magit
 'magit-annex
 'magit-filenotify
 'magit-gh-pulls
 'magit-svn
 'markdown-mode
 'org
 'python
 'projectile
 'yasnippet
 )

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq backup-inhibited t)

(setq custom-file (concat user-emacs-directory "customize.el"))
(load custom-file)

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(color-theme-initialize)
(color-theme-tty-dark)

(eval-after-load "linum"
  '(require 'linum-relative))

(global-set-key (kbd "<f12>") 'magit-status)

(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-assignment
                  (regexp . "\\(\\s-+\\)=\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-arrows
                  (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-left-arrows
                  (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))


(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'elpy)
(elpy-enable)


(require 'projectile)
(projectile-global-mode)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'flymake-jslint)
(add-hook 'js2-mode-hook 'flymake-jslint-load)
(add-hook 'js2-mode-hook 'linum-mode)

(require 'eshell)
(global-set-key (kbd "C-z") 'eshell)
(add-hook 'eshell-mode-hook
	  (lambda ()
            (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)
	    (local-set-key (kbd "C-z") 'bury-buffer)
	    (local-set-key (kbd "C-a") 'eshell-bol)
	    (local-set-key (kbd "<up>") 'previous-line)
	    (local-set-key (kbd "<down>") 'next-line)))
(defalias 'eshell/emacs 'find-file)
(defalias 'eshell/man 'woman)
(defun eshell/dired () (dired (eshell/pwd)))

(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

(require 'helm-projectile)
(helm-projectile-on)

(desktop-save-mode)
(server-start)
(put 'downcase-region 'disabled nil)

(require 'uniquify)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(require 'ace-window)
(global-set-key (kbd "<f8>") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(eval-after-load "flymake"
  '(require 'flymake-cursor))

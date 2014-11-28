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

(ensure-package-installed
 'ace-jump-mode
 'ace-window
 'ack-and-a-half
 'auctex
 'color-theme
 'expand-region
 'gist
 'haskell-mode
 'js2-mode
 'ledger-mode
 'linum-relative
 'magit
 'magit-annex
 'magit-filenotify
 'magit-gh-pulls
 'magit-svn
 'markdown-mode
 'python
 'yasnippet
 )

(package-initialize)

(tool-bar-mode -1)
(setq backup-inhibited t)
(ido-mode t)

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(color-theme-initialize)

(setq custom-file (concat user-emacs-directory "customize.el"))
(load custom-file)

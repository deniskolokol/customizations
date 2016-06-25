(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 307 t)
 '(aquamacs-tool-bar-user-customization
   (quote
    ((16777249 new-file open-file recent-files save-buffer aquamacs-print nil undo redo cut copy paste isearch-forward nil customize help))) t)
 '(auto-word-wrap-default-function nil)
 '(default-frame-alist
    (quote
     ((tool-bar-lines . 1)
      (fringe)
      (right-fringe)
      (left-fringe . 1)
      (internal-border-width . 0)
      (vertical-scroll-bars . right)
      (cursor-type . box)
      (menu-bar-lines . 1)
      (background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "yellow")
      (foreground-color . "white")
      (mouse-color . "sienna1"))))
 '(fill-column 80)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(ns-tool-bar-display-mode (quote icons) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(visual-line-mode nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(html-mode-default ((t (:inherit sgml-mode-default :height 120 :family "Monaco"))) t)
 '(python-mode-default ((t (:inherit autoface-default :height 120 :family "Monaco"))) t))
(color-theme-initialize)
(color-theme-dark-laptop)

;; electric mode ON - switch only if needed beyond Python! otherwise it is in python-mode
;; (electric-pair-mode 1)

;; toolbar OFF
(tool-bar-mode 0)

;; Layout for Python projects
(defun open-python-project ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (linum-mode 0)
  (next-multiframe-window)
  (linum-mode 0)
)
;; End layout for Python projects

(global-set-key (kbd "C-c p") 'open-python-project)

;; Check custom-file compatibility
(when (and (boundp 'aquamacs-version-id)
	   (< (floor (/ aquamacs-version-id 10))
	   (floor (/ aquamacs-customization-version-id 10))))
  (defadvice frame-notice-user-settings (before show-version-warning activate)
    (defvar aquamacs-backup-custom-file nil "Backup of `custom-file', if any.")
    (setq aquamacs-backup-custom-file "~/Library/Preferences/Aquamacs Emacs/customizations.2.1.el")
    (let ((msg "Aquamacs options were saved by a more recent program version.
Errors may occur.  Save Options to overwrite the customization file. The original, older customization file was backed up to ~/Library/Preferences/Aquamacs Emacs/customizations.2.1.el."))
      (if window-system
	  (x-popup-dialog t (list msg '("OK" . nil) 'no-cancel) "Warning")
	(message msg)))))
;; End compatibility check

;; ELPA
;; source in ~/.emacs.d/elpa
;; Requisites: Emacs >= 24
(require 'package)
(package-initialize)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(defvar to-install)
(setq to-install
      '(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository flycheck))

(mapc 'install-if-needed to-install)

(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(global-flycheck-mode t)

(global-set-key [f7] 'find-file-in-repository)

; auto-complete mode extra settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)

;; Python mode settings
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

;; Jedi settings
(require 'jedi)
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
;; (setq jedi:server-command
;;       '("python2" "/home/andrea/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))

(add-hook 'python-mode-hook
	  (lambda ()
	    (jedi:setup)
	    (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))

(add-hook 'python-mode-hook 'auto-complete-mode)

(ido-mode t)

;; YASnippets
(yas-global-mode 1)
(setq yas-snippet-dirs
      '("~/.emacs.d/elpa/yasnippet-20151011.1823/snippets/python-mode"))

;; -------------------- extra nice things --------------------
;; use control + cursor to move around windows
(require 'windmove)
(windmove-default-keybindings 'control)

(show-paren-mode t)

;; Turn beep off
(setq visible-bell nil)
(provide 'customizations)
;;; customizations.el ends here

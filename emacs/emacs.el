;;; package --- Emacs customisations

;; Warning: copy this to /home/$USER/.emacs

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
      '(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository flycheck exec-path-from-shell yaml-mode))

(mapc 'install-if-needed to-install)

(require 'magit)
(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(require 'exec-path-from-shell)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (sclang-extensions-mode exec-path-from-shell flycheck find-file-in-repository autopair jedi yasnippet magit python-mode yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "blue4" :box (:line-width 1 :color "medium blue") :family "\"DejaVu Sans Mono-10\"")))))

(global-flycheck-mode t)

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
;; Clone https://github.com/AndreaCrotti/yasnippet-snippets
;; and put snippets to ~/.emacs.d/snippets/
(yas-global-mode 1)

;; YAML mode hook
(require 'yaml-mode)
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Color theme
(load-theme 'tsdh-dark)

;; Disable toolbar
(tool-bar-mode -1)

;; Toggle fullscreen
(defun my-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth)) ;this makes the frame go fullscreen

(defun my-non-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'width 82)
  (set-frame-parameter nil 'fullscreen 'fullheight))

(defun toggle-fullscreen ()
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)  ;tests if already fullscreened
      (my-non-fullscreen)
    (my-fullscreen)))

;; Cua-mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;; Useful key bindings
(windmove-default-keybindings 'meta)

; isearch requires some customization to work with none default keys,
; since it uses its own keymap during a search.  These changes are *always*
; active, and not toggled with touchstream mode!  Luckly for us, the keys are
; we need are not used by isearch so there are no conflicts.
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-o") 'menu-find-file-existing)
(global-set-key (kbd "C-w") 'kill-buffer)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<f7>") 'find-file-in-repository)

;; Cosmetics
(global-linum-mode t)
(global-hl-line-mode t)
(column-number-mode t)

(provide '.emacs)
;;; emacs.el ends here

;;; package --- Emacs customisations

;; Warning: copy this to /home/$USER/.emacs

;; Copy to X-clipboard 
(defun my-copy-to-xclipboard(arg)
  (interactive "P")
  (cond
    ((not (use-region-p))
      (message "Nothing to yank to X-clipboard"))
    ((and (not (display-graphic-p))
         (/= 0 (shell-command-on-region
                 (region-beginning) (region-end) "xsel -i -b")))
      (error "Is program `xsel' installed?"))
    (t
      (when (display-graphic-p)
        (call-interactively 'clipboard-kill-ring-save))
      (message "Yanked region to X-clipboard")
      (when arg
        (kill-region  (region-beginning) (region-end)))
      (deactivate-mark))))

(defun my-cut-to-xclipboard()
  (interactive)
  (my-copy-to-xclipboard t))

(defun my-paste-from-xclipboard()
  "Uses shell command `xsel -o' to paste from x-clipboard. With
one prefix arg, pastes from X-PRIMARY, and with two prefix args,
pastes from X-SECONDARY."
  (interactive)
  (if (display-graphic-p)
    (clipboard-yank)
   (let*
     ((opt (prefix-numeric-value current-prefix-arg))
      (opt (cond
       ((=  1 opt) "b")
       ((=  4 opt) "p")
       ((= 16 opt) "s"))))
    (insert (shell-command-to-string (concat "xsel -o -" opt))))))

(global-set-key (kbd "C-c C-w") 'my-cut-to-xclipboard)
(global-set-key (kbd "C-c M-w") 'my-copy-to-xclipboard)
(global-set-key (kbd "C-c C-y") 'my-paste-from-xclipboard)


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
(global-set-key (kbd "C-x g") 'magit-status)

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
    (exec-path-from-shell flycheck find-file-in-repository autopair jedi yasnippet magit python-mode yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(global-set-key (kbd "<f11>") 'toggle-fullscreen) 

;; Cua-mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;; Useful key bindings
(windmove-default-keybindings 'control)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-w") 'kill-buffer-and-window)
(global-set-key (kbd "C-o") 'menu-find-file-existing)

; isearch requires some customization to work with none default keys,
; since it uses its own keymap during a search.  These changes are *always*
; active, and not toggled with touchstream mode!  Luckly for us, the keys are
; we need are not used by isearch so there are no conflicts.
(define-key isearch-mode-map [(control f)] 'isearch-repeat-forward)

(provide '.emacs)
;;; emacs.el ends here

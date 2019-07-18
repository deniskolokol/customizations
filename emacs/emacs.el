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
      '(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository flycheck exec-path-from-shell yaml-mode pyvenv pyenv-mode neotree))

(mapc 'install-if-needed to-install)

(require 'magit)
(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(require 'exec-path-from-shell)
(require 'w3m)

;; Neotree settings
(add-to-list 'load-path "~/Projects")
(require 'neotree)

;; SuperCollider mode settings
(add-to-list 'load-path "~/.emacs.d/vendor/sclang")
(require 'sclang)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (neotree calist w3m sclang pyenv-mode pyvenv sclang-extensions-mode exec-path-from-shell flycheck find-file-in-repository autopair jedi yasnippet magit python-mode yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "blue4" :box (:line-width 1 :color "medium blue") :family "\"DejaVu Sans Mono-10\"")))))

(global-flycheck-mode t)

; auto-complete mode extra settings
(setq
 ac-auto-start 3
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
(pyenv-mode)

;; Run ipython when possible.
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; Switch on ido mode
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

;; Move lines up/down.
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))
;; End of Move lines up/down.


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
  ; Test if already fullscreen.
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)
      (my-non-fullscreen)
    (my-fullscreen)))


;; Smart resizing windows
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the middle."
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the middle."
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))
;; End smart resizing windows


;; Layout for Python projects
(defun open-python-project ()
  (interactive)
  (delete-other-windows)
  (my-fullscreen)
  (split-window-horizontally)
  (split-window-vertically)
  (next-multiframe-window))

(global-set-key (kbd "C-x p") 'open-python-project)
;; End layout for Python projects


;; Layout for SuperCollider projects
(defun open-sclang-project ()
  (interactive)
  (delete-other-windows)
  (toggle-fullscreen)
  (sclang-start))

(global-set-key (kbd "C-x l") 'open-sclang-project)
;; End layout for Python projects


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
(global-set-key (kbd "C-w") 'kill-buffer)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "C-S-u") 'move-text-up)
(global-set-key (kbd "C-S-j") 'move-text-down)

;; Open files
(global-set-key (kbd "C-o") 'menu-find-file-existing)
(global-set-key (kbd "<f7>") 'find-file-in-repository)
(global-set-key (kbd "<f8>") 'neotree-toggle)

;; Resize windows
(global-set-key (kbd "M-S-<down>") 'win-resize-minimize-vert)
(global-set-key (kbd "M-S-<up>") 'win-resize-enlarge-vert)
(global-set-key (kbd "M-S-<left>") 'win-resize-minimize-horiz)
(global-set-key (kbd "M-S-<right>") 'win-resize-enlarge-horiz)
(global-set-key (kbd "M-S-<up>") 'win-resize-enlarge-horiz)
(global-set-key (kbd "M-S-<down>") 'win-resize-minimize-horiz)
(global-set-key (kbd "M-S-<left>") 'win-resize-enlarge-vert)
(global-set-key (kbd "M-S-<right>") 'win-resize-minimize-vert)

;; Python related
(global-set-key (kbd "C-c i") 'run-python)
(global-set-key (kbd "C-c v") 'pyvenv-activate)
(global-set-key (kbd "C-M->") 'py-shift-block-right)
(global-set-key (kbd "C-M-<") 'py-shift-block-left)

;; Cosmetics
(global-linum-mode t)
(global-hl-line-mode t)
(column-number-mode t)
(display-time-mode 1)

(provide '.emacs)
;;; emacs.el ends here

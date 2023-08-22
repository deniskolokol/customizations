;;; package --- Emacs customisations

;;; Commentary:
;; copy this to /home/$USER/.emacs

;;; Code:

;; Kill process buffer without confirmation
(setq kill-buffer-query-functions nil)

;; ELPA
;; source in ~/.emacs.d/elpa
;; Requisites: Emacs >= 24
(require 'package)
(package-initialize)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Run (package-refresh-contents) on first install each time.
;; Solution taken from https://github.com/jwiegley/use-package/issues/256 (raxod502)
(defun my-package-install-refresh-contents (&rest args)
  (package-refresh-contents)
  (advice-remove 'package-install 'my-package-install-refresh-contents))

(advice-add 'package-install :before 'my-package-install-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(defvar to-install)
(setq to-install
      '(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository flycheck exec-path-from-shell yaml-mode virtualenvwrapper dockerfile-mode all-the-icons realgud sudo-edit deft neotree))

(mapc 'install-if-needed to-install)

(require 'magit)
(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(require 'dockerfile-mode)
(require 'exec-path-from-shell)
(require 'w3m)
(require 'all-the-icons)
(require 'deft)


;; Neotree settings
(require 'neotree)
(setq neo-window-fixed-size nil)
(neotree-dir "~/Projects")

;; Set the neo-window-width to the current width of the
;; neotree window, to trick neotree into resetting the
;; width back to the actual window width.
;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
(eval-after-load "neotree"
  '(add-to-list 'window-size-change-functions
		(lambda (frame)
		  (let ((neo-window (neo-global--get-window)))
		    (unless (null neo-window)
		      (setq neo-window-width (window-width neo-window)))))))

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
    (deft realgud sudo-edit dockerfile-mode all-the-icons calist w3m sclang virtualenvwrapper sclang-extensions-mode exec-path-from-shell flycheck find-file-in-repository autopair jedi yasnippet magit python-mode yaml-mode neotree))))


;; DARK
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "blue4" :box (:line-width 1 :color "medium blue"))))))

;; Color theme
(load-theme 'tsdh-dark t)

;; Black transparent background.
(set-background-color "gray1")
(add-to-list 'default-frame-alist '(background-color . "gray1"))
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))

;; ;; LIGHT
;; ;; Color theme
;; (load-theme 'adwaita t)


;; Deft settings
(setq deft-recursive t)
(setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")
        (case-fn . downcase)))

;; Switch on global flycheck
(global-flycheck-mode t)

;; Auto-complete mode extra settings
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


;; Run ipython when possible.
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; Enable IDO everywhere.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
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


;; Move buffer to other window.
;; after c-x 3 to split screen this lets you move buffers between sides.
;; altered code from:
;; http://stackoverflow.com/questions/1774832/how-to-swap-the-buffers-in-2-windows-emacs
(defun swap-buffer-window ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this)))
    (set-window-buffer other this-buffer)
    (tabbar-close-tab) ;;close current tab
    (other-window 1) ;;swap cursor to new buffer
    )
  )


;; Layout for Python projects
(defun open-python-project ()
  "Layout for Python projects."
  (interactive)
  (delete-other-windows)
  (my-fullscreen)
  (split-window-horizontally)
  (next-multiframe-window)
  (set-frame-width (selected-frame) 1200)
  (split-window-horizontally)
  (next-multiframe-window)
  (set-frame-width (selected-frame) 600)
  (split-window-vertically)
  (next-multiframe-window)
  (eshell)
  )


;; Virtualenvwrapper settings
;; See reference at https://github.com/porterjamesj/virtualenvwrapper.el
(require 'virtualenvwrapper)
(venv-initialize-eshell)
(setq venv-location "~/venv")


;; Layout for SuperCollider projects
(defun open-sclang-project ()
  "Layout for SuperCollider projects."
  (interactive)
  (delete-other-windows)
  (my-fullscreen)
  (split-window-horizontally)
  (next-multiframe-window)
  (set-frame-width (selected-frame) 100)
  (sclang-start)
  (Previous-multiframe-window)
  )


;; Cua-mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;; Useful key bindings
(windmove-default-keybindings 'meta)

; isearch requires some customization to work with none default keys,
; since it uses its own keymap during a search. These changes are *always*
; active, and not toggled with touchstream mode! Luckly for us, the keys are
; we need are not used by isearch so there are no conflicts.
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)

;; Buffers
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "M-b") 'switch-to-buffer)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "C-S-u") 'move-text-up)
(global-set-key (kbd "C-S-j") 'move-text-down)
(global-set-key (kbd "C-c d") 'deft)

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

;; Move windows
(global-set-key (kbd "C-x /") 'swap-buffer-window)
(global-set-key (kbd "M-\-") 'split-window-below)
(global-set-key (kbd "M-\\") 'split-window-right)

;; Python related
(global-set-key (kbd "C-x p") 'open-python-project)
(global-set-key (kbd "C-c i") 'run-python)
(global-set-key (kbd "C-c v") 'venv-workon)
(global-set-key (kbd "C-x v") 'venv-deactivate)
(global-set-key (kbd "C-M->") 'py-shift-block-right)
(global-set-key (kbd "C-M-<") 'py-shift-block-left)

;; Yasnippet cheat-sheet
(global-set-key (kbd "C-M-y") 'yas/describe-tables)

;; Supercollider Related
(global-set-key (kbd "C-x l") 'open-sclang-project)
(global-set-key (kbd "M-<return>") 'sclang-eval-region-or-line)
(global-set-key (kbd "C-x .") 'sclang-main-stop)

;; Various
(global-set-key (kbd "C-e") 'eval-buffer) ;; i.e. `execute`
(global-set-key (kbd "C-S-b") 'list-buffers)
(global-set-key (kbd "C-S-c") 'switch-to-buffer)

;; Navigation
(global-set-key (kbd "C-s-<right>") 'move-end-of-line)
(global-set-key (kbd "C-s-<left>") 'move-beginning-of-line)

;; Little help for python-mode
; M-k - delete until the end of block

;; Cosmetics
(global-hl-line-mode t)
(column-number-mode t)
(display-time-mode 1)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(all-the-icons-icon-for-mode 'cua-mode)

;; Make things a little more responsive in general.
(setq echo-keystrokes 0.1
      tooltip-delay 0
      tooltip-short-delay 0)

;; Only ask for y or n rather than yes or no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable toolbar
(tool-bar-mode -1)

(desktop-save-mode 1)

(provide '.emacs)
;;; emacs.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's nt-windows & gnu/linux .emacs
;;;; updated: 2014-08-08
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; initial setup
;; removing the gui elements first keeps them from showing on startup
(when window-system
  (progn
    (tool-bar-mode 0)
    (scroll-bar-mode 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup and load external packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; os-based settings
;; maximize the frame on startup
(cond
    ((string-equal initial-window-system "w32")
    (progn
      ;(w32-send-sys-command #xf030) ; nt command for maximizing a window
      (set-face-attribute 'default nil :font "Terminus-12")
      (setq default-directory "c:/Users/jeff.tecca/")))
  ((string-equal initial-window-system "x") ; emacs running in an x window
   (progn 
     (set-face-attribute 'default nil :font "Ubuntu Mono-11")
     (setq default-directory "~/")
     (hl-line-mode t)
     ))
  ((string-equal initial-window-system "nil") ; running in a term
   (setq default-directory "~/")))

;; commented out to see if this breaks anything since i have
;; switched to using python-mode.el instead of python.el
;; set the default python shell to ipython
;; (cond
;;     ((string-equal system-type "windows-nt")
;;     (progn
;;       (setq
;;        python-shell-interpreter "C:\\Python33\\python.exe"
;;        python-shell-interpreter-args "-i C:\\Python33\\Scripts\\ipython3-script.py" )      
;;       (setq
;;        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;        python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
;;        python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
;;        python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
;;     'setup-ipython-windows)
;;   ((string-equal system-type "gnu/linux")
;;    (progn
;;      (setq
;;       python-shell-interpreter "ipython")
;;       ;python-shell-interpreter-args ""
;;       ;python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;       ;python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;       ;python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
;;       ;python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
;;       ;python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;;      'setup-ipython-gnu/linux)))

;; setup apsell for spell checking
;; M-$ is the default keybinding for it
(cond
 ((string-equal system-type "windows-nt")
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin")
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict")
  (require 'ispell)
  ;; set flyspell-mode invocation to C-$
  (global-set-key (kbd "C-$") 'flyspell-mode))
((string-equal system-type "gnu/linux")
 (global-set-key (kbd "C-$") 'flyspell-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; os-agnostic settings
;; here's a nifty auto compile of .emacs after a save
;; stolen from: http://www.emacswiki.org/emacs/AutoRecompile
(defun compile-dotemacs ()
  "compile .emacs automagically on saving the .emacs file"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
        (byte-compile-file dotemacs))))
(add-hook 'after-save-hook 'compile-dotemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom keybindings

;; rebind C-x o to M-o for faster buffer switching
(global-set-key "\M-o" 'other-window)
;; rebind undo (C-x u) to M-u
;; C-/ is also usable as an undo key
;; this overwrites the upcase-word binding, but I rarely use that
(global-set-key "\M-u" 'undo)
;; bind some functions for getting around more efficiently
(global-set-key (kbd "<f12>") 'find-function)
; <f11> will be bound to fullscreen the frame
; <f10> is bound to the menu bar
(global-set-key (kbd "<f9>") 'imenu) ; icicles will help with completions

;;;;;;;;;;;;;;;;;;
;; markdown settings
;; set markdown filetypes
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.info\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;
;;;; cosmetic customizations
(setq frame-title-format "emacs - %b")
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq visible-bell t)
(global-visual-line-mode 1)
(eldoc-mode t) ; why isn't this always on?

;;;;;;;;;;;;;;;;;;
;;;; autosave/backup/file options
(setq auto-save-default nil)
(recentf-mode 1)
(setq make-backup-files t)
(setq delete-old-versions t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq delete-by-moving-to-trash t)

;;;;;;;;;;;;;;;;;;
 ;;;; general editor setting
(ido-mode 1) ; because ido
(windmove-default-keybindings 'ctrl)
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(line-number-mode 1)
(column-number-mode 1)
(toggle-word-wrap)
(setq next-line-add-newlines t)
(setq-default fill-column 72) ; this controls the fcl-mode line
(require 'fill-column-indicator)
(fci-mode t)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(delete-selection-mode t)

;;;;;;;;;;;;;;;;;;
;; use ido for finding files and switching buffers
;; icicles has problems with autocomplete, and has a hard time finding
;; $HOME on windows, but ido works well across both linux and win
;; note that this needs to be set after icicles so it doesn't clobber
;; the keybinding
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
;; note that C-x C-b still opens ibuffer

;; auto-complete setup
;; trying turning off auto-complete because I think it is conflicting with jedi
;; in strange, non-fatal ways
(require 'auto-complete)
(global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;
;; org-mode settings
;; make each new layer indent for easier reading
(add-hook 'org-mode-hook '(lambda () (org-indent-mode t)))
;; set the possible task keywords
(setq org-todo-keywords '((sequence "TODO" "WORKING" "STOPPED" "REVIEW" "DONE")))

;;;;;;;;;;;;;;;;;;
;;;; python-specific settings
(require 'python-mode)
(add-hook 'python-mode-hook 'set-linum-mode-hook)
; use ipython3 as the default interpreter
(setq-default py-shell-name "ipython3")
(setq-default py-which-bufname "IPython")(setq py-smart-indentation)
(setq py-split-windows-on-execute-p nil) ; this is frustrating with this on
(setq py-force-py-shell-name-p t)
(setq py-smart-indentation t)
;; bind the breakpoint function to C-c i(nsert breakpoint)
(add-hook 'python-mode-hook '(lambda () (local-set-key (kbd "C-c i") 'python-insert-breakpoint)))
;; jedi hooks
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set custom functions
(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (+ (face-attribute 'default :height) 10)))

(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (- (face-attribute 'default :height) 10)))

(defun insert-date (arg)
"inserts the current date into the buffer.  if called with an arg, changes the format to the windows-style format."
  (interactive "P")
  (insert (if arg
              (format-time-string "%m/%d/%Y")
            (format-time-string "%Y-%m-%d"))))
  
(defun insert-time ()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun insert-datetime (arg)
"inserts the date and time into the buffer.  if called with an arg, changes the date format to the windows-style format."
(interactive "P")
(insert (if arg
            (format-time-string "%m/%d/%Y %H:%M:%S")
          (format-time-string "%Y-%m-%d %H:%M:%S"))))

(defun set-linum-mode-hook ()
  (linum-mode 1))

(defun python-insert-breakpoint ()
  "Inserts a breakpoint to the buffer and highlights all other breakpoints in the buffer."
  (interactive)
  (insert "import pdb; pdb.set_trace()")
  (highlight-lines-matching-regexp "^[]*import pdb; pdb.set_trace()"))

(defun make-frame-fullscreen ()
  "If not running Emacs in a terminal (through a window manager), f11 maximizes the frame. otherwise returns nil."
  (interactive)
  (cond
      ((string-equal system-type "windows-nt")
      (w32-send-sys-command #xf030)) ;; 0xf030 is the command for maximizing a window)
      ((string-equal system-type "gnu/linux")
       (progn 
         (set-frame-parameter nil 'fullscreen
                              (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))))

;; lifted from www.masteringemacs.org
(defun revert-this-buffer ()
  "Reloads (reverts) the current buffer"
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer: " (buffer-name))))

;; custom keybinding hooks
;; commented because these functions have been removed as of 2014-08-04
;; (defun set-vetting-keybinds ()
;;   (local-set-key (kbd "C-c a") 'format-kill-point)
;;   (local-set-key (kbd "C-c t") 'add-gpx-header-template))
;; (add-hook 'xml-mode-hook 'set-vetting-keybinds)

;; global custom keybindings
(global-set-key (kbd "<f5>") 'revert-this-buffer)
; below is less bulky than C-x z, z... for repeating, more like vim
(global-set-key (kbd "C-z") 'repeat) 
(global-set-key (kbd "<f11>") 'make-frame-fullscreen)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; smex bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "M-z") 'execute-extended-command) ; because i don't use zap-to-char and this lets me be sloppier
;; alternative keybindings for M-x
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)
; -------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes (quote ("454dc6f3a1e9e062f34c0f988bcef5d898146edc5df4aa666bf5c30bed2ada2e" default)))
 '(delete-selection-mode t)
 '(initial-scratch-message "")
 '(org-CUA-compatible nil)
 '(recentf-mode t)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


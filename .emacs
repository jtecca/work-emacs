;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's nt-windows & gnu/linux .emacs
;;;; updated: 2014-08-12
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
      (set-face-attribute 'default nil :font "Ubuntu Mono-12")
      (setq default-directory "c:/Users/jeff.tecca/")))
  ((string-equal initial-window-system "x") ; emacs running in an x window
   (progn 
     (set-face-attribute 'default nil :font "Ubuntu Mono-11")
     (setq default-directory "~/")
     (hl-line-mode t)
     ))
  ((string-equal initial-window-system "nil") ; running in a term
   (setq default-directory "~/")))

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
(global-set-key (kbd "<f9>") 'imenu)
(global-set-key (kbd "<f5>") 'revert-this-buffer)
; below is less bulky than C-x z, z... for repeating, more like vim
(global-set-key (kbd "C-z") 'repeat) 
(global-set-key (kbd "<f11>") 'make-frame-fullscreen)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; smex bindings
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "M-z") 'smex) ; because i don't use zap-to-char and this lets me be sloppier
;; alternative keybindings for M-x
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

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
(eldoc-mode t)

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
(tooltip-mode -1)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)
(setq redisplay-dont-pause t)


;;;;;;;;;;;;;;;;;;
;; use ido for finding files and switching buffers
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
;; note that C-x C-b still opens ibuffer

;;;;;;;;;;;;;;;;;;
;; org-mode settings
;; make each new layer indent for easier reading
(add-hook 'org-mode-hook '(lambda () (org-indent-mode t)))
;; set the possible task keywords
(setq org-todo-keywords '((sequence "TODO" "WORKING" "STOPPED" "REVIEW" "DONE")))

;;;;;;;;;;;;;;;;;;
;;;; python-specific settings
(cond
    ((string-equal initial-window-system "w32")
    (progn
      (setq python-command "ipython2")))
  ((string-equal initial-window-system "x") ; emacs running in an x window
   (progn 
     (setq python-command "ipython3")
     )))
(add-hook 'python-mode-hook 'set-linum-mode-hook)
;; ;; bind the breakpoint function to C-c i(nsert breakpoint)
(add-hook 'python-mode-hook '(lambda () (local-set-key (kbd "C-c i") 'python-insert-breakpoint)))

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
  "Reloads (reverts) the current buffer to its saved state in a file"
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer: " (buffer-name))))

; -------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes (quote ("49d35b72a2eff94e674ff93ef8b699e832b6cd4795acc63194320c37e746d9e8" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "454dc6f3a1e9e062f34c0f988bcef5d898146edc5df4aa666bf5c30bed2ada2e" default)))
 '(delete-selection-mode t)
 '(elpy-rpc-python-command "python2")
 '(fci-rule-color "#383838")
 '(initial-scratch-message "")
 '(org-CUA-compatible nil)
 '(recentf-mode t)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )






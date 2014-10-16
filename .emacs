;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's nt-windows & gnu/linux .emacs
;;;; updated: 2014-10-15
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
      (set-face-attribute 'default nil :font "Cousine-10")
      (setq default-directory "c:/Users/jeff.tecca/")))
  ((string-equal initial-window-system "x") ; emacs running in an x window
   (progn
     (set-face-attribute 'default nil :font "Ubuntu Mono-11")
     (setq default-directory "~/")
     (set-language-environment "utf-8")))
   ((string-equal initial-window-system "nil") ; running in a term
    (setq default-directory "~/")))

;; lisp
(cond
 ((string-equal initial-window-system "w32")
  (progn
    (setq inferior-lisp-program "wx86cl64")))
 ((string-equal initial-window-system "x")
  (progn
    (setq inferior-lisp-program "sbcl"))))
;; (add-to-list 'load-path (expand-file-name "~/quicklisp/slime-helper.el"))
;; (require 'slime)
;; (slime-setup '(slime-fance))

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
;; custom global keybindings
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
;; make C-x C-k another kill buffer, because I can't type
(global-set-key (kbd "C-x C-k") 'ido-kill-buffer)
;; rebind C-x o to M-o for faster buffer switching
(global-set-key (kbd "M-o") 'other-window)
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
;; alternative keybindings for M-x
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
;; copy-line keybinding
(global-set-key (kbd "C-c k") 'copy-line)

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
(setq visible-bell nil)
(global-visual-line-mode 1)
(eldoc-mode t)
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)
(setq cursor-type 'box)
(highlight-numbers-mode t)

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
(require 'ido)
(ido-mode t)
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(line-number-mode t)
(column-number-mode t)
(toggle-word-wrap t)
(setq next-line-add-newlines t)
(setq-default fill-column 79)
(require 'fill-column-indicator)
(fci-mode t)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(delete-selection-mode t)
(tooltip-mode -1)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)
(setq redisplay-dont-pause t)
(require 'auto-complete)
;; TODO turn this on for elisp mode
(global-auto-complete-mode t)
(global-linum-mode t)

;;;;;;;;;;;;;;;;;;
;; paredit settings
;; note that this is required for both linux and windows, since elisp tends
;; to get written on both types of oses
(autoload 'enable-paredit-mode "paredit" "Turn on structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
;; make eldoc aware of paredit
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;;;;;;;;;;;;;;;;;;
;; org-mode settings
;; make each new layer indent for easier reading
(add-hook 'org-mode-hook '(lambda () (org-indent-mode t)))
;; set the possible task keywords
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "STOPPED" "REVIEW" "DONE")))
;; update counts after removing a line from an org todo-list
(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;;;;;;;;;;;;;;;;;;
;;;; python settings
;; windows settings, assumes that you have the Anaconda distribution installed
;; to the default location
(setq
 python-shell-interpreter "C:\\Users\\jeff.tecca\\AppData\\Local\\Continuum\\Anaconda\\python.exe"
 python-shell-interpreter-args
 "-i C:\\Users\\jeff.tecca\\AppData\\Local\\Continuum\\Anaconda\\Scripts\\ipython-script.py"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;; custom python keybindings
(add-hook 'python-mode-hook
          '(lambda ()
             (turn-on-fci-mode)
             (local-set-key (kbd "C-c i") 'python-insert-breakpoint)
             (local-set-key (kbd "<f1>") 'magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define custom functions
(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (+ (face-attribute 'default :height) 10)))

(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (- (face-attribute 'default :height) 10)))

(defun insert-date (arg)
"inserts the current date into the buffer.

if called with an arg, changes the format to the windows-style format."
  (interactive "P")
  (insert (if arg
              (format-time-string "%m/%d/%Y")
            (format-time-string "%Y-%m-%d"))))

(defun insert-time ()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun insert-datetime (arg)
"inserts the date and time into the buffer.

if called with an arg, changes the date format to the windows-style format."
(interactive "P")
(insert (if arg
            (format-time-string "%m/%d/%Y %H:%M:%S")
          (format-time-string "%Y-%m-%d %H:%M:%S"))))

(defun prettify-json-document ()
"Runs a python module to prettify a selected JSON document.

A region must be used to highlight the JSON document to be parsed, otherwise nothing is parsed."
  (interactive)
  (if (use-region-p)
        (shell-command-on-region (region-beginning) (region-end) "python -mjson.tool" nil t)))

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

(defun copy-line ()
  "Copies the entire line to the kill-ring without killing the text.  
Intended to behave like vi's 'yy' command."
  (interactive)
  (save-excursion
    (let ((bol (progn (back-to-indentation) (point)))
          (eol (progn (end-of-visual-line) (point))))
      (kill-ring-save bol eol)))
  (message "Copied line to kill-ring"))

; -------------------------------------------

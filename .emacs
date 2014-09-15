;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's nt-windows & gnu/linux .emacs
;;;; updated: 2014-09-15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; required packages:
;; auto-complete
;; concurrent
;; church-theme
;; fill-column-indicator
;; idomenu
;; magit
;; markdown-mode
;; paredit
;; popup
;; rainbow-delimiters
;; smex
;; smooth-scrolling
;; tree-mode
;; undo-tree
;; websocket

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
      (set-face-attribute 'default nil :font "Cousine Bold-10")
      (setq default-directory "c:/Users/jeff.tecca/")))
  ((string-equal initial-window-system "x") ; emacs running in an x window
   (progn 
     (set-face-attribute 'default nil :font "Ubuntu Mono-11")
     (setq default-directory "~/")
     (set-language-environment "utf-8")
     ;; lisp/slime setup
     (setq inferior-lisp-program "sbcl")
     (add-to-list 'load-path (expand-file-name "~/slime/"))
     (require 'slime))
   (slime-setup '(slime-fancy))
     
   ((string-equal initial-window-system "nil") ; running in a term
    (setq default-directory "~/"))))

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
(global-set-key "\C-x \C-k" 'ido-kill-buffer)
;; rebind C-x o to M-o for faster buffer switching
(global-set-key "\M-o" 'other-window)
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
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(require 'smooth-scrolling)
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
;(require 'auto-complete)
;; TODO turn this on for elisp mode
;(global-auto-complete-mode t)
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
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
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
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-enabled-themes (quote (church)))
 '(custom-safe-themes (quote ("4c4c7d19b9a3844da4b499664ff2be2edd44e56303e896936329b21f657d75fa" "45d3aa79d35b2b3e31e6f6c8e79bc8e8a8a4183b4294ff41c8027f26e42dbe5a" "6036c315dd7f9d8f930d9c7fe0e46a6b8942736f0a1f3e3c060d5324dc7d3a86" "6ba69d237b3e2465492b87f4836ab1b82e324056eafae6aaa3ebde9b2cd5ec7c" "3fdd718695d948ca0576bea245af183f053b01567ea2b5da7266b564ad07a5ac" "5699a205d01cfa88ed9292e2244e26b82a3f55bd18caecbba90f376efe31f52e" "1e00c42b7ae2d826e233b65b57822579a0dd7b9cd95b7a5eef94555d6f507b09" "950a135f35029110f8c938dad4ff5c35f77a038995b6b8bba98b65dae1c3cabe" "04fc5453daaba865d6ae45f4e45c8a50e4167a008d9481a0aafe0de7f6b237da" "eaa0610de527691a0bd4648e0541821bca3478ae9d8bb957cf8995de7805800e" "53766ce623ded9a5677ebbaeca49b52e6c9be6b678c8240906954e2d01b72c9e" "028ad3c88a0c9e0e21946617871318e91c337e8a43f50644b78619865d130691" "9d321de2c3f777c6edea494c77337bb0c230ea12032a4c0c0125a7b2fdea25a3" "b8e3c880be5aeee7f0134fb241a9ba9636bf63a9827249474b471ac7c3e8ef93" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" default)))
 '(fci-rule-color "#383838")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

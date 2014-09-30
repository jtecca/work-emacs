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
(add-hook 'slime-repl-mode-hook '(lambda () (paredit-mode t)))
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-enabled-themes (quote (church)))
 '(custom-safe-themes (quote ("0969c6c3dd7ea3a806cf9c02e880ae8eb25fa3c5acda384c831f16d997e5969e" "bf022cbe1e77f694439857b812eedcb320313ef725e1bdddb980a4630888db3a" "47c4286586da078b8946889f008276d209022df41cca28a2b4d3147f5b450162" "953263458f4b251401274b2224aa0804191cb90655976a9e852ea4ca9b76d398" "3d381872a4502adbc9db1b46a270bc4fbff5202ed5bb9d5e95d15491ed5601eb" "891c439a6dc3f077345bf773b2a9bfafdd4cefeab08d13ee700ba9ea66f5aa62" "85968d3e525a9dddde055088e65e78c0a1fc5737b8e4ae906ecdacef76341ee8" "aebbabdcf83ef29c6384eb6d75b6f8da05bcf4c6c8b89e762670a476cc3af55a" "afd805c5e6d682741bd1f3e52047c10deda38aed85f16f37d97c31b226245952" "cb7e81fd18e1c43469c6bb529cce9268fe452cce01326fc625d7e2071c81b4c1" "31dc7bc9a83e27bcefc10d0d6e7001c644cdbdc5b2955e775cf18bc138dab7d9" "7b0df3af11ea466aa1831754b74388d96a4b64ef98971cc7126c4b35c8cc60df" "a53d74bd879773bcea6e85257f78e67fdb61f0c6ec4e648acdff0a7dc879c6c9" "5c98366f283b97e43b6576296663532620f47851364df9875a6af354cee73a89" "b9278adb8f29c8b57e30bf925eaa9134c08420d31ad82bdffb2c3d803152588c" "a951e552a76ca104c48ee9cfd68fc2386db5660e880d54ee33882cf4e486f591" "4c36bc42ca033ed989ffa34e48679b2a6e6c97aa4d50e3619338296d20b883f8" "0e2966df314f1fd3ab00484ac3f8a59cfbf10157ce1c927311c2c0a79fb7bbff" "fbab2780a309fed6c7000d522de70e95ca4812fde9e54931b577a9e07b2d1641" "b23121deed3790fbb1bc567ca2a0b04de733143fa06a99124ed2f1c4ee811c90" "0bce907b038c49bac4e60c329c084c29d4f88347352642f443be716f733fe2f5" "e791713b8d918c7f3c6fd499f0cd87b9f40e03c3ae882ff6057b2cd2d13a35af" "d8dbcb1fa5a26ff351f104f8144b00498819ab1a9b053eb98cceb0bf1a02b9a0" "4c4c7d19b9a3844da4b499664ff2be2edd44e56303e896936329b21f657d75fa" "45d3aa79d35b2b3e31e6f6c8e79bc8e8a8a4183b4294ff41c8027f26e42dbe5a" "6036c315dd7f9d8f930d9c7fe0e46a6b8942736f0a1f3e3c060d5324dc7d3a86" "6ba69d237b3e2465492b87f4836ab1b82e324056eafae6aaa3ebde9b2cd5ec7c" "3fdd718695d948ca0576bea245af183f053b01567ea2b5da7266b564ad07a5ac" "5699a205d01cfa88ed9292e2244e26b82a3f55bd18caecbba90f376efe31f52e" "1e00c42b7ae2d826e233b65b57822579a0dd7b9cd95b7a5eef94555d6f507b09" "950a135f35029110f8c938dad4ff5c35f77a038995b6b8bba98b65dae1c3cabe" "04fc5453daaba865d6ae45f4e45c8a50e4167a008d9481a0aafe0de7f6b237da" "eaa0610de527691a0bd4648e0541821bca3478ae9d8bb957cf8995de7805800e" "53766ce623ded9a5677ebbaeca49b52e6c9be6b678c8240906954e2d01b72c9e" "028ad3c88a0c9e0e21946617871318e91c337e8a43f50644b78619865d130691" "9d321de2c3f777c6edea494c77337bb0c230ea12032a4c0c0125a7b2fdea25a3" "b8e3c880be5aeee7f0134fb241a9ba9636bf63a9827249474b471ac7c3e8ef93" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" default)))
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

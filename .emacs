;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's .emacs
;;;; updated: 2015-08-10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  (progn
    (tool-bar-mode 0)
    (scroll-bar-mode 0)))

; list of site-specific customizations
(setq custom-file  (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup and load external packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
;; lightweight package manager (assumes emacs version > 24)
;; maybe look into cask in the future
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 '(avy company company-c-headers company-jedi diminish delight
       evil evil-smartparens evil-leader f
       fill-column-indicator ggtags helm helm-gtags helm-projectile
       highlight-numbers hydra magit markdown-mode projectile
       rainbow-delimiters rainbow-mode s seq slime smartparens
       sr-speedbar smart-mode-line swiper swiper-helm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define custom functions
(defun file-to-string (filepath)
  "Opens a file and returns its contents as a string (with newlines, if any)."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (+ (face-attribute 'default :height) 10)))

(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (- (face-attribute 'default :height) 10)))

(defun insert-date (arg)
"Inserts the current date into the buffer.

if called with an arg, changes the format to the windows-style format."
  (interactive "P")
  (insert (if arg
              (format-time-string "%m/%d/%Y")
            (format-time-string "%Y-%m-%d"))))

(defun insert-time ()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun insert-datetime (arg)
"Inserts the date and time into the buffer.

if called with an arg, changes the date format to the windows-style format."
(interactive "P")
(insert (if arg
            (format-time-string "%m/%d/%Y %H:%M:%S")
          (format-time-string "%Y-%m-%d %H:%M:%S"))))

(defun compress-region (reg-start reg-end)
  "Reduces multiple lines in a selected region down to one.
  Does nothing if there is no active region.
  Can do awful things with Paredit-like modes, be careful."
  (interactive "r")
  (if (use-region-p)
      (while (> (count-lines reg-start reg-end) 1)
        (call-interactively 'delete-indentation))))

(defun prettify-json-document ()
  "Runs a python module to prettify a selected JSON document.
  A region must be used to highlight the JSON document to be parsed,
  otherwise nothing is parsed."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region (region-beginning) (region-end) "python -m json.tool" nil t)))

(defun python-insert-breakpoint ()
  "Inserts a breakpoint to the buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(defun make-frame-fullscreen ()
  "If not running Emacs in a terminal (through a window manager),
  f11 maximizes the frame. otherwise returns nil."
  (interactive)
  (cond
      ((string-equal system-type "windows-nt")
      (w32-send-sys-command #xf030))
      ((string-equal system-type "gnu/linux")
       (progn
         (set-frame-parameter nil 'fullscreen
                              (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))))

(defun revert-this-buffer ()
  "Reloads (reverts) the current buffer to its saved state in a file.
lifted from www.masteringemacs.org"
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

(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current line.
With negative prefix, apply to -N lines above.
Shamelessly stolen from:
http://endlessparentheses.com/implementing-comment-line.html
Note that this function will be included in emacs 25.1. as #'comment-line."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(defun copy-rectangle (start end)
  "Copies a region-rectangle
Stolen from www.emacswiki.org/emacs/RectangleCommands"
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))

(defun increment-number-at-point ()
  "A time-saver that increments any number (not hex) at the point.
Stolen from http://www.danielehrman.com/blog/2014/5/25/11-must-haves-for-every-power-programmer
which I think was in turn stolen from the emacswiki.org."
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun start-quickproject (folder)
  "Initializes a new Common Lisp project using quickproject.  Assumes that you
  have all of the prerequisites installed: ASDF, Quicklisp, Quickproject."
  (interactive "SInitialize new CL project in which directory? > ")
  (print folder))

(defun th/swap-window-buffers-by-dnd (drag-event)
  "Swaps the buffers displayed in the DRAG-EVENT's start and end
window.
Stolen from https://tsdh.wordpress.com/2015/03/03/swapping-emacs-windows-using-dragndrop/"
  (interactive "e")
  (let ((start-win (cl-caadr drag-event))
        (end-win   (cl-caaddr drag-event)))
    (when (and (windowp start-win)
               (windowp end-win)
               (not (eq start-win end-win))
               (not (memq (minibuffer-window)
                          (list start-win end-win))))
      (let ((bs (window-buffer start-win))
            (be (window-buffer end-win)))
        (unless (eq bs be)
          (set-window-buffer start-win be)
          (set-window-buffer end-win bs))))))

(defun helm-esc-close ()
  "Closes a helm buffer by hitting the ESC key.
Stolen from: http://emacs.stackexchange.com/a/4064"
  (define-key helm-map (kbd "ESC") 'helm-keyboard-quit))

(defun compile-dotemacs ()
  "compile .emacs automagically on saving the .emacs file.
stolen from: http://www.emacswiki.org/emacs/AutoRecompile"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
        (byte-compile-file dotemacs))))
(add-hook 'after-save-hook 'compile-dotemacs)

(defun jt/load-external-lisp (slime-helper-path inferior-lisp-name)
  "loads the site slime-helper.el file and makes sure that is is byte-compiled.
compiles slime-helper.el if it is not compiled and loads it regardless.  also
sets the default inferior lisp program name."
  (byte-recompile-file slime-helper-path nil 0 t)
  (setq inferior-lisp-program inferior-lisp-name))

;; load temporary site functions if file exists
;; this file is used to house multi-session functions
;; but not permanent enough to be placed in my .emacs
(setq temp-func-file  (expand-file-name "~/.emacs.d/temp-func.el"))
(if (file-exists-p temp-func-file)
    (load temp-func-file))

(defmacro evalafter (pkg-function &rest body)
  "After PKG is loaded, eval body."
  `(eval-after-load ,pkg-function
     '(progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defadvice macros
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead.
courtesy of https://github.com/itsjeyd/emacs-config/blob/emacs24/init.el"
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice mark-paragraph (after fix-point activate compile)
  "Moves the point down a line so no extra newline is in the region.
If you test this advice in this def block, the cursor should end up at the first paren
before the 'd' in defadvice.  Otherwise, the cursor would end up in the line above this."
  (next-line)
  (back-to-indentation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; os-specific settings
;; maximize the frame on startup
(cond
    ((string-equal initial-window-system "w32")
    (progn
      (setq default-directory "c:/Users/jeff.tecca/")
      (push "c:/MinGW/bin" exec-path)
      (push (expand-file-name "~/AppData/Local/Continuum/Anaconda") exec-path)
      (push (expand-file-name "~/../../bin/cmder/vendor/msysgit/bin") exec-path)
      (push (expand-file-name "~/bin/cmder/vendor/msysgit/bin/") exec-path)
      (push (expand-file-name "~/AppData/Local/Chromium/Application/") exec-path)
      (global-set-key (kbd "<f6>") (lambda () (interactive) (find-file org-default-notes-file)))))
  ((string-equal initial-window-system "x")
   (progn
     (global-set-key (kbd "<f6>") (lambda () (interactive) (find-file org-default-notes-file)))
     (setq default-directory "~/")))
   ((string-equal initial-window-system "nil")
    (setq default-directory "~/")))

;;;;;;;;;;;;;;;;;;
;;;; os-dependent python settings
(cond
 ((string-equal initial-window-system "w32")
  (progn
    (setq
     python-shell-interpreter "C:\\Users\\jeff.tecca\\AppData\\Local\\Continuum\\Anaconda\\python.exe"
     python-shell-interpreter-args
     "-i C:\\Users\\jeff.tecca\\AppData\\Local\\Continuum\\Anaconda\\Scripts\\ipython-script.py"
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))
  ((or (string-equal initial-window-system "x") (string-equal initial-window-system "nil"))
   (progn
     (when (executable-find "ipython")
       (setq python-shell-interpreter "ipython")))))

;; lisp setups
(cond
 ((string-equal initial-window-system "w32")
  (jt/load-external-lisp
   (expand-file-name "~/AppData/Roaming/quicklisp/slime-helper.el")
   "sbcl"))
 ((string-equal initial-window-system "x")
  (jt/load-external-lisp
   (expand-file-name "~/quicklisp/slime-helper.el")
   "sbcl"))
 ((and (string-equal initial-window-system "nil")
       (not (string-equal system-type "windows-nt")))
  (jt/load-external-lisp
   (expand-file-name "~/quicklisp/slime-helper.el")
   "sbcl"))
 ((and (string-equal initial-window-system "nil")
       (string-equal system-type "windows-nt"))
  (jt/load-external-lisp
   (expand-file-name "~/AppData/Roaming/quicklisp/slime-helper.el")
   "sbcl")))

;; setup apsell for spell checking
;; M-$ is the default keybinding for it
(cond
 ((string-equal system-type "windows-nt")
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin")
  (autoload 'ispell "ispell")
  (eval-after-load "ispell"
    '(progn
       (setq ispell-program-name "aspell")
       (setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict")
       (global-set-key (kbd "C-$") 'flyspell-mode))))
((string-equal system-type "gnu/linux")
 (global-set-key (kbd "C-$") 'flyspell-mode)))

;;;;;;;;;;;;;;;;;;
;; evil settings
;; use C-z to switch between evil/emacs keybindings
(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(global-evil-leader-mode)
(require 'evil)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'turn-on-evil-mode)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'turn-on-evil-mode)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(evil-mode 1)
(global-set-key [escape] 'keyboard-quit)
(setq evil-insert-state-cursor '("red" box))
(setq evil-move-cursor-back nil)
(setq evil-replace-state-cursor '(box))
(setq evil-shift-width 4)
(setq evil-visual-state-cursor '(box))

;;;;;;;;;;;;;;;;;;
;; hydras
;; trying out hydra package with a few examples set to the function keys
;; just to see if it fits well with my workflow
(autoload 'hydra "hydra")
(require 'hydra-examples)
(defhydra hydra-splitter (evil-leader--default-map "<f10>")
  "splitter"
  ("h" hydra-move-splitter-left)
  ("j" hydra-move-splitter-down)
  ("k" hydra-move-splitter-up)
  ("l" hydra-move-splitter-right)
  ("q" nil "quit" :color blue))

(require 'windmove)
(defhydra hydra-windmove (evil-leader--default-map "<f11>")
  "windmove"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("q" nil "quit" :color blue))

; these needs work to have the keybindings show up with just the prefix
(defhydra hydra-org (evil-leader--default-map "<f7>"
                                :hint nil
                                :color blue)
  "
  _c_: capture
  _l_: store-link
  _a_: agenda
  _b_: iswitchb
  "
  ("c" org-capture)
  ("l" org-store-link)
  ("a" org-agenda)
  ("b" org-iswitchb)
  ("q" nil "quit" :color blue))

;;;;;;;;;;;;;;;;;;
;; markdown settings
;; set markdown filetypes
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.rm\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;
;;;; cosmetic customizations
(setq frame-title-format "emacs - %b")
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq linum-dela t)
(setq visible-bell nil)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t
      scroll-step 1)
(setq cursor-type 'box)

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
;;;; general editor settings
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq confirm-kill-emacs (quote y-or-n-p))
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(toggle-word-wrap 1)
(setq next-line-add-newlines t)
(setq-default fill-column 79)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(delete-selection-mode 1)
(tooltip-mode -1)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)
(setq redisplay-dont-pause 1)
(setf x-stretch-cursor 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(put 'dired-find-alternate-file 'disabled nil)
(setq gc-cons-threshold 50000000) ; garbage collection threshold at 50MB
(setq use-dialog-box nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

;;;;;;;;;;;;;;;;;;
;;  generic programming settings
(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(autoload 'fill-column-indicator "fill-column-indicator")
(add-hook 'prog-mode-hook 'turn-on-fci-mode)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;
;; projectile setup
(autoload 'projectile-mode "projectile-mode" "Functions for getting around a project." t)
(add-hook 'prog-mode-hook 'projectile-global-mode)
(setq projectile-enable-caching t)

;;;;;;;;;;;;;;;;;;
;; c/c++ settings
;; (require 'cc-mode)
(eval-after-load "cc-mode"
  '(progn
     (setq c-default-style "linux" c-basic-offset 4)
     (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
     (setq gdb-many-windows t gdb-show-main t)
     (define-key c-mode-base-map (kbd "C-c c") (lambda ()
                                                 (interactive)
                                                 (setq-local compilation-read-command nil)
                                                 (call-interactively
                                                  'compile)))
     (define-key c-mode-base-map (kbd "C-c C-f") 'ff-find-other-file)
     ))

;;;;;;;;;;;;;;;;;;
;; sr-speedbar setup
(autoload 'sr-speedbar "sr-speedbar")
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-skip-other-window-p t) ; C-x o won't go to speedbar if opened

;;;;;;;;;;;;;;;;;;
;; helm
(require 'helm)
(require 'helm-config)
;; look into helm-swoop as a find replacement
;; helm's default prefix keybinding is too close to C-x C-c
(helm-autoresize-mode 1)
(global-set-key (kbd "C-c j") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(setq helm-lisp-fuzzy-completion t)
(add-hook 'after-init-hook #'helm-esc-close)
(helm-mode 1)

;;;;;;;;;;;;;;;;;;
;; tags setup
(autoload 'ggtags "ggtags" "Use GNU Global for TAGS." t)
(autoload 'helm-gtags "helm-gtags" "Use helm to interact with TAGS." t)
(eval-after-load "helm-gtags"
  '(progn
     (setq
      helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-prefix-key "\C-cg" ; so C-c g is your tag prefix key
      helm-gtags-suggested-key-mapping nil
      )

     ;; Enable helm-gtags-mode
     (add-hook 'dired-mode-hook 'helm-gtags-mode)
     (add-hook 'eshell-mode-hook 'helm-gtags-mode)
     (add-hook 'c-mode-hook 'helm-gtags-mode)
     (add-hook 'c++-mode-hook 'helm-gtags-mode)
     (add-hook 'asm-mode-hook 'helm-gtags-mode)

     (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
     (define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-select)
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     ;; remember to cd to project root and run 'gtags' to generate tags
     ;; you'll have a GTAGS def db, GRTAGS ref database, and GPATH path name db
     ))

;;;;;;;;;;;;;;;;;;
;; sql settings
;; i think there's readline issues with the default pgadmin psql
;; may need to try cygwin's psql.exe for output
;;;;;;;;;;;;;;;;;;
;; company-mode settings
(autoload 'company "company")
(autoload 'company-c-headers "company-c-headers")
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load "company"
  '(progn
     (require 'company-c-headers)
     ;; on linux + clang, remove semantic backend
     (cond ((eq system-type 'gnu/linux)
            (progn
              (setq company-backends (delete 'company-semantic company-backends)))))
     ;; define your project include dirs here to be seen by company-clang-complete
     ;; ((nil . ((company-clang-arguments . ("-I/home/<user>/project_root/include1/"
     ;; "-I/home/<user>/project_root/include2/")))))
     (add-to-list 'company-backends 'company-c-headers)
     (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9/")
     (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
     (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.7/")))

;;;;;;;;;;;;;;;;;;
;; smartparens setup
(require 'smartparens-config) ; load the default config
(require 'evil-smartparens)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;
;; org-mode settings
;; make each new layer indent for easier reading
(add-hook 'org-mode-hook '(lambda () (org-indent-mode t)))
;; set the task keywords
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "STOPPED" "OOS" "DONE")))
(setq org-src-fontify-natively t)
(setq org-log-done 'time) ; adds a timestamp when a TODO is marked as DONE
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
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (lisp . t)
   (lilypond . t)))

;;;;;;;;;;;;;;;;;;
;;;; magit settings
(setq magit-last-seen-setup-instructions "1.4.0")

;;;;;;;;;;;;;;;;;;
;;;; global python settings
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c i") 'python-insert-breakpoint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom global keybindings
(global-unset-key (kbd "C-x C-d")) ; i usually just mean to get to dired
(global-set-key (kbd "<S-wheel-up>") 'increase-font-size)
(global-set-key (kbd "<S-wheel-down>") 'decrease-font-size)
(global-set-key (kbd "M-o") 'other-window) ; duplicated to use in insert mode
(global-set-key (kbd "C-;") 'endless/comment-line)
(global-set-key (kbd "M-C-<f5>") 'revert-this-buffer) ; purposely cumbersome to reduce accidental reversions
(global-set-key (kbd "<C-up>") 'enlarge-window)
(global-set-key (kbd "<C-down>") 'shrink-window)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-drag-mouse-1>") #'th/swap-window-buffers-by-dnd)
(global-set-key (kbd "C-<tab>") 'company-complete)
(global-set-key (kbd "M-g g") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-g M-g") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-s") 'swiper-helm)
;; swiper-helm is bi-directional searching
;; so free up the keybinding for reverse search
(global-unset-key (kbd "C-r"))
;; add smartparens slurp and barf commands to evil insert state
(define-key smartparens-mode-map (kbd "C-M-'") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-;") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-:") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-\"") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-e") 'sp-end-of-sexp)
;; evil-leader keybindings
;; PROTIP: use command-history to get an idea about frequently used commands
;; that should be bound to evil-leader
(evil-leader/set-key
  "h" 'helm-M-x
  "b" 'helm-mini
  "y" 'helm-show-kill-ring
  "i" 'helm-semantic-or-imenu
  "f" 'helm-find-files
  "k" 'kill-buffer
  "w" 'delete-window
   "<f5>" 'menu-bar-open
  "<f1>" 'insert-date
  "<f2>" 'insert-datetime
  "<f8>" 'magit-status
  "<SPC>" 'other-window
  "g" 'avy-goto-word-or-subword-1
  "c" 'avy-goto-char
 "." 'find-function-at-point
  ;; 'u'tility commands are grouped together below
  "ud" 'dired
  "ue" 'eshell
  "ut" 'sr-speedbar-toggle
  )

;;;;;;;;;;;;;;;;;;
;;;; proced settings
(defun proced-settings ()
  (proced-toggle-auto-update)
(add-hook 'proced-mode-hook 'proced-settings))

;;;;;;;;;;;;;;;;;;
;;;; proced settings
(require 'smart-mode-line)
(setq sml/theme nil)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(add-to-list 'sml/replacer-regexp-list '("^~/source/" ":SRC:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/org/" ":ORG:"))
(add-to-list 'sml/replacer-regexp-list '("^~/AppData/Roaming/" ":ROAM:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Documents/" ":DOC:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Downloads/" ":DL:"))

(require 'delight)
(delight '((hs-minor-mode nil hideshow)
           (auto-fill-function nil)
           (projectile-mode nil projectile)
           (evil-smartparens-mode nil evil-smartparens)
           (smartparens-mode nil evil-smartparens)
           (helm-mode nil helm)
           (undo-tree-mode nil evil)
           (company-mode nil company)
           (org-indent-mode nil org-indent)
           (ivy-mode nil ivy)
           ))

;; THEMING NOTE:
;; if you want to inspect what face is being used under the cursor,
;; use C-u C-x = and search for 'face'.
;; end .emacs

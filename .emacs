;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's .emacs
;;;; updated: 2015-05-27
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
 '(ace-jump-mode company company-c-headers company-jedi
                 evil evil-smartparens evil-leader f
                 fill-column-indicator ggtags helm helm-gtags helm-projectile
                 highlight-numbers hydra magit markdown-mode projectile
                 rainbow-delimiters rainbow-mode s seq slime smartparens
                 sr-speedbar smart-mode-line))

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

(defadvice isearch-repeat-forward (after forward-recenter-pos activate compile)
  "After a repeated isearch-forward (C-s), recenter the next found item to the center of the buffer."
  (recenter))

(defadvice isearch-repeat-backward (after backward-recenter-pos activate compile)
  "After a repeated isearch-backwar (C-r), recenter the next found item to the center of the buffer."
  (recenter))

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
;; (cond
;;  ((string-equal initial-window-system "w32")
;;   (progn
;;     (load (expand-file-name "~/AppData/Roaming/quicklisp/slime-helper.el"))
;;     (setq inferior-lisp-program "wx86cl64")))
;;  ((string-equal initial-window-system "x")
;;   (progn
;;     (setq inferior-lisp-program "sbcl")
;;     (load (expand-file-name "~/quicklisp/slime-helper.el"))))
;;  ((and (string-equal initial-window-system "nil")
;;        (not (string-equal system-type "windows-nt")))
;;   (progn
;;     (setq inferior-lisp-program "sbcl")
;;     (load (expand-file-name "~/quicklisp/slime-helper.el"))))
;;  ((and (string-equal initial-window-system "nil")
;;        (string-equal system-type "windows-nt"))
;;   (progn
;;     (setq inferior-lisp-program "wx86cl64")
;;     (load (expand-file-name "~/AppData/Roaming/quicklisp/slime-helper.el")))))

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
;; hydras
;; trying out hydra package with a few examples set to the function keys
;; just to see if it fits well with my workflow
(autoload 'hydra "hydra")
(eval-after-load "hydra"
  '(progn
(hydra-add-font-lock)
(defhydra hydra-zoom (global-map "<f9>")
  "zoom"
  ("i" text-scale-increase "in")
  ("o" text-scale-decrease "out")
  ("q" nil "quit" :color blue))

(require 'hydra-examples)
(defhydra hydra-splitter (global-map "<f10>")
  "splitter"
  ("h" hydra-move-splitter-left)
  ("j" hydra-move-splitter-down)
  ("k" hydra-move-splitter-up)
  ("l" hydra-move-splitter-right)
  ("q" nil "quit" :color blue))

(require 'windmove)
(defhydra hydra-windmove (global-map "<f11>")
  "windmove"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("q" nil "quit" :color blue))

; these needs work to have the keybindings show up with just the prefix
(defhydra hydra-org (global-map "C-c o"
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
))

;;;;;;;;;;;;;;;;;;
;; evil settings
;; use C-z to switch between evil/emacs keybindings
(require 'evil-leader)
(global-evil-leader-mode)
(require 'evil)
(setq evil-shift-width 4)
(add-hook 'prog-mode-hook 'turn-on-evil-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'turn-on-evil-mode)
(add-hook 'text-mode-hook 'linum-mode)
(evil-mode 1)
(evalafter "evil-mode"
           (define-key evil-normal-state-map [escape] 'keyboard-quit)
           (define-key evil-visual-state-map [escape] 'keyboard-quit)
           (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
           (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
           (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
           (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
           (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
           (global-set-key [escape] 'keyboard-quit))

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
      scroll-preserve-screen-position t)
(setq cursor-type 'box)
(hl-line-mode 1)

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
(set-language-environment "utf-8")
(setq confirm-kill-emacs (quote y-or-n-p))
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
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
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;
;; generic programming settings
(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(autoload 'fill-column-indicator "fill-column-indicator")
(add-hook 'prog-mode-hook 'turn-on-fci-mode)

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
                                                 (call-interactively 'compile)))
     ))

;;;;;;;;;;;;;;;;;;
;; sr-speedbar setup
(autoload 'sr-speedbar "sr-speedbar")
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-skip-other-window-p t) ; C-x o won't go to speedbar if opened

;;;;;;;;;;;;;;;;;;
;; helm
(require 'helm-config)
;; look into helm-swoop as a find replacement
;; helm's default prefix keybinding is too close to C-x C-c
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
(setq sql-postgres-program "C:/Program Files (x86)/pgAdmin III/1.20/psql.exe")
;(setq sql-postgres-options (file-to-string "~/postgresql/connection.info"))
;; disable truncating lines for large tables
(add-hook 'sql-interactive-mode-hook
          (function (lambda ()
                      (linum-mode 1)
                      (setq truncate-lines t))))

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
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(smartparens-global-mode t)
;; note: keybindings are done at in the global keybinding section

;;;;;;;;;;;;;;;;;;
;; org-mode settings
;; make each new layer indent for easier reading
(add-hook 'org-mode-hook '(lambda () (org-indent-mode t)))
;; set the task keywords
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "STOPPED" "OOS" "DONE")))
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

;;;;;;;;;;;;;;;;;;
;;;; ace-jump-mode
; load settings per source file
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Quick move minor mode"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
(setq ace-jump-mode-submode-list ; i prefer char-mode for finer-grained jumping
      '(ace-jump-char-mode
        ace-jump-word-mode
        ace-jump-line-mode))
(setq ace-jump-mode-gray-background t)
(setq ace-jump-mode-scope 'window)
;; PROTIP: you can use C-c C-c to switch between char, word, and line modes
;; after you enter a character to search for.  however, it doesn't work
;; when you haven't entered anything to search for
;;; For more information
;; Intro Doc: https://github.com/winterTTr/ace-jump-mode/wiki
;; FAQ      : https://github.com/winterTTr/ace-jump-mode/wiki/AceJump-FAQ
;; setup ace window for quick jumping between windows

;;;;;;;;;;;;;;;;;;
;;;; magit settings
(setq magit-last-seen-setup-instructions "1.4.0")
;; (setq magit-auto-revert-mode nil)

;;;;;;;;;;;;;;;;;;
;;;; global python settings
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c i") 'python-insert-breakpoint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom global keybindings
(global-set-key (kbd "<S-wheel-up>") 'increase-font-size)
(global-set-key (kbd "<S-wheel-down>") 'decrease-font-size)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-;") 'endless/comment-line)
(global-set-key (kbd "M-C-<f5>") 'revert-this-buffer) ; purposely cumbersome to reduce accidental reversions
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-right>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-drag-mouse-1>") #'th/swap-window-buffers-by-dnd)
(global-set-key (kbd "C-<tab>") 'company-complete)
;; add smartparens slurp and barf commands to evil insert state
(define-key evil-insert-state-map (kbd "C-M-'") 'sp-forward-slurp-sexp)
(define-key evil-insert-state-map (kbd "C-M-;") 'sp-forward-barf-sexp)
(define-key evil-normal-state-map (kbd "C-M-'") 'sp-forward-slurp-sexp)
(define-key evil-normal-state-map (kbd "C-M-;") 'sp-forward-barf-sexp)
(define-key evil-insert-state-map (kbd "C-M-:") 'sp-backward-slurp-sexp)
(define-key evil-insert-state-map (kbd "C-M-\"") 'sp-backward-barf-sexp)
(define-key evil-normal-state-map (kbd "C-M-:") 'sp-backward-slurp-sexp)
(define-key evil-normal-state-map (kbd "C-M-\"") 'sp-backward-barf-sexp)
;; evil-leader keybindings
(evil-leader/set-key
  "x" 'helm-M-x
  "b" 'helm-mini
  "k" 'kill-buffer
  "o" 'other-window
  "i" 'helm-semantic-or-imenu
  "." 'find-function-at-point
  "<f8>" 'magit-status
  "s" 'sr-speedbar-toggle
  "<f5>" 'menu-bar-open)

;;;;;;;;;;;;;;;;;;
;;;; proced settings
(defun proced-settings ()
  (proced-toggle-auto-update)
(add-hook 'proced-mode-hook 'proced-settings))

;;;;;;;;;;;;;;;;;;
;;;; proced settings
(require 'smart-mode-line)
(add-to-list 'rm-blacklist "helm-mode")
(add-to-list 'rm-blacklist "hs-minor-mode")
(add-to-list 'rm-blacklist "undo-tree-mode")
(sml/setup)
(add-to-list 'sml/replacer-regexp-list '("^~/source/" ":SRC:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/org/" ":ORG:"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; set colors
;;; PROTIP: use (list-faces-display) to see all of the current faces
(when
    window-system
  (set-face-background 'cursor "#ff1744")
  (set-face-background 'default "#ececec")
  (set-face-foreground 'default "#262626")
  (set-face-background 'helm-selection "#76ff03")
  (set-face-background 'region "#e6ee9c")
  (set-face-background 'show-paren-match "#1de9b6")
  (set-face-background 'lazy-highlight "#Ffff8d")
  (set-face-background 'isearch "#673ab7")
  (set-face-foreground 'isearch "#ffffff")
  (set-face-bold 'isearch t)
  (set-face-foreground 'comint-highlight-prompt "#4caf50")
  (set-face-foreground 'font-lock-builtin-face "#0d47a1")
  (set-face-foreground 'font-lock-function-name-face "#2979ff")
  (set-face-bold 'font-lock-function-name-face t)
  (set-face-foreground 'font-lock-keyword-face "#8e24aa")
  (set-face-foreground 'font-lock-type-face "#26a69a")
  (set-face-foreground 'font-lock-string-face "#33691e")
  (set-face-foreground 'font-lock-comment-face "#f44336")
  (set-face-background 'font-lock-comment-face "#ffefef")
  (set-face-foreground 'font-lock-variable-name-face "#f4511e")
  (set-face-attribute 'hl-line nil :background "#cceecc" :foreground nil
                      :inherit t)
  )

;; org colors level tweaks
(when
    window-system
  (set-face-bold 'org-level-1 nil)
  (set-face-foreground 'org-level-1 "#242424")
  (set-face-foreground 'org-level-2 "##121212")
  (set-face-foreground 'org-level-3 "##1a1a1a")
  (set-face-foreground 'org-level-4 "##1f1f1f")
  (set-face-background 'org-level-4 "##292929")
  (set-face-foreground 'org-level-5 "##2e2e2e")
  (set-face-foreground 'org-level-5 "##3d3d3d")
  (set-face-foreground 'org-table "#333333")
  )

;; sml colors
(if (and (package-installed-p 'smart-mode-line) window-system)
    (progn
      (set-face-foreground 'sml/git "##Ff4500")
      (set-face-foreground 'sml/filename "#000000")
      (set-face-foreground 'sml/position-percentage "#2979ff")
      (set-face-bold 'sml/position-percentage t)
      (set-face-foreground 'sml/vc-edited "#ff1744")
      (set-face-foreground 'sml/vc "#Ff4500")
      (set-face-foreground 'sml/col-number "#000000")
      (set-face-foreground 'sml/line-number "#000000")
      (set-face-bold 'sml/col-number t)
      (set-face-background 'mode-line-inactive "#ababab")
      (set-face-foreground 'mode-line-inactive "#000000")
      (set-face-background 'mode-line "#Fdf5e6")
      (set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-9")
      (set-face-attribute 'mode-line-inactive nil :font "DejaVu Sans Mono-9")))

;; if you want to inspect what face is being used under the cursor,
;; use C-u C-x = and search for 'face'.

; -------------------------------------------

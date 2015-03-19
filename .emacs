;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's .emacs
;;;; updated: 2015-03-19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; initial setup
;; removing the gui elements first keeps them from showing on startup
(when window-system
  (progn
    (tool-bar-mode 0)
    (scroll-bar-mode 0)))

(setq custom-file  (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup and load external packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

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
  "Reduces multiple lines in a selected region down to one.  Does nothing if there is no active region."
  (interactive "r")
  (if (use-region-p)
      (while (> (count-lines reg-start reg-end) 1)
      (call-interactively 'delete-indentation))))

(defun prettify-json-document ()
"Runs a python module to prettify a selected JSON document.
A region must be used to highlight the JSON document to be parsed, otherwise nothing is parsed."
  (interactive)
  (if (use-region-p)
        (shell-command-on-region (region-beginning) (region-end) "python -m json.tool" nil t)))

(defun python-insert-breakpoint ()
  "Inserts a breakpoint to the buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(defun make-frame-fullscreen ()
  "If not running Emacs in a terminal (through a window manager), f11 maximizes the frame. otherwise returns nil."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defadvice macros

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead.
courtesy of https://github.com/itsjeyd/emacs-config/blob/emacs24/init.el"
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

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
      (global-set-key (kbd "<f6>") (lambda () (interactive) (find-file "~/../../Dropbox/todo.org")))))
  ((string-equal initial-window-system "x")
   (progn
     (set-face-attribute 'default nil :font "Ubuntu Mono-11")
     (global-set-key (kbd "<f6>") (lambda () (interactive) (find-file "~/Dropbox/todo.org")))
     (setq default-directory "~/")))
   ((string-equal initial-window-system "nil")
    (setq default-directory "~/")))

;; lisp setups
(cond
 ((string-equal initial-window-system "w32")
  (progn
    (load (expand-file-name "~/AppData/Roaming/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "wx86cl64")))
 ((string-equal initial-window-system "x")
  (progn
    (setq inferior-lisp-program "sbcl")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))))
 ((and (string-equal initial-window-system "nil")
       (not (string-equal system-type "windows-nt")))
  (progn
    (setq inferior-lisp-program "sbcl")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))))
 ((and (string-equal initial-window-system "nil")
       (string-equal system-type "windows-nt"))
  (progn
    (setq inferior-lisp-program "wx86cl64")
    (load (expand-file-name "~/AppData/Roaming/quicklisp/slime-helper.el")))))

;; setup apsell for spell checking
;; M-$ is the default keybinding for it
(cond
 ((string-equal system-type "windows-nt")
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin")
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict")
  (require 'ispell)
  (global-set-key (kbd "C-$") 'flyspell-mode))
((string-equal system-type "gnu/linux")
 (global-set-key (kbd "C-$") 'flyspell-mode)))

(defun compile-dotemacs ()
  "compile .emacs automagically on saving the .emacs file.
stolen from: http://www.emacswiki.org/emacs/AutoRecompile"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
        (byte-compile-file dotemacs))))
(add-hook 'after-save-hook 'compile-dotemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom global keybindings
(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "<S-wheel-up>") 'increase-font-size)
(global-set-key (kbd "<S-wheel-down>") 'decrease-font-size)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu) ; or C-c j i
(global-set-key (kbd "<f11>") 'make-frame-fullscreen)
(global-set-key (kbd "C-;") 'endless/comment-line)
(global-set-key (kbd "C-c .") 'find-function-at-point)
(global-set-key (kbd "M-C-<f5>") 'revert-this-buffer) ; keybinding is purposely cumbersome to reduce accidental reversions
;; copy-line keybinding
(global-set-key (kbd "C-c k") 'copy-line)
;; move up and down lines with meta as well as ctrl
;; very useful for meta-heavy commands without switching between M and C
(global-set-key (kbd "M-n") 'next-line)
(global-set-key (kbd "M-p") 'previous-line)
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-right>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x r M-w") 'copy-rectangle)
(global-set-key (kbd "<f8>") 'magit-status)
(global-set-key (kbd "C-M-y") 'yank-pop)

;;;;;;;;;;;;;;;;;;
;; hydras
;; trying out hydra package with a few examples set to the function keys
;; just to see if it fits well with my workflow
(require 'hydra)
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
(setq visible-bell nil)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)
(setq cursor-type 'box)
(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

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
(require 'fill-column-indicator)
(turn-on-fci-mode)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(delete-selection-mode 1)
(tooltip-mode -1)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)
(setq redisplay-dont-pause 1)
(setf x-stretch-cursor 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq scroll-conservatively 10000)

;;;;;;;;;;;;;;;;;;
;; helm
(require 'helm-config)
;; look into helm-swoop as a find replacement
;; helm's default prefix keybinding is too close to C-x C-c
(global-set-key (kbd "C-c j") 'helm-command-prefix)
;; i - imenu, which shows major function definitions, variable defitinitions
(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(setq helm-lisp-fuzzy-completion t)
(helm-mode 1)

;;;;;;;;;;;;;;;;;;
;; sql settings
;; i think there's readline issues with the default pgadmin psql
;; may need to try cygwin's psql.exe for output
(setq sql-postgres-program "C:/Program Files (x86)/pgAdmin III/1.18/psql.exe")
;(setq sql-postgres-options (file-to-string "~/postgresql/connection.info"))
;; disable truncating lines for large tables
(add-hook 'sql-interactive-mode-hook
          (function (lambda ()
                      (setq truncate-lines nil))))

;;;;;;;;;;;;;;;;;;
;; auto complete
(require 'auto-complete)
(ac-config-default)
;; plugins with other packages
(require 'ac-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)
(require 'ac-python)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;;;;;;;;;;;;;;;;;;
;; paredit settings
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'eldoc-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook #'eldoc-mode)
;; make eldoc aware of paredit
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)
(cond
;; something in windows is capturing C-), which messes up my paredit
;; keybindings, so rebind them when on windows and remove old keybindings
;; i hope this doesn't screw up my linux muscle memory for paredit...
((string-equal initial-window-system "w32")
 (progn
   (define-key paredit-mode-map (kbd "C-*") 'paredit-forward-slurp-sexp)
   (define-key paredit-mode-map (kbd "C-&") 'paredit-backward-slurp-sexp)
   (define-key paredit-mode-map (kbd "C-(") nil))))

;;;;;;;;;;;;;;;;;;
;; org-mode settings
;; make each new layer indent for easier reading
(add-hook 'org-mode-hook '(lambda () (org-indent-mode t)))
;; set the task keywords
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "STOPPED" "DONE")))
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
(require 'ace-jump-mode)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Quick move minor mode"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
(setq ace-jump-mode-submode-list
      '(ace-jump-word-mode
        ace-jump-char-mode
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
;;;; python settings
;;; TODO things are still kind of broken on windows, like autocomplete framework
;;; TODO also think about using yassnippet for common patterns
;;; TODO and research some good project management tools
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
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;; custom python keybindings
(add-hook 'python-mode-hook
          '(lambda ()
             (turn-on-fci-mode)
             (local-set-key (kbd "C-c i") 'python-insert-breakpoint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; set colors
;;; PROTIP: use (list-faces-display) to see all of the current faces
(when
    window-system
  (set-face-background 'cursor "#ff1111")
  (set-face-background 'default "#ffffff")
  ;; (set-face-background 'default "#ececec")
  (set-face-background 'helm-selection "#9aff9a")
  (set-face-background 'region "#Eedd82")
  (set-face-background 'show-paren-match "#00fa9a")
  (set-face-background 'lazy-highlight "#Ffff00")
  (set-face-background 'isearch "#9400d3")
  (set-face-foreground 'isearch "#ffffff")
  (set-face-bold 'isearch t)
  (set-face-foreground 'comint-highlight-prompt "#228b22")
  ;; (set-face-foreground 'font-lock-builtin-face "#27408b")
  (set-face-foreground 'font-lock-builtin-face "#00688b")
  (set-face-foreground 'font-lock-function-name-face "#1c86ee")
  (set-face-bold 'font-lock-function-name-face t)
  (set-face-foreground 'font-lock-keyword-face "#8a2be2")
  (set-face-foreground 'font-lock-type-face "#00bb00")
  (set-face-foreground 'font-lock-string-face "#698b22")
  (set-face-foreground 'font-lock-comment-face "#Ff0000")
  (set-face-background 'font-lock-comment-face "#ffefef")
  (set-face-foreground 'font-lock-variable-name-face "#Ee4000")
  )
;; if you want to inspect what face is being used under the cursor,
;; use C-u C-x = and search for 'face'.

; -------------------------------------------

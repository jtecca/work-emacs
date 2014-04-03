;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's nt-windows & gnu/linux .emacs
;;;; updated: 2014-04-01
;;;;
;;;; dependencies:
;;;;   * auto-complete
;;;;   * concurrent
;;;;   * deferred
;;;;   * epc
;;;;   * jedi
;;;;   * markdown-mode
;;;;   * popup
;;;; not required (but recommended):
;;;;   * python-mode
;;;;   * python-pep8
;;;;   * python-pylint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; initial setup
;; removing the gui elements first keeps them from showing on startup
(when window-system
  (progn
    (tool-bar-mode 0)
    (scroll-bar-mode 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; os-based settings
;; maximize the frame on startup
(cond ((string-equal system-type "windows-nt")
       (menu-bar-mode t))
      ((string-equal system-type "gnu/linux")
       (menu-bar-mode 0)))

(cond
    ((string-equal initial-window-system "w32")
    (progn
      (w32-send-sys-command #xf030) ; nt command for maximizing a window)
      (set-face-attribute 'default nil :font "ProggyClean 9")
      (setq default-directory "c:/Users/jeff.tecca/")))
  ((string-equal initial-window-system "x") ; emacs running in an x window
   (progn 
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
     (set-face-attribute 'default nil :font "ProggyCleanTT-12")
     (setq default-directory "~/")))
  ((string-equal initial-window-system "nil") ; running in a term
   (setq default-directory "~/")))

;; set the default python shell to ipython
(cond
    ((string-equal system-type "windows-nt")
    (progn
      (setq
       python-shell-interpreter "C:\\Python33\\python.exe"
       python-shell-interpreter-args "-i C:\\Python33\\Scripts\\ipython3-script.py" )      
      (setq
       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
       python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
       python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
       python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
    'setup-ipython-windows)
  ((string-equal system-type "gnu/linux")
   (progn
     (setq
      python-shell-interpreter "ipython"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
     'setup-ipython-gnu/linux)))

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

;; lisp settings
;; TODO this function should fail silently if one of the lisp execs can't be found
;; C-u before M-x slime allows a specific lisp dialect to be loaded
;; as long as the executable is in the system PATH or you give the full path.
(cond
 ((string-equal system-type "windows-nt")
    (load "C:\\quicklisp\\slime-helper.el")
    (setq inferior-lisp-program "C:/Program Files (x86)/ccl/wx86cl.exe")
    (setq slime-net-coding-system 'utf-8-unix)
    (slime-setup '(slime-fancy))
    (add-hook 'lisp-mode-hook 'set-linum-mode-hook)
    (add-hook 'lisp-interaction-mode-hook 'set-linum-mode-hook))
 ((string-equal system-type "gnu/linux")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))))
       (setq inferior-lisp-program "sbcl")
       ;(setq inferior-lisp-program "clisp")
       ;(setq inferior-lisp-program (expand-file-name "~/bin/ccl/lx86cl64")
       (load (expand-file-name "~/quicklisp/slime-helper.el"))))))
(os-cond-slime-setup)

;; add ess to load-path on windows
(if (string-equal system-type "windows-nt")
     (progn
       (add-to-list 'load-path (expand-file-name "~/.emacs.d/ess/lisp/"))
       (require 'ess-site)
       (ess-execute-screen-options) ; re-run this if the columns change in the buffer running R
       'ess)
    'no-ess)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; os-agnostic settings

;; alternative keybindings for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.info\\'" . markdown-mode))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; pull in files from package.el
(require 'deferred)
(require 'epc)
(require 'auto-complete)
(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)

;; jedi hooks
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'c-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;;;;;;;;;;;;;;;;;;
;;;; cosmetic customizations
(setq frame-title-format "emacs - %b")
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq visible-bell t)
(global-visual-line-mode 1)
(eldoc-mode) ; why isn't this always on?

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
(setq-default fill-column 72)
(show-paren-mode 1)
(setq show-paren-style 'expression)
(delete-selection-mode t)

;;;;;;;;;;;;;;;;;;
;;;; python-specific settings:
(add-hook 'python-mode-hook '(lambda() (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'python-mode-hook '(lambda() (setq python-indent-4)))
(add-hook 'python-mode-hook 'set-linum-mode-hook)

;;;;;;;;;;;;;;;;;;
;;;; sql-specific settings:
(add-hook 'sql-mode-hook 'set-linum-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set custom functions
(defun set-linum-mode-hook ()
  (linum-mode 1))

(defun format-kill-point ()
  "Removes Garmin GPX formatting and sends formatted string to the kill-ring.

Works on a line-by-line basis, so just move the point to a line with GPX
information on it and execute the command."
  (interactive)
  (setq s1 (buffer-substring (line-beginning-position) (line-end-position)))
  (message "%s" s1)
  (setq s2 (replace-regexp-in-string ".*<trkpt lat=\"" "" s1))
  (setq s3 (replace-regexp-in-string "\">.*" "" s2))
  (setq s4 (replace-regexp-in-string "\".lon=\"" ", " s3))
  (message "%s is now in the kill-ring." s4)
  (kill-new s4))

(defun format-kill-point-copy (p1 p2)
  "Removes Garmin GPX formatting and sends formatted string to the kill-ring.

Works on a line-by-line basis, so just move the point to a line with GPX
information on it and execute the command."
  (interactive "r")
  (save-excursion (
                   (beginning-of-line)
  (setq s1 (buffer-substring p1 p2))
  (setq s2 (replace-regexp-in-string "\".lon=\"" ", " s1))
  (message "%s is now in the kill-ring." s2)
  (kill-new s2))))

(defun add-gpx-header-template ()
  "Adds a template gpx track header when track trimming."
  (interactive)
  (insert "    </trkseg>
  </trk>

  <trk>
    <name>Active Log: DD MMM YYYY HH:SS - TRIMMED</name>
    <extensions>
      <gpxx:TrackExtension>
        <gpxx:DisplayColor>DarkGray</gpxx:DisplayColor>
      </gpxx:TrackExtension>
    </extensions>
    <trkseg>
"))

(defun make-frame-fullscreen ()
  "If running Emacs in Windows, F11 maximizes the frame. Otherwise returns nil."
  (interactive)
  (cond
      ((string-equal system-type "windows-nt")
      (w32-send-sys-command #xf030)) ;; 0xf030 is the command for maximizing a window)
      ((string-equal system-type "gnu/linux")
       (progn 
         (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
         (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
         ))))

;; lifted from www.masteringemacs.org
(defun revert-this-buffer ()
  "Reloads (reverts) the current buffer"
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer: " (buffer-name))))

;; custom keybinding hooks
(defun set-vetting-keybinds ()
  (local-set-key (kbd "C-c a") 'format-kill-point)
  (local-set-key (kbd "C-c t") 'add-gpx-header-template))
(add-hook 'xml-mode-hook 'set-vetting-keybinds)

;; global custom keybindings
(global-set-key (kbd "<f5>") 'revert-this-buffer)
(global-set-key (kbd "<f9>") 'minimap-create)
(global-set-key (kbd "<f11>") 'make-frame-fullscreen)
(global-set-key (kbd "C-c a") 'format-kill-point)
(global-set-key (kbd "C-c t") 'add-gpx-header-template)
(global-set-key (kbd "C-x C-b") 'ibuffer)
; -------------------------------------------
; using wombat theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(background-color "#202020")
 '(background-mode dark)
 '(column-number-mode t)
 '(cursor-color "#cccccc")
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "97a2b10275e3e5c67f46ddaac0ec7969aeb35068c03ec4157cf4887c401e74b1" "4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" "a30d5f217d1a697f6d355817ac344d906bb0aae3e888d7abaa7595d5a4b7e2e3" "70cf411fbf9512a4da81aa1e87b064d3a3f0a47b19d7a4850578c8d64cac2353" default)))
 '(foreground-color "#cccccc")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(menu-bar-mode nil)
 '(show-paren-mode t)
 '(sql-product (quote sqlite))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

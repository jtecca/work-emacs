;;;; Jeff's work emacs
;;;; updated: 2014-03-07

;; set default font to Consolas 11
(set-face-attribute 'default nil :font "Consolas 10")

;; set the default open file
(setq default-directory "c:/Users/jeff.tecca/")

;; alternative keybindings for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; load markdown mode for most basic text editing
(load-file "~/.emacs.d/packages/markdown-mode.el")
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.doc\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.info\\'" . markdown-mode))

;; set ipython to be the default shell
(setq
 python-shell-interpreter "C:\\Python33\\python.exe"
 python-shell-interpreter-args
 "-i C:\\Python33\\Scripts\\ipython3-script.py")
(setq
  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; package manager
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

;; jedi hooks
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'c-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; autocomplete hooks
(add-to-list 'load-path "~/elpa/auto-complete-20130330.1836")
(add-to-list 'ac-dictionary-directories "~/elpa/auto-complete-20130330.1836/dict")
(require 'auto-complete-config)
(ac-config-default)

;; setup apsell for spell checking
;; M-$ is the default keybinding for it
(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict")
(require 'ispell)
;; set flyspell-mode invocation to C-$
(global-set-key (kbd "C-$") 'flyspell-mode)

(defun set-linum-mode-hook ()
  (linum-mode 1))

;; lisp settings
(load "C:\\quicklisp\\slime-helper.el")
(setq inferior-lisp-program "wx86cl64.exe")
(setq slime-lisp-implementations
      '((clisp ("clisp.exe"))
        (ccl ("wx86cl64.exe"))))
(add-hook 'lisp-mode-hook 'set-linum-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'set-linum-mode-hook)
;; pro-tip: use M-: major-mode RET to find the value of major-mode for hooks

;; show current buffer as frame title
(setq frame-title-format "%b")

;; don't show the startup screen
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq visible-bell t)

;; stop the #autosave# files
(setq auto-save-default nil)

;; turn on global line wrap
(global-visual-line-mode 1)

;; show arg list in echo area
(eldoc-mode)

;; persistant cataloging of recent files
(recentf-mode 1)

; because ido
(ido-mode 1)

;; move point to buffers with ctrl+arrow keys
(windmove-default-keybindings 'ctrl)

;; remove tool bar and scroll bar
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0) ; because you've got to force yourself to learn 'c-h m'

;; set standard indent to 4 rather than 2
(setq standard-indent 4)

;; turn off the tab character
(setq-default indent-tabs-mode nil)

;; python-specific settings:
(add-hook 'python-mode-hook '(lambda() (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'python-mode-hook '(lambda() (setq python-indent-4)))
(add-hook 'python-mode-hook 'set-linum-mode-hook)

;; enable backup files and always automatically delete backups
(setq make-backup-files t)
(setq delete-old-versions t)
(setq version-control t)

;; save all backup files in this dir
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; move deleted files to the trash instead of destroying them outright
(setq delete-by-moving-to-trash t)

;; set yes or no prompts to only need y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; turn on line/column info
(line-number-mode 1)
(column-number-mode 1)

;; sets word wrap
(toggle-word-wrap)

;; set the fill column when pasting
(setq-default fill-column 72)

;; enable matching parens
(show-paren-mode 1)
(setq show-paren-style 'expression) 

;; replaces a selection if you start typing
(delete-selection-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set custom functions
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
  (when 
   (string-equal system-type "windows-nt")
    (w32-send-sys-command #xf030) ;; 0xf030 is the command for maximizing a window)
    )
  nil)

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
; -------------------------------------------
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
 '(custom-safe-themes (quote ("0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "97a2b10275e3e5c67f46ddaac0ec7969aeb35068c03ec4157cf4887c401e74b1" "4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" "a30d5f217d1a697f6d355817ac344d906bb0aae3e888d7abaa7595d5a4b7e2e3" "70cf411fbf9512a4da81aa1e87b064d3a3f0a47b19d7a4850578c8d64cac2353" default)))
 '(foreground-color "#cccccc")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(show-paren-mode t)
 '(sql-product (quote sqlite))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

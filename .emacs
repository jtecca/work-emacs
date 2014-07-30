;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's nt-windows & gnu/linux .emacs
;;;; updated: 2014-07-29
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
(cond
    ((string-equal initial-window-system "w32")
    (progn
      ;(w32-send-sys-command #xf030) ; nt command for maximizing a window
      (set-face-attribute 'default nil :font "Consolas-9")
      (setq default-directory "c:/Users/jeff.tecca/")))
  ((string-equal initial-window-system "x") ; emacs running in an x window
   (progn 
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
     (set-face-attribute 'default nil :font "ProggyCleanTT-12")
     (setq default-directory "~/")
;     (load-theme 'gruvbox) ; need to install gruvbox on linux
     ))
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

;; alternative keybindings for M-x
;; commenting out the rebinds below because ergoemacs
;; completely remaps these keys to C-x + enter
;(global-set-key "\C-x\C-m" 'execute-extended-command)
;(global-set-key "\C-c\C-m" 'execute-extended-command)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.info\\'" . markdown-mode))

;; rebind C-x o to M-o for faster buffer switching
(global-set-key "\M-o" 'other-window)

;; rebind undo (C-x u) to M-u
;; this overwrites the upcase-word binding, but I rarely use that
(global-set-key "\M-u" 'undo) 

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

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
(hl-line-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(toggle-word-wrap)
(setq next-line-add-newlines t)
(setq-default fill-column 72) ; this controls the fcl-mode line
(require 'fill-column-indicator)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(delete-selection-mode t)

;;;;;;;;;;;;;;;;;;
;; auto-complete setup
;(add-to-list 'load-path "path-to-autocomplete")
;(require 'auto-complete)
;(global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;
;; ergoemacs settings
;; below sets the theme to training wheels lvl2, which only sets the 
;; movements keys to i,j,k,l + meta
;; move by word to M-u M-o, and delete by word or char
;; to M-e, M-r, and M-d M-f
(setq ergoemacs-theme "lvl2")
(ergoemacs-mode t)

;;;;;;;;;;;;;;;;;;
;; org-mode settings:
(add-hook 'org-mode-hook '(lambda () (org-indent-mode t)))
(setq org-todo-keywords '((sequence "TODO" "WORKING" "STOPPED" "REVIEW" "DONE")))

;;;;;;;;;;;;;;;;;;
;;;; python-specific settings:
(require 'python-mode) ; use the ehanced python mode instead of python.el
;(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'python-mode-hook '(lambda () (setq python-indent-4)))
(add-hook 'python-mode-hook '(lambda () (fci-mode t)))
;; remap py-execute-region
;; (this might be pointing to only python.el)
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-c \C-r" 'py-execute-region)))
; use ipython3 as the default interpreter
(setq-default py-shell-name "ipython3")
(setq-default py-which-bufname "IPython")
(setq py-force-py-shell-name-p t)
(setq py-smart-indentation t)
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

(global-set-key (kbd "C-+") 'increase-font-size) ; which is really C-<Shift>-=
(global-set-key (kbd "C-=") 'decrease-font-size)

(defun set-pleasant-frame-size ()
  "Returns a frame to a default, windowed position"
  (interactive)
  (when window-system
    (cond
     ((string-equal initial-window-system "w32")
      (set-frame-size (selected-frame) 160 55)
      (let* ((width-offset (/ (eval (x-display-pixel-width)) 5))
             (height-offset (/ (eval (x-display-pixel-height)) 6)))
        (set-frame-position (selected-frame) width-offset height-offset)))
     ((string-equal initial-window-system "x")
      (set-frame-size (selected-frame) 160 55)
      (let* ((width-offset (/ (eval (x-display-pixel-width)) 9))
             (height-offset (/ (eval (x-display-pixel-height)) 12)))
        (set-frame-position (selected-frame) width-offset height-offset))))))
      
(global-set-key (kbd "<f12>") 'set-pleasant-frame-size)

(defun insert-date (arg)
"inserts the current date into the buffer.  if called with an arg, changes the format to the windows-style format."
  (interactive "P")
  (insert (if arg
              (format-time-string "%m/%d/%Y ")
            (format-time-string "%Y-%m-%d "))))
  
(defun insert-time ()
  (interactive)
  (insert (format-time-string "%H:%M:%S ")))

(defun insert-datetime (arg)
"inserts the date and time into the buffer.  if called with an arg, changes the date format to the windows-style format."
(interactive "P")
(insert (if arg
            (format-time-string "%m/%d/%Y %H:%M:%S ")
          (format-time-string "%Y-%m-%d %H:%M:%S "))))

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
(global-set-key (kbd "M-z") 'execute-extended-command) ; because i don't use zap-to-char and this lets me be sloppier
; -------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes (quote ("454dc6f3a1e9e062f34c0f988bcef5d898146edc5df4aa666bf5c30bed2ada2e" default)))
 '(delete-selection-mode t)
 '(ergoemacs-mode t)
 '(initial-scratch-message "")
 '(org-CUA-compatible nil)
 '(recentf-mode t)
 '(shift-select-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's nt-windows & gnu/linux .emacs
;;;; updated: 2014-08-04 
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
      (set-face-attribute 'default nil :font "Consolas-10")
      (setq default-directory "c:/Users/jeff.tecca/")
      (hl-line-mode t) 
      ))
  ((string-equal initial-window-system "x") ; emacs running in an x window
   (progn 
     (set-face-attribute 'default nil :font "ProggyCleanTT-12")
     (setq default-directory "~/")
     (hl-line-mode t)
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
(setq-default fill-column 72) ; this controls the fcl-mode line
(require 'fill-column-indicator)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(delete-selection-mode t)

;;;;;;;;;;;;;;;;;;
;; auto-complete setup
(require 'auto-complete)
(global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;
;; ergoemacs settings
;; below sets the theme to training wheels lvl2, which only sets the 
;; movements keys to i,j,k,l + meta
;; move by word to M-u M-o, and delete by word or char
;; to M-e, M-r, and M-d M-f
;; as of 2014-08-04, turned off by default.  not sure if this is what I want
;(setq ergoemacs-theme "lvl2")
;(ergoemacs-mode t)

;;;;;;;;;;;;;;;;;;
;; org-mode settings:
(add-hook 'org-mode-hook '(lambda () (org-indent-mode t)))
;; set the possible task keywords
(setq org-todo-keywords '((sequence "TODO" "WORKING" "STOPPED" "REVIEW" "DONE")))

;;;;;;;;;;;;;;;;;;
;;;; python-specific settings:
(require 'python-mode) ; use the ehanced python mode instead of python.el
;(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'python-mode-hook '(lambda () (setq python-indent-4)))
(add-hook 'python-mode-hook '(lambda () (fci-mode t)))
(add-hook 'python-mode-hook '(lambda () (set-linum-mode-hook)))
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

;; disabled these functions because i may not have to use them again
;; but left commented for the time being in case I need to quickly reenable them
;; (defun format-kill-point ()
;;   "Removes Garmin GPX formatting and sends formatted string to the kill-ring.

;; Works on a line-by-line basis, so just move the point to a line with GPX
;; information on it and execute the command."
;;   (interactive)
;;   (setq s1 (buffer-substring (line-beginning-position) (line-end-position)))
;;   (message "%s" s1)
;;   (setq s2 (replace-regexp-in-string ".*<trkpt lat=\"" "" s1))
;;   (setq s3 (replace-regexp-in-string "\">.*" "" s2))
;;   (setq s4 (replace-regexp-in-string "\".lon=\"" ", " s3))
;;   (message "%s is now in the kill-ring." s4)
;;   (kill-new s4))

;; (defun format-kill-point-copy (p1 p2)
;;   "Removes Garmin GPX formatting and sends formatted string to the kill-ring.

;; Works on a line-by-line basis, so just move the point to a line with GPX
;; information on it and execute the command."
;;   (interactive "r")
;;   (save-excursion (
;;                    (beginning-of-line)
;;                    (setq s1 (buffer-substring p1 p2))
;;                    (setq s2 (replace-regexp-in-string "\".lon=\"" ", " s1))
;;                    (message "%s is now in the kill-ring." s2)
;;                    (kill-new s2))))

;; (defun add-gpx-header-template ()
;;   "Adds a template gpx track header when track trimming."
;;   (interactive)
;;   (insert "    </trkseg>
;;   </trk>

;;   <trk>
;;     <name>Active Log: DD MMM YYYY HH:SS - TRIMMED</name>
;;     <extensions>
;;       <gpxx:TrackExtension>
;;         <gpxx:DisplayColor>DarkGray</gpxx:DisplayColor>
;;       </gpxx:TrackExtension>
;;     </extensions>
;;     <trkseg>
;; "))

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
  "Reloads (reverts) the current buffer"
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer: " (buffer-name))))

;; custom keybinding hooks
;; commented because these functions have been removed as of 2014-08-04
;; (defun set-vetting-keybinds ()
;;   (local-set-key (kbd "C-c a") 'format-kill-point)
;;   (local-set-key (kbd "C-c t") 'add-gpx-header-template))
;; (add-hook 'xml-mode-hook 'set-vetting-keybinds)

;; global custom keybindings
(global-set-key (kbd "<f5>") 'revert-this-buffer)
(global-set-key (kbd "<f11>") 'make-frame-fullscreen)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'execute-extended-command) ; because i don't use zap-to-char and this lets me be sloppier
; -------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes (quote ("454dc6f3a1e9e062f34c0f988bcef5d898146edc5df4aa666bf5c30bed2ada2e" default)))
 '(delete-selection-mode t)
 '(ergoemacs-mode nil)
 '(initial-scratch-message "")
 '(org-CUA-compatible nil)
 '(recentf-mode t)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


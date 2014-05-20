;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's nt-windows & gnu/linux .emacs
;;;; updated: 2014-05-20 
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
      (set-face-attribute 'default nil :font "ProggyCleanTT-12")
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
;; C-u before M-x slime allows a specific lisp dialect to be loaded
;; as long as the executable is in the system PATH or you give the full path.
(cond
 ((string-equal system-type "windows-nt")
    (load "C:\\quicklisp\\slime-helper.el")
    (setq inferior-lisp-program "wx86cl")
    ;(setq inferior-lisp-program "clisp")
    (setq slime-net-coding-system 'utf-8-unix)
    (slime-setup '(slime-fancy)))
 ((string-equal system-type "gnu/linux")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
       (setq inferior-lisp-program "sbcl")
       ;(setq inferior-lisp-program "clisp")
       ;(setq inferior-lisp-program (expand-file-name "~/bin/ccl/lx86cl64")
       (load (expand-file-name "~/quicklisp/slime-helper.el"))))

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
;; (require 'deferred)
;; (require 'epc)
;; (require 'auto-complete)
;; (global-auto-complete-mode t)
;; (require 'auto-complete-config)
;; (ac-config-default)

;; ;; jedi hooks
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'c-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)

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
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(background-color "#202020")
 '(background-mode dark)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cursor-color "#cccccc")
 '(custom-enabled-themes (quote (church)))
 '(custom-safe-themes (quote ("49d35b72a2eff94e674ff93ef8b699e832b6cd4795acc63194320c37e746d9e8" "a949b0329fd0567e4398d27195abbbbc47b50d8c18f8a40721c37adc1c11e592" "072a2ffa9c332c5b73806627fdac0de6c2d012bcd9eb1e3615d25e3db3b5d474" "c05d1c24cc48d51a990cf7fd3b5beae9030588dc7f80118e50d6fe7cddd80ae7" "95e24cbe7137be4cd24d3e41a5fedf57b693e8744ec31d3ca659d1e15a4f980c" "39766937b7d7b1aba46ab9b4112f48b1e2d8cf967cbbecdbc65defb7126280c6" "1efafa066fbb3d6f9b90169ecd885ddd22b1d5dad2d8f3e9bf945622cec3a6cd" "0c8df69e41d08d49fd0da576d1064f988d54583629d8ce77f221d7007de12ea7" "c29b3444661fff11e29d80edd709dbd0a856a4223e4cd9bd1af7002cf7a3bb39" "14d09b4db0022d1c2238114bd8fba3f4438b066a5025f18959b2cc0cbb39d4db" "75a9fdc76a4b11c5a85458dbd957fe7fa9404036330651163b62a1a2cef4ade5" "7398d4ef8049572ff27137b4643637674d332af357afb44587fd5c4509f24f05" "8876d145a938620a7a4f9aea5ae34004cd371b5d46412568988ed3a61cc247ad" "ecbd57493e3266d96092d6abf10b3bb16c9d3592c2dc861264cd2cf76c714943" "088c09bdaa76ac5de16351eb01bafe474f3ba56009484118b138aae9c6db9a44" "f0934ee1d46b5467bc6f2d4de93e164bba900e5a70a97107a69d7111bbff0eec" "3083e39bb93515b5addfb8be6604d3ba3ba2717007ec590b7cc534a8f6122f80" "605eaa8f2a943d8bf9cbdea3aff56e90723b3718b89dc66b84f173ca66ba138a" "44d7fd8dacafd6cae3f0974cfc839a2dc1170ea545bcf783b3858f46ed9b4903" "4a59180216a3b8393107e537b16f0cefc67e55c535b3067fe341612f1f143a34" "535ca3dd301ad12d920fbf5f546e97a58de0552aac31b16cab50c8d507960d75" "c1aa9b3db94f34184940cde1266fe535d76315efde7ea20a68d289ef24232148" "eaf474d1d5e03ed12932d391d29146a3a76fd17bc374f61af18da648bec4c3b1" "193427aa65bea45dbfb7781f583bc1b0a59cb6b469289719bc5f016c00cd75ef" "ac4a6814a427b7678fdf954eb76d46acf24d5f7b936c8573396074b9e8768628" "d8cf63bfd1a7fa42e0c1a11fce4f1c0347e29fd7049cbaff622c2b55bfc5d7e1" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "d77cea9809c860ee271a8e9dbcca85f6dcdd5ffd3a3eb9a5524529b9b54ac214" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "4eaad15465961fd26ef9eef3bee2f630a71d8a4b5b0a588dc851135302f69b16" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "97a2b10275e3e5c67f46ddaac0ec7969aeb35068c03ec4157cf4887c401e74b1" "4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" "a30d5f217d1a697f6d355817ac344d906bb0aae3e888d7abaa7595d5a4b7e2e3" "70cf411fbf9512a4da81aa1e87b064d3a3f0a47b19d7a4850578c8d64cac2353" default)))
 '(fci-rule-color "#383838")
 '(foreground-color "#cccccc")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors (quote (("#49483E" . 0) ("#67930F" . 20) ("#349B8D" . 30) ("#21889B" . 50) ("#968B26" . 60) ("#A45E0A" . 70) ("#A41F99" . 85) ("#49483E" . 100))))
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 18)
 '(show-paren-mode t)
 '(sql-product (quote sqlite))
 '(syslog-debug-face (quote ((t :background unspecified :foreground "#A1EFE4" :weight bold))))
 '(syslog-error-face (quote ((t :background unspecified :foreground "#F92672" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#A6E22E"))))
 '(syslog-info-face (quote ((t :background unspecified :foreground "#66D9EF" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#E6DB74"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#FD5FF0"))))
 '(syslog-warn-face (quote ((t :background unspecified :foreground "#FD971F" :weight bold))))
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list (quote (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

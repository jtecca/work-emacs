;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jeff tecca's .emacs
;;;; updated: 2015-01-09
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; os-specific settings
;; maximize the frame on startup
(cond
    ((string-equal initial-window-system "w32")
    (progn
      ;(w32-send-sys-command #xf030) ; nt command for maximizing a window
      (set-face-attribute 'default nil :font "Cousine-9")
      (setq default-directory "c:/Users/jeff.tecca/")
      ;; add in mingw shell utils
      (add-to-list 'exec-path "C:/MinGW/bin" (expand-file-name "~/AppData/Local/Continuum/Anaconda"))))
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
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "wx86cl64")))
 ((string-equal initial-window-system "x")
  (progn
    (setq inferior-lisp-program "ccl64")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))))
 ((string-equal initial-window-system "nil")
  (progn
    (setq inferior-lisp-program "ccl64")
    (load (expand-file-name "~/quicklisp/slime-helper.el")))))

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
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f11>") 'make-frame-fullscreen)
;; (global-set-key (kbd "<f12>") 'find-function)
; <f10> is bound to pull down the menu bar (useful when in terminal)
; next keybinding is purposely cumbersome to reduce accidental reversions
(global-set-key (kbd "M-C-<f5>") 'revert-this-buffer)
;; copy-line keybinding
(global-set-key (kbd "C-c k") 'copy-line)
;; move up and down lines with meta as well as ctrl
;; very useful for meta-heavy commands without switching between M and C
(global-set-key (kbd "M-n") 'next-line)
(global-set-key (kbd "M-p") 'previous-line)

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
(eldoc-mode 1)
(require 'rainbow-delimiters)
(rainbow-delimiters-mode t)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)
(setq cursor-type 'box)
(require 'highlight-numbers)
(highlight-numbers-mode 1)

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
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(toggle-word-wrap 1)
(setq next-line-add-newlines t)
(setq-default fill-column 79)
(require 'fill-column-indicator)
(fci-mode 1)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(delete-selection-mode 1)
(tooltip-mode -1)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)
(setq redisplay-dont-pause 1)

;;;;;;;;;;;;;;;;;;
;; helm
(require 'helm-config)
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
(setq sql-postgres-options (file-to-string "~/postgresql/connection.info"))
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
(autoload 'enable-paredit-mode "paredit" "Turn on structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
;; make eldoc aware of paredit
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;;;;;;;;;;;;;;;;;;
;; org-mode settings
;; make each new layer indent for easier reading
(add-hook 'org-mode-hook '(lambda () (org-indent-mode t)))
;; set the task keywords
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
;;; For more information
;; Intro Doc: https://github.com/winterTTr/ace-jump-mode/wiki
;; FAQ      : https://github.com/winterTTr/ace-jump-mode/wiki/AceJump-FAQ

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
 python-shell-completion-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;; custom python keybindings
(add-hook 'python-mode-hook
          '(lambda ()
             (turn-on-fci-mode)
             (local-set-key (kbd "C-c i") 'python-insert-breakpoint)
             (local-set-key (kbd "<f1>") 'magit-status)))

; -------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(ansi-term-color-vector
   [unspecified "#282a2e" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#e0e0e0"])
 '(custom-enabled-themes (quote (flatui)))
 '(custom-safe-themes
   (quote
    ("1011be33e9843afd22d8d26b031fbbb59036b1ce537d0b250347c19e1bd959d0" "013e87003e1e965d8ad78ee5b8927e743f940c7679959149bbee9a15bd286689" "0f002f8b472e1a185dfee9e5e5299d3a8927b26b20340f10a8b48beb42b55102" "26247bcb0b272ec9a5667a6b854125450c88a44248123a03d9f242fd5c6ec36f" "e35ef4f72931a774769da2b0c863e11d94e60a9ad97fb9734e8b28c7ee40f49b" "ab3e4108e9b6d9b548cffe3c848997570204625adacef60cbd50a39306866db1" "0d19ff470ad7029d2e1528b3472ca2d58d0182e279b9ab8acd65e2508845d2b6" "0795e2c85394140788d72d34969be4acb305e4a54149e7237787d9df27832fbb" "256ab343f4935915bf640ead48582bd9ca86c1afe9b17ab84a327b02a60b2698" "89586444c668bae9ec7e594bc38b3a956f31dc6cb7c851ed40411cc4ff770708" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "0c97dcff4ea6ac23af383e6153723a712c1de3a4b427e97d1e473504dbc2fe06" "930a202ae41cb4417a89bc3a6f969ebb7fcea5ffa9df6e7313df4f7a2a631434" "c87cc60d01cf755375759d165c1d60d9586c6a31f0b5437a0378c2a93cfc8407" "96efbabfb6516f7375cdf85e7781fe7b7249b6e8114676d65337a1ffe78b78d9" "7ed6913f96c43796aa524e9ae506b0a3a50bfca061eed73b66766d14adfa86d1" "c739f435660ca9d9e77312cbb878d5d7fd31e386a7758c982fa54a49ffd47f6e" "b458d10c9ea0c8c465635b7b13e1bd23f04e6b696b1ca96cb2c4eca35a31641e" "573e46dadf8c2623256a164831cfe9e42d5c700baed1f8ecd1de0675072e23c2" "7dd515d883520286fc8936ce32381fb01b978d0d7cfb6fe56f7f55d8accbf63a" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "ca229a0a89717c8a6fe5cd580ee2a85536fbafce6acb107d33cf38d52e2f492c" "024b0033a950d6a40bbbf2b1604075e6c457d40de0b52debe3ae994f88c09a4a" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "c3e567dedaa800e869d879c4df8478237d6ea31fd04464086fd674c864fe4d71" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "c0dd5017b9f1928f1f337110c2da10a20f76da0a5b14bb1fec0f243c4eb224d4" "7dd0db710296c4cec57c39068bfffa63861bf919fb6be1971012ca42346a417f" "d809ca3cef02087b48f3f94279b86feca896f544ae4a82b523fba823206b6040" "2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" "97a2b10275e3e5c67f46ddaac0ec7969aeb35068c03ec4157cf4887c401e74b1" "c01f093ab78aad6ae2c27abc47519709c6b3aaa2c1e35c712d4dd81ff1df7e31" "569dc84822fc0ac6025f50df56eeee0843bffdeceff2c1f1d3b87d4f7d9fa661" "d9fe836a71bcff99f122206f2ffaf4417a84b25dc9aa17007ffbbee6aa95366d" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "ad9fc392386f4859d28fe4ef3803585b51557838dbc072762117adad37e83585" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "72407995e2f9932fda3347e44e8c3f29879c5ed88da71f06ba4887b0596959a4" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" "8d6fb24169d94df45422617a1dfabf15ca42a97d594d28b3584dc6db711e0e0b" "08efabe5a8f3827508634a3ceed33fa06b9daeef9c70a24218b70494acdf7855" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "a31c86c0a9ba5d06480b02bb912ae58753e09f13edeb07af8927d67c3bb94d68" "bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4" "27f7350e7050c46ecfc40efb23f9e9e85159b2dd597618bc5d91b83f1a18bcf5" "6f6d50ba2a1755dcc16000e436d83bbc791b614bf3465165e33deebc4102773c" "2688d8acecee0a1f7025fc413c02d4da18eec3bf8bf28153200df5436edb9d16" "5e4c1a37b7ab2755f7b28435378ed3910fc02cfe67457c1f6ae24bdb05619462" "ef949d2e4db27aa9d40d4a2a81b447683e2f81895664fe7c1bc0ced03b673a04" "f7f45b82eea472a615b88eb31d4136822aedc1c79d7409b343153103965aa8d3" "4575653b95c0d094521036333401165a7ee4f21a5fe66cdc3e2d26fc4210586c" "e006255e26c3da97e75f27d95787ccf4e8a745e6777d0069b8ace2ad169dc5b2" "0969c6c3dd7ea3a806cf9c02e880ae8eb25fa3c5acda384c831f16d997e5969e" "bf022cbe1e77f694439857b812eedcb320313ef725e1bdddb980a4630888db3a" "47c4286586da078b8946889f008276d209022df41cca28a2b4d3147f5b450162" "953263458f4b251401274b2224aa0804191cb90655976a9e852ea4ca9b76d398" "3d381872a4502adbc9db1b46a270bc4fbff5202ed5bb9d5e95d15491ed5601eb" "891c439a6dc3f077345bf773b2a9bfafdd4cefeab08d13ee700ba9ea66f5aa62" "85968d3e525a9dddde055088e65e78c0a1fc5737b8e4ae906ecdacef76341ee8" "aebbabdcf83ef29c6384eb6d75b6f8da05bcf4c6c8b89e762670a476cc3af55a" "afd805c5e6d682741bd1f3e52047c10deda38aed85f16f37d97c31b226245952" "cb7e81fd18e1c43469c6bb529cce9268fe452cce01326fc625d7e2071c81b4c1" "31dc7bc9a83e27bcefc10d0d6e7001c644cdbdc5b2955e775cf18bc138dab7d9" "7b0df3af11ea466aa1831754b74388d96a4b64ef98971cc7126c4b35c8cc60df" "a53d74bd879773bcea6e85257f78e67fdb61f0c6ec4e648acdff0a7dc879c6c9" "5c98366f283b97e43b6576296663532620f47851364df9875a6af354cee73a89" "b9278adb8f29c8b57e30bf925eaa9134c08420d31ad82bdffb2c3d803152588c" "a951e552a76ca104c48ee9cfd68fc2386db5660e880d54ee33882cf4e486f591" "4c36bc42ca033ed989ffa34e48679b2a6e6c97aa4d50e3619338296d20b883f8" "0e2966df314f1fd3ab00484ac3f8a59cfbf10157ce1c927311c2c0a79fb7bbff" "fbab2780a309fed6c7000d522de70e95ca4812fde9e54931b577a9e07b2d1641" "b23121deed3790fbb1bc567ca2a0b04de733143fa06a99124ed2f1c4ee811c90" "0bce907b038c49bac4e60c329c084c29d4f88347352642f443be716f733fe2f5" "e791713b8d918c7f3c6fd499f0cd87b9f40e03c3ae882ff6057b2cd2d13a35af" "d8dbcb1fa5a26ff351f104f8144b00498819ab1a9b053eb98cceb0bf1a02b9a0" "4c4c7d19b9a3844da4b499664ff2be2edd44e56303e896936329b21f657d75fa" "45d3aa79d35b2b3e31e6f6c8e79bc8e8a8a4183b4294ff41c8027f26e42dbe5a" "6036c315dd7f9d8f930d9c7fe0e46a6b8942736f0a1f3e3c060d5324dc7d3a86" "6ba69d237b3e2465492b87f4836ab1b82e324056eafae6aaa3ebde9b2cd5ec7c" "3fdd718695d948ca0576bea245af183f053b01567ea2b5da7266b564ad07a5ac" "5699a205d01cfa88ed9292e2244e26b82a3f55bd18caecbba90f376efe31f52e" "1e00c42b7ae2d826e233b65b57822579a0dd7b9cd95b7a5eef94555d6f507b09" "950a135f35029110f8c938dad4ff5c35f77a038995b6b8bba98b65dae1c3cabe" "04fc5453daaba865d6ae45f4e45c8a50e4167a008d9481a0aafe0de7f6b237da" "eaa0610de527691a0bd4648e0541821bca3478ae9d8bb957cf8995de7805800e" "53766ce623ded9a5677ebbaeca49b52e6c9be6b678c8240906954e2d01b72c9e" "028ad3c88a0c9e0e21946617871318e91c337e8a43f50644b78619865d130691" "9d321de2c3f777c6edea494c77337bb0c230ea12032a4c0c0125a7b2fdea25a3" "b8e3c880be5aeee7f0134fb241a9ba9636bf63a9827249474b471ac7c3e8ef93" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" default)))
 '(fci-rule-color "#383838")
 '(fringe-mode 6 nil (fringe))
 '(linum-format " %7d ")
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111")
 '(rainbow-identifiers-cie-l*a*b*-lightness 70)
 '(rainbow-identifiers-cie-l*a*b*-saturation 20)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

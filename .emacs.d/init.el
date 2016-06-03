;; Set up package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; auto-update packages
(require 'auto-package-update)
(setq auto-package-update-interval 7)
(setq auto-package-update-delete-old-versions t)
(auto-package-update-at-time "12:00")
(auto-package-update-maybe)

;;----------------------------------------------------------------------------
;; data structure manipuation functions
;;----------------------------------------------------------------------------
;; alist manipulation
(defun mapassoc (func alist)
  "map func down alist, listing results"
  (mapcar (lambda (entry) (funcall func (car entry) (cdr entry))) alist))

(defun mapa (func alist)
  "map func down alist, ignoring results"
  (mapc (lambda (entry) (funcall func (car entry) (cdr entry))) alist))

;;----------------------------------------------------------------------------
;; Interactive editing commands
;;----------------------------------------------------------------------------
;; scroll a page to the right
(defun ewd-scroll-left ()
  "Move cursor forward one page-width (or to end of line if that comes first)."
  (interactive)
  (scroll-left 10))

;; scroll a page to the left
(defun ewd-scroll-right ()
  "Move cursor backward one page-width (or to beginning of line if that comes first)."
  (interactive)
  (scroll-right 10))

;; execute a shell command, inserting output at point
(defun ewd-insert-shell-command (str)
  "execute a shell command, inserting output at point"
  (interactive "sShell command: ")
  (insert (shell-command-to-string str)))

;; repaint current block and recenter around current line
(defun ewd-font-lock-repaint (&optional pos)
  "Repaint current visible window and recenter buffer around current line.  With pos,
pos is passed as an argument to recenter."
  (interactive "P")
  (recenter pos)
  (save-excursion
    (font-lock-fontify-region (window-start) (window-end))))

;; duplicate current line
(require 'misc)
(defun ewd-dup-line()
  "Copy the current line and insert copy as the following line."
  (interactive)
  (save-excursion
    (forward-line 1)
    (copy-from-above-command)
    (insert "\n")))

;; move to the beginning of buffer without clobbering the mark
(defun ewd-beginning-of-buffer-nomark ()
  "Move point to the beginning of the current buffer
without clobbering the mark."
  (interactive)
  (goto-char (point-min)))

;; move to the end of buffer without clobbering the mark
(defun ewd-end-of-buffer-nomark ()
  "Move point to the end of the current buffer without
clobbering the mark."
  (interactive)
  (goto-char (point-max)))

;; print X font name for Windows font
(when (eq window-system 'w32)
    (defun ewd-get-x-font-name ()
      "Select font from dialog box, then print X font name in current buffer."
      (interactive)
      (insert (prin1-to-string (w32-select-font)))))

;; add a string in front of all lines in the region
(defun ewd-prepend (start end s)
  "Add a string in front of all lines in the region."
  (interactive "*r\nMEnter a string: ")
  (save-excursion
    (save-restriction
      (narrow-to-region
       (progn (goto-char start) (beginning-of-line) (point))
       (progn (goto-char end) (end-of-line) (point)))
      (goto-char (point-min))
      (beginning-of-line)
      (while (not (eobp))
        (insert s)
        (forward-line 1)))))

;; remove a string from the beginning of all lines in the region
(defun ewd-unprepend (start end s)
  "Remove a string from the front of all lines in the region."
  (interactive "*r\nMEnter a string: ")
 (save-excursion
    (save-restriction
      (narrow-to-region
       (progn (goto-char start) (beginning-of-line) (point))
       (progn (goto-char end) (end-of-line) (point)))
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at (regexp-quote s))
            (delete-region (match-beginning 0) (match-end 0)))
        (forward-line 1)))))

;; move cursor to the previous line or get previous history item, depending
;; on whether we're at a shell mode prompt
(defun ewd-comint-up (arg)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-previous-input arg)
    (previous-line arg)))

;; move cursor to the next line or get next history item, depending
;; on whether we're at a shell mode prompt
(defun ewd-comint-down (arg)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-next-input arg)
    (next-line arg)))

;; move cursor to the previous line or get previous history item, depending
;; on whether we're at a shell mode prompt
(defun ewd-term-up (arg)
  (interactive "p")
  (if (term-after-pmark-p)
      (term-send-up)
    (previous-line arg)))

(require 'term)
;; move cursor to the next line or get next history item, depending
;; on whether we're at a shell mode prompt
(defun ewd-term-down (arg)
  (interactive "p")
  (if (term-after-pmark-p)
      (term-send-down)
    (next-line arg)))

;; toggle term mode between char and line
(defun ewd-toggle-term-mode ()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

;;----------------------------------------------------------------------------
;; Buffer manipulation functions
;;----------------------------------------------------------------------------
(defun ewd-other-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (switch-to-buffer (other-buffer)))

;; don't really ever kill the *scratch* buffer
(defun ewd-kill-current-buffer ()
  "Kill current buffer, unless it is the *scratch* buffer."
  (interactive)
  (cond ((equal (buffer-name) "*scratch*")
         (delete-region (point-min) (point-max))
         (bury-buffer))
        (t (kill-buffer (current-buffer)))))

;;----------------------------------------------------------------------------
;; Make cursor stay in the same column when scrolling.  Thanks to
;; David Biesack (sasdjb@unx.sas.com).
;;----------------------------------------------------------------------------
(defvar default-column-tracking-functions
  '(scroll-up
    scroll-down
    previous-line
    next-line
    forward-line)
  "List of functions which track the current column")

(defvar default-column nil
  "The column to track in functions registered with `track-column'.
This variable is buffer local")
(make-variable-buffer-local 'default-column)

(defun default-column ()
  "Return the default column to track in cursor motion functions
which are advised by `track-column'"
  (if (memq last-command default-column-tracking-functions)
      default-column
    (setq default-column (current-column))))

(defun track-column-advice (orig-fun &rest args)
    "Keep cursor in the same column if possible"
  (let ((col (default-column)))
    (apply orig-fun args)
    (move-to-column col)))

(defun track-column-advice-name (function)
  (make-symbol (format "track-column-in-%s" function)))

(defun track-column-for (function)
  (add-to-list 'default-column-tracking-functions function)
  (add-function :around function (track-column-advice-name function)))

(mapc 'track-column-for default-column-tracking-functions)

;;----------------------------------------------------------------------------
;; Make subsequently opened frames offset from the first one
;;----------------------------------------------------------------------------
(defvar ewd-frame-offset 25
  "*Amount to offset each subsequently created frame")
(defadvice x-create-frame-with-faces (before ewd-create-frame activate)
  "Make subsequent frames open at an offset"
  (let* ((topelt (assoc 'top default-frame-alist))
         (leftelt (assoc 'left default-frame-alist))
         (top (if (null topelt)
                  (car (setq default-frame-alist
                             (cons '(top . 0) default-frame-alist)))
                topelt))
         (left (if (null leftelt)
				   
                   (car (setq default-frame-alist
                              (cons '(left . 0) default-frame-alist)))
                 leftelt))
         (toppos (cdr top))
         (leftpos (cdr left)))
    (setcdr top (+ toppos ewd-frame-offset))
    (setcdr left (+ leftpos ewd-frame-offset))))

;;----------------------------------------------------------------------------
;; Tweak built-in behaviors
;;----------------------------------------------------------------------------
;; use y-or-n-p instead of yes-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable some functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;;----------------------------------------------------------------------------
;; Toggle keypad as prefix / keypad as numbers
;;----------------------------------------------------------------------------
;; use keypad keys as numbers by default
(defvar ewd-kp-usage 'num "Keypad key usage")

;; make keypad numbers be treated as prefix keys
(defun ewd-kp-as-prefix () 
  "Make keypad numbers act as prefix keys."
  (define-key function-key-map [kp-0] [27 ?0])
  (define-key function-key-map [kp-1] [27 ?1])
  (define-key function-key-map [kp-2] [27 ?2])
  (define-key function-key-map [kp-3] [27 ?3])
  (define-key function-key-map [kp-4] [27 ?4])
  (define-key function-key-map [kp-5] [27 ?5])
  (define-key function-key-map [kp-6] [27 ?6])
  (define-key function-key-map [kp-7] [27 ?7])
  (define-key function-key-map [kp-8] [27 ?8])
  (define-key function-key-map [kp-9] [27 ?9])
  (define-key function-key-map [kp-decimal] [27 ?-])
  (message "Keypad numbers are prefix keys."))

;; make keypad numbers be treated as numbers
(defun ewd-kp-as-num ()
  "Make keypad numbers act as numbers."
  (define-key function-key-map [kp-0] [?0])
  (define-key function-key-map [kp-1] [?1])
  (define-key function-key-map [kp-2] [?2])
  (define-key function-key-map [kp-3] [?3])
  (define-key function-key-map [kp-4] [?4])
  (define-key function-key-map [kp-5] [?5])
  (define-key function-key-map [kp-6] [?6])
  (define-key function-key-map [kp-7] [?7])
  (define-key function-key-map [kp-8] [?8])
  (define-key function-key-map [kp-9] [?9])
  (define-key function-key-map [kp-decimal] [?.])
  (message "Keypad numbers are numbers."))

;; toggle the way keypad numbers are interpreted
(defun ewd-toggle-kp-usage ()
  "Switch the way keypad numbers are interpreted."
  (interactive)
  (cond ((eq ewd-kp-usage 'num)
         (ewd-kp-as-prefix)
         (setq ewd-kp-usage 'prefix))
        ((eq ewd-kp-usage 'prefix)
         (ewd-kp-as-num)
         (setq ewd-kp-usage 'num))))

;;----------------------------------------------------------------------------
;; Set up shell and bash commands
;;----------------------------------------------------------------------------
(require 'multi-term)
(setq multi-term-program "zsh")
(setq shell-file-name "zsh")
(setq term-term-name "xterm-16color")

(require 'shell)
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map [up] 'ewd-comint-up)
            (define-key shell-mode-map [down] 'ewd-comint-down)))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-mode-map [up] 'ewd-term-up)
            (define-key term-raw-map [up] 'ewd-term-up)
            (define-key term-mode-map [down] 'ewd-term-down)
            (define-key term-raw-map [down] 'ewd-term-down)
            (define-key term-mode-map "\C-c\C-j" 'ewd-toggle-term-mode)
            (define-key term-mode-map "\C-c\C-k" 'ewd-toggle-term-mode)
            (define-key term-raw-map "\C-c\C-j" 'ewd-toggle-term-mode)
            (define-key term-raw-map "\C-c\C-k" 'ewd-toggle-term-mode)
            ))



;;----------------------------------------------------------------------------
;; set up exec-path
;;----------------------------------------------------------------------------
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

;;----------------------------------------------------------------------------
;; Load 'grep' library, which among other things prevents grep-find
;; from recursing into .svn directories
;; ----------------------------------------------------------------------------
(require 'grep)

;;----------------------------------------------------------------------------
;; Ruby programming
;;----------------------------------------------------------------------------
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;;----------------------------------------------------------------------------
;; Magit for git integration
;;----------------------------------------------------------------------------
(global-magit-file-mode)

;;----------------------------------------------------------------------------
;; Set up auxiliary stuff for python
;;----------------------------------------------------------------------------
(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(global-flycheck-mode t)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

; auto-complete mode extra settings
(setq ac-auto-start 2)
(setq ac-use-menu-map t)
(setq ac-candidate-limit 20)

;;----------------------------------------------------------------------------
;; set up python
;;----------------------------------------------------------------------------
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'auto-complete-mode)

(add-hook 'python-mode-hook
	  (lambda ()
	    (jedi:setup)
	    (jedi:ac-setup)
		(local-set-key "\C-cd" 'jedi:show-doc)
		(local-set-key (kbd "M-SPC") 'jedi:complete)
		(local-set-key (kbd "M-.") 'jedi:goto-definition)))



;;----------------------------------------------------------------------------
;; Make sure TZ env var isn't set
;;----------------------------------------------------------------------------
(setenv "TZ" nil)

;;----------------------------------------------------------------------------
;; Hide the tool bar
;;----------------------------------------------------------------------------
(tool-bar-mode -1)
(menu-bar-mode (if window-system 1 -1))
(scroll-bar-mode -1)

;;----------------------------------------------------------------------------
;; Set personal information
;;----------------------------------------------------------------------------

(setq user-mail-address "eli.daniel@gmail.com")
(setq user-full-name "Eli Daniel")

;;----------------------------------------------------------------------------
;; Set up frame position and coloring
;;----------------------------------------------------------------------------
(setq frame-background-mode 'dark)
(setq default-frame-alist
      '((top . 50)
        (left . 100)
        (width . 150)
        (height . 50)
        (cursor-type . bar)
        (font . "Monaco-12")
        ))

;;----------------------------------------------------------------------------
;; Save/restore window layouts
;;----------------------------------------------------------------------------
(require 'workgroups2)
(setq wg-prefix-key "\C-cw")
(workgroups-mode 1)

;;----------------------------------------------------------------------------
;; Set up environment
;;----------------------------------------------------------------------------
(setq grep-find-use-xargs 'exec)		; use -print0 and xargs -0
(setq compilation-scroll-output t)      ; scroll output in compilation window
(setq-default tab-width 4)              ; tab = 4 spaces
(setq-default ediff-ignore-similar-regions t) ; ignore whitespace differences in ediff
(setq-default indent-tabs-mode nil)     ; use spaces (not tabs) for indenting
(setq require-final-newline t)          ; always terminate last line in file
(setq-default major-mode 'text-mode)    ; default mode is text mode
(setq next-screen-context-lines 1)      ; # of lines of overlap when scrolling
(setq auto-save-interval 300)           ; autosave every N characters typed
(setq-default fill-column 70)           ; the column beyond which do word wrap
(setq scroll-preserve-screen-position t); make pgup/dn remember current line
(setq next-line-add-newlines nil)       ; don't scroll past end of file
(global-auto-revert-mode 1)             ; autorevert buffers if files change
(setq revert-without-query '(".*"))     ; revert all files automatically
(setq w32-swap-mouse-buttons t)         ; swap middle and right mouse buttons on Windows
(mouse-avoidance-mode 'animate)         ; make cursor get out of the way when I type near it
(setq w32-enable-synthesized-fonts nil) ; enable synthesized italics/bold on Windows
(setq w32-list-proportional-fonts t)	; include proportional fonts in the font dialog
(setq max-specpdl-size 4000)            ; boost number of Lisp bindings allowed
(setq max-lisp-eval-depth 10000)         ; boost eval depth
(setq backup-directory-alist '(("." . "/tmp/emacs-backups"))) ; put all backups in one place
(setq-default truncate-lines t)         ; don't wrap long lines... just have them go off the screen
(setq confirm-kill-emacs 'y-or-n-p)     ; confirm before closing emacs
(ansi-color-for-comint-mode-on)         ; show ANSI color codes
(setq gdb-many-windows t)               ; use many windows for debugging
(setq ring-bell-function 'ignore)	    ; don't beep
(setq split-height-threshold nil)       ; split windows horizontally

;;----------------------------------------------------------------------------
;; Line numbers, fringe, and modeline
;;----------------------------------------------------------------------------
(require 'hlinum)
(defvar linum-format-fmt
  "Buffer-local variable for format of line numbers in left margin")

(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-format-fmt
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'fringe)))

(unless window-system
  (setq linum-format 'linum-format-func))

(line-number-mode -1)
(hlinum-activate)
(global-linum-mode)

(require 'smart-mode-line)
(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(setq sml/modified-char "*")
(setq sml/read-only-char "%%")
(setq sml/show-eol t)
(setq rm-blacklist (mapconcat 'identity '("FlyC" "ARev") "\\|"))
(sml/setup)

;;----------------------------------------------------------------------------
;; Set up additional packages
;;----------------------------------------------------------------------------
;; emacs-server
(server-start)

;; use a more windowsy undo-redo system
(require 'redo+)
(define-key-after
  (lookup-key global-map [menu-bar edit])
  [redo]
  '("Redo" . redo) 'undo)
(define-key-after
  (lookup-key global-map [menu-bar edit])
  [undo-seperator] '("--" . undo-seperator) 'redo)
(setq undo-no-redo t)

;; set up nicer buffer switching and other stuff
(ido-mode)
(add-hook 'ido-setup-hook
          (lambda()
            (define-key ido-completion-map
              "\C-xg" 'ido-enter-magit-status)))
(smex-initialize)
          
;; make end-of-line conversion easier.  The eol-conversion package was written
;; by Francis J. Wright <F.J.Wright@qmw.ac.uk>.  Available from
;; http://centaur.maths.qmw.ac.uk/Emacs/
(setq eol-mnemonic-dos "\\")
(setq eol-mnemonic-mac ":")
(setq eol-mnemonic-unix "/")
(setq eol-mnemonic-undecided "?")
(require 'eol-conversion)

;; Allow sorting in dired.  The dired-sort package was written by Francis J. Wright
;; <F.J.Wright@qmw.ac.uk>.  Available from http://centaur.maths.qmw.ac.uk/Emacs/.
(add-hook 'dired-load-hook
          (lambda ()
            (require 'dired-sort-menu)))

;; show directories in buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) 

;; Allow some variables to be set on a mode-by-mode-basis
(make-variable-buffer-local 'tempo-interactive)  ; prompting to fill in templates
(make-variable-buffer-local 'ps-line-number)     ; print with line numbers

;; ;; Paren matching
(require 'smartparens-config)
(show-smartparens-global-mode +1)

;; Make .com, .out, and .tpl files be text rather than binary
(when (eq window-system 'w32) 
  (setq file-name-buffer-file-type-alist
		(cons '("\\.\\([Cc][Oo][Mm]\\|out\\|tpl\\)$") file-name-buffer-file-type-alist)))

;; Printing
(setq ps-printer-name "//corpprint/15-Development")
(setq ps-spool-duplex t)

;; Add "Get X font name" to menu
(when (eq window-system 'w32)
       (define-key-after (lookup-key global-map [menu-bar edit])
         [select-font-menu-separator] '("--" . select-font-menu-separator) t)
       (define-key-after (lookup-key global-map [menu-bar edit])
         [get-font-name] '("Get X font name" . ewd-get-x-font-name) t))

;; Map arrow keys to correct functions in dial-in
(add-hook 'term-setup-hook
          (function
           (lambda ()
             (define-key esc-map "["  nil)
             (define-key esc-map "[A" 'previous-line)
             (define-key esc-map "[B" 'next-line)
             (define-key esc-map "[C" 'forward-char)
             (define-key esc-map "[D" 'backward-char)
             (define-key esc-map "[OA" 'previous-line)
             (define-key esc-map "[OB" 'next-line)
             (define-key esc-map "[OC" 'forward-char)
             (define-key esc-map "[OD" 'backward-char)
             )))


;; Make telnet-mode work on windows
;; telnet with telnet.exe written by Naftali Ramati (naftali@harmonic.co.il).
;; Thanks to Zoltan Kemenczy (zoltan@nabu.isg.mot.com).
(when (eq window-system 'w32)
  (defun zoltan-telnet (host)
    "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*telnet-HOST*'.
Normally input is edited in Emacs and sent a line at a time."
    (interactive "sOpen telnet connection to host: ")
    (let* ((comint-delimiter-argument-list '(?\  ?\t))
           (name (concat "telnet-" (comint-arguments host 0 nil) ))
           (buffer (get-buffer (concat "*" name "*")))
           process)
      (when (eq window-system 'w32)
        (setq telnet-new-line "\n"))
      (if (and buffer (get-buffer-process buffer))
          (pop-to-buffer (concat "*" name "*"))
        (pop-to-buffer (make-comint name telnet-program nil host))
        (setq process (get-buffer-process (current-buffer)))
        (set-process-filter process 'telnet-initial-filter)
        (accept-process-output process)
        (telnet-mode)
        (setq comint-input-sender 'telnet-simple-send)
        (setq telnet-count telnet-initial-count))))
  (require 'telnet)
  (fset 'telnet 'zoltan-telnet))

;; Set up msb to make a spiffy, organized buffer menu
(require 'msb)
(add-to-list 'msb-menu-cond
             ' ((memq major-mode
                      '(java-mode jde-mode))
                3040 "Java Files (%d)"))
(msb-mode t)

;; set up generic-x for windows-specific stuff
(setq generic-define-mswindows-modes t)
(require 'generic-x)

;;----------------------------------------------------------------------------
;; Color theme
;; ----------------------------------------------------------------------------
;; color-theme-solarized from MELPA, hacked to switch definitions for
;; region and secondary-selection, and to add the right colors for
;; bold ANSI term colors.

;; define separate faces for "bright" term colors instead of applying
;; bold to the non-bright colors
(defface term-color-brblack nil "")
(defface term-color-brred nil "")
(defface term-color-brgreen nil "")
(defface term-color-bryellow nil "")
(defface term-color-brblue nil "")
(defface term-color-brmagenta nil "")
(defface term-color-brcyan nil "")
(defface term-color-brwhite nil "")

;; load theme (which is hacked to set specs for these new faces)
(setq solarized-bold nil)
(load-theme 'solarized t)
(enable-theme 'solarized)

;; set up advice to use the appropriate colors instead of bolding the
;; underlying colors
(defun ansi-term--ewd-get-bold-color (oldfun &rest args)
  (let ((ansi-term-color-vector 
         (if term-ansi-current-bold
             '[term
               term-color-brblack
               term-color-brred
               term-color-brgreen
               term-color-bryellow
               term-color-brblue
               term-color-brmagenta
               term-color-brcyan
               term-color-brwhite]
           ansi-term-color-vector)))
    (when (>= (car args) 30)
      (setq term-ansi-current-bold nil))
    (apply oldfun args)))
(advice-add 'term-handle-colors-array :around #'ansi-term--ewd-get-bold-color)

;;----------------------------------------------------------------------------
;; a better Perl mode.  Available from ftp://ftp.math.ohio-state.edu/pub/users/ilya/perl/
;;----------------------------------------------------------------------------
(defalias 'perl-mode 'cperl-mode)
(autoload 'cperl-mode "cperl-mode" "better mode for editing Perl programs" t)
(add-hook 'cperl-mode-hook
          (lambda()
            (setq cperl-indent-level 4)))
(add-to-list 'auto-mode-alist '("\\.ci\\'" . cperl-mode))


;; read man pages without external programs.  Woman package written by
;; Francis J. Wright <F.J.Wright@qmw.ac.uk>. Available from
;; http://www.maths.qmw.ac.uk/~fjw/public_emacs/
(when (eq window-system 'w32)
  (autoload 'woman "woman" "Read man pages without external programs" t)
  (setq woman-path "$EMACSDATA")
  (setq woman-manpath '("c:/cygwin/usr/man" "c:/cygwin/usr/share/man" "c:/cygwin/usr/local/man"))
  (setq woman-imenu t))

;;----------------------------------------------------------------------------
;; Set up JSP editing with mmm-mode
;;----------------------------------------------------------------------------
(require 'mmm-mode)
(require 'mmm-sample)

;; Use jsp-helper-mode + mmm-mode for JSP files
(add-to-list 'auto-mode-alist '("\\.jsp$" . html-helper-mode))

;; Use mmm-mode to activate jde mode for the Java part of the JSP file
;; and javascript mode for any JavaScript code in it.
(mmm-add-mode-ext-class 'html-helper-mode "\\.jsp\\'" 'jsp)
(mmm-add-mode-ext-class 'html-helper-mode "\\.jsp\\'" 'html-js)
(mmm-add-mode-ext-class 'html-helper-mode     "\\.js\\'"  'html-js)
(mmm-add-mode-ext-class 'html-helper-mode     "\\.html\\'"  'html-js)

(setq mmm-global-mode 'some)

;;----------------------------------------------------------------------------
;; Set up XQuery editing with xquery-mode
;;----------------------------------------------------------------------------
(require 'xquery-mode)

;; Use jsp-helper-mode + mmm-mode for JSP files
(add-to-list 'auto-mode-alist '("\\.xq$" . xquery-mode))

;;----------------------------------------------------------------------------
;; Use nxml-mode for xml files
;;----------------------------------------------------------------------------
(setq nxml-child-indent 4)
(add-to-list 'auto-mode-alist '("\\.x[ms]l\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.wsdl\\'" . nxml-mode))

;; prompt to add new java files to subversion
(defadvice jde-gen-class-buffer (around ewd-maybe-add-to-svn activate)
  "Prompt to add new java files to subversion."
  (let ((add-file-to-svn (y-or-n-p "Add this file to subversion? ")))
    ad-do-it
    (when add-file-to-svn
      (save-buffer)
      (vc-register))))

;;----------------------------------------------------------------------------
;; Set up C/C++ programming
;;----------------------------------------------------------------------------
;; define a new indentation style
(c-add-style "Eli"
             '("gnu"
               (c-basic-offset . 4)
               (c-comment-continuation-stars . "*")
               (compile-command . nil)
               (dabbrev-case-replace . nil)
               (c-auto-newline . t)
               (c-electric-pound-behavior . 'alignleft)
               (c-hanging-braces-alist . ((brace-list-open)
                                          (brace-list-close)
                                          (brace-entry-open)
                                          (substatement-open after)
                                          (block-close . c-snug-do-while)
                                          (extern-lang-open after)
                                          (inexpr-class-open after)
                                          (inexpr-class-close before)
                                          (inline-open after)
                                          (class-open after)
                                          (defun-open after)))
               (c-cleanup-list . (scope-operator
                                  brace-else-brace
                                  brace-elseif-brace
                                  defun-close-semi))
               (c-offsets-alist . ((string . 0)
                                   (substatement-open . 0)
                                   (knr-argdecl-intro . 0)
                                   (inline-open . 0)
                                   (case-label . +)
                                   (access-label . -)
								   (inclass . +)
                                   (statement-case-intro . +)
                                   (statement-case-open . 0)
                                   (arglist-close . c-lineup-close-paren)
                                   (inextern-lang . 0)))))

;; define extra C types to font-lock
(setq c-font-lock-extra-types
      (append
       '("BOOL" "BSTR" "LPC?\\(W\\|T\\|OLE\\)?STR" "HRESULT"
         "BYTE" "DWORD" "SOCKET" "idl_char"
         "idl_boolean" "idl_byte" "idl_\\(short\\|long\\)_float"
         "idl_u?\\(small\\|short\\|long\\)_int"
         "boolean32" "unsigned\\(32\\|16\\)"
         "SAFEARRAY" "boolean" "UINT" "ULONG")
       c-font-lock-extra-types))

;; define extra C++ types to font-lock
(setq c++-font-lock-extra-types
      (append
       c-font-lock-extra-types
       '("C[A-Z][a-z]\\w*")
       c++-font-lock-extra-types))

;; Make .h files go into C++-mode instead of C-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Set up keybindings and setting general to all C-like languages
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "Eli")))

;; Set up C-specific stuff
(add-hook 'c-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w" c-mode-syntax-table)))

;; Set up C++-specific stuff
(add-hook 'c++-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
            (local-unset-key "\C-c:")))


;;----------------------------------------------------------------------------
;; Set global keybindings
;;----------------------------------------------------------------------------
(mapa 'global-set-key
 `(([f4] . next-error)
   ([f11] . ewd-dup-line-line)
   ([end] . end-of-line)
   ([C-end] . ewd-end-of-buffer-nomark)
   ([home] . beginning-of-line)
   ([C-home] . ewd-beginning-of-buffer-nomark)
   ([apps] . execute-extended-command)
   ([C-right] . sp-forward-sexp)
   ([C-left] . sp-backward-sexp)
   ([wheel-right] . ewd-scroll-left)
   ([wheel-left] . ewd-scroll-right)
   ("\M-!" . ewd-insert-shell-command)
   ("\C-cd" . ediff-buffers)
   ("\C-x\C-p" . previous-buffer)
   ("\C-x\C-n" . next-buffer)
   ("\C-x\C-o" . ewd-other-buffer)
   ("\C-x\C-k" . ewd-kill-current-buffer)
   ("\C-h\C-v" . apropos-variable)
   ("\C-c;" . comment-region)
   ("\C-c:" . uncomment-region)
   ("\C-ck" . ewd-toggle-kp-usage)
   ("\C-cf" . ewd-get-x-font-name)
   ("\M-t" . tab-to-tab-stop)
   ("\M-g" . goto-line)
   ("\C-l" . ewd-font-lock-repaint)
   ("\C-m" . newline-and-indent)
   ("\M-\C-_" . redo)
   ("\M-\\" . hippie-expand)
   ("\M-x" . smex)
   ("\M-X" . smex-major-mode-commands)
   ("\C-c\C-c\M-x" . smex-major-mode-commands)
   ([C-next] . ewd-scroll-left)
   ([C-prior] . ewd-scroll-right)
   ("\C-xv=" . ediff-revision)
   ("\C-xf" . find-file-in-repository)
   ))

;;---------------------------------------------------------------------------
;; Stuff from M-x customize
;;---------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-sort-menu-saved-config
   (quote
    ((dired-actual-switches . "-al")
     (ls-lisp-ignore-case)
     (ls-lisp-dirs-first . t)))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; Make sure line numbers aren't italic
 '(linum ((t (:slant normal))))
 )

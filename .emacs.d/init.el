;; Set up package system
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(require 'dash)
(dash-enable-font-lock)

;;----------------------------------------------------------------------------
;; data structure manipuation functions
;;----------------------------------------------------------------------------

;; alist manipulation
(defun mapassoc (func alist)
  "map func down alist, listing results"
  (--map (funcall func (car it) (cdr it)) alist))

(defun mapa (func alist)
  "map func down alist, ignoring results"
  (--each alist (funcall func (car it) (cdr it))))

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

(-each default-column-tracking-functions 'track-column-for)

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
(setq term-term-name "xterm-16color")

(require 'shell)
(setq shell-file-name "zsh")
(add-hook 'shell-mode-hook
          (lambda ()
            ;; disable linum-mode
            (linum-mode -1)
            (linum-mode-set-explicitly)
            
            (define-key shell-mode-map [up] 'ewd-comint-up)
            (define-key shell-mode-map [down] 'ewd-comint-down)))

(add-hook 'term-mode-hook
          (lambda ()
            ;; disable linum-mode
            (linum-mode -1)
            (linum-mode-set-explicitly)
            
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

(require 'company)
(global-company-mode)
(company-quickhelp-mode)

;;----------------------------------------------------------------------------
;; Typescript
;;----------------------------------------------------------------------------
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;;----------------------------------------------------------------------------
;; Ruby programming
;;----------------------------------------------------------------------------
(add-hook 'ruby-mode-hook
          (lambda()
            (robe-mode)
            (yard-mode)
            (inf-ruby-minor-mode)
            (rbenv-use-corresponding)))

(require 'rbenv)
(global-rbenv-mode t)

;;----------------------------------------------------------------------------
;; Magit for git integration
;;----------------------------------------------------------------------------
(global-magit-file-mode)
;; (with-eval-after-load 'magit
;;   (require 'forge))


;; terraform
(require 'company-terraform)
(company-terraform-init)

(add-hook 'terraform-mode-hook
          (lambda ()
            (company-mode)))


;;----------------------------------------------------------------------------
;; Set up auxiliary stuff for python
;;----------------------------------------------------------------------------
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(global-flycheck-mode t)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc python-pylint))

;; blacken for python formatting
(setq blacken-executable "~/.pyenv/shims/black")

;;----------------------------------------------------------------------------
;; set up python
;;----------------------------------------------------------------------------
(setq jedi:use-shortcuts t)

(add-hook 'elpy-mode-hook
          (lambda ()
            (highlight-indentation-mode -1)
            
            ;; use `blacken` (external tool), not the `black` library directly
            (blacken-mode)
            ))
(elpy-enable)
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


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
        (font . "OperatorMono Nerd Font-15")
        ))

;; ;;----------------------------------------------------------------------------
;; ;; Save/restore window layouts
;; ;;----------------------------------------------------------------------------
;; (require 'workgroups2)
;; (setq wg-prefix-key "\C-cw")
;; (workgroups-mode 1)

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
(setq-default fill-column 72)           ; the column beyond which do word wrap
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

;; mac keys
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;;----------------------------------------------------------------------------
;; load exec-path from PATH in shell
;;----------------------------------------------------------------------------
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

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
(global-hl-line-mode)

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
(if window-system
    (server-start))

;; undo/redo as a tree
(require 'undo-tree)
(global-undo-tree-mode)

;; search with ripgrep
(require 'rg)
(rg-enable-default-bindings)

;; set up nicer buffer switching and other stuff
(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)

(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

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
(show-paren-mode 1)

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
;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)

(setq solarized-use-less-bold t)
(setq solarized-use-more-italic t)
(load-theme 'solarized-dark t)
(enable-theme 'solarized-dark)


;; solarized-them from MELPA, hacked to edit region highlight, and to
;; add the right colors for bold ANSI term colors.
;;
;; In solarized-definitions.el:
;;   - set region:
;;     `(region ((,class (:background ,base2))))
;;   - add the following to the term-color section:
;;     `(term-color-brblack ((t (:foreground ,base03 :background ,base03))))
;;     `(term-color-brred ((t (:foreground ,orange :background ,orange))))
;;     `(term-color-brgreen ((t (:foreground ,base01 :background ,base01))))
;;     `(term-color-bryellow ((t (:foreground ,base00 :background ,base00))))
;;     `(term-color-brblue ((t (:foreground ,base0 :background ,base0))))
;;     `(term-color-brmagenta ((t (:foreground ,violet :background ,violet))))
;;     `(term-color-brcyan ((t (:foreground ,base1 :background ,base1))))
;;     `(term-color-brwhite ((t (:foreground ,base3 :background ,base3))))


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

;; solarized sets up term-colors automatically (for term-mode etc),
;; but doesn't seem to set up ansi-colors (for comint-mode), so set them
;; to the same values as term-mode.
(require 'cl)
(setq ansi-color-names-vector
      (cl-map 'vector 'face-foreground
           [term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-cyan term-color-white]))
(setq ansi-color-faces-vector [default default default italic underline success warning error])
(setq ansi-color-map (ansi-color-make-color-map))

;;----------------------------------------------------------------------------
;; a better Perl mode.  Available from ftp://ftp.math.ohio-state.edu/pub/users/ilya/perl/
;;----------------------------------------------------------------------------
(defalias 'perl-mode 'cperl-mode)
(autoload 'cperl-mode "cperl-mode" "better mode for editing Perl programs" t)
(add-hook 'cperl-mode-hook
          (lambda()
            (setq cperl-indent-level 4)))
(add-to-list 'auto-mode-alist '("\\.ci\\'" . cperl-mode))

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
 '(([f4] . next-error)
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
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(dired-sort-menu-saved-config
   (quote
    ((dired-actual-switches . "-al")
     (ls-lisp-ignore-case)
     (ls-lisp-dirs-first . t))))
 '(package-selected-packages
   (quote
    (rg company-quickhelp company-terraform solarized-theme elpy terraform-mode go-mode forge blacken ido-completing-read+ ido-vertical-mode realgud undo-tree dash-functional exec-path-from-shell yard-mode yaml-mode xquery-mode workgroups2 virtualenvwrapper tide smex redo+ rbenv pcache multi-term mmm-mode logito jedi hlinum find-file-in-repository enh-ruby-mode ecb discover dired-sort-menu+ autopair auto-package-update ascii-art-to-unicode))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:slant italic))))
 '(linum ((t :slant normal)))
 '(sh-heredoc ((t (:inherit font-lock-string-face))))
 '(sh-quoted-exec ((t (:inherit font-lock-preprocessor-face)))))

;; Set up package system
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)


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

(defun mapapply (func list)
  "Map func down list of lists, applying the func to each list of arguments.
Return a list of the results."
  (mapcar (lambda (entry) (ewd-apply func entry)) list))

;; list/dotted pair manipulation
(defun ewd-filter (l pred)
  "return a list containing only the elements of L for which PRED is true"
  (cond ((null l) '())
        ((funcall pred (car l)) (cons (car l) (ewd-filter (cdr l) pred)))
        (t (ewd-filter (cdr l) pred))))

(defun ewd-fold (init op list)
  "OP is a function taking two arguments.  For each element in LIST, call OP
on the accumulated result so far (beginning with INIT) and the element."
  (if (null list) init
    (ewd-fold (funcall op init (car list)) op (cdr list))))

(defun ewd-apply (func &rest args)
  "Act just like `apply', only the final argument can end with a dotted pair
instead of nil."
  (let ((revargs (reverse args)))
    (apply 'apply func (nreverse (cons (ewd-standardize (car revargs))
                                      (cdr revargs))))))
(defun ewd-standardize (l)
  "Return a standard list (ending in nil) which contains the same elements
as L."
  (cond
   ((null l) '())
   ((not (listp l)) (list l))
   (t (cons (car l) (ewd-standardize (cdr l))))))

;;----------------------------------------------------------------------------
;; Interactive editing commands
;;----------------------------------------------------------------------------
;; scroll a page to the right
(defun ewd-scroll-left ()
  "Move cursor forward one page-width (or to end of line if that comes first)."
  (interactive)
  (let* ((rest-of-line (- (line-end-position) (point)))    
         (page-width (- (window-width) 10))
         (scroll-width (min rest-of-line page-width)))
    (forward-char scroll-width)))

;; scroll a page to the left
(defun ewd-scroll-right ()
  "Move cursor backward one page-width (or to beginning of line if that comes first)."
  (interactive)
  (let* ((rest-of-line (- (point) (line-beginning-position)))
         (page-width (- (window-width) 10))
         (scroll-width (min rest-of-line page-width)))
    (backward-char scroll-width)))


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
  (when window-system
	(save-excursion
	  (font-lock-fontify-region (window-start) (window-end)))))

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
  (if (comint-pmark-p)
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

;; move cursor to the next line or get next history item, depending
;; on whether we're at a shell mode prompt
(defun ewd-term-down (arg)
  (interactive "p")
  (if (term-after-pmark-p)
      (term-send-down)
    (next-line arg)))

;;----------------------------------------------------------------------------
;; Buffer manipulation functions
;;----------------------------------------------------------------------------
(defun ewd-buf-exists-p (bufname)
  "Non-nil if a buffer named BUFNAME exists."
  (member bufname (mapcar 'buffer-name (buffer-list))))

(defun yic-prev-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (let ((buffers (nreverse (buffer-list))))
    (while (eq (string-to-char (buffer-name (car buffers))) ? )
      (setq buffers (cdr buffers)))
    (switch-to-buffer (car buffers))))
						
(defun yic-other-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (switch-to-buffer (other-buffer)))

;; don't really ever kill the *scratch* buffer
(defun ewd-kill-current-buffer ()
  "Kill current buffer, unless it is the *scratch* buffer."
  (interactive)
  (cond ((equal (buffer-name) "*scratch*")
         (delete-region (point-min) (point-max))
         (if (> (ewd-buffer-count) 1) (bury-buffer)))
        (t (kill-buffer (current-buffer)))))




;;----------------------------------------------------------------------------
;; Make cursor stay in the same column when scrolling.  Thanks to
;; David Biesack (sasdjb@unx.sas.com).
;;----------------------------------------------------------------------------
(defvar default-column nil
  "The column to track in functions registered with `track-column'.
This variable is buffer local")
(make-variable-buffer-local 'default-column)

(defvar default-column-tracking-functions nil
  "List of functions which track the current column")

(defun default-column ()
  "Return the default column to track in cursor motion functions
which are advised by `track-column'"
  (if (memq last-command default-column-tracking-functions)
      default-column
    (setq default-column (current-column))))

(defmacro track-column(function)
  "Advise FUNCTION to keep track of the default column.
All functions so advised will strive to maintain the same column."
  (add-to-list 'default-column-tracking-functions function)
  `(defadvice ,function (around ,(track-column-advice-name function) first activate)
  "Keep cursor in the same column."
  (let ((col (default-column)))
    ad-do-it
    (move-to-column col))))

(defun track-column-advice-name (function)
  (make-symbol (format "track-column-in-%s" function)))

;; Make subsequently opened frames offset from the first one
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

(track-column scroll-up)
(track-column scroll-down)
(track-column previous-line)
(track-column next-line)

;;----------------------------------------------------------------------------
;; Tweak built-in behaviors
;;----------------------------------------------------------------------------
;; use y-or-n-p instead of yes-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

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
;; Miscellaneous utility functions
;;----------------------------------------------------------------------------
;; Get list of directories in a directory
(defun ewd-directories (dir)
  "Return list of subdirectories of DIR, excluding \".\" and \"..\"."
  (let ((dirs (ewd-filter (directory-files dir t) '(lambda (f) (file-directory-p f)))))
    (ewd-filter dirs '(lambda (f) (not (string-match "\\.\\.?$" f))))))

;; Get current number of non-internal buffers
(defun ewd-buffer-count (&optional FRAME)
  "Return number of non-internal buffers currently alive.  If FRAME is
specified,count buffers in FRAME."
  (ewd-fold 0 (lambda (buffers each)
                (if (eq ?  (string-to-char (buffer-name each))) buffers
                  (+ 1 buffers)))
            (buffer-list FRAME)))

;; Change how charaters are transposed
(defun gosmacs-transpose-chars ()
  "The real way to transpose characters with ^T: always
transpose the previous two characters from where the
point is."
  (interactive nil)
  (forward-char -1)
  (transpose-chars 1))

;;----------------------------------------------------------------------------
;; Set up shell and bash commands
;;----------------------------------------------------------------------------
(require 'multi-term)
(setq multi-term-program "/usr/local/bin/zsh")

;; create a shell buffer, but not necessarily called *shell*
(defun ewd-shell (&optional bufname)
"Just like `shell', only takes buffer name as argument."
  (interactive)
  (require 'shell)
  (if (null bufname) (setq bufname "shell"))
  (if (not (comint-check-proc (concat "*" bufname "*")))
      (let* ((prog (or explicit-shell-file-name
		       (getenv "ESHELL")
		       (getenv "SHELL")
		       "/bin/sh"))		     
	     (name (file-name-nondirectory prog))
	     (startfile (concat "~/.emacs_" name))
	     (xargs-name (intern-soft (concat "explicit-" name "-args")))
	     shell-buffer)
	(save-excursion
	  (set-buffer (apply 'make-comint bufname prog
			     (if (file-exists-p startfile) startfile)
			     (if (and xargs-name (boundp xargs-name))
				 (symbol-value xargs-name)
			       '("-i"))))
	  (setq shell-buffer (current-buffer))
	  (shell-mode))
	(pop-to-buffer shell-buffer))
    (pop-to-buffer (concat "*" bufname "*"))))

;; create a bash buffer called *bash*
(defun ewd-bash()
  "Runs the bash shell"
  (interactive)
  (let* ((shell-file-name "bash")
         (explicit-shell-file-name shell-file-name)
         (explicit-sh-args '("-login" "-i"))
         (w32-quote-process-args ?\")) ;; Use Cygnus quoting rules.
    (ewd-shell "bash")))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map [up] 'ewd-comint-up)
            (define-key shell-mode-map [down] 'ewd-comint-down)))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map [up] 'ewd-term-up)
            (define-key term-raw-map [down] 'ewd-term-down)))



;;----------------------------------------------------------------------------
;; Package management
;;----------------------------------------------------------------------------
(package-initialize)

;;----------------------------------------------------------------------------
;; set up exec-path
;;----------------------------------------------------------------------------
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

;;----------------------------------------------------------------------------
;; Load 'grep' library, which among other things prevents grep-find from recursing into .svn directories
;;----------------------------------------------------------------------------
(require 'grep)


;;----------------------------------------------------------------------------
;; Set up auxiliary stuff for python
;;----------------------------------------------------------------------------
(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(global-flycheck-mode t)
(setq-default flycheck-pylintrc "~/dev/lib/trunk/echonest/pylintrc")

(global-set-key [f7] 'find-file-in-repository)

; auto-complete mode extra settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)


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

;;----------------------------------------------------------------------------
;; Set personal information
;;----------------------------------------------------------------------------

(setq user-mail-address "eli.daniel@gmail.com")
(setq user-full-name "Eli Daniel")


;;----------------------------------------------------------------------------
;; Set up environment
;;----------------------------------------------------------------------------
;; Set up frame position and coloring
(setq default-frame-alist
      '((top . 50)
        (left . 100)
        (width . 150)
        (height . 50)
        (cursor-type . bar)
        (font . "Monaco-12")
        (vertical-scroll-bars . right)))


;; Make emacs use the clipboard
(when (= emacs-major-version 23)
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))


;; Environment
(setq grep-find-use-xargs 'exec)		; use -print0 and xargs -0
(setq compilation-scroll-output t)      ; scroll output in compilation window
(setq-default tab-width 4)              ; tab = 4 spaces
(setq-default ediff-ignore-similar-regions t) ; ignore whitespace differences in ediff
(setq-default indent-tabs-mode nil)     ; use spaces (not tabs) for indenting
(setq require-final-newline t)          ; always terminate last line in file
(setq default-major-mode 'text-mode)    ; default mode is text mode
(setq next-screen-context-lines 1)      ; # of lines of overlap when scrolling
(setq auto-save-interval 300)           ; autosave every N characters typed
(setq default-fill-column 72)           ; the column beyond which do word wrap
(setq scroll-preserve-screen-position t); make pgup/dn remember current line
(setq next-line-add-newlines nil)       ; don't scroll past end of file
(global-auto-revert-mode 1)             ; autorevert buffers if files change
(setq revert-without-query '(".*"))     ; revert all files automatically
(setq w32-swap-mouse-buttons t)         ; swap middle and right mouse buttons on Windows
(mouse-avoidance-mode 'animate)         ; make cursor get out of the way when I type near it
(setq w32-enable-synthesized-fonts nil)   ; enable synthesized italics/bold on Windows
(setq w32-list-proportional-fonts t)	; include proportional fonts in the font dialog
(setq max-specpdl-size 4000)            ; boost number of Lisp bindings allowed
(setq max-lisp-eval-depth 10000)         ; boost eval depth
(setq backup-directory-alist '(("." . "/tmp/emacs-backups"))) ; put all backups in one place
(setq-default truncate-lines t)         ; don't wrap long lines... just have them go off the screen
(setq confirm-kill-emacs 'y-or-n-p)     ; confirm before closing emacs
(setq archive-zip-use-pkzip nil) ; don't use pkzip for zip files
(setq ediff-split-window-function 'split-window-horizontally)	; show files side-by-side in ediff
;;(setq ediff-split-window-function 'split-window-vertically)
(ansi-color-for-comint-mode-on)  ; show ANSI color codes
(setq gdb-many-windows t)    ; use many windows for debugging
(setq ring-bell-function 'ignore)	; don't beep

(require 'hlinum)
(hlinum-activate)
(global-linum-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; show directories in buffer names

;; Allow some variables to be set on a mode-by-mode-basis
(make-variable-buffer-local 'tempo-interactive)  ; prompting to fill in templates
(make-variable-buffer-local 'ps-line-number)     ; print with line numbers

;; ;; Paren matching
;; (show-paren-mode t)                        ; highlight matching parens, etc
;; (setq show-paren-style 'parenthesis)       ; highlight character, not expression
;; (setq blink-matching-paren-distance 51200) ; distance to match paren as
(require 'smartparens-config)
(show-smartparens-global-mode +1)

;; modeline options
(which-func-mode t)                 ; show current function in modeline
(column-number-mode t)              ; show current column number
(setq display-time-day-and-date t)  ; display day and date
(display-time)                      ; display the time

;; Make .com, .out, and .tpl files be text rather than binary
(when (eq window-system 'w32) 
  (setq file-name-buffer-file-type-alist
		(cons '("\\.\\([Cc][Oo][Mm]\\|out\\|tpl\\)$") file-name-buffer-file-type-alist)))

;; Printing
(setq ps-printer-name "//corpprint/15-Development")
;; (setq ps-landscape-mode t)
;; (setq ps-n-up-printing 2)
;; (setq ps-n-up-margin 0)
;; (setq ps-n-up-border-p nil)
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

;;----------------------------------------------------------------------------
;; Set up additional packages
;;----------------------------------------------------------------------------
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

;; Make telnet-mode work on windows
;; telnet with telnet.exe written by Naftali Ramati (naftali@harmonic.co.il).
;; Thanks to Zoltan Kemenczy (zoltan@nabu.isg.mot.com).
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
(when (eq window-system 'w32)
;;   (setq telnet-program "~/bin/telnet.exe")
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
;; Set up syntax highlighting (font-lock)
;;----------------------------------------------------------------------------
(when window-system
  (require 'font-lock-menus)
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration '((c++-mode . 2) (cperl-mode . 1) (t . t)))
)

;;----------------------------------------------------------------------------
;; Color theme
;;----------------------------------------------------------------------------
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;;----------------------------------------------------------------------------
;; Emacs 23 only features
;;----------------------------------------------------------------------------
(when (>= emacs-major-version 23)
  ;; (load "cedet")
  ;; (load "semantic")
  ;; (global-semantic-decoration-mode -1)
  ;; (global-semantic-stickyfunc-mode -1)
  ;; (add-to-list 'load-path (concat (getenv "JDE_HOME") "/lisp"))

  ;; Set up JDE for Java programming
  ;; (require 'jde)
  ;; (add-hook 'jde-mode-hook
  ;; 			(lambda ()
  ;; 			  ;; use jde-import-organize instead of jde-import-sort
  ;; 			  (fset 'jde-import-sort 'jde-import-organize)
  ;; 			  ;; set local variables
  ;; 			  (setq ps-line-number t)))

  ;; ;; Use ECB for code browsing
  ;; (require 'ecb)
  ;; (setq ecb-tip-of-the-day nil)
  ;; (require 'winring)
  ;; (add-hook 'ecb-activate-hook
  ;; 			(lambda ()
  ;; 			  (ecb-winman-winring-enable-support)
  ;; 			  (winring-initialize)))

  ;; (add-to-list 'auto-mode-alist '("\\.java.in$" . jde-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.java.server$" . jde-mode))
  
  ;; run the class in the current buffer in junit
  (defun ewd-run-junit ()
	"Runs junit on the current class"
	(interactive)
	(let ((jde-run-option-application-args (list (jde-run-get-main-class)))
		  (jde-run-read-app-args nil)
		  (jde-run-application-class "junit.textui.TestRunner"))
	  (jde-run 1))))

;; a better Perl mode.  Available from ftp://ftp.math.ohio-state.edu/pub/users/ilya/perl/
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
(when (>= emacs-major-version 23)
  (require 'xquery-mode)

  ;; Use jsp-helper-mode + mmm-mode for JSP files
  (add-to-list 'auto-mode-alist '("\\.xq$" . xquery-mode)))

;;----------------------------------------------------------------------------
;; Use nxml-mode for xml files
;;----------------------------------------------------------------------------
(when (>= emacs-major-version 23)
  (setq nxml-child-indent 4)
  (add-to-list 'auto-mode-alist '("\\.x[ms]l\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.wsdl\\'" . nxml-mode)))

;;----------------------------------------------------------------------------
;; Set up version control
;;----------------------------------------------------------------------------

;; Use svn for version control
;;
(setq vc-svn-program "svn")

;; prompt to add new java files to subversion
(defadvice jde-gen-class-buffer (around ewd-maybe-add-to-svn activate)
  "Prompt to add new java files to subversion"
  (let ((add-file-to-svn (y-or-n-p "Add this file to subversion? ")))
    ad-do-it
    (when add-file-to-svn
      (save-buffer)
      (vc-register))))

;; ;; Use perforce for version control
;;
;; (require 'p4)
;; (setq p4-executable "c:/program files/perforce/p4.exe")

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

(when window-system
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
         c++-font-lock-extra-types)))

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
;; Ruby programming
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(eval-after-load 'enh-ruby-mode
 '(remove-hook 'enh-ruby-mode-hook 'erm-define-faces))
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'yard-mode)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

;;----------------------------------------------------------------------------
;; Magit for git integration
;;----------------------------------------------------------------------------
(global-magit-file-mode)

;;----------------------------------------------------------------------------
;; Set global keybindings
;;----------------------------------------------------------------------------
(mapa 'global-set-key
 `(([f4] . next-error)
   ([f11] . ewd-dup-line)
   ([end] . end-of-line)
   ([C-end] . ewd-end-of-buffer-nomark)
   ([home] . beginning-of-line)
   ([C-home] . ewd-beginning-of-buffer-nomark)
   ([apps] . execute-extended-command)
   ([C-right] . sp-forward-sexp)
   ([C-left] . sp-backward-sexp)
   ("\M-!" . ewd-insert-shell-command)
   ("\C-cd" . ediff-buffers)
   ("\C-x\C-p" . yic-prev-buffer)
   ("\C-x\C-n" . bury-buffer)
   ("\C-x\C-o" . yic-other-buffer)
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
   ("\C-c\C-v\M-n" . jde-gen-class-buffer)
   ("\C-c\C-v\M-i" . jde-gen-interface-buffer)
   ("\C-c\C-v\C-j" . ewd-run-junit)
   ("\C-c\C-v\M-y" . jde-show-class-source)
   ("\M-\C-_" . redo)
   ([C-next] . ewd-scroll-left)
   ([C-prior] . ewd-scroll-right)
   ("\C-xv=" . ediff-revision)
   ))

;;---------------------------------------------------------------------------
;; Stuff from M-x customize
;;---------------------------------------------------------------------------
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(dired-sort-menu-saved-config (quote ((dired-actual-switches . "-al") (ls-lisp-ignore-case) (ls-lisp-dirs-first . t))))
 '(ecb-jde-set-directories-buffer-to-jde-sourcepath (quote replace))
 '(ecb-layout-name "left3")
 '(ecb-options-version "2.32")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote ("c:\\dev\\" "/localdisk/edaniel/dev" "/localssd/edaniel/dev")))
 '(ecb-wget-setup (quote cons))
 '(which-function-mode t nil (which-func)))

;;---------------------------------------------------------------------------
;; Enable some functions
;;---------------------------------------------------------------------------
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

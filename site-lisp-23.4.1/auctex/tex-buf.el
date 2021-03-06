;;; tex-buf.el - External commands for AUC TeX.
;;
;; Maintainer: Per Abrahamsen <auc-tex@sunsite.auc.dk>
;; Version: 9.9j

;; Copyright (C) 1991 Kresten Krab Thorup
;; Copyright (C) 1993, 1996 Per Abrahamsen 
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(require 'tex)

;;; Customization:

(defcustom TeX-process-asynchronous (not (eq system-type 'ms-dos))
  "*Use asynchronous processes."
  :group 'TeX-commands
  :type 'boolean)

(defcustom TeX-shell
  (if (memq system-type '(ms-dos emx windows-nt))
      shell-file-name
    "/bin/sh")
  "Name of shell used to parse TeX commands."
  :group 'TeX-commands
  :type 'file)

(defcustom TeX-shell-command-option
  (cond ((memq system-type '(ms-dos emx windows-nt) )
	 (cond ((boundp 'shell-command-option)
		shell-command-option)
	       ((boundp 'shell-command-switch)
		shell-command-switch)
	       (t 
		"/c")))
	(t				;Unix & EMX (Emacs 19 port to OS/2)
	 "-c"))
  "Shell argument indicating that next argument is the command."
  :group 'TeX-commands
  :type 'string)

;;; Interactive Commands
;;
;; The general idea is, that there is one process and process buffer
;; associated with each master file, and one process and process buffer
;; for running TeX on a region.   Thus, if you have N master files, you
;; can run N + 1 processes simultaneously.  
;;
;; Some user commands operates on ``the'' process.  The following
;; algorithm determine what ``the'' process is.
;;
;; IF   last process started was on a region 
;; THEN ``the'' process is the region process
;; ELSE ``the'' process is the master file (of the current buffer) process

(defun TeX-save-document (name)
  "Save all files belonging to the current document.
Return non-nil if document need to be re-TeX'ed."
  (interactive (list (TeX-master-file)))
  (if (string-equal name "")
      (setq name (TeX-master-file)))
  
  (TeX-check-files (concat name ".dvi")
		   (cons name (TeX-style-list))
		   TeX-file-extensions))

(defun TeX-command-master ()
  "Run command on the current document."
  (interactive)
  (TeX-command (TeX-command-query (TeX-master-file)) 'TeX-master-file))

(defvar TeX-command-region-begin nil)
(defvar TeX-command-region-end nil)
;; Used for marking the last region.

(make-variable-buffer-local 'TeX-command-region-begin)
(make-variable-buffer-local 'TeX-command-region-end)

(defun TeX-command-region (&optional old)
  "Run TeX on the current region.

Query the user for a command to run on the temporary file specified by
the variable TeX-region.  If the chosen command is so marked in
TeX-command-list, and no argument (or nil) is given to the command,
the region file file be recreated with the current region.  If mark is
not active, the new text in the previous used region will be used.

If the master file for the document has a header, it is written to the
temporary file before the region itself.  The document's header is all
text before TeX-header-end.

If the master file for the document has a trailer, it is written to
the temporary file before the region itself.  The document's trailer is
all text after TeX-trailer-start."
  (interactive "P")
  (if (and (TeX-mark-active) (not old))
      (let ((begin (min (point) (mark)))
	     (end (max (point) (mark))))
	(if TeX-command-region-begin
	    ()
	  (setq TeX-command-region-begin (make-marker)
		TeX-command-region-end (make-marker)))
	(set-marker TeX-command-region-begin begin)
	(set-marker TeX-command-region-end end)))
  (if (null TeX-command-region-begin)
      (error "Mark not set"))
  (let ((begin (marker-position TeX-command-region-begin))
	(end (marker-position TeX-command-region-end)))
    (TeX-region-create (TeX-region-file TeX-default-extension)
		       (buffer-substring begin end)
		       (file-name-nondirectory (buffer-file-name))
		       (count-lines (save-restriction (widen) (point-min))
				    begin)))
  (TeX-command (TeX-command-query (TeX-region-file)) 'TeX-region-file))

(defun TeX-command-buffer ()
  "Run TeX on the current buffer.

Query the user for a command to run on the temporary file specified by
the variable TeX-region.  The region file file be recreated from the
visible part of the buffer."
  (interactive)
  (let ((TeX-command-region-begin (point-min-marker))
	(TeX-command-region-end (point-max-marker)))
    (TeX-command-region t)))

(defun TeX-recenter-output-buffer (line)
  "Redisplay buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on line LINE of the window, or
at bottom if LINE is nil." 
  (interactive "P")
  (let ((buffer (TeX-active-buffer)))
    (if buffer
	(let ((old-buffer (current-buffer)))
	  (pop-to-buffer buffer t)
	  (bury-buffer buffer)
	  (goto-char (point-max))
	  (recenter (if line
			(prefix-numeric-value line)
		      (/ (window-height) 2)))
	  (pop-to-buffer old-buffer))
      (message "No process for this document."))))

(defun TeX-kill-job ()
  "Kill the currently running TeX job."
  (interactive)
  (let ((process (TeX-active-process)))
    (if process
	(kill-process process)
      ;; Should test for TeX background process here.
      (error "No TeX process to kill"))))

(defun TeX-home-buffer ()
  "Go to the buffer where you last issued a TeX command.  
If there is no such buffer, or you already are in that buffer, find
the master file."
  (interactive)
  (if (or (null TeX-command-buffer)
	  (eq TeX-command-buffer (current-buffer)))
      (find-file (TeX-master-file TeX-default-extension))
    (switch-to-buffer TeX-command-buffer)))

(defun TeX-next-error (reparse)
  "Find the next error in the TeX output buffer.
Prefix by C-u to start from the beginning of the errors."
  (interactive "P")
  (if (null (TeX-active-buffer))
      (error "No TeX output buffer")
    (funcall (TeX-process-get-variable (TeX-active-master) 'TeX-parse-function)
	     reparse)))

(defun TeX-toggle-debug-boxes ()
  "Toggle if the debugger should display \"bad boxes\" too."
  (interactive)
  (cond (TeX-debug-bad-boxes
	 (setq TeX-debug-bad-boxes nil))
	(t
	 (setq TeX-debug-bad-boxes t)))
  (message (concat "TeX-debug-bad-boxes: " (cond (TeX-debug-bad-boxes "on")
						 (t "off")))))

;;; Command Query

(defun TeX-command (name file)
  "Run command NAME on the file you get by calling FILE.

FILE is a function return a file name.  It has one optional argument,
the extension to use on the file.

Use the information in TeX-command-list to determine how to run the
command."
  (setq TeX-current-process-region-p (eq file 'TeX-region-file))
  (let ((command (TeX-command-expand (nth 1 (assoc name TeX-command-list))
				     file))
	(hook (nth 2 (assoc name TeX-command-list)))
	(confirm (nth 3 (assoc name TeX-command-list))))

    ;; Verify the expanded command
    (if confirm
	(setq command
	      (read-from-minibuffer (concat name " command: ") command)))
    
    ;; Now start the process
    (TeX-process-set-variable name 'TeX-command-next TeX-command-Show)
    (apply hook name command (apply file nil) nil)))

(defun TeX-command-expand (command file &optional list)
  "Expand COMMAND for FILE as described in LIST.
LIST default to TeX-expand-list."
  (if (null list)
      (setq list TeX-expand-list))
  (while list
    (let ((case-fold-search nil) ; Do not ignore case.
	  (string (car (car list)))	;First element
	  (expansion (car (cdr (car list)))) ;Second element
	  (arguments (cdr (cdr (car list))))) ;Remaining elements
      (while (string-match string command)
	(let ((prefix (substring command 0 (match-beginning 0)))
	      (postfix (substring command (match-end 0))))
	  (setq command (concat prefix
				(cond ((TeX-function-p expansion)
				       (apply expansion arguments))
				      ((boundp expansion)
				       (apply (eval expansion) arguments))
				      (t
				       (error "Nonexpansion %s" expansion)))
				postfix)))))
    (setq list (cdr list)))
  command)

(defun TeX-check-files (derived originals extensions)
  "Check that DERIVED is newer than any of the ORIGINALS.
Try each original with each member of EXTENSIONS, in all directories
in TeX-check-path."
  (let ((found nil)
	(regexp (concat "\\`\\("
			(mapconcat (function (lambda (dir)
				      (regexp-quote (expand-file-name dir))))
				   TeX-check-path "\\|")
			"\\).*\\("
			(mapconcat 'regexp-quote originals "\\|")
			"\\)\\.\\("
			(mapconcat 'regexp-quote extensions "\\|")
			"\\)\\'"))
	(buffers (buffer-list)))
    (while buffers
      (let* ((buffer (car buffers))
	     (name (buffer-file-name buffer)))
	(setq buffers (cdr buffers))
	(if (and name (string-match regexp name))
	    (progn
	      (and (buffer-modified-p buffer)
		   (or (not TeX-save-query)
		       (y-or-n-p (concat "Save file "
					 (buffer-file-name buffer)
					 "? ")))
		   (save-excursion (set-buffer buffer) (save-buffer)))
	      (if (file-newer-than-file-p name derived)
		  (setq found t))))))
    found))

(defun TeX-run-ispell-on-document (command ignored name)
  "Run ispell on  all files belonging to the current document."
  (interactive)
  (TeX-ispell-document ""))

(defun TeX-ispell-document (name)
  "Run ispell on  all files belonging to the current document."
  (interactive (list (TeX-master-file)))
  (if (string-equal name "")
      (setq name (TeX-master-file)))
  
  (let ((found nil)
	(regexp (concat "\\`\\("
			(mapconcat (function (lambda (dir)
				      (regexp-quote (expand-file-name dir))))
				   TeX-check-path "\\|")
			"\\).*\\("
			(mapconcat 'regexp-quote (cons name (TeX-style-list)) "\\|")
			"\\)\\.\\("
			(mapconcat 'regexp-quote TeX-file-extensions "\\|")
			"\\)\\'"))
	(buffers (buffer-list)))
    (while buffers
      (let* ((buffer (car buffers))
	     (name (buffer-file-name buffer)))
	(setq buffers (cdr buffers))
	(if (and name (string-match regexp name))
	    (progn
	      (save-excursion (switch-to-buffer buffer) (ispell-buffer))
	      (setq found t)))))))

(defcustom TeX-save-query t
  "*If non-nil, ask user for permission to save files before starting TeX."
  :group 'TeX-commands
  :type 'boolean)

(defun TeX-command-query (name)
  "Query the user for a what TeX command to use."
  (let* ((default (cond ((if (string-equal name TeX-region)
			     (TeX-check-files (concat name ".dvi")
					      (list name)
					      TeX-file-extensions)
			   (TeX-save-document (TeX-master-file)))
			 TeX-command-default)
			((and (eq major-mode 'latex-mode)
			      (TeX-check-files (concat name ".bbl")
					       (mapcar 'car
						       (LaTeX-bibliography-list))
					       BibTeX-file-extensions))
			 ;; We should check for bst files here as well.
			 TeX-command-BibTeX)
			((TeX-process-get-variable name
						   'TeX-command-next
						   TeX-command-Show))
			(TeX-command-Show)))
	 (completion-ignore-case t)
	 (answer (or TeX-command-force
		     (completing-read
		      (concat "Command: (default " default ") ")
		      TeX-command-list nil t))))
    ;; If the answer "latex" it will not be expanded to "LaTeX"
    (setq answer (car-safe (TeX-assoc answer TeX-command-list)))
    (if (and answer
	     (not (string-equal answer "")))
	answer
      default)))

(defvar TeX-command-next nil
  "The default command next time TeX-command is invoked.")

 (make-variable-buffer-local 'TeX-command-next)

(defun TeX-printer-query (&optional command element)
  "Query the user for a printer name.
COMMAND is the default command to use if the entry for the printer in
TeX-printer-list does not itself have it specified in the ELEMENT'th
entry." 
  (or command (setq command TeX-print-command))
  (or element (setq element 1))
  (let ((printer (if TeX-printer-list
		     (let ((completion-ignore-case t))
		       (completing-read (concat "Printer: (default "
						TeX-printer-default ") ")
					TeX-printer-list))
		   "")))
    
    (setq printer (or (car-safe (TeX-assoc printer TeX-printer-list))
		      printer))
    (if (or (null printer) (string-equal "" printer))
	(setq printer TeX-printer-default)
      (setq TeX-printer-default printer))

    (let ((expansion (let ((entry (assoc printer TeX-printer-list)))
		       (if (and entry (nth element entry))
			   (nth element entry)
			 command))))
      (if (string-match "%p" printer)
	  (error "Don't use %s in printer names" "%p"))
      (while (string-match "%p" expansion)
	(setq expansion (concat (substring expansion 0 (match-beginning 0))
				printer
				(substring expansion (match-end 0)))))
      expansion)))

(defun TeX-style-check (styles)
  "Check STYLES compared to the current style options."

  (let ((files (TeX-style-list)))
    (while (and styles
		(not (TeX-member (car (car styles)) files 'string-match)))
      (setq styles (cdr styles))))
  (if styles
      (nth 1 (car styles))
    ""))

;;; Command Hooks

(defvar TeX-after-start-process-function nil
  "Hooks to run after starting an asynchronous process.
Used by Japanese TeX to set the coding system.")

(defcustom TeX-show-compilation nil
  "*If non-nil, show output of TeX compilation in other window."
  :group 'TeX-commands
  :type 'boolean)

(defun TeX-run-command (name command file)
  "Create a process for NAME using COMMAND to process FILE.
Return the new process."
  (let ((default TeX-command-default)
	(buffer (TeX-process-buffer-name file))
	(dir (TeX-master-directory)))
    (TeX-process-check file)		; Check that no process is running
    (setq TeX-command-buffer (current-buffer))
    (get-buffer-create buffer)
    (set-buffer buffer)
    (erase-buffer)
    (if dir (cd dir))
    (insert "Running `" name "' on `" file "' with ``" command "''\n")
    (setq mode-name name)
    (if TeX-show-compilation
	(display-buffer buffer)
      (message "Type `C-c C-l' to display results of compilation."))
    (setq TeX-parse-function 'TeX-parse-command)
    (setq TeX-command-default default)
    (setq TeX-sentinel-function
	  (function (lambda (process name)
		      (message (concat name ": done.")))))
    (if TeX-process-asynchronous
	(let ((process (start-process name buffer TeX-shell
				      TeX-shell-command-option command)))
	  (if TeX-after-start-process-function
	      (funcall TeX-after-start-process-function process))
	  (TeX-command-mode-line process)
	  (set-process-filter process 'TeX-command-filter)
	  (set-process-sentinel process 'TeX-command-sentinel)
	  (set-marker (process-mark process) (point-max))
	  (setq compilation-in-progress (cons process compilation-in-progress))
	  process)
      (setq mode-line-process ": run")
      (set-buffer-modified-p (buffer-modified-p))
      (sit-for 0)				; redisplay
      (call-process TeX-shell nil buffer nil
		    TeX-shell-command-option command))))

(defun TeX-run-format (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."
  (let ((buffer (TeX-process-buffer-name file))
	(process (TeX-run-command name command file)))
    ;; Hook to TeX debuger.
    (save-excursion
      (set-buffer buffer)
      (TeX-parse-reset)
      (setq TeX-parse-function 'TeX-parse-TeX)
      (setq TeX-sentinel-function 'TeX-TeX-sentinel)
      (if TeX-process-asynchronous
	  (progn
	    ;; Updating the mode line.
	    (setq TeX-current-page "[0]")
	    (TeX-format-mode-line process)
	    (set-process-filter process 'TeX-format-filter)))
      process)))

(defun TeX-run-TeX (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."
  (let ((process (TeX-run-format name command file)))
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-LaTeX (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."
  (let ((process (TeX-run-format name command file)))
    (setq TeX-sentinel-function 'TeX-LaTeX-sentinel)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-BibTeX (name command file)
  "Create a process for NAME using COMMAND to format FILE with BibTeX."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function 'TeX-BibTeX-sentinel)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-compile (name command file)
  "Ignore first and third argument, start compile with second argument."
  (compile command))

(defun TeX-run-shell (name command file)
  "Ignore first and third argument, start shell-command with second argument."
  (shell-command command)
  (if (eq system-type 'ms-dos)
      (redraw-display)))

(defun TeX-run-discard (name command file)
  "Start process with second argument, discarding its output."
  (process-kill-without-query (start-process (concat name " discard")
					     nil TeX-shell
					     TeX-shell-command-option
					     command)))

(defun TeX-run-dviout (name command file)
  "Call process wbith second argument, discarding its output. With support
for the dviout previewer, especially when used with PC-9801 series."
    (if (and (boundp 'dos-machine-type) (eq dos-machine-type 'pc98)) ;if PC-9801
      (send-string-to-terminal "\e[2J")) ; clear screen
    (call-process TeX-shell (if (eq system-type 'ms-dos) "con") nil nil
                TeX-shell-command-option command)
    (if (eq system-type 'ms-dos)
      (redraw-display)))

(defun TeX-run-background (name command file)
  "Start process with second argument, show output when and if it arrives."
  (let ((dir (TeX-master-directory)))
    (set-buffer (get-buffer-create "*TeX background*"))
    (if dir (cd dir))
    (erase-buffer)
    (let ((process (start-process (concat name " background")
				  nil TeX-shell
				  TeX-shell-command-option command)))
      (if TeX-after-start-process-function
	  (funcall TeX-after-start-process-function process))
      (set-process-filter process 'TeX-background-filter)
      (process-kill-without-query process))))

(defun TeX-run-silent (name command file)
  "Start process with second argument."
  (let ((dir (TeX-master-directory)))
    (set-buffer (get-buffer-create "*TeX silent*"))
    (if dir (cd dir))
    (erase-buffer)
    (let ((process (start-process (concat name " silent")
				  nil TeX-shell
				  TeX-shell-command-option command)))
      (if TeX-after-start-process-function
	  (funcall TeX-after-start-process-function process))
      (process-kill-without-query process))))

(defun TeX-run-interactive (name command file)
  "Run TeX interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts user
interaction. If you return to the file buffer after the TeX run,
Error parsing on C-x ` should work with a bit of luck."
  (require 'comint)
  (let ((default TeX-command-default)
	(buffer (TeX-process-buffer-name file))
	(process nil)
	(dir (TeX-master-directory)))
    (TeX-process-check file)          ; Check that no process is running
    (setq TeX-command-buffer (current-buffer))
    (let (temp-buffer-show-function temp-buffer-show-hook)
      (with-output-to-temp-buffer buffer))
    (set-buffer buffer)
    (if dir (cd dir))
    (insert "Running `" name "' on `" file "' with ``" command "''\n")
    (comint-exec buffer name TeX-shell nil
		 (list TeX-shell-command-option command))
    (comint-mode)
    (setq mode-name name)
    (setq TeX-command-default default)
    (setq process (get-buffer-process buffer))
    (if TeX-after-start-process-function
	(funcall TeX-after-start-process-function process))
    (TeX-command-mode-line process)
    (set-process-sentinel process 'TeX-command-sentinel)
    (set-marker (process-mark process) (point-max))
    (setq compilation-in-progress (cons process compilation-in-progress))
    (TeX-parse-reset)
    (setq TeX-parse-function 'TeX-parse-TeX)
    (setq TeX-sentinel-function 'TeX-LaTeX-sentinel)))

;;; Command Sentinels

(defun TeX-synchronous-sentinel (name file result)
  "Process TeX command output buffer after the process dies."
  (let* ((buffer (TeX-process-buffer file)))
    (save-excursion
      (set-buffer buffer)
      
      ;; Append post-mortem information to the buffer
      (goto-char (point-max))
      (insert "\n" mode-name (if (and result (zerop result))
				 " finished" " exited") " at "
	      (substring (current-time-string) 0 -5))
      (setq mode-line-process ": exit")
      
      ;; Do command specific actions.
      (setq TeX-command-next TeX-command-Show)
      (goto-char (point-min))
      (apply TeX-sentinel-function nil name nil)
      
      ;; Force mode line redisplay soon
      (set-buffer-modified-p (buffer-modified-p)))))

(defun TeX-command-sentinel (process msg)
  "Process TeX command output buffer after the process dies."
  (let* ((buffer (process-buffer process))
	 (name (process-name process)))
    (cond ((null (buffer-name buffer))	; buffer killed
	   (set-process-buffer process nil)
	   (set-process-sentinel process nil))
	  ((memq (process-status process) '(signal exit))
	   (save-excursion
	     (set-buffer buffer)
	     
	     ;; Append post-mortem information to the buffer
	     (goto-char (point-max))
	     (insert "\n" mode-name " " msg)
	     (forward-char -1)
	     (insert " at "
		     (substring (current-time-string) 0 -5))
	     (forward-char 1)
	     
	     ;; Do command specific actions.
	     (TeX-command-mode-line process)
	     (setq TeX-command-next TeX-command-Show)
	     (goto-char (point-min))
	     (apply TeX-sentinel-function process name nil)
	     
	     
	     ;; If buffer and mode line will show that the process
	     ;; is dead, we can delete it now.  Otherwise it
	     ;; will stay around until M-x list-processes.
	     (delete-process process)
	     
	     ;; Force mode line redisplay soon
	     (set-buffer-modified-p (buffer-modified-p))))))
  (setq compilation-in-progress (delq process compilation-in-progress)))


(defvar TeX-sentinel-function (function (lambda (process name)))
  "Hook to cleanup TeX command buffer after temination of PROCESS.
NAME is the name of the process.")

  (make-variable-buffer-local 'TeX-sentinel-function)

(defun TeX-TeX-sentinel (process name)
  "Cleanup TeX output buffer after running TeX."
  (if (TeX-TeX-sentinel-check process name)
      ()
    (message (concat name ": formatted " (TeX-current-pages)))
    (setq TeX-command-next TeX-command-Show)))

(defun TeX-current-pages ()
  ;; String indictating the number of pages formatted.
  (cond ((null TeX-current-page)
	 "some pages.")
	((string-match "[^0-9]1[^0-9]" TeX-current-page)
	 (concat TeX-current-page " page."))
	(t
	 (concat TeX-current-page " pages."))))

(defun TeX-TeX-sentinel-check (process name)
  "Cleanup TeX output buffer after running TeX.
Return nil ifs no errors were found."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "^Output written on.* (\\([0-9]+\\) page" nil t)
	(setq TeX-current-page (concat "{" (TeX-match-buffer 1) "}"))))
  (if process (TeX-format-mode-line process))
  (if (re-search-forward "^! " nil t)
      (progn
	(message (concat name " errors in `" (buffer-name)
			 "'. Use C-c ` to display."))
	(setq TeX-command-next TeX-command-default)
	t)
    (setq TeX-command-next TeX-command-Show)
    nil))

(defun TeX-LaTeX-sentinel (process name)
  "Cleanup TeX output buffer after running LaTeX."
  (cond ((TeX-TeX-sentinel-check process name))
	((and (save-excursion
		(or
                 (re-search-forward "^LaTeX Warning: Citation" nil t)
                 (re-search-forward "^Package natbib Warning: Citation" nil t)))
	      (let ((current (current-buffer)))
		(set-buffer TeX-command-buffer)
		(prog1 (and (LaTeX-bibliography-list)
			    (TeX-check-files (TeX-master-file "bbl")
					    (TeX-style-list)
					    (append TeX-file-extensions
						    BibTeX-file-extensions)))
		  (set-buffer current))))
	 (message (concat "You should run BibTeX to get citations right, "
                          (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-BibTeX))
	((or
          (re-search-forward "^LaTeX Warning: Label(s)" nil t)
          (re-search-forward "^Package natbib Warning: Citation(s)" nil t))
	 (message (concat "You should run LaTeX again "
			  "to get references right, "
                          (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward "^LaTeX Warning: Reference" nil t)
	 (message (concat name ": there were unresolved references, "
                          (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-Show))
	((or
          (re-search-forward "^LaTeX Warning: Citation" nil t)
          (re-search-forward "^Package natbib Warning:.*undefined citations" nil t))
	 (message (concat name ": there were unresolved citations, "
                          (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-Show))
	((re-search-forward
	  "^\\(\\*\\* \\)?J?I?p?\\(La\\|Sli\\)TeX\\(2e\\)? \\(Version\\|ver\\.\\|<[0-9/]*>\\)" nil t)
	 (message (concat name ": successfully formatted "
			  (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-Show))
	(t
	 (message (concat name ": problems after "
			  (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-default))))

(defun TeX-BibTeX-sentinel (process name)
  "Cleanup TeX output buffer after running BibTeX."
  (message "You should perhaps run LaTeX again to get citations right.")
  (setq TeX-command-next TeX-command-default))

;;; Process Control


;; This variable is chared with `compile.el'.
(defvar compilation-in-progress nil
  "List of compilation processes now running.")

(or (assq 'compilation-in-progress minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-in-progress " Compiling")
				 minor-mode-alist)))

(defun TeX-process-get-variable (name symbol &optional default)
  "Return the value in the process buffer for NAME of SYMBOL.

Return DEFAULT if the process buffer does not exist or SYMBOL is not
defined."
  (let ((buffer (TeX-process-buffer name)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (if (boundp symbol)
	      (eval symbol)
	    default))
      default)))

(defun TeX-process-set-variable (name symbol value)
  "Set the variable SYMBOL in the process buffer to VALUE.
Return nil iff no process buffer exist."
  (let ((buffer (TeX-process-buffer name)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (set symbol value)
	  t)
      nil)))

(defun TeX-process-check (name)
  "Check if a process for the TeX document NAME already exist.
If so, give the user the choice of aborting the process or the current
command."
  (let ((process (TeX-process name)))
    (cond ((null process))
	  ((not (eq (process-status process) 'run)))
	  ((yes-or-no-p (concat "Process `"
				(process-name process)
				"' for document `"
				name
				"' running, kill it? "))
	   (delete-process process))
	  (t
	   (error "Cannot have two processes for the same document")))))

(defun TeX-process-buffer-name (name)
  "Return name of AUC TeX buffer associated with the document NAME."
  (concat "*" (abbreviate-file-name (expand-file-name name)) " output*"))

(defun TeX-process-buffer (name)
  "Return the AUC TeX buffer associated with the document NAME."
  (get-buffer (TeX-process-buffer-name name)))

(defun TeX-process (name)
  "Return AUC TeX process associated with the document NAME."
  (and TeX-process-asynchronous
       (get-buffer-process (TeX-process-buffer name))))

;;; Process Filters

(defun TeX-command-mode-line (process)
  "Format the mode line for a buffer containing output from PROCESS."
    (setq mode-line-process (concat ": "
				    (symbol-name (process-status process))))
    (set-buffer-modified-p (buffer-modified-p)))

(defun TeX-command-filter (process string)
  "Filter to process normal output."
  (save-excursion
    (set-buffer (process-buffer process))
    (save-excursion
      (goto-char (process-mark process))
      (insert-before-markers string)
      (set-marker (process-mark process) (point)))))

(defvar TeX-current-page nil
  "The page number currently being formatted, enclosed in brackets.")

 (make-variable-buffer-local 'TeX-current-page)

(defun TeX-format-mode-line (process)
  "Format the mode line for a buffer containing TeX output from PROCESS."
    (setq mode-line-process (concat " " TeX-current-page ": "
				    (symbol-name (process-status process))))
    (set-buffer-modified-p (buffer-modified-p)))

(defun TeX-format-filter (process string)
  "Filter to process TeX output."
  (save-excursion
    (set-buffer (process-buffer process))
    (save-excursion
      (goto-char (process-mark process))
      (insert-before-markers string)
      (set-marker (process-mark process) (point))) 
    (save-excursion
      (save-match-data
	(if (re-search-backward "\\[[0-9]+\\(\\.[0-9\\.]+\\)?\\]" nil t)
	    (setq TeX-current-page (TeX-match-buffer 0)))))
    (TeX-format-mode-line process)))

(defvar TeX-parse-function nil
  "Function to call to parse content of TeX output buffer.")
 (make-variable-buffer-local 'TeX-parse-function)

(defun TeX-background-filter (process string)
  "Filter to process background output."
  (let ((old-window (selected-window))
	(pop-up-windows t))
    (pop-to-buffer "*TeX background*")
    (goto-char (point-max))
    (insert string)
    (select-window old-window)))


;;; Active Process

(defvar TeX-current-process-region-p nil
  "This variable is set to t iff the last TeX command is on a region.")

(defun TeX-active-process ()
  "Return the active process for the current buffer."
  (if TeX-current-process-region-p
      (TeX-process (TeX-region-file))
    (TeX-process (TeX-master-file))))

(defun TeX-active-buffer ()
  "Return the buffer of the active process for this buffer."
  (if TeX-current-process-region-p
      (TeX-process-buffer (TeX-region-file))
    (TeX-process-buffer (TeX-master-file))))

(defun TeX-active-master (&optional extension)
  "The master file currently being compiled."
  (if TeX-current-process-region-p
      (TeX-region-file extension)
    (TeX-master-file extension)))

(defvar TeX-command-buffer nil
  "The buffer from where the last TeX command was issued.")

;;; Region File

(defcustom TeX-region-extra "{\\makeatletter\\gdef\\AucTeX@cite#1[#2]#3{[#3#1#2]}\\gdef\\cite{\\@ifnextchar[{\\AucTeX@cite{, }}{\\AucTeX@cite{}[]}}}\n"
  "*String to insert in the region file between the header and the text."
  :group 'TeX-commands
  :type 'string)

(defvar TeX-region-hook nil
  "List of hooks to run before the region file is saved.
The hooks are run in the region buffer, you may use the variable
`master-buffer' to access the buffer of the master file and
`orig-buffer' to access the buffer where \\[TeX-command-region] or
\\[TeX-command-buffer] is invoked from.")

(defun TeX-region-create (file region original offset)
  "Create a new file named FILE with the string REGION
The region is taken from ORIGINAL starting at line OFFSET.

The current buffer and master file is searched, in order to ensure
that the TeX header and trailer information is also included.

The OFFSET is used to provide the debugger with information about the
original file."
  (let* (;; We shift buffer a lot, so we must keep track of the buffer
	 ;; local variables.  
	 (header-end TeX-header-end)
	 (trailer-start TeX-trailer-start)
	 
	 ;; We seach for header and trailer in the master file.
	 (orig-buffer (current-buffer))
	 (master-name (TeX-master-file TeX-default-extension))
	 (master-buffer (find-file-noselect master-name))

	 ;; Attempt to disable font lock.
	 (font-lock-defaults-alist nil)
	 (font-lock-defaults nil)
	 (font-lock-maximum-size 0)
	 (font-lock-mode-hook nil)	
	 (font-lock-auto-fontify nil)
	 (font-lock-mode-enable-list nil)
	 ;; And insert them into the FILE buffer.
	 (file-buffer (find-file-noselect file))
	 ;; But remember original content.
	 original-content

	 ;; We search for the header from the master file, if it is
	 ;; not present in the region.
	 (header (if (string-match header-end region)
		     ""
		   (save-excursion
		     (save-restriction
		       (set-buffer master-buffer)
		       (save-excursion
			 (save-restriction
			   (widen)
			   (goto-char (point-min))
			   ;; NOTE: We use the local value of
			   ;; TeX-header-end from the master file.
			   (if (not (re-search-forward TeX-header-end nil t))
			       ""
			     (re-search-forward "[\r\n]" nil t)
			     (buffer-substring (point-min) (point)))))))))
	 
	 ;; We search for the trailer from the master file, if it is
	 ;; not present in the region.
	 (trailer-offset 0)
	 (trailer (if (string-match trailer-start region)
		      ""
		    (save-excursion
		      (save-restriction
			(set-buffer master-buffer)
			(save-excursion
			  (save-restriction
			    (widen)
			    (goto-char (point-max))
			    ;; NOTE: We use the local value of
			    ;; TeX-trailer-start from the master file.
			    (if (not (re-search-backward TeX-trailer-start nil t))
				""
			      ;;(beginning-of-line 1)
			      (re-search-backward "[\r\n]" nil t)
			      (setq trailer-offset
				    (count-lines (point-min) (point)))
			      (buffer-substring (point) (point-max))))))))))
    (save-excursion
      (set-buffer file-buffer)
      (setq buffer-undo-list t)
      (setq original-content (buffer-string))
      (erase-buffer)
      (insert "\\message{ !name(" master-name ")}"
	      header
	      TeX-region-extra
	      "\n\\message{ !name(" original ") !offset(")
      (insert (int-to-string (- offset
				(count-lines (point-min) (point))))
	      ") }\n"
	      region
	      "\n\\message{ !name("  master-name ") !offset(")
      (insert (int-to-string (- trailer-offset
				(count-lines (point-min) (point))))
	      ") }\n"
	      trailer)
      (run-hooks 'TeX-region-hook)
      (if (string-equal (buffer-string) original-content)
	  (set-buffer-modified-p nil)
	(save-buffer 0)))))

(defun TeX-region-file (&optional extension nondirectory)
  "Return TeX-region file name with EXTENSION.
If optional second argument NONDIRECTORY is nil, do not include 
the directory."
  (concat (if nondirectory "" (TeX-master-directory))
	  (cond ((eq extension t)
		 (concat TeX-region "." TeX-default-extension))
		(extension
		 (concat TeX-region "." extension))
		(t
		 TeX-region))))

(defcustom TeX-region "_region_"
  "*Base name for temporary file for use with TeX-region."
  :group 'TeX-commands
  :type 'string)

;;; Parsing

;;; - Global Parser Variables

(defvar TeX-error-point nil
  "How far we have parsed until now.")

 (make-variable-buffer-local 'TeX-error-point)

(defvar TeX-error-file nil
  "Stack of files in which errors have occured")

 (make-variable-buffer-local 'TeX-error-file)

(defvar TeX-error-offset nil
  "Add this to any line numbers from TeX.  Stack like TeX-error-file.")

 (make-variable-buffer-local 'TeX-error-offset)

(defun TeX-parse-reset ()
  "Reset all variables used for parsing TeX output."
  (setq TeX-error-point (point-min))
  (setq TeX-error-offset nil)
  (setq TeX-error-file nil))

;;; - Parsers Hooks

(defun TeX-parse-command (reparse)
  "We can't parse anything but TeX."
  (error "I cannot parse %s output, sorry"
	 (if (TeX-active-process)
	     (process-name (TeX-active-process))
	   "this")))

(defun TeX-parse-TeX (reparse)
  "Find the next error produced by running TeX.
Prefix by C-u to start from the beginning of the errors.

If the file occurs in an included file, the file is loaded (if not
already in an Emacs buffer) and the cursor is placed at the error."

  (let ((old-buffer (current-buffer)))
    (pop-to-buffer (TeX-active-buffer))
    (if reparse
	(TeX-parse-reset))
    (goto-char TeX-error-point)
    (TeX-parse-error old-buffer)))

;;; - Parsing (La)TeX

(defvar TeX-translate-location-hook nil
  "List of functions to be called before showing an error or warning.

You might want to examine and modify the free variables `file',
`offset', `line', `string', `error', and `context' from this hook.")

(defun TeX-parse-error (old)
  "Goto next error.  Pop to OLD buffer if no more errors are found."
    (while
	(progn
	  (re-search-forward (concat "\\("
				     "^! \\|"
				     "(\\|"
				     ")\\|"
				     "\\'\\|"
				     "!offset([---0-9]*)\\|"
				     "!name([^)]*)\\|"
				     "^.*erfull \\\\.*[0-9]*--[0-9]*\\|"
				     "^LaTeX Warning: .*[0-9]+\\.$"
				     "\\)"))
	  (let ((string (TeX-match-buffer 1)))

	    (cond (;; TeX error
		   (string= string "! ")
		   (TeX-error)
		   nil)

		  ;; LaTeX warning
		  ((string-match (concat "\\("
					 "^.*erfull \\\\.*[0-9]*--[0-9]*\\|"
					 "^LaTeX Warning: .*[0-9]+\\.$"
					 "\\)")

				 string)
		   (TeX-warning string))

		  ;; New file -- Push on stack
		  ((string= string "(")
		   (re-search-forward "\\([^()\n \t]*\\)")
		   (setq TeX-error-file
			 (cons (TeX-match-buffer 1) TeX-error-file))
		   (setq TeX-error-offset (cons 0 TeX-error-offset))
		   t)

		  ;; End of file -- Pop from stack
		  ((string= string ")")
		   (setq TeX-error-file (cdr TeX-error-file))
		   (setq TeX-error-offset (cdr TeX-error-offset))
		   t)

		  ;; Hook to change line numbers
		  ((string-match "!offset(\\([---0-9]*\\))" string)
		   (rplaca TeX-error-offset
			   (string-to-int (substring string
						     (match-beginning 1)
						     (match-end 1))))
		   t)

		  ;; Hook to change file name
		  ((string-match "!name(\\([^)]*\\))" string)
		   (rplaca TeX-error-file (substring string
						     (match-beginning 1)
						     (match-end 1)))
		   t)

		  ;; No more errors.
		  (t
		   (message "No more errors.")
		   (beep)
		   (pop-to-buffer old)
		   nil))))))

(defun TeX-error ()
  "Display an error."

  (let* (;; We need the error message to show the user.
	 (error (progn
		  (re-search-forward "\\(.*\\)")
		  (TeX-match-buffer 1)))

	 ;; And the context for the help window.
	 (context-start (point))

	 ;; And the line number to position the cursor.
	 (line (if (re-search-forward "l\\.\\([0-9]+\\)" nil t)
		   (string-to-int (TeX-match-buffer 1))
		 1))
	 ;; And a string of the context to search for.
	 (string (progn
		   (beginning-of-line)
		   (re-search-forward " \\(\\([^ \t]*$\\)\\|\\($\\)\\)")
		   (TeX-match-buffer 1)))

	 ;; And we have now found to the end of the context. 
	 (context (buffer-substring context-start (progn 
						    (forward-line 1)
						    (end-of-line)
						    (point))))
	 ;; We may use these in another buffer.
	 (offset (car TeX-error-offset) )
	 (file (car TeX-error-file)))
	 
    ;; Remember where we was.
    (setq TeX-error-point (point))

    ;; Find the error.
    (if (null file)
	(error "Error occured after last TeX file closed"))
    (run-hooks 'TeX-translate-location-hook)
    (find-file-other-window file)
    (goto-line (+ offset line))
    (if (not (string= string " "))
	(search-forward string nil t))

    ;; Explain the error.
    (if TeX-display-help
	(TeX-help-error error context)
      (message (concat "! " error)))))

(defun TeX-warning (string)
  "Display a warning for STRING.
Return nil if we gave a report."

  (let* ((error (concat "** " string))

	 ;; bad-box is nil if this is a "LaTeX Warning"
	 (bad-box (string-match "^.*erfull \\\\.*[0-9]*--[0-9]*" string))
	 ;; line-string: match 1 is beginning line, match 2 is end line
	 (line-string (if bad-box " \\([0-9]*\\)--\\([0-9]*\\)"
			"on input line \\([0-9]*\\)\\."))
	 ;; word-string: match 1 is the word
	 (word-string (if bad-box "[][\\W() ---]\\(\\w+\\)[][\\W() ---]*$"
			"`\\(\\w+\\)'"))

	 ;; Get error-line (warning)
	 (line (progn
		 (re-search-backward line-string)
		 (string-to-int (TeX-match-buffer 1))))
	 (line-end (if bad-box (string-to-int (TeX-match-buffer 2))
		     line))
	 
	 ;; Find the context
	 (context-start (progn (if bad-box (end-of-line)
				 (beginning-of-line))
			       (point)))

	 (context (progn
		    (forward-line 1)
		    (end-of-line)
		    (while (equal (current-column) 79)
		      (forward-line 1)
		      (end-of-line))
		    (buffer-substring context-start (point))))

	 ;; This is where we want to be.
	 (error-point (point))

	 ;; Now find the error word.
	 (string (progn
		   (re-search-backward word-string
				       context-start t)
		   (TeX-match-buffer 1)))

	 ;; We might use these in another file.
	 (offset (car TeX-error-offset))
	 (file (car TeX-error-file)))

    ;; This is where we start next time.
    (goto-char error-point)
    (setq TeX-error-point (point))

    ;; Go back to TeX-buffer
    (if TeX-debug-bad-boxes
	(progn
	  (run-hooks 'TeX-translate-location-hook)
	  (find-file-other-window file)
	  ;; Find line and string
	  (goto-line (+ offset line))
	  (beginning-of-line 0)
	  (let ((start (point)))
	    (goto-line (+ offset line-end))
	    (end-of-line)
	    (search-backward string start t)
	    (search-forward string nil t))
	  ;; Display help
	  (if TeX-display-help
	      (TeX-help-error error (if bad-box context (concat "\n" context)))
	    (message (concat "! " error)))
	  nil)
      t)))

;;; - Help

(defun TeX-help-error (error output)
  "Print ERROR in context OUTPUT in another window."

  (let ((old-buffer (current-buffer))
	(log-file (TeX-active-master "log"))
	(TeX-error-pointer 1))

    ;; Find help text entry.
    (while (not (string-match (car (nth TeX-error-pointer 
					TeX-error-description-list))
			      error))
      (setq TeX-error-pointer (+ TeX-error-pointer 1)))

    (pop-to-buffer (get-buffer-create "*TeX Help*"))
    (erase-buffer)
    (insert "ERROR: " error
	    "\n\n--- TeX said ---"
	    output
	    "\n--- HELP ---\n"
	    (save-excursion
	      (if (and (string= (cdr (nth TeX-error-pointer
					  TeX-error-description-list))
				"No help available")
		       (let* ((log-buffer (find-file-noselect log-file)))
			 (set-buffer log-buffer)
			 (auto-save-mode nil)
			 (setq buffer-read-only t)
			 (goto-line (point-min))
			 (search-forward error nil t 1)))
		  (progn
		    (re-search-forward "^l.")
		    (re-search-forward "^ [^\n]+$")
		    (forward-char 1)
		    (let ((start (point)))
		      (re-search-forward "^$")
		      (concat "From the .log file...\n\n"
			      (buffer-substring start (point)))))
		(cdr (nth TeX-error-pointer
			  TeX-error-description-list)))))
    (goto-char (point-min))
    (pop-to-buffer old-buffer)))

;;; Error Messages

(defcustom TeX-error-description-list
  '(("Bad \\\\line or \\\\vector argument.*" .
"The first argument of a \\line or \\vector command, which specifies the
slope, is illegal\.")

    ("Bad math environment delimiter.*" .
"TeX has found either a math-mode-starting command such as \\[ or \\(
when it is already in math mode, or else a math-mode-ending command
such as \\) or \\] while in LR or paragraph mode.  The problem is caused
by either unmatched math mode delimiters or unbalanced braces\.")

    ("Bad use of \\\\\\\\.*" .
"A \\\\ command appears between paragraphs, where it makes no sense. This
error message occurs when the \\\\ is used in a centering or flushing
environment or else in the scope of a centering or flushing
declaration.")

    ("\\\\begin{[^ ]*} ended by \\\\end{[^ ]*}." .
"LaTeX has found an \\end command that doesn't match the corresponding
\\begin command. You probably misspelled the environment name in the
\\end command, have an extra \\begin, or else forgot an \\end.")

    ("Can be used only in preamble." .
"LaTeX has encountered, after the \\begin{document}, one of the
following commands that should appear only in the preamble:
\\documentstyle, \\nofiles, \\includeonly, \\makeindex, or
\\makeglossary.  The error is also caused by an extra \\begin{document}
command.")

    ("Command name [^ ]* already used.*" .
"You are using \\newcommand, \\newenvironment, \\newlength, \\newsavebox,
or \\newtheorem to define a command or environment name that is
already defined, or \\newcounter to define a counter that already
exists. (Defining an environment named gnu automatically defines the
command \\gnu.) You'll have to choose a new name or, in the case of
\\newcommand or \\newenvironment, switch to the \\renew ...  command.")

    ("Counter too large." .
"Some object that is numbered with letters, probably an item in a
enumerated list, has received a number greater than 26. Either you're
making a very long list or you've been resetting counter values.")

    ("Environment [^ ]* undefined." .
"LaTeX has encountered a \\begin command for a nonexistent environment.
You probably misspelled the environment name. ")

    ("Float(s) lost." .
"You put a figure or table environment or a \\marginpar command inside a
parbox---either one made with a minipage environment or \\parbox
command, or one constructed by LaTeX in making a footnote, figure,
etc. This is an outputting error, and the offending environment or
command may be quite a way back from the point where LaTeX discovered
the problem. One or more figures, tables, and/or marginal notes have
been lost, but not necessarily the one that caused the error.")

    ("Illegal character in array arg." .
"There is an illegal character in the argument of an array or tabular
environment, or in the second argument of a \\multicolumn command.")

    ("Missing \\\\begin{document}." .
"LaTeX produced printed output before encountering a \\begin{document}
command. Either you forgot the \\begin{document} command or there is
something wrong in the preamble. The problem may be a stray character
or an error in a declaration---for example, omitting the braces around
an argument or forgetting the \\ in a command name.")

    ("Missing p-arg in array arg.*" .
"There is a p that is not followed by an expression in braces in the
argument of an array or tabular environment, or in the second argument
of a \\multicolumn command.")

    ("Missing @-exp in array arg." .
"There is an @ character not followed by an @-expression in the
argument of an array or tabular environment, or in the second argument
of a \\multicolumn command.")

    ("No such counter." .
"You have specified a nonexistent counter in a \\setcounter or
\\addtocounter command. This is probably caused by a simple typing
error.  However, if the error occurred while a file with the extension
aux is being read, then you probably used a \\newcounter command
outside the preamble.")

    ("Not in outer par mode." .
"You had a figure or table environment or a \\marginpar command in math
mode or inside a parbox.")

    ("\\\\pushtabs and \\\\poptabs don't match." .
"LaTeX found a \\poptabs with no matching \\pushtabs, or has come to the
\\end{tabbing} command with one or more unmatched \\pushtabs commands.")

    ("Something's wrong--perhaps a missing \\\\item." .
"The most probable cause is an omitted \\item command in a list-making
environment. It is also caused by forgetting the argument of a
thebibliography environment.")

    ("Tab overflow." .
"A \\= command has exceeded the maximum number of tab stops that LaTeX
permits.")

    ("There's no line here to end." .
"A \\newline or \\\\ command appears between paragraphs, where it makes no
sense. If you're trying to ``leave a blank line'', use a \\vspace
command.")

    ("This may be a LaTeX bug." .
"LaTeX has become thoroughly confused. This is probably due to a
previously detected error, but it is possible that you have found an
error in LaTeX itself. If this is the first error message produced by
the input file and you can't find anything wrong, save the file and
contact the person listed in your Local Guide.")

    ("Too deeply nested." .
"There are too many list-making environments nested within one another.
How many levels of nesting are permitted may depend upon what computer
you are using, but at least four levels are provided, which should be
enough.")

    ("Too many unprocessed floats." .
"While this error can result from having too many \\marginpar commands
on a page, a more likely cause is forcing LaTeX to save more figures
and tables than it has room for.  When typesetting its continuous
scroll, LaTeX saves figures and tables separately and inserts them as
it cuts off pages. This error occurs when LaTeX finds too many figure
and/or table environments before it is time to cut off a page, a
problem that is solved by moving some of the environments farther
towards the end of the input file. The error can also be caused by a
``logjam''---a figure or table that cannot be printed causing others
to pile up behind it, since LaTeX will not print figures or tables out
of order. The jam can be started by a figure or table that either is
too large to fit on a page or won't fit where its optional placement
argument says it must go. This is likely to happen if the argument
does not contain a p option.")

    ("Undefined tab position." .
"A \\>, \\+, \\-, or \\< command is trying to go to a nonexistent tab
position---one not defined by a \\= command.")

    ("\\\\< in mid line." .
"A \\< command appears in the middle of a line in a tabbing environment.
This command should come only at the beginning of a line.")

    ("Counter too large." .
"Footnotes are being ``numbered'' with letters or footnote symbols and
LaTeX has run out of letters or symbols. This is probably caused by
too many \\thanks commands.")

    ("Double subscript." .
"There are two subscripts in a row in a mathematical
formula---something like x_{2}_{3}, which makes no sense.")

    ("Double superscript." .
"There are two superscripts in a row in a mathematical
formula---something like x^{2}^{3}, which makes no sense.")

    ("Extra alignment tab has been changed to \\\\cr." .
"There are too many separate items (column entries) in a single row of
an array or tabular environment. In other words, there were too many &
's before the end of the row. You probably forgot the \\\\ at the end of
the preceding row.")

    ("Extra \\}, or forgotten \\$." .
"The braces or math mode delimiters don't match properly. You probably
forgot a {, \\[, \\(, or $.")

    ("Font [^ ]* not loaded: Not enough room left." .
"The document uses more fonts than TeX has room for. If different parts
of the document use different fonts, then you can get around the
problem by processing it in parts.")

    ("I can't find file `.*'." .
"TeX can't find a file that it needs. If the name of the missing file
has the extension tex, then it is looking for an input file that you
specified---either your main file or another file inserted with an
\\input or \\include command. If the missing file has the extension sty
, then you have specified a nonexistent document style or style
option.")

    ("Illegal parameter number in definition of .*" .
"This is probably caused by a \\newcommand, \\renewcommand,
\\newenvironment, or \\renewenvironment command in which a # is used
incorrectly.  A # character, except as part of the command name \\#,
can be used only to indicate an argument parameter, as in #2, which
denotes the second argument. This error is also caused by nesting one
of the above four commands inside another, or by putting a parameter
like #2 in the last argument of a \\newenvironment or \\renewenvironment
command.")

    ("Illegal unit of measure ([^ ]* inserted)." .
"If you just got a

      ! Missing number, treated as zero.

error, then this is part of the same problem.  If not, it means that
LaTeX was expecting a length as an argument and found a number
instead.  The most common cause of this error is writing 0 instead of
something like 0in for a length of zero, in which case typing return
should result in correct output. However, the error can also be caused
by omitting a command argument.")

    ("Misplaced alignment tab character \\&." .
"The special character &, which should be used only to separate items
in an array or tabular environment, appeared in ordinary text. You
probably meant to type \\&.")

    ("Missing control sequence inserted." .
"This is probably caused by a \\newcommand, \\renewcommand, \\newlength,
or \\newsavebox command whose first argument is not a command name.")

    ("Missing number, treated as zero." .
"This is usually caused by a LaTeX command expecting but not finding
either a number or a length as an argument. You may have omitted an
argument, or a square bracket in the text may have been mistaken for
the beginning of an optional argument. This error is also caused by
putting \\protect in front of either a length command or a command such
as \\value that produces a number.")

    ("Missing [{}] inserted." .
"TeX has become confused. The position indicated by the error locator
is probably beyond the point where the incorrect input is.")

    ("Missing \\$ inserted." .
"TeX probably found a command that can be used only in math mode when
it wasn't in math mode.  Remember that unless stated otherwise, all
the commands of Section can be used only in math mode. TeX is not in
math mode when it begins processing the argument of a box-making
command, even if that command is inside a math environment. This error
also occurs if TeX encounters a blank line when it is in math mode.")

    ("Not a letter." .
"Something appears in the argument of a \\hyphenation command that
doesn't belong there.")

    ("Paragraph ended before [^ ]* was complete." .
"A blank line occurred in a command argument that shouldn't contain
one. You probably forgot the right brace at the end of an argument.")

    ("\\\\[^ ]*font [^ ]* is undefined .*" .
"These errors occur when an uncommon font is used in math mode---for
example, if you use a \\sc command in a formula inside a footnote,
calling for a footnote-sized small caps font.  This problem is solved
by using a \\load command.")

    ("Font .* not found." .                                    
"You requested a family/series/shape/size combination that is totally
unknown.  There are two cases in which this error can occur:
  1) You used the \\size macro to select a size that is not available.
  2) If you did not do that, go to your local `wizard' and
     complain fiercely that the font selection tables are corrupted!")
 
    ("TeX capacity exceeded, sorry .*" .
"TeX has just run out of space and aborted its execution. Before you
panic, remember that the least likely cause of this error is TeX not
having the capacity to process your document.  It was probably an
error in your input file that caused TeX to run out of room. The
following discussion explains how to decide whether you've really
exceeded TeX's capacity and, if so, what to do. If the problem is an
error in the input, you may have to use the divide and conquer method
described previously to locate it. LaTeX seldom runs out of space on a
short input file, so if running it on the last few pages before the
error indicator's position still produces the error, then there's
almost certainly something wrong in the input file.

The end of the error indicator tells what kind of space TeX ran out
of. The more common ones are listed below, with an explanation of
their probable causes.

buffer size 
===========
Can be caused by too long a piece of text as the argument
of a sectioning, \\caption, \\addcontentsline, or \\addtocontents
command. This error will probably occur when the \\end{document} is
being processed, but it could happen when a \\tableofcontents,
\\listoffigures, or \\listoftables command is executed. To solve this
problem, use a shorter optional argument. Even if you're producing a
table of contents or a list of figures or tables, such a long entry
won't help the reader.

exception dictionary 
====================
You have used \\hyphenation commands to give TeX
more hyphenation information than it has room for. Remove some of the
less frequently used words from the \\hyphenation commands and insert
\\- commands instead.

hash size 
=========
Your input file defines too many command names and/or uses
too many cross-ref- erencing labels.

input stack size 
================
This is probably caused by an error in a command
definition. For example, the following command makes a circular
definition, defining \\gnu in terms of itself:

          \\newcommand{\\gnu}{a \\gnu} % This is wrong!

When TeX encounters this \\gnu command, it will keep chasing its tail
trying to figure out what \\gnu should produce, and eventually run out
of ``input stack''.

main memory size 
================
This is one kind of space that TeX can run out of when processing a
short file. There are three ways you can run TeX out of main memory
space: (1) defining a lot of very long, complicated commands, (2)
making an index or glossary and having too many \\index or \\glossary
commands on a single page, and (3) creating so complicated a page of
output that TeX can't hold all the information needed to generate it.
The solution to the first two problems is obvious: define fewer
commands or use fewer \\index and \\glossary commands. The third problem
is nastier. It can be caused by large tabbing, tabular, array, and
picture environments. TeX's space may also be filled up with figures
and tables waiting for a place to go.  To find out if you've really
exceeded TeX's capacity in this way, put a \\clearpage command in your
input file right before the place where TeX ran out of room and try
running it again. If it doesn't run out of room with the \\clearpage
command there, then you did exceed TeX's capacity.  If it still runs
out of room, then there's probably an error in your file.  If TeX is
really out of room, you must give it some help. Remember that TeX
processes a complete paragraph before deciding whether to cut the
page. Inserting a \\newpage command in the middle of the paragraph,
where TeX should break the page, may save the day by letting TeX write
the current page before processing the rest of the paragraph. (A
\\pagebreak command won't help.) If the problem is caused by
accumulated figures and tables, you can try to prevent them from
accumulating---either by moving them further towards the end of the
document or by trying to get them to come out sooner.  If you are
still writing the document, simply add a \\clearpage command and forget
about the problem until you're ready to produce the final version.
Changes to the input file are likely to make the problem go away.

pool size 
=========
You probably used too many cross-ref-erencing \\labels and/or defined
too many new command names. More precisely, the labels and command
names that you define have too many characters, so this problem can be
solved by using shorter names. However, the error can also be caused
by omitting the right brace that ends the argument of either a counter
command such as \\setcounter, or a \\newenvironment or \\newtheorem
command.

save size 
=========
This occurs when commands, environments, and the scopes of
declarations are nested too deeply---for example, by having the
argument of a \\multiput command contain a picture environment that in
turn has a \\footnotesize declaration whose scope contains a \\multiput
command containing a ....")

    ("Text line contains an invalid character." .
"The input contains some strange character that it shouldn't. A mistake
when creating the file probably caused your text editor to insert this
character. Exactly what could have happened depends upon what text
editor you used. If examining the input file doesn't reveal the
offending character, consult the Local Guide for suggestions.")

    ("Undefined control sequence."   .  
"TeX encountered an unknown command name. You probably misspelled the
name. If this message occurs when a LaTeX command is being processed,
the command is probably in the wrong place---for example, the error
can be produced by an \\item command that's not inside a list-making
environment. The error can also be caused by a missing \\documentstyle
command.")

    ("Use of [^ ]* doesn't match its definition." .
"It's probably one of the picture-drawing commands, and you have used
the wrong syntax for specifying an argument. If it's \\@array that
doesn't match its definition, then there is something wrong in an
@-expression in the argument of an array or tabular
environment---perhaps a fragile command that is not \\protect'ed.")

    ("You can't use `macro parameter character \\#' in [^ ]* mode." .
"The special character # has appeared in ordinary text. You probably
meant to type \\#.")

    ("Overfull \\\\hbox .*" .
"Because it couldn't find a good place for a line break, TeX put more
on this line than it should.")

    ("Overfull \\\\vbox .*" .
"Because it couldn't find a good place for a page break, TeX put more
on the page than it should. ")

    ("Underfull \\\\hbox .*" .
"Check your output for extra vertical space.  If you find some, it was
probably caused by a problem with a \\\\ or \\newline command---for
example, two \\\\ commands in succession. This warning can also be
caused by using the sloppypar environment or \\sloppy declaration, or
by inserting a \\linebreak command.")

    ("Underfull \\\\vbox .*" .
"TeX could not find a good place to break the page, so it produced a
page without enough text on it. ")

;; New list items should be placed here 
;; 
;; ("err-regexp" . "context") 
;; 
;; the err-regexp item should match anything

    (".*" . "No help available"))	; end definition 
"A list of the form (\"err-regexp\" . \"context\") used by function
\\{TeX-help-error} to display help-text on an error message or warning.
err-regexp should be a regular expression matching the error message
given from TeX/LaTeX, and context should be some lines describing that
error"
  :group 'TeX-output
  :type '(repeat (cons :tag "Entry"
		       (regexp :tag "Match")
		       (string :format "Description:\n%v"))))

(provide 'tex-buf)

;;; tex-buf.el ends here

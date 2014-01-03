;;beginning of tex-site.el 
;; modifications by W. Minten
;; Last modification August 17, 1997

;;; Customization: 
;;  If you want to make AUC TeX aware of style files and multi-file 
;;  documents right away. 
(setq TeX-auto-save t) 
(setq TeX-parse-self t) 
(setq default-TeX-master nil) 

;; Copy variables you need to change from the start of `tex.el' and 
;; insert them here. 
(defvar no-doc 
  "This function is part of AUC TeX, but has not yet been loaded. 
    Full documentation will be available after autoloading the function." 
  "Documentation for autoload functions.") 

(defvar TeX-lisp-directory (concat (getenv "emacs_dir") "/site-lisp/auctex/")
  "*The directory where the AUC TeX lisp files are located.") 

;;; The two following definitions are in accordance with the installation
;; setting of MiKTeX as described in the Local Guide of MiKTeX
;; c:\texmf\doc\miktex\miktex.html
(defvar TeX-macro-global  '("h:/texmf/tex") 
  "*Directories containing the  sites TeX macro and style files.
        Normally, AUC TeX will only allow you to complete a short list of
        built-in macros and environments and on the macros you define yourself.
        If you issue the `M-x TeX-auto-generate-global' command after loading
        AUC TeX, you will be able to complete on all macros available in the
        standard style files used by your document.  To do this, you must set
        this variable to a list of directories where the standard style files
        are located.  ") 

;; The 'TeX-command-list' (pull-down menu at the top of emacs appearing when 
;; emacs is in TeX major mode) consists of the options below.
;; Invoking 'C-c C-c' in a TeX major mode will run the "LaTeX" command
;; of the command list. (After compiling, errors can be retrieved by
;; invoking 'C-c `' (Control-c accent-gr\`ave).
;; If no errors occur and if all cross-references are known, a second
;; 'C-c C-c' will run the 'View' command of the list.
(defvar TeX-command-list 
  (list(list "LaTeX" "latex \\nonstopmode\\input{%t}" 
             'TeX-run-LaTeX nil t) 
       (list "View" "yap %d" 
             'TeX-run-command nil t) 
       (list "dviPS" "dvips %d -o %f" 
             'TeX-run-command nil t) 
       (list "Ghostview" "c:/GStools/gsview/gsview32.exe %f" 
             'TeX-run-command nil t) 
       (list "BibTeX" "bibtex %s" 'TeX-run-BibTeX nil nil) 
       (list "Index" "makeindex %s" 'TeX-run-command nil t) 
       (list "Check" "lacheck %s" 'TeX-run-compile nil t) 
       (list "Other" "" 'TeX-run-command t t))) 

(setq TeX-default-mode 'LaTeX-mode) 
;; -------------------------------------------------------------------- 

;;; Autoloads: 
(or (assoc TeX-lisp-directory (mapcar 'list load-path))  ; No `member' yet. 
    (assoc (substring TeX-lisp-directory 0 -1) ; Without trailing slash. 
           (mapcar 'list load-path)) 
    (setq load-path (cons TeX-lisp-directory load-path))) 

;; This hook will store bibitems when you save a BibTeX buffer. 
(defvar bibtex-mode-hook nil) 
(or (memq 'BibTeX-auto-store bibtex-mode-hook) ;No `add-hook' yet. 
    (setq bibtex-mode-hook (cons 'BibTeX-auto-store bibtex-mode-hook))) 

(autoload 'BibTeX-auto-store "latex" no-doc t) 
(autoload 'tex-mode "tex" no-doc t) 
(autoload 'plain-tex-mode "tex" no-doc t) 
(autoload 'texinfo-mode "tex-info" no-doc t) 
(autoload 'latex-mode "tex" no-doc t) 

(setq text-mode-hook 'turn-on-auto-fill) 

;;;;    for font-latex running on fast machines
;;(setq font-lock-maximum-decoration t)
;;;;   for font-latex in general
;;(require 'font-latex)

(provide 'tex-site)
;;; tex-site.el ends here

;;; amstext.el --- Style hook for the AMS-LaTeX amstext package.
;;;
;;; AUTHOR: Carsten Dominik <dominik@strw.leidenuniv.nl>

;;; Code:

(TeX-add-style-hook "amstext"
 (function
  (lambda ()
    (TeX-add-symbols
     '("text" t)))))

;;; amstext.el ends here.

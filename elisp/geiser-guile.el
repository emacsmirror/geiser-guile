;; geiser-guile.el -- guile's implementation of the geiser protocols

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Mar 08, 2009 23:03

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comentary:

;; Implementation of all required Elisp Geiser protocols for Guile.

;;; Code:

(require 'geiser-impl)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)


;;; Customization:

(defgroup geiser-guile nil
  "Customization for Geiser's Guile flavour."
  :group 'geiser)

(defcustom geiser-guile-binary
  (cond ((eq system-type 'windows-nt) "guile.exe")
        ((eq system-type 'darwin) "guile")
        (t "guile"))
  "Name to use to call the Guile executable when starting a REPL."
  :type 'string
  :group 'geiser-guile)

(defcustom geiser-guile-init-file "~/.guile-geiser"
  "Initialization file with user code for the Guile REPL."
  :type 'string
  :group 'geiser-guile)


;;; REPL support:

(defun geiser-guile-parameters ()
  "Return a list with all parameters needed to start Guile.
This function uses `geiser-guile-init-file' if it exists."
  (let ((init-file (and (stringp geiser-guile-init-file)
                        (expand-file-name geiser-guile-init-file))))
  `("-q" "-L" ,(expand-file-name "guile/" geiser-scheme-dir)
    ,@(and init-file (file-readable-p init-file) (list "-l" init-file)))))

(defconst geiser-guile-prompt-regexp "^[^() \n]+@([^)]*?)> ")

(defun switch-to-guile (&optional ask)
  (interactive "P")
  (switch-to-geiser ask 'guile))

(defun run-guile ()
  "Run Geiser using Guile."
  (interactive)
  (run-geiser 'guile))


;;; Evaluation support:

(defun geiser-guile-geiser-procedure (proc)
  "Translate a bare procedure symbol to one executable in Guile's
context. Return NULL for unsupported ones; at the very least,
EVAL, COMPILE, LOAD-FILE and COMPILE-FILE should be supported."
  (let ((proc (intern (format "ge:%s" proc))))
    `(@ (geiser emacs) ,proc)))

(defconst geiser-guile--module-re
  "(define-module +\\(([^)]+)\\)")

(defun geiser-guile-get-module (&optional module)
  "Return a scheme datum representing the current module.
If MODULE is provided, transform it to such a datum."
  (cond ((null module)
         (save-excursion
           (goto-char (point-min))
           (if (re-search-forward geiser-guile--module-re nil t)
               (geiser-guile-get-module (match-string-no-properties 1))
             :f)))
        ((listp module) module)
        ((stringp module) (or (ignore-errors (car (read-from-string module))) :f))
        (t :f)))


;;; Trying to ascertain whether a buffer is Guile Scheme:

(defun geiser-guile-guess ()
  "Return `t' if the current buffer looks like a Guile file."
  (listp (geiser-guile-get-module)))


;;; Register this implementation:

(geiser-impl--register 'guile)


(provide 'geiser-guile)
;;; geiser-guile.el ends here

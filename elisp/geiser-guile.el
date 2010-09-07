;; geiser-guile.el -- guile's implementation of the geiser protocols

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sun Mar 08, 2009 23:03



(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)
(require 'geiser-eval)
(require 'geiser-edit)
(require 'geiser)

(require 'compile)


;;; Customization:

(defgroup geiser-guile nil
  "Customization for Geiser's Guile flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-guile-binary
  (cond ((eq system-type 'windows-nt) "guile.exe")
        ((eq system-type 'darwin) "guile")
        (t "guile"))
  "Name to use to call the Guile executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-guile)

(geiser-custom--defcustom geiser-guile-load-path nil
  "A list of paths to be added to Guile's load path when it's
started."
  :type '(repeat file)
  :group 'geiser-guile)

(geiser-custom--defcustom geiser-guile-init-file "~/.guile-geiser"
  "Initialization file with user code for the Guile REPL."
  :type 'string
  :group 'geiser-guile)

(geiser-custom--defcustom geiser-guile-debug-show-bt-p nil
  "Whether to autmatically show a full backtrace when entering the debugger.
If `nil', only the last frame is shown."
  :type 'boolean
  :group 'geiser-guile)

(geiser-custom--defcustom geiser-guile-jump-on-debug-p nil
  "Whether to autmatically jump to error when entering the debugger.
If `t', Geiser will use `next-error' to jump to the error's location."
  :type 'boolean
  :group 'geiser-guile)

(geiser-custom--defcustom geiser-guile-show-debug-help-p t
  "Whether to show brief help in the echo area when entering the debugger."
  :type 'boolean
  :group 'geiser-guile)

(geiser-custom--defcustom geiser-guile-warning-level 'medium
  "Verbosity of the warnings reported by Guile.

You can choose either one of the predefined warning sets, or
provide a list of symbols identifying the ones you want. Possible
choices are arity-mismatch, unbound-variable, unused-variable and
unused-toplevel. Unrecognised symbols are ignored.

The predefined levels are:

  - Medium: arity-mismatch, unbound-variable
  - High: arity-mismatch, unbound-variable, unused-variable
  - None: no warnings

Changes to the value of this variable will automatically take
effect on new REPLs. For existing ones, use the command
\\[geiser-guile-update-warning-level]."
  :type '(choice (const :tag "Medium (arity and unbound vars)" medium)
                 (const :tag "High (also unused vars)" high)
                 (const :tag "No warnings" none)
                 (repeat :tag "Custom" symbol))
  :group 'geiser-guile)


;;; REPL support:

(defun geiser-guile--binary ()
  (if (listp geiser-guile-binary)
      (car geiser-guile-binary)
    geiser-guile-binary))

(defun geiser-guile--parameters ()
  "Return a list with all parameters needed to start Guile.
This function uses `geiser-guile-init-file' if it exists."
  (let ((init-file (and (stringp geiser-guile-init-file)
                        (expand-file-name geiser-guile-init-file))))
  `(,@(and (listp geiser-guile-binary) (cdr geiser-guile-binary))
    "-q" "-L" ,(expand-file-name "guile/" geiser-scheme-dir)
    ,@(apply 'append (mapcar (lambda (p) (list "-L" p)) geiser-guile-load-path))
    ,@(and init-file (file-readable-p init-file) (list "-l" init-file)))))

(defconst geiser-guile--prompt-regexp "^[^() \n]+@([^)]*?)> ")
(defconst geiser-guile--debugger-prompt-regexp
  "^[^() \n]+@([^)]*?) \\[[0-9]+\\]> ")
(defconst geiser-guile--debugger-preamble-regexp
  "^Entering a new prompt\\. ")


;;; Evaluation support:

(defun geiser-guile--geiser-procedure (proc)
  (let ((proc (intern (format "ge:%s" (if (eq proc 'eval) 'compile proc)))))
    `(@ (geiser emacs) ,proc)))

(defconst geiser-guile--module-re
  "(define-module +\\(([^)]+)\\)")

(defun geiser-guile--get-module (&optional module)
  (cond ((null module)
         (save-excursion
           (ignore-errors
             (while (not (zerop (geiser-syntax--nesting-level)))
               (backward-up-list)))
           (if (re-search-backward geiser-guile--module-re nil t)
               (geiser-guile--get-module (match-string-no-properties 1))
             :f)))
        ((listp module) module)
        ((stringp module)
         (or (ignore-errors
               (car (geiser-syntax--read-from-string module))) :f))
        (t :f)))

(defun geiser-guile--module-cmd (module fmt &optional def)
  (when module
    (let* ((module (geiser-guile--get-module module))
           (module (cond ((or (null module) (eq module :f)) def)
                         (t (format "%s" module)))))
      (and module (format fmt module)))))

(defun geiser-guile--import-command (module)
  (geiser-guile--module-cmd module ",use %s"))

(defun geiser-guile--enter-command (module)
  (geiser-guile--module-cmd module ",m %s" "(guile-user)"))


(defun geiser-guile--exit-command () ",q")

(defun geiser-guile--symbol-begin (module)
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^-()>") (point))))


;;; Error display

(defun geiser-guile--enter-debugger ()
  (let ((bt-cmd (format ",%s\n"
                        (if geiser-guile-debug-show-bt-p "bt" "fr"))))
    (compilation-forget-errors)
    (goto-char (point-max))
    (comint-send-string nil "((@ (geiser emacs) ge:newline))\n")
    (comint-send-string nil ",error-message\n")
    (comint-send-string nil bt-cmd)
    (when geiser-guile-show-debug-help-p
      (message "Debug REPL. Enter ,q to quit, ,h for help."))
    (when geiser-guile-jump-on-debug-p
      (accept-process-output (get-buffer-process (current-buffer))
                             0.2 nil t)
      (ignore-errors (next-error)))))

(defun geiser-guile--display-error (module key msg)
  (newline)
  (when (stringp msg)
    (save-excursion (insert msg))
    (geiser-edit--buttonize-files))
  (and (not key) msg (not (zerop (length msg)))))


;;; Trying to ascertain whether a buffer is Guile Scheme:

(defun geiser-guile--guess ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward geiser-guile--module-re nil t)))


;;; Compilation shell regexps

(defconst geiser-guile--path-rx "^In \\([^:\n ]+\\):\n")

(defconst geiser-guile--rel-path-rx "^In +\\([^/\n :]+\\):\n")

(defvar geiser-guile--file-cache (make-hash-table :test 'equal))

(defun geiser-guile--resolve-file (file)
  (when (and (stringp file) (not (string-equal file "unknown file")))
    (if (file-name-absolute-p file) file
      (or (gethash file geiser-guile--file-cache)
          (puthash file
                   (geiser-eval--send/result `(:eval ((:ge find-file) ,file)))
                   geiser-guile--file-cache)))))

(defun geiser-guile--resolve-file-x ()
  (let ((f (geiser-guile--resolve-file (match-string-no-properties 1))))
    (and f (list f))))


;;; REPL startup

(defun geiser-guile-update-warning-level ()
  "Update the warning level used by the REPL.
The new level is set using the value of `geiser-guile-warning-level'."
  (interactive)
  (let ((code `(:eval (ge:set-warnings ',geiser-guile-warning-level)
                      (geiser evaluation))))
    (geiser-eval--send/result code)))

(defun geiser-guile--startup ()
  (set (make-local-variable 'compilation-error-regexp-alist)
       `((,geiser-guile--path-rx geiser-guile--resolve-file-x)
         ("^  +\\([0-9]+\\):\\([0-9]+\\)" nil 1 2)))
  (setq geiser-con--debugging-inhibits-eval nil)
  (compilation-setup t)
  (font-lock-add-keywords nil
                          `((,geiser-guile--path-rx 1
                                                    compilation-error-face)))
  (geiser-guile-update-warning-level))


;;; Implementation definition:

(define-geiser-implementation guile
  (binary geiser-guile--binary)
  (arglist geiser-guile--parameters)
  (startup geiser-guile--startup)
  (prompt-regexp geiser-guile--prompt-regexp)
  (enter-debugger geiser-guile--enter-debugger)
  (debugger-prompt-regexp geiser-guile--debugger-prompt-regexp)
  (debugger-preamble-regexp geiser-guile--debugger-preamble-regexp)
  (marshall-procedure geiser-guile--geiser-procedure)
  (find-module geiser-guile--get-module)
  (enter-command geiser-guile--enter-command)
  (exit-command geiser-guile--exit-command)
  (import-command geiser-guile--import-command)
  (find-symbol-begin geiser-guile--symbol-begin)
  (display-error geiser-guile--display-error)
  (display-help)
  (check-buffer geiser-guile--guess))

(geiser-impl--add-to-alist 'regexp "\\.scm$" 'guile nil)


(provide 'geiser-guile)
;;; geiser-guile.el ends here

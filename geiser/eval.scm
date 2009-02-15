;; eval.scm -- evaluation procedures

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Fri Feb 06, 2009 22:54

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

;; Module defining evaluation procedures called from the Emacs side.

;;; Code:

(define-module (geiser eval)
  #:export (eval-in comp-file load-file)
  #:use-module (srfi srfi-1))

(define (eval-in form module-name)
  "Evals @var{form} in the module designated by @var{module-name}.
If @var{module-name} is @var{#f} or resolution fails, the current module is used instead.
The result is a list of the form ((RESULT . <form-value>) (OUTPUT . <string>))
if no evaluation error happens, or ((ERROR (KEY . <error-key>) <error-arg>...))
in case of errors. Each error arg is a cons (NAME . VALUE), where NAME includes
SUBR, MSG and REST."
  (let ((module (or (and module-name (resolve-module module-name))
                    (current-module))))
    (catch #t
      (lambda ()
        (let* ((result #f)
               (output
                (with-output-to-string
                  (lambda ()
                    (save-module-excursion
                     (lambda ()
                       (set-current-module module)
                       (set! result (compile form))))))))
          (list (cons 'result result) (cons 'output output))))
      (lambda (key . args)
        (list (cons 'error (apply parse-error (cons key args))))))))

(define (parse-error key . args)
  (let* ((len (length args))
         (subr (and (> len 0) (first args)))
         (msg (and (> len 1) (second args)))
         (margs (and (> len 2) (third args)))
         (rest (and (> len 3) (fourth args))))
    (list (cons 'key key)
          (cons 'subr (or subr '()))
          (cons 'msg (if msg (apply format (cons #f (cons msg margs))) '()))
          (cons 'rest (or rest '())))))

(define (comp-file path)
  "Compile and load file, given its full @var{path}."
  (and (compile-file path)
       (load-compiled (compiled-file-name path))))

(define (load-file path)
  "Load file, given its full @var{path}."
  (compile-and-load path))


;;; eval.scm ends here

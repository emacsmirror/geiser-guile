;; evaluation.scm -- evaluation, compilation and macro-expansion

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Mar 02, 2009 02:46

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

;; Core evaluation engine.

;;; Code:

(define-module (geiser evaluation)
  #:export (ge:eval
            ge:compile
            ge:macroexpand
            ge:compile-file
            ge:load-file)
  #:use-module (srfi srfi-1)
  #:use-module (system base compile)
  #:use-module (system base pmatch)
  #:use-module (system vm program)
  #:use-module (ice-9 pretty-print))

(define (handle-error stack . args)
  (pmatch args
    ((,key ,subr ,msg ,args . ,rest)
     (display "Backtrace:\n")
     (if (stack? stack)
         (display-backtrace stack (current-output-port)))
     (newline)
     (display-error stack (current-output-port) subr msg args rest))
    (else (display (format "ERROR: ~a, args: ~a" (car args) (cdr args)))))
  `(error (key . ,(car args))))

(define (ge:compile form module-name)
  (let* ((module (or (and (list? module-name)
                          (resolve-module module-name))
                     (current-module)))
         (result #f)
         (captured-stack #f)
         (error #f)
         (ev (lambda ()
               (save-module-excursion
                (lambda ()
                  (set-current-module module)
                  (set! result (call-with-values
                                   (lambda () (compile form))
                                 (lambda vs
                                   (map (lambda (v)
                                          (with-output-to-string
                                            (lambda () (write v))))
                                        vs)))))))))
    (let ((output
           (with-output-to-string
             (lambda ()
               (catch #t
                 (lambda () (start-stack 'geiser-eval (ev)))
                 (lambda args
                   (set! error #t)
                   (apply handle-error captured-stack args))
                 (lambda args
                   (set! captured-stack (make-stack #t 2 15))))))))
      (write `(,(if error result (cons 'result result))
               (output . ,output)))
      (newline))))

(define ge:eval ge:compile)

(define (ge:compile-file path)
  "Compile a file, given its full @var{path}."
  (ge:compile `(compile-and-load ,path) '(geiser evaluation)))

(define (ge:load-file path)
  "Load file, given its full @var{path}."
  (ge:compile `(load-compiled ,(compiled-file-name path)) '(geiser evaluation)))

(define (ge:macroexpand form . all)
  (let ((all (and (not (null? all)) (car all))))
    (with-output-to-string
      (lambda ()
        (pretty-print ((if all macroexpand macroexpand-1) form))))))

;;; evaluation.scm ends here

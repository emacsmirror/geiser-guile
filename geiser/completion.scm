;; completion.scm -- completing known symbols and module names

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Mar 02, 2009 02:22

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

;; Completion interface with emacs.

;;; Code:

(define-module (geiser completion)
  #:export (completions)
  #:use-module (geiser utils)
  #:use-module (ice-9 session)
  #:use-module (ice-9 regex))

(define (completions prefix . context)
  (let ((context (and (not (null? context)) (car context)))
        (prefix (string-append "^" (regexp-quote prefix))))
    (append (filter (lambda (s) (string-match prefix s))
                    (map symbol->string (local-bindings context)))
            (sort! (map symbol->string (apropos-internal prefix)) string<?))))

(define (local-bindings form)
  (define (body f) (if (> (length f) 2) (cddr f) '()))
  (let loop ((form form) (bindings '()))
    (cond ((not (pair? form)) bindings)
          ((list? (car form))
           (loop (cdr form) (append (local-bindings (car form)) bindings)))
          ((and (list? form) (< (length form) 2)) bindings)
          ((memq (car form) '(define define* lambda))
           (loop (body form) (append (pair->list (cadr form)) bindings)))
          ((and (memq (car form) '(let let* letrec letrec*))
                (list? (cadr form)))
           (loop (body form) (append (map car (cadr form)) bindings)))
          ((and (eq? 'let (car form)) (symbol? (cadr form)))
           (loop (cons 'let (body form)) (cons (cadr form) bindings)))
          (else (loop (cdr form) bindings)))))

;;; completions.scm ends here

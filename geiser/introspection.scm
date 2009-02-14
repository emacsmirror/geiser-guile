;; introspection.scm -- name says it all

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Feb 08, 2009 18:44

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

;; Procedures introspecting on scheme objects and their properties.

;;; Code:

(define-module (geiser introspection)
  #:export (arguments completions symbol-location docstring)
  #:use-module (system vm program)
  #:use-module (ice-9 session)
  #:use-module (ice-9 documentation)
  #:use-module (srfi srfi-1))

(define (arguments sym . syms)
  (let loop ((sym sym) (syms syms))
    (cond ((obj-args (symbol->obj sym)) => (lambda (args) (cons sym args)))
          ((null? syms) #f)
          (else (loop (car syms) (cdr syms))))))

(define (symbol->obj sym)
  (and (symbol? sym) (module-ref (current-module) sym)))

(define (obj-args obj)
  (cond ((not obj) #f)
        ((program? obj) (program-args obj))
        ((procedure? obj) (procedure-args obj))
        ((macro? obj) (macro-args obj))
        (else #f)))

(define (symbol-module sym)
  (call/cc
   (lambda (k)
     (apropos-fold (lambda (module name var init)
                     (if (eq? name sym) (k (module-name module)) init))
                   #f
                   (symbol->string sym)
                   (apropos-fold-accessible (current-module))))))

(define (program-args program)
  (let* ((arity (program-arity program))
         (arg-no (first arity))
         (opt (> (second arity) 0))
         (args (map first (take (program-bindings program) arg-no))))
    (format-args (if opt (drop-right args 1) args)
                 (and opt (last args))
                 (program-module program))))

(define (procedure-args proc)
  (let ((name (procedure-name proc)))
    (cond ((procedure-source proc) => (lambda (src)
                                        (procedure-args-from-source name src)))
          (else (let* ((arity (procedure-property proc 'arity))
                       (req (first arity))
                       (opt (third arity)))
                  (format-args (map (lambda (n)
                                      (string->symbol (format "arg~A" (+ 1 n))))
                                    (iota req))
                               (and opt 'rest)
                               (and name (symbol-module name))))))))

(define (procedure-args-from-source name src)
  (let ((formals (cadr src)))
    (cond ((list? formals) (format-args formals #f (symbol-module name)))
          ((pair? formals) (let ((req (car formals))
                                 (opt (cdr formals)))
                             (format-args (if (list? req) req (list req))
                                          opt
                                          (symbol-module name))))
          (else #f))))

(define (macro-args macro)
  (let ((prog (macro-transformer macro)))
    (if prog
        (obj-args prog)
        (format-args '(...) #f #f))))

(define (format-args args opt module)
  (list (cons 'required args)
        (cons 'optional (or opt '()))
        (cons 'module (cond ((module? module) (module-name module))
                            ((list? module) module)
                            (else '())))))

(define (completions prefix)
  (sort! (map symbol->string
              (apropos-internal (string-append "^" prefix)))
         string<?))

(define (symbol-location sym)
  (cond ((symbol-module sym) => make-location-from-module-name)
        (else '())))

(define (make-location file line)
  (list (cons 'file (if (string? file) file '()))
        (cons 'line (if (number? line) (+ 1 line) '()))))

(define module-filename (@@ (ice-9 session) module-filename))

(define (make-location-from-module-name name)
  (make-location (module-filename name) #f))


(define (docstring sym)
  (object-documentation (symbol->obj sym)))

;;; introspection.scm ends here

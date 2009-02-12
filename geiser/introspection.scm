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
  #:export (proc-args completions symbol-location)
  #:use-module (system vm program)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-1))

(define (resolve-symbol sym)
  (and (symbol? sym)
       (module-bound? (current-module) sym)
       (eval sym (current-module))))

(define (obj-args obj)
  (cond ((not obj) #f)
        ((program? obj) (program-args obj))
        ((procedure? obj) (procedure-args obj))
        ((macro? obj) (macro-args obj))
        (else #f)))

(define (program-args program)
  (let* ((arity (program-arity program))
         (arg-no (first arity))
         (opt (> (second arity) 0))
         (args (map first (take (program-bindings program) arg-no))))
    (format-args (if opt (drop-right args 1) args)
                 (and opt (last args))
                 (program-module program))))

(define (procedure-args proc)
  (let* ((arity (procedure-property proc 'arity))
         (req (first arity))
         (opt (third arity))
         (env (procedure-environment proc)))
    (format-args (map (lambda (n)
                        (string->symbol (format "arg~A" (+ 1 n))))
                      (iota req))
                 (and opt 'rest)
                 (and (not (null? env)) env))))

(define (macro-args macro)
  (let ((prog (macro-transformer macro)))
    (if prog
        (obj-args prog)
        (format-args '(...) #f #f))))

(define (format-args args opt module)
  (list (cons 'required args)
        (cons 'optional (or opt '()))
        (cons 'module (if module (module-name module) '()))))

(define (proc-args proc)
  (obj-args (resolve-symbol proc)))

(define (completions prefix)
  (sort! (map symbol->string
              (apropos-internal (string-append "^" prefix)))
         string<?))

(define (make-location file line)
  (list (cons 'file (if (string? file) file '()))
        (cons 'line (if (number? line) (+ 1 line) '()))))

(define (program-line prog)
  (let ((src (program-source prog 0)))
    (and src (source:line src))))

(define module-filename (@@ (ice-9 session) module-filename))

(define (program-file prog)
  (let* ((mod (and prog (program-module prog)))
         (name (and mod (module-name mod))))
    (and name (module-filename name))))

(define (program-location prog)
  (make-location (program-file prog) (program-line prog)))

(define (symbol-module sym)
  (call/cc
   (lambda (k)
     (apropos-fold (lambda (module name var init)
                     (if (eq? name sym) (k (module-name module)) init))
                   #f
                   (symbol->string sym)
                   (apropos-fold-accessible (current-module))))))

(define (make-location-from-module-name name)
  (make-location (module-filename name) #f))

(define (symbol-location sym)
  (cond ((symbol-module sym) => make-location-from-module-name)
        (else '())))

;;; introspection.scm ends here

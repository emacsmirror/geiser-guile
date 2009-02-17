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
  #:export (arguments
            completions
            symbol-location
            symbol-documentation
            all-modules
            module-children
            module-location)
  #:use-module (system vm program)
  #:use-module (ice-9 session)
  #:use-module (ice-9 documentation)
  #:use-module (srfi srfi-1))

(define (arguments sym . syms)
  (let loop ((sym sym) (syms syms))
    (cond ((obj-args (symbol->obj sym)) => (lambda (args)
                                             (cons sym (apply args-alist args))))
          ((null? syms) #f)
          (else (loop (car syms) (cdr syms))))))

(define (args-alist args opt module)
  (list (cons 'required args)
        (cons 'optional (or opt '()))
        (cons 'module (cond ((module? module) (module-name module))
                            ((list? module) module)
                            (else '())))))

(define (symbol->obj sym)
  (and (symbol? sym)
       (module-defined? (current-module) sym)
       (module-ref (current-module) sym)))

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
    (list (if opt (drop-right args 1) args)
          (and opt (last args))
          (program-module program))))

(define (procedure-args proc)
  (let ((name (procedure-name proc)))
    (cond ((procedure-source proc) => (lambda (src)
                                        (procedure-args-from-source name src)))
          (else (let* ((arity (procedure-property proc 'arity))
                       (req (first arity))
                       (opt (third arity)))
                  (list (map (lambda (n)
                               (string->symbol (format "arg~A" (+ 1 n))))
                             (iota req))
                        (and opt 'rest)
                        (and name (symbol-module name))))))))

(define (procedure-args-from-source name src)
  (let ((formals (cadr src)))
    (cond ((list? formals) (list formals #f (symbol-module name)))
          ((pair? formals) (let ((req (car formals))
                                 (opt (cdr formals)))
                             (list (if (list? req) req (list req))
                                   opt
                                   (symbol-module name))))
          (else #f))))

(define (macro-args macro)
  (let ((prog (macro-transformer macro)))
    (if prog
        (obj-args prog)
        (list '(...) #f #f))))

(define (completions prefix)
  (sort! (map symbol->string
              (apropos-internal (string-append "^" prefix)))
         string<?))

(define (module-location name)
  (make-location (module-filename name) #f))

(define (symbol-location sym)
  (cond ((symbol-module sym) => module-location)
        (else '())))

(define (make-location file line)
  (list (cons 'file (if (string? file) file '()))
        (cons 'line (if (number? line) (+ 1 line) '()))))

(define module-filename (@@ (ice-9 session) module-filename))

(define (docstring sym obj)
  (with-output-to-string
    (lambda ()
      (let* ((type (cond ((macro? obj) "A macro")
                         ((procedure? obj) "A  procedure")
                         ((program? obj) "A compiled program")
                         (else "An object")))
             (modname (symbol-module sym))
            (doc (object-documentation obj)))
        (display type)
        (if modname
            (begin
              (display " in module ")
              (display modname)))
        (newline)
        (if doc (display doc))))))

(define (obj-signature sym obj)
  (let* ((args (obj-args obj))
         (req (and args (car args)))
         (opt (and args (cadr args))))
    (and args (if (not opt) `(,sym ,@req) `(,sym ,@req . ,opt)) sym)))

(define (symbol-documentation sym)
  (let ((obj (symbol->obj sym)))
    (if obj
        `((signature . ,(or (obj-signature sym obj) sym))
          (docstring . ,(docstring sym obj))))))

(define (all-modules)
  (let ((roots ((@@ (ice-9 session) root-modules))))
    (sort! (map (lambda (m)
                  (format "~A" (module-name m)))
                (fold (lambda (m all)
                        (append (all-child-modules m) all))
                      roots
                      roots))
           string<?)))

(define (child-modules mod)
  (delq mod ((@@ (ice-9 session) submodules) mod)))

(define (all-child-modules mod)
  (let ((children (child-modules mod)))
    (fold (lambda (m all)
            (append (all-child-modules m) all))
          children children)))

(define (module-children mod-name)
  (let* ((elts (hash-fold classify-module-object
                          (list '() '() '())
                          (module-obarray (maybe-module-interface mod-name))))
         (elts (map sort-symbols! elts)))
    (list (cons 'modules (map (lambda (m) `(,@mod-name ,m)) (car elts)))
          (cons 'procs (cadr elts))
          (cons 'vars (caddr elts)))))

(define (sort-symbols! syms)
  (let ((cmp (lambda (l r)
               (string<? (symbol->string l) (symbol->string r)))))
    (sort! syms cmp)))

(define (maybe-module-interface mod-name)
  (catch #t
         (lambda () (resolve-interface mod-name))
         (lambda args (resolve-module mod-name))))

(define (classify-module-object name var elts)
  (let ((obj (and (variable-bound? var)
                  (variable-ref var))))
    (cond ((not obj) elts)
          ((and (module? obj) (eq? (module-kind obj) 'directory))
           (list (cons name (car elts))
                 (cadr elts)
                 (caddr elts)))
          ((or (procedure? obj) (program? obj) (macro? obj))
           (list (car elts)
                 (cons name (cadr elts))
                 (caddr elts)))
          (else (list (car elts)
                      (cadr elts)
                      (cons name (caddr elts)))))))

;;; introspection.scm ends here

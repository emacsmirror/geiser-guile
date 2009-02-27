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
  #:export (autodoc
            completions
            symbol-location
            symbol-documentation
            all-modules
            module-children
            module-location)
  #:use-module (system vm program)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (ice-9 documentation)
  #:use-module (srfi srfi-1))

(define (autodoc form)
  (cond ((null? form) #f)
        ((symbol? form) (describe-application (list form)))
        ((list? form)
         (let ((lst (last form)))
           (cond ((symbol? lst) (or (describe-application (list lst))
                                    (describe-application form)))
                 ((list? lst)
                  (or (autodoc lst)
                      (autodoc (map (lambda (s) (if (list? s) (gensym) s)) form))))
                 (else (describe-application form)))))
        (else #f)))

(define (describe-application form)
  (let* ((fun (car form))
         (args (obj-args (symbol->obj fun))))
    (and args
         (list (cons 'signature (signature fun args))
               (cons 'position (find-position args form))
               (cons 'module (symbol-module fun))))))

(define (signature fun args)
  (let ((req (assq-ref args 'required))
        (opt (assq-ref args 'optional))
        (key (assq-ref args 'keyword))
        (rest (assq-ref args 'rest)))
    (let ((sgn `(,fun ,@(or req '())
                      ,@(if opt (cons #:optional opt) '())
                      ,@(if key (cons #:key key) '()))))
      (if rest `(,@sgn . ,rest) sgn))))

(define (find-position args form)
  (let* ((lf (length form))
         (lf-1 (- lf 1)))
    (if (= 1 lf) 0
        (let ((req (length (or (assq-ref args 'required) '())))
              (opt (length (or (assq-ref args 'optional) '())))
              (keys (map (lambda (k) (symbol->keyword (if (list? k) (car k) k)))
                         (or (assq-ref args 'keyword) '())))
              (rest (assq-ref args 'rest)))
          (cond ((<= lf (+ 1 req)) lf-1)
                ((<= lf (+ 1 req opt)) (if (> opt 0) lf lf-1))
                ((or (memq (last form) keys)
                     (memq (car (take-right form 2)) keys)) =>
                 (lambda (sl)
                   (+ 2 req
                      (if (> opt 0) (+ 1 opt) 0)
                      (- (length keys) (length sl)))))
                (else (+ 1 req
                         (if (> opt 0) (+ 1 opt) 0)
                         (if (null? keys) 0 (+ 1 (length keys)))
                         (if rest 1 0))))))))

(define (symbol->obj sym)
  (and (symbol? sym)
       (module-defined? (current-module) sym)
       (module-ref (current-module) sym)))

(define (obj-args obj)
  (cond ((not obj) #f)
        ((or (procedure? obj) (program? obj)) (procedure-arguments obj))
        ((macro? obj) (or (obj-args (macro-transformer obj))
                          '((required ...))))
        (else #f)))

(define (symbol-module sym)
  (and sym
       (call/cc
        (lambda (k)
          (apropos-fold (lambda (module name var init)
                          (if (eq? name sym) (k (module-name module)) init))
                        #f
                        (regexp-quote (symbol->string sym))
                        (apropos-fold-accessible (current-module)))))))

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
  (let ((args (obj-args obj)))
    (and args (signature sym args))))

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

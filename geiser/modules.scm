;;; modules.scm -- module metadata

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Mon Mar 02, 2009 02:00

(define-module (geiser modules)
  #:export (symbol-module
            module-name?
            module-path
            find-module
            all-modules
            module-exports
            module-location)
  #:use-module (geiser utils)
  #:use-module (system vm program)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-1))

(define (module-name? module-name)
  (and (list? module-name)
       (> (length module-name) 0)
       (every symbol? module-name)))

(define (symbol-module sym . all)
  (and sym
       (catch 'module-name
         (lambda ()
           (apropos-fold (lambda (module name var init)
                           (if (eq? name sym)
                               (throw 'module-name (module-name module))
                               init))
                         #f
                         (regexp-quote (symbol->string sym))
                         (if (or (null? all) (not (car all)))
                             (apropos-fold-accessible (current-module))
                             apropos-fold-all)))
         (lambda (key . args)
           (and (eq? key 'module-name) (car args))))))

(define (module-location name)
  (make-location (module-path name) #f))

(define (find-module module-name)
  (and (module-name? module-name)
       (or (nested-ref (resolve-module '() #f) module-name)
           (let ((m (resolve-module module-name #f)))
             (beautify-user-module! m)
             m))))

(define (module-path module-name)
  (and (module-name? module-name)
       (or ((@@ (ice-9 session) module-filename) module-name)
           (module-filename (resolve-module module-name)))))

(define (all-modules)
  (let ((roots ((@@ (ice-9 session) root-modules))))
    (map (lambda (m)
           (format "~A" (module-name m)))
         (fold (lambda (m all)
                 (append (all-child-modules m) all))
               roots
               roots))))

(define (module-exports mod-name)
  (let* ((elts (hash-fold classify-module-object
                          (list '() '() '())
                          (module-obarray (maybe-module-interface mod-name))))
         (elts (map sort-symbols! elts)))
    (list (cons 'modules (map (lambda (m) `(,@mod-name ,m)) (car elts)))
          (cons 'procs (cadr elts))
          (cons 'vars (caddr elts)))))

(define (maybe-module-interface mod-name)
  (catch #t
    (lambda () (resolve-interface mod-name))
    (lambda args (resolve-module mod-name))))

(define (child-modules mod)
  (delq mod ((@@ (ice-9 session) submodules) mod)))

(define (all-child-modules mod)
  (let ((children (child-modules mod)))
    (fold (lambda (m all)
            (append (all-child-modules m) all))
          children children)))

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

;;; modules.scm ends here

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
           (module-filename (resolve-module module-name #f)))))

(define (submodules mod)
  (hash-map->list (lambda (k v) v) (module-submodules mod)))

(define (root-modules)
  (submodules (resolve-module '() #f)))

(define (all-modules)
  (let ((guile (resolve-module '(guile))))
    (cons "(guile)"
          (apply append
                 (map (lambda (r)
                        (map (lambda (m)
                               (format "~A" (module-name m)))
                             (all-child-modules r '())))
                      (remove (lambda (m) (eq? m guile)) (root-modules)))))))

(define (all-child-modules mod seen)
  (let ((cs (filter (lambda (m) (not (member m seen))) (submodules mod))))
    (fold (lambda (m all) (append (all-child-modules m all) all))
          (list mod)
          cs)))

(define (module-exports mod-name)
  (let* ((mod (catch #t
                (lambda () (resolve-interface mod-name))
                (lambda args (resolve-module mod-name))))
         (elts (hash-fold classify-module-object
                          (list '() '() '())
                          (module-obarray mod)))
         (elts (map sort-symbols! elts))
         (subs (map module-name (submodules (resolve-module mod-name #f)))))
    (list (cons 'modules (append subs
                                 (map (lambda (m)
                                        `(,@mod-name ,m)) (car elts))))
          (cons 'procs (cadr elts))
          (cons 'vars (caddr elts)))))

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

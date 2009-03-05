;; modules.scm -- module metadata

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Mar 02, 2009 02:00

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

;; Utilities for accessing metadata about modules.

;;; Code:

(define-module (geiser modules)
  #:export (symbol-module
            module-filename
            all-modules
            module-children
            module-location)
  #:use-module (geiser utils)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-1))

(define (symbol-module sym)
  (and sym
       (catch 'module-name
         (lambda ()
           (apropos-fold (lambda (module name var init)
                           (if (eq? name sym)
                               (throw 'module-name (module-name module)) init))
                         #f
                         (regexp-quote (symbol->string sym))
                         (apropos-fold-accessible (current-module))))
         (lambda (key . args)
           (and (eq? key 'module-name) (car args))))))

(define (module-location name)
  (make-location (module-filename name) #f))

(define module-filename (@@ (ice-9 session) module-filename))

(define (all-modules)
  (let ((roots ((@@ (ice-9 session) root-modules))))
    (sort! (map (lambda (m)
                  (format "~A" (module-name m)))
                (fold (lambda (m all)
                        (append (all-child-modules m) all))
                      roots
                      roots))
           string<?)))

(define (module-children mod-name)
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

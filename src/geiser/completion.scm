;;; completion.scm -- completing known symbols and module names

;; Copyright (C) 2009, 2012, 2023 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Mon Mar 02, 2009 02:22

(define-module (geiser completion)
  #:export (completions module-completions)
  #:use-module (geiser utils)
  #:use-module (geiser modules)
  #:use-module (ice-9 session)
  #:use-module (ice-9 regex))

(define (completions prefix)
  (let ((prefix (string-append "^" (regexp-quote prefix))))
    (sort! (map symbol->string (apropos-internal prefix)) string<?)))

(define (module-completions prefix)
  (let* ((prefix (regexp-substitute/global #f "\\)*$" prefix 'pre "" 'post))
         (prefix (string-append "^" (regexp-quote prefix)))
         (matcher (lambda (s) (string-match prefix s)))
         (names (filter matcher (all-modules))))
    (sort! names string<?)))

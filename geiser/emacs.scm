;;; emacs.scm -- procedures for emacs interaction: entry point

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sun Feb 08, 2009 18:39

(define-module (geiser emacs)
  #:re-export (ge:eval
               ge:compile
               ge:macroexpand
               ge:compile-file
               ge:load-file
               ge:autodoc
               ge:completions
               ge:module-completions
               ge:symbol-location
               ge:generic-methods
               ge:symbol-documentation
               ge:module-exports
               ge:module-location
               ge:callers
               ge:callees
               ge:find-file)
  #:export (ge:no-values)
  #:use-module (geiser evaluation)
  #:use-module ((geiser modules) :renamer (symbol-prefix-proc 'ge:))
  #:use-module ((geiser completion) :renamer (symbol-prefix-proc 'ge:))
  #:use-module ((geiser xref) :renamer (symbol-prefix-proc 'ge:))
  #:use-module ((geiser doc) :renamer (symbol-prefix-proc 'ge:)))

(define (ge:no-values) (values))

;;; emacs.scm ends here

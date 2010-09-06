;;; evaluation.scm -- evaluation, compilation and macro-expansion

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Mon Mar 02, 2009 02:46

(define-module (geiser evaluation)
  #:export (ge:eval
            ge:compile
            ge:macroexpand
            ge:compile-file
            ge:load-file)
  #:use-module (geiser modules)
  #:use-module (srfi srfi-1)
  #:use-module (language tree-il)
  #:use-module (system base compile)
  #:use-module (system base message)
  #:use-module (system base pmatch)
  #:use-module (system vm program)
  #:use-module (ice-9 pretty-print))

(define (handle-error stack . args)
  (pmatch args
    ((,key ,subr ,msg ,args . ,rest)
     (display "Backtrace:\n")
     (if (stack? stack)
         (display-backtrace stack (current-output-port)))
     (newline)
     (display-error stack (current-output-port) subr msg args rest))
    (else (display (format "ERROR: ~a, args: ~a" (car args) (cdr args)))))
  `(error (key . ,(car args))))

(define (write-result result output)
  (write (list (cons 'result result) (cons 'output output)))
  (newline))

(define compile-opts '(#:warnings (arity-mismatch unbound-variable)))

(define (call-with-result thunk)
  (letrec* ((result #f)
            (output
             (with-output-to-string
               (lambda ()
                 (with-fluids ((*current-warning-port* (current-output-port)))
                   (set! result (thunk)))))))
    (write-result result output)))

(define (ge:compile form module-name)
  (let* ((module (or (find-module module-name) (current-module)))
         (ev (lambda ()
               (call-with-values
                   (lambda ()
                     (let* ((o (compile form
                                        #:to 'objcode
                                        #:env module
                                        #:opts compile-opts))
                            (thunk (make-program o)))
                       (start-stack 'geiser-evaluation-stack
                                    (eval `(,thunk) module))))
                 (lambda vs (map object->string vs))))))
    (call-with-result ev)))

(define ge:eval ge:compile)

(define (ge:compile-file path)
  (call-with-result
   (lambda ()
     (let ((cr (compile-file path
                             #:canonicalization 'absolute
                             #:opts compile-opts)))
       (and cr
            (list (object->string (save-module-excursion
                                   (lambda () (load-compiled cr))))))))))

(define ge:load-file ge:compile-file)

(define (ge:macroexpand form . all)
  (let ((all (and (not (null? all)) (car all))))
    (with-output-to-string
      (lambda ()
        (pretty-print (tree-il->scheme (macroexpand form)))))))

;;; evaluation.scm ends here

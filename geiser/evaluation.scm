;;; evaluation.scm -- evaluation, compilation and macro-expansion

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

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
  #:use-module (srfi srfi-1)
  #:use-module (system base compile)
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

(define (ge:compile form module-name)
  (let* ((module (or (and (list? module-name)
                          (resolve-module module-name))
                     (current-module)))
         (result #f)
         (captured-stack #f)
         (err #f)
         (ev (lambda ()
               (set! result (call-with-values
                                (lambda () (compile form #:env module))
                              (lambda vs (map object->string vs)))))))
    (let ((output
           (with-output-to-string
             (lambda ()
               (catch #t
                 (lambda () (start-stack 'geiser-eval (ev)))
                 (lambda args
                   (set! err (apply handle-error captured-stack args)))
                 (lambda args
                   (set! captured-stack (make-stack #t 11 11))))))))
      (write `(,(or err (cons 'result result))
               (output . ,output)))
      (newline))))

(define ge:eval ge:compile)

(define (ge:compile-file path)
  "Compile a file, given its full @var{path}."
  (ge:compile `(load-compiled (compile-file ,path)) '(geiser evaluation)))

(define ge:load-file ge:compile-file)

(define (ge:macroexpand form . all)
  (let ((all (and (not (null? all)) (car all))))
    (with-output-to-string
      (lambda ()
        (pretty-print ((if all macroexpand macroexpand-1) form))))))

;;; evaluation.scm ends here

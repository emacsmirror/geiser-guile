;; emacs.scm -- procedures for emacs interaction

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Feb 08, 2009 18:39

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

;; Re-exports of procedures used by Emacs.

;;; Code:

(define-module (geiser emacs)
  #:export (ge:eval
            ge:compile
            ge:compile-file
            ge:load-file)
  #:re-export (ge:arguments
               ge:completions
               ge:symbol-location
               ge:symbol-documentation
               ge:all-modules
               ge:module-children
               ge:module-location)
  #:use-module (srfi srfi-1)
  #:use-module (system base compile)
  #:use-module (system vm program)
  #:use-module (ice-9 debugger utils)
  #:use-module ((geiser introspection) :renamer (symbol-prefix-proc 'ge:)))

(define (make-result result output)
  (list (cons 'result result) (cons 'output output)))

(define (make-error key args stack)
  (list (cons 'error (apply parse-error (cons key args)))
        (cons 'stack (parse-stack stack))))

(define (parse-stack stack)
  (if stack
      (map (lambda (n) (parse-frame (stack-ref stack n)))
           (iota (stack-length stack)))
      '()))

(define (parse-frame frame)
  (list (cons 'frame (frame-number frame))
        (cons 'procedure (or (and (frame-procedure? frame)
                                  (procedure-name (frame-procedure frame)))
                             '()))
        (cons 'source (or (frame->source-position frame) '()))
        (cons 'description (with-output-to-string
                             (lambda ()
                               (if (frame-procedure? frame)
                                   (write-frame-short/application frame)
                                   (write-frame-short/expression frame)))))))

(define (frame->source-position frame)
  (let ((source (if (frame-procedure? frame)
                    (or (frame-source frame)
                        (let ((proc (frame-procedure frame)))
                          (and proc
                               (procedure? proc)
                               (procedure-source proc))))
                    (frame-source frame))))
    (and source
         (cond ((string? (source-property source 'filename))
                (list (source-property source 'filename)
                      (+ 1 (source-property source 'line))
                      (source-property source 'column)))
               ((and (pair? source) (list? (cadr source)))
                (list (caadr source)
                      (+ 1 (caddr source))
                      (cdddr source)))
               (else #f)))))

(define (parse-error key . args)
  (let* ((len (length args))
         (subr (and (> len 0) (first args)))
         (msg (and (> len 1) (second args)))
         (margs (and (> len 2) (third args)))
         (rest (and (> len 3) (fourth args))))
    (list (cons 'key key)
          (cons 'subr (or subr '()))
          (cons 'msg (if msg (apply format (cons #f (cons msg margs))) '()))
          (cons 'rest (or rest '())))))

(define (evaluate form module-name evaluator)
  (let ((module (or (and (list? module-name)
                         (resolve-module module-name))
                    (current-module)))
        (result #f)
        (captured-stack #f)
        (error #f))
    (let ((output
           (with-output-to-string
             (lambda ()
               (set! result
                     (catch #t
                       (lambda ()
                         (start-stack 'id (evaluator form module)))
                       (lambda (key . args)
                         (set! error (make-error key args captured-stack)))
                       (lambda (key . args)
                         (set! captured-stack (make-stack #t 2 2)))))))))
      (write (or error (make-result result output)))
      (newline))))

(define (eval-compile form module)
  (save-module-excursion
   (lambda ()
     (set-current-module module)
     (compile form))))

(define (ge:eval form module-name)
  "Evals @var{form} in the module designated by @var{module-name}.
If @var{module-name} is @var{#f} or resolution fails, the current module is used instead.
The result is a list of the form ((RESULT . <form-value>) (OUTPUT . <string>))
if no evaluation error happens, or ((ERROR (KEY . <error-key>) <error-arg>...))
in case of errors. Each error arg is a cons (NAME . VALUE), where NAME includes
SUBR, MSG and REST."
  (evaluate form module-name eval))

(define (ge:compile form module-name)
  "Compiles @var{form} in the module designated by @var{module-name}.
If @var{module-name} is @var{#f} or resolution fails, the current module is used instead.
The result is a list of the form ((RESULT . <form-value>) (OUTPUT . <string>))
if no evaluation error happens, or ((ERROR (KEY . <error-key>) <error-arg>...))
in case of errors. Each error arg is a cons (NAME . VALUE), where NAME includes
SUBR, MSG and REST."
  (evaluate form module-name eval-compile))

(define (ge:compile-file path)
  "Compile and load file, given its full @var{path}."
  (evaluate `(and (compile-file ,path)
                  (load-compiled ,(compiled-file-name path)))
            '(geiser emacs)
            eval))

(define (ge:load-file path)
  "Load file, given its full @var{path}."
  (evaluate `(load ,path) '(geiser emacs) eval))

;;; emacs.scm ends here

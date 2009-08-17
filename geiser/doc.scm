;; doc.scm -- name says it all

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

;; Procedures providing documentation on scheme objects.

;;; Code:

(define-module (geiser doc)
  #:export (autodoc
            symbol-documentation
            object-signature)
  #:use-module (geiser utils)
  #:use-module (geiser modules)
  #:use-module (system vm program)
  #:use-module (ice-9 session)
  #:use-module (ice-9 documentation)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1))

(define (autodoc ids)
  (if (not (list? ids))
      '()
      (map (lambda (id) (or (autodoc* id) (list id))) ids)))

(define (autodoc* id)
  (let ((args (obj-args (symbol->object id))))
    (and args
         `(,@(signature id args)
           (module . ,(symbol-module id))))))

(define (object-signature name obj)
  (let ((args (obj-args obj)))
    (and args (signature name args))))

(define (signature id args)
  (define (arglst kind)
    (let ((args (assq-ref args kind)))
      (cond ((or (not args) (null? args)) '())
            ((list? args) args)
            (else (list args)))))
  `(,id
    (args ,@(if (list? args)
                `((required ,@(arglst 'required))
                  (optional ,@(arglst 'optional)
                            ,@(let ((rest (assq-ref args 'rest)))
                                (if rest (list "...") '())))
                  (key ,@(arglst 'keyword)))
                '()))))

(define (obj-args obj)
  (cond ((not obj) #f)
        ((or (procedure? obj) (program? obj)) (arguments obj))
        ((macro? obj) (or (obj-args (macro-transformer obj)) '((required ...))))
        (else 'variable)))

(define (arguments proc)
  (cond
   ((is-a? proc <generic>) (generic-args proc))
   ((procedure-property proc 'arglist) => arglist->args)
   ((procedure-source proc) => source->args)
   ((program? proc) ((@ (system vm program) program-arguments) proc))
   ((doc->args proc))
   ((procedure-property proc 'arity) => arity->args)
   (else #f)))

(define (source->args src)
  (let ((formals (cadr src)))
    (cond ((list? formals) `((required . ,formals)))
          ((pair? formals)
           `((required . ,(car formals)) (rest . ,(cdr formals))))
          (else #f))))

(define (arity->args art)
  (let ((req (car art))
        (opt (cadr art))
        (rest (caddr art)))
    `(,@(if (> req 0) (list (cons 'required (gen-arg-names 1 req))) '())
      ,@(if (> opt 0) (list (cons 'optional (gen-arg-names (+ 1 req) opt))) '())
      ,@(if rest (list (cons 'rest 'rest)) '()))))

(define (gen-arg-names fst count)
  (let* ((letts (list->vector '(#\x #\y #\z #\u #\v #\w #\t)))
         (len (vector-length letts))
         (lett (lambda (n) (vector-ref letts (modulo n len)))))
    (map (lambda (n) (string->symbol (format "~A" (lett (- n 1)))))
         (iota (max count 1) fst))))

(define (arglist->args arglist)
  `((required . ,(car arglist))
    (optional . ,(cadr arglist))
    (keyword . ,(caddr arglist))
    (rest . ,(car (cddddr arglist)))))

(define (doc->args proc)
  (define proc-rx "-- Scheme Procedure: ([^[\n]+)\n")
  (define proc-rx2 "-- Scheme Procedure: ([^[\n]+\\[[^\n]*(\n[^\n]+\\]+)?)")
  (cond ((procedure-property proc 'geiser-document-args))
        ((object-documentation proc)
         => (lambda (doc)
              (let* ((match (or (string-match proc-rx doc)
                                (string-match proc-rx2 doc)))
                     (args (and match
                                (parse-signature-string (match:substring match 1)))))
                (set-procedure-property! proc 'geiser-document-args args)
                args)))
        (else #f)))

(define (parse-signature-string str)
  (define opt-arg-rx "\\[([^] ]+)\\]?")
  (define opt-arg-rx2 "([^ ])+\\]+")
  (let ((tokens (string-tokenize str)))
    (if (< (length tokens) 2)
        '()
        (let loop ((tokens (cdr tokens)) (req '()) (opt '()) (rest #f))
          (cond ((null? tokens)
                 `((required ,@(map string->symbol (reverse! req)))
                   (optional ,@(map string->symbol (reverse! opt)))
                   ,@(if rest
                         (list (cons 'rest (string->symbol rest)))
                         '())))
                ((string=? "." (car tokens))
                 (if (not (null? (cdr tokens)))
                     (loop (cddr tokens) req opt (cadr tokens))
                     (loop '() req opt "rest")))
                ((or (string-match opt-arg-rx (car tokens))
                     (string-match opt-arg-rx2 (car tokens)))
                 => (lambda (m)
                      (loop (cdr tokens)
                            req
                            (cons (match:substring m 1) opt)
                            rest)))
                (else (loop (cdr tokens) (cons (car tokens) req) opt rest)))))))

(define (generic-args gen)
  (define (src> src1 src2)
    (> (length (cadr src1)) (length (cadr src2))))
  (define (src m)
    (catch #t
      (lambda () (method-source m))
      (lambda (k . a) #f)))
  (let* ((methods (generic-function-methods gen))
         (srcs (filter identity (map src methods))))
    (cond ((and (null? srcs) (null? methods)) '((rest . rest)))
          ((and (null? srcs)
                (not (null? methods))
                (method-procedure (car methods)))
           => arguments)
          ((not (null? srcs)) (source->args (car (sort! srcs src>))))
          (else '((rest . rest))))))

(define (symbol-documentation sym)
  (let ((obj (symbol->object sym)))
    (if obj
        `((signature . ,(or (obj-signature sym obj) sym))
          (docstring . ,(docstring sym obj))))))

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

;;; doc.scm ends here

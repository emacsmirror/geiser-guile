;; utils.scm -- utility functions

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Mar 02, 2009 01:48

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

;; Some utilities used by other modules.

;;; Code:

(define-module (geiser utils)
  #:export (make-location
            symbol->object
            pair->list
            sort-symbols!))

(define (symbol->object sym)
  (and (symbol? sym)
       (module-defined? (current-module) sym)
       (module-ref (current-module) sym)))

(define (pair->list pair)
  (let loop ((d pair) (s '()))
    (cond ((null? d) (reverse! s))
          ((symbol? d) (reverse! (cons d s)))
          (else (loop (cdr d) (cons (car d) s))))))

(define (make-location file line)
  (list (cons 'file (if (string? file) file '()))
        (cons 'line (if (number? line) (+ 1 line) '()))))

(define (sort-symbols! syms)
  (let ((cmp (lambda (l r)
               (string<? (symbol->string l) (symbol->string r)))))
    (sort! syms cmp)))

;;; utils.scm ends here

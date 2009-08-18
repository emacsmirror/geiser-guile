;; completion.scm -- completing known symbols and module names

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Mar 02, 2009 02:22

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

;; Completion interface with emacs.

;;; Code:

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
  (let* ((prefix (string-append "^" (regexp-quote prefix)))
         (matcher (lambda (s) (string-match prefix s)))
         (names (filter matcher (all-modules))))
    (sort! names string<?)))

;;; completions.scm ends here

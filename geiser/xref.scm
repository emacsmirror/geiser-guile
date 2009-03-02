;; xref.scm -- cross-referencing utilities

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Mar 02, 2009 02:37

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

;; Procedures to locate symbols and their xrefs.

;;; Code:

(define-module (geiser xref)
  #:export (symbol-location
            generic-methods)
  #:use-module (geiser utils)
  #:use-module (geiser modules))

(define (symbol-location sym)
  (cond ((symbol-module sym) => module-location)
        (else '())))

;;; xref.scm ends here

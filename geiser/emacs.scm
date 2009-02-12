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
  #:re-export (ge:proc-args
               ge:completions
               ge:symbol-location
               ge:compile-file
               ge:load-file)
  #:use-module ((geiser introspection)
                :renamer (symbol-prefix-proc 'ge:))
  #:use-module ((geiser eval)
                :select ((comp-file . ge:compile-file)
                         (load-file . ge:load-file))))

;;; emacs.scm ends here

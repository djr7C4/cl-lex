;;;; Loads all files.
;;;; Copyright (C) 2009 David J. Rosenbaum, email: davidjrosenbaum2@gmail.com
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, under version 3 of the License
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; load.lisp Copyright (C) 2009 David J. Rosenbaum
;;;; This program comes with ABSOLUTELY NO WARRANTY; for details see COPYING
;;;; This is free software, and you are welcome to redistribute it
;;;; under certain conditions; for details see COPYING

(in-package :cl-user)

;; This library causes a lot of warnings that don't matter.
(declaim (sb-ext:muffle-conditions warning))

(loop for file in '("packages.lisp"
		    "specials.lisp"
		    "util.lisp"
		    "errors.lisp"
		    "lexer.lisp"
		    "parser.lisp"
		    "regex-class.lisp"
		    "convert.lisp"
		    "optimize.lisp"
		    "closures.lisp"
		    "repetition-closures.lisp"
		    "scanner.lisp"
		    "api.lisp") do
     (load (concatenate 'string "/usr/share/common-lisp/source/cl-ppcre/" file)))

(dolist (file '("packages.lisp" "lex.lisp"))
  (load (concatenate 'string "cl-lex/" file)))

(declaim (sb-ext:unmuffle-conditions warning))

;;; js-import.el --- Import Javascript files from your current project or dependencies  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Jakob Lind

;; Author: Jakob Lind <karl.jakob.lind@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'f)
(require 'json)
(require 'subr-x)
(require 'projectile)

(defun jsi/get-project-dependencies ()
  "Get dependencies section in package.json for the current Projectile project"
  (let ((json-object-type 'hash-table))
    (hash-table-keys
     (gethash "dependencies"
              (json-read-from-string (f-read-text (concat (projectile-project-root) "package.json") 'utf-8))))))

(defun jsi/string-ends-with-p (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun jsi/is-js-file (file)
  (or (jsi/string-ends-with-p file ".js") (jsi/string-ends-with-p file ".jsx")))

(defun js-import ()
  (interactive)
  (let* ((filtered-project-files
          (-filter 'jsi/is-js-file (projectile-current-project-files)))
         (all (append (jsi/get-project-dependencies) filtered-project-files))
         (selected-file (ido-completing-read "Select a file to import: " all))
         (selected-file-name (f-filename (f-no-ext selected-file)))
         (selected-file-relative-path
          (f-relative
           (concat (projectile-project-root) (f-no-ext selected-file))
           (file-name-directory (buffer-file-name)))))
    (insert (concat
             "import "
             selected-file-name
             " from \""
             (if (jsi/is-js-file selected-file) (concat "./" selected-file-relative-path) selected-file-name)
             "\";"))))


(provide 'js-import)
;;; js-import.el ends here

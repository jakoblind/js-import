;;; js-import.el --- Import Javascript files from your current project or dependencies  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Jakob Lind

;; Author: Jakob Lind <karl.jakob.lind@gmail.com>
;; URL: https://github.com/jakoblind/js-import
;; Package-Requires: ((emacs "24.4") (f "0.19.0") (projectile "0.14.0") (dash "2.13.0"))
;; Version: 1.0
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

(defun js-import-get-project-dependencies ()
  "Get dependencies section in package.json for the current Projectile project."
  (let ((json-object-type 'hash-table))
    (hash-table-keys
     (gethash "dependencies"
              (json-read-from-string (f-read-text (concat (projectile-project-root) "package.json") 'utf-8))))))

(defun js-import-string-ends-with-p (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun js-import-is-js-file (filename)
  "Check if FILENAME ends with either .js or .jsx."
  (or (js-import-string-ends-with-p filename ".js") (js-import-string-ends-with-p filename ".jsx")))

;;;###autoload
(defun js-import ()
  "Import Javascript files from your current project or dependencies."
  (interactive)
  (let* ((filtered-project-files
          (-filter 'js-import-is-js-file (projectile-current-project-files)))
         (all (append (js-import-get-project-dependencies) filtered-project-files))
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
             (if (js-import-is-js-file selected-file) (concat "./" selected-file-relative-path) selected-file-name)
             "\";"))))

(provide 'js-import)
;;; js-import.el ends here

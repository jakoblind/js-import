;;; js-import-test.el --- Test for js-import         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Jakob Lind

;; Author: Jakob Lind <karl.jakob.lind@gmail.com>
;; Keywords:

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

(require 'ert)
(require 'js-import)

(ert-deftest load-package-json ()
  "Test loading of package-json file"
  (should (equal (js-import-get-project-dependencies "not-exist" "dependencies") nil))
  (should (equal (js-import-get-project-dependencies "./package.json.empty.dependencies" "dependencies") nil))
  (should (equal (js-import-get-project-dependencies "package.json" "dependencies") '("redux" "react")))
  (should (equal (js-import-get-project-dependencies "./package.json.devdependencies" "devDependencies") '("react-hot-loader" "sinon"))))

;;; js-import-test.el ends here

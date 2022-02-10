;;; azure.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Christiaan Janssen

;; Author: Christiaan Janssen <christiaanjanssen@outlook.com>
;; Keywords: azure lisp
;; Version: 0.0.1

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

;; Put a description of the package here

;;; Code:
(defun azure/account-show ()
  (interactive)
  (async-shell-command "az account show"))

(defun azure/accounts-list ()
  "List the account ac"
  (interactive)
  (async-shell-command "az account list"))

(provide 'azure)
;;; azure.el ends here

;;; smart-compilation.el --- enhance compile to go up to project directory  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Darren Embry <dse@webonastick.com>

;; Author: Darren Embry <dse@webonastick.com>
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

;; Certain project compilation tools including `make' do not search
;; parent directories until they find a `Makefile`, while other tools
;; such as `gulp' do.  This package implements an advice function
;; around the low-level function `compilation-start' that provides
;; `M-x compile' and other functions that use it with that
;; functionality.

;;; Code:

(defvar smart-compilation/command-project-file-alist
  '(("make" . ("GNUmakefile" "makefile" "Makefile")))
  "Alist of commands requiring a directory parent search and project files.

An associative list whose keys are commands and whose values are
either project file names those commands look for or lists of
file names.

You only need to include commands that do not search parent
directories for project files.

Example 1: the `gulp' command searches parent directories for
`gulpfile.js', so you do not need to include it.

Example 2: the `make' command does not search parent directories
for `GNUmakefile', `makefile', and `Makefile', so you would need
to include it in this alist.")

(defun smart-compilation/proglist ()
  "Return a list of programs requiring a directory parent search...

...to find project files."
  (mapcar 'car smart-compilation/command-project-file-alist))

(defun smart-compilation/any-file-exists (filename-list &optional directory)
  "Return the first filename in FILENAME-LIST that exists in DIRECTORY.

DIRECTORY can be a directory name or file name (with or without a
trailing directory separator character, respectively) of the
directory.

If DIRECTORY is NIL, the value of `default-directory' is used.

Returns NIL if none of the filenames are found."
  (let ((directory-name (file-name-as-directory (or directory default-directory))))
    (catch 'found-it
      (dolist (filename filename-list)
        (let ((path (expand-file-name filename directory-name)))
          (if (file-exists-p path)
              (progn
                (message "%s exists in %s" filename directory-name)
                (throw 'found-it path))
            (message "%s does not exist in %s" filename directory-name)
            ))))))

(defun smart-compilation/directory-separator-regexp ()
  "Return a regexp that will match any directory separator character."
  (if (memq system-type '(windows-nt ms-dos))
      "[\\/]"
    "/"))

(defun smart-compilation/command-path-name-regexp (progname)
  "Return a regular expression matching any command that is PROGNAME.

PROGNAME can be a string or a list.  If a list, the regular expression
matches any command that is a member of PROGNAME."
  (concat "\\`"
          "\\(?:"
          ".*"
          (smart-compilation/directory-separator-regexp)
          "\\)?"
          (if (listp progname)
              (regexp-opt progname "\\(")
            (concat "\\(" (regexp-quote progname) "\\)"))
          "\\'"))

(defun smart-compilation/command-begins-with (command progname)
  "Return T if COMMAND's first command is PROGNAME, NIL otherwise.

If PROGNAME is a list, return T if COMMAND's first command is any
member of PROGNAME."
  (let ((first-command-name (car (split-string-and-unquote command))))
    (if (let ((case-fold-search t))
          (string-match (smart-compilation/command-path-name-regexp progname)
                        first-command-name))
        (match-string 1 first-command-name)
      nil)))

;; (smart-compilation/command-begins-with "make" "make")
;; (smart-compilation/command-begins-with "make" '("make" "gulp"))
;; (smart-compilation/command-begins-with "gulp" '("make" "gulp"))
;; (smart-compilation/command-begins-with "fart" '("make" "gulp"))
;; (smart-compilation/command-begins-with "xmakex" '("make" "gulp"))
;; (smart-compilation/command-begins-with "make" "MAKE")
;; (smart-compilation/command-begins-with "MAKE" "make")
;; (smart-compilation/command-begins-with "MAKE" "MAKE")
;; (smart-compilation/command-begins-with "make" "xmake")
;; (smart-compilation/command-begins-with "xmake" "make")
;; (smart-compilation/command-begins-with "make" "makex")
;; (smart-compilation/command-begins-with "makex" "make")
;; (smart-compilation/command-begins-with "make -k" "make")
;; (smart-compilation/command-begins-with "make -k " "make")
;; (smart-compilation/command-begins-with " make -k " "make")
;; (smart-compilation/command-begins-with " c:\\gnu\\bin\\make -k " "make")
;; (smart-compilation/command-begins-with " 'c:\\gnu\\bin\\make' -k " "make")
;; (smart-compilation/command-begins-with " \"c:\\\\gnu\\\\bin\\\\make\" -k " "make")
;; (smart-compilation/command-begins-with " cd foo && \"c:\\\\gnu\\\\bin\\\\make\" -k " "make")

;; (smart-compilation/command-path-name-regexp "make")
;; (car (split-string-and-unquote "    '/foo/bar'  -k   "))
;; (car (split-string-and-unquote "    \"c:\\\\foo\\\\bin\\\\MaKe\"  -k   "))
;; (smart-compilation/command-begins-with "    c:\\foo\\bin\\MaKe  -k   " "make")

(defun smart-compilation/find-project-root (filename &optional directory)
  "Search for FILENAME in DIRECTORY and ancestors; return full path if found.

FILENAME can be a string or a list.  If FILENAME is a list, each
FILENAME is searched in DIRECTORY until one is found.

DIRECTORY can be a directory name or file name (with or without a
trailing directory separator character, respectively) of the
directory.

If DIRECTORY is NIL, the value of `default-directory' is used.

Returns NIL if no path is found.

If FILE is not found in DIRECTORY, the parent of DIRECTORY will
be searched."
  (let* ((directory-name (file-name-as-directory (file-truename (or directory default-directory))))
         (parent-directory-name (file-name-as-directory (file-truename (expand-file-name ".." directory-name))))
         (is-root (equal directory-name parent-directory-name)))
    (if (listp filename)
        (let ((path (smart-compilation/any-file-exists filename directory-name)))
          (or path
              (if is-root
                  nil
                (smart-compilation/find-project-root filename parent-directory-name))))
      (let ((path (expand-file-name filename directory)))
        (if (file-exists-p path)
            path
          (progn
            (message "%s does not exist" path)
            (if is-root
                nil
              (smart-compilation/find-project-root filename parent-directory-name))))))))

(defun smart-compilation/compilation-start (orig-fun command &optional mode name-function highlight-regexp)
  "Change directory to the project root if needed, and run `compilation-start'.

ORIG-FUN is the original `compilation-start' function.

COMMAND, MODE, NAME-FUNCTION, and HIGHLIGHT-REGEXP are as in
`compilation-start'.

This is an advice function around `compilation-start'."
  (message "smart-compilation/compilation-start: command is: %s" command)
  (let ((matching-command-name
         (smart-compilation/command-begins-with command (smart-compilation/proglist))))
    (if matching-command-name
        (let* ((matching-command-name-downcase (downcase matching-command-name))
               (project-filename (cdr (assoc matching-command-name-downcase smart-compilation/command-project-file-alist))))
          (let ((project-root (smart-compilation/find-project-root project-filename)))
            (if project-root
                (let ((new-command (concat "true && " ;work around compilation-start's handling of "cd"
                                           "cd "
                                           (shell-quote-argument (file-name-directory project-root))
                                           " && "
                                           command)))
                  (progn
                    (message "smart-compilation/compilation-start: new command is: %s" new-command)
                    (apply orig-fun new-command mode name-function highlight-regexp)))
              (apply orig-fun command mode name-function highlight-regexp))))
      (apply orig-fun command mode name-function highlight-regexp))))

;;;###autoload
(defun smart-compilation-enable ()
  "Enable smart-compilation."
  (advice-add 'compilation-start :around #'smart-compilation/compilation-start))

;;;###autoload
(defun smart-compilation-disable ()
  "Disable smart-compilation."
  (advice-remove 'compilation-start #'smart-compilation/compilation-start))

(provide 'smart-compilation)
;;; smart-compilation.el ends here

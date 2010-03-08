;;; lgit.el --- conveniently call git from elisp libraries

;; Copyright (C) 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100308
;; Updated: 20100308
;; Version: 0.1
;; Homepage: https://github.com/tarsius/lgit
;; Keywords: git

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for conveniently calling git from elisp libraries.

;; Several solutions exist that provide an interface to git from within
;; Emacs.  However if you want to use the low-level functions provided by
;; these libraries in your own code as opposed to using their front-end
;; commands to interact with git as a user (as they are intended to) this
;; results in quite verbose incantations.

;; This library does not try to do everything for everyone - instead it
;; just provides the basic functionality that I need, in several of my
;; libraries.  Creating this library allowed me to remove the almost
;; identical git functions from all of them.  Since the libary name
;; `egit' was already taken this library is called `lgit' instead.
;; Incidentally this also emphasizes the fact that this library is
;; intended to be used by other libraries, not as a front-end.

;;; Code:

(defvar lgit-log nil
  "Whether to log output of all git commands.

You should only use this to debug errors, alternatively you can also log
only specific calls to `lgit' and `lgit*' by setting the LOG argument.
In both cases output goes to buffer \"*lgit-log*\".")

(defun lgit* (&rest args)
  "Execute a git command in `default-directory'.

Return git's exit status.  It is an error if git's exit status is greater
than OKSTATUS or if that is not specified 0.  OKSTATUS has to be an
integer or be omitted.

CMDARGS is applied to CMDFORMAT using `format' to get the command line.

Output goes to the current buffer and is logged to buffer \"*lgit-log*\" if
LOG is t or if `lgit-log' is non-nil.  LOG has to be t or be omitted.

\(fn [OKSTATUS] [LOG] CMDFORMAT [CMDARGS...])"
  (let* ((okstatus (if (integerp (car args))
		       (pop args)
		     0))
	 (log (when (eq (car args) t)
		(pop args)))
	 (cmdline (concat "git " (apply #'format args)))
	 (exit (call-process shell-file-name nil t nil
			     shell-command-switch
			     cmdline)))
    (when (or log lgit-log)
      (let ((output (buffer-string)))
	(with-current-buffer (get-buffer-create "*lgit-log*")
	  (insert (format "\n$ %s\n" cmdline))
	  (insert output))))
    (if (<= exit okstatus)
	exit
      (pop-to-buffer (current-buffer))
      (error "Failed (%s): %s" exit cmdline))))

(defun lgit (&rest args)
  "Execute a git command inside the repository REPO.

Return git's output as a list of lines.  If OKSTATUS is specified git's
return value is consed onto the return value.  Also see `lgit*' which is
used by this function.

\(fn REPO [OKSTATUS] [LOG] CMDFORMAT [CMDARGS...])"
  (with-temp-buffer
    (let* ((default-directory (pop args))
	   (ret (apply 'lgit* args))
	   (output (split-string (buffer-string) "\n" t)))
      (if (integerp (car args))
	  (cons ret output)
	output))))

(defmacro lgit-with-file (repo rev file &rest body)
  "Execute the forms in BODY with FILE temporarly current.

REPO is the path to a git repository and REV has to be an existing
revision in that repository.  FILE is the path to the file relative
to the repositories root which has to exist in REV.

`buffer-file-name' is set to the basename of FILE while the forms in BODY
are evaluated.  The value returned is the value of the last form in BODY."
  (declare (indent 3))
  (let ((filesym (gensym "file"))
	(revsym (gensym "rev")))
    `(let ((default-directory ,repo)
	   (,filesym ,file)
	   (,revsym ,rev))
       (with-temp-buffer
	 (lgit* "show %s:%s" ,revsym ,filesym)
	 (let ((buffer-file-name (file-name-nondirectory ,filesym)))
	   (with-syntax-table emacs-lisp-mode-syntax-table
	     ,@body))))))

(provide 'lgit)
;;; lgit.el ends here

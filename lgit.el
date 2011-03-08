;;; lgit.el --- conveniently call git from elisp libraries

;; Copyright (C) 2010-2011  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100308
;; Updated: 20110308
;; Version: 0.4
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

;; Conveniently call Git from Emacs lisp.

;; This is the final release - coincidentally exactly one year after the
;; first commit - before deprecating this package in favor of `magit',
;; which you should be using instead.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'arc-mode)

(defvar lgit-log nil
  "Whether to log output of all git commands.

You should only use this to debug errors, alternatively you can also log
only specific calls to `lgit' and `lgit*' by setting the LOG argument.
In both cases output goes to buffer \"*lgit-log*\".")

(put 'lgit 'error-conditions '(error lgit))
(put 'lgit 'error-message "Lgit Error")

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
      (signal 'lgit (format "%s [code=%s, path=%s]"
			    cmdline exit default-directory)))))

(defun lgit (repo &rest args)
  "Execute a git command inside the repository REPO.

Return git's output as a list of lines.  If OKSTATUS is specified git's
return value is consed onto the return value.  Also see `lgit*' which is
used by this function.

\(fn REPO [OKSTATUS] [LOG] CMDFORMAT [CMDARGS...])"
  (with-temp-buffer
    (let* ((default-directory repo)
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

`buffer-file-name' is set to FILE while the forms in BODY are evaluated.
The value returned is the value of the last form in BODY."
  (declare (indent 3))
  (let ((filesym (gensym "file"))
	(revsym (gensym "rev")))
    `(let ((default-directory ,repo)
	   (,filesym ,file)
	   (,revsym ,rev))
       (with-temp-buffer
	 (lgit* "show %s:%s" ,revsym ,filesym)
	 (archive-set-buffer-as-visiting-file ,filesym)
	 (setq buffer-file-name ,filesym)
	 ,@body))))

(defun lgit-bare-repo-p (repo)
  "Return t if REPO is a bare repository."
  (when (equal (car (lgit repo "config --bool core.bare")) "true") t))

(defun lgit-get (repo key &optional allp)
  "Extract KEY's value(s) from the config file of REPO.
Return the extracted value or nil if KEY is not set.  If KEY could have
multiple values raise an error unless optional ALLP is non-nil.  In this
case return a list of KEY's values (even if it has only one value)."
  (funcall (if allp 'identity 'car)
	   (cdr (lgit repo 1 (concat "config "
				     (when allp "--get-all ")
				     key)))))

(defun lgit-get-regexp (repo regexp)
  "Return an alist of all keys that match REGEXP from the config file of REPO.
Each entry in the returned list has the form (KEY . VALUE)."
  (mapcar (lambda (line)
	    (string-match "^[^ ]+" line)
	    (cons (substring line 0   (match-end 0))
		  (substring line (1+ (match-end 0)))))
	  (cdr (lgit repo 1 "config --get-regexp %s" regexp))))

(defun lgit-branch-get (repo branch key &optional allp)
  "Extract a key's value(s) from the config file of REPO.
The actual key whose value is returned is \"branch.BRANCH.KEY\".
See `lgit-get' for more details on the return value and ALLP."
  (lgit-get repo (format "branch.%s.%s" branch key) allp))

(defun lgit-set (repo key value &optional addp)
  "Change the value of KEY in the config file of REPO to VALUE.
If optional ADDP is non-nil add a new value to KEY without altering
existing values instead."
  (lgit repo (concat "config " (when addp "--add ") key " " value)))

(defun lgit-branch-set (repo branch key value &optional addp)
  "Change the value of a key in the config file of REPO to VALUE.
The actual key whose value is set is \"branch.BRANCH.KEY\".
If optional ADDP is non-nil add a new value to KEY without altering
existing values."
  (lgit-set repo (format "branch.%s.%s" branch key) value addp))

(provide 'lgit)
;;; lgit.el ends here

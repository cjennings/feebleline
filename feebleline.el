;;; feebleline.el --- Replace modeline with a slimmer proxy -*- lexical-binding: t; -*-

;; Copyright 2018 Benjamin Lindqvist

;; Author: Benjamin Lindqvist <benjamin.lindqvist@gmail.com>
;; Maintainer: Benjamin Lindqvist <benjamin.lindqvist@gmail.com>

;; Contributions: Craig Jennings <c@cjennings.net>

;; URL: https://github.com/tautologyclub/feebleline
;; Fork URL: https://github.com/cjennings/feebleline

;; Package-Version: 2.0
;; Package-Requires: ((emacs "25.1"))
;; Version: 2.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Feebleline removes the modeline and replaces it with a slimmer proxy
;; version, which displays some basic information in the echo area
;; instead.  This information is only displayed if the echo area is not used
;; for anything else (but if you switch frame/window, it will replace whatever
;; message is currently displayed).

;; The elements should be functions, accepting no arguments, returning either
;; nil or a valid string. Even lambda functions work (but don't forget to quote
;; them). Optionally, you can include keywords after each function, like so:

;; (feebleline-line-number :post "" :fmt "%5s")

;; Accepted keys are pre, post, face, fmt and align.
;; See source code for inspiration.

;;; Code:
(require 'cl-lib)
(require 'subr-x)

;; tell byte-compiler this is a valid function defined at runtime
(declare-function tramp-tramp-file-p "tramp")

;; ------------------------------ Custom Variables -----------------------------

(defgroup feebleline nil
  "Feebleline customizations."
  :prefix "feebleline-"
  :group 'convenience)

(defcustom feebleline-timer-interval 0.5
  "Refresh interval of feebleline mode-line proxy."
  :type  'float
  :group 'feebleline)

;; ----------------------------- Internal Variables ----------------------------

(defvar feebleline--home-dir nil
  "The user's home directory.
This variable is used to abbreviate file paths in feebleline messages.
Set during feebleline-mode activation.")

(defvar-local feebleline--msg-timer nil
  "Timer object for mode line updates in the current buffer.")

(defvar-local feebleline--original-mode-line-format nil
  "Backup storage for the previous mode line format in the current buffer.")

(defvar feebleline--original-window-divider-setting nil
  "Previous window divider setting before feebleline mode was enabled.
This variable is used to restore the old setting when feebleline mode is
disabled.")

(defvar feebleline--last-error-shown nil
  "The last error that was displayed by feebleline mode.
This variable is used to prevent the same error from being displayed
repeatedly.")

(defvar feebleline--minibuf " *Minibuf-0*"
  "Buffer name used by feebleline for displaying messages.
This buffer is primarily used to overwrite the echo area.")

;; -------------------------- Line And Column Element --------------------------

(defface feebleline-line-column-face
  '((t :inherit 'default :foreground "#E37464"))
  "The default face used to display version control project names and branches."
  :group 'feebleline)

(defun feebleline-line-column-info ()
  "Return a formatted string displaying the current line and column number.
The line number is formatted to occupy 5 spaces, and the
column number exactly 3 spaces, separated by a colon.
For example, the string for line 120 and column 15 would be '  120:15 '."
  (format "%5s:%-3s" (format "%s" (line-number-at-pos)) (current-column)))

;; ----------------- Buffer Name Or File Name With Path Element ----------------

(defface feebleline-dir-name-face
  '((t :inherit 'default :foreground "#5E4D37"))
  "The default face used to display directory paths."
  :group 'feebleline)

(defface feebleline-buffer-name-face
  '((t :inherit 'default :foreground "#2B61B6"))
  "The default face used to display file and buffer names."
  :group 'feebleline)

(defun feebleline-file-directory ()
  "Return the directory if buffer is displaying a file."
  (when (buffer-file-name)
	(replace-regexp-in-string
	 (concat "^" feebleline--home-dir) "~"
	 default-directory)))

(defun feebleline-file-or-buffer-name ()
  "Return the current file name, or the buffer name if not a file."
  (if (buffer-file-name)
	  (file-name-nondirectory (buffer-file-name))
	(buffer-name)))

(defun feebleline-buffer-modified-star ()
  "Return a star if buffer was modified."
  (when (and (buffer-file-name) (buffer-modified-p)) "*"))

;; --------------------------- VC Information Element --------------------------

(defface feebleline-vc-info-face
  '((t :inherit 'default :foreground "#94E151"))
  "The default face used to display version control project names and branches."
  :group 'feebleline)

(defun feebleline-vc-project-name ()
  "Return project name if exists."
  (when-let ((proj (project-current)))
	(file-name-nondirectory
	 (directory-file-name (cdr proj)))))

(defun feebleline-vc-branch ()
  "Return current version control branch prefixed with the branch symbol.
If file is remote or not in a repo, show nothing. Currently works with git only."
  (if-let ((fname (buffer-file-name))
		   (branch
			(and fname
				 (not (file-remote-p fname))
				 (not (tramp-tramp-file-p fname))
				 (vc-backend fname)
				 (vc-state fname)
				 (car (vc-git-branches)))))
	  (string-trim (concat "" branch))
	""))

(defun feebleline-vc-info ()
  "Return the concatenation of the project name and the git branch.
When the project name doesn't exist, return nil."
  (let ((project-name (feebleline-vc-project-name))
		(branch (feebleline-vc-branch)))
	(if project-name (concat project-name " " branch)
	  nil)))

;; -------------------------------- Element List -------------------------------

(defcustom feebleline-element-list
  '(
	(feebleline-vc-info              :face feebleline-vc-info-face :align right)
	(feebleline-file-directory       :face feebleline-dir-name-face :post "")
	;; the message to invalid face reference is with file-or-buffer-name
	;; here in the next line when it tries to apply to file or buffer name
	(feebleline-file-or-buffer-name  :post "")
	(feebleline-buffer-modified-star :face feebleline-buffer-name-face)
	(feebleline-line-column-info     :face feebleline-line-column-face)
	)
  "List of functions or elements to display in the echo area.
Each element is a function giving a string to display directly or a list where:
- The first element is the function.
- The other elements are keyword-value pairs for advanced formatting options.

Available keywords are:
- :pre, an optional string to insert before the function output.
- :post, an optional string to insert after the function output.
- :fmt, a format string, defaults to \"%s\".
- :face, a optional face to apply to the whole string.
- :align, an optional symbol specifying alignment (\='left or \='right).
Example:
:align \='right will align the output string to the right of the echo area.

Note: an empty space is automatically inserted after each element. To avoid
this, the element should end with :post \"\""

  :type  '(repeat sexp)
  :group 'feebleline)

;; ---------------------------- Echo Area Insertion ----------------------------

(defmacro feebleline-append-msg-function (&rest b)
  "Macro for adding B to the feebleline mode-line, at the end."
  `(add-to-list 'feebleline-element-list ,@b t (lambda (x y) nil)))

(defmacro feebleline-prepend-msg-function (&rest b)
  "Macro for adding B to the feebleline mode-line, at the beginning."
  `(add-to-list 'feebleline-element-list ,@b nil (lambda (x y) nil)))

(defun feebleline--insert-ignore-errors ()
  "Insert stuff into the echo area, ignoring potential errors."
  (unless (current-message)
	(condition-case err (feebleline--insert)
	  (error (unless (equal feebleline--last-error-shown err)
			   (setq feebleline--last-error-shown err)
			   (message (format "feebleline error: %s" err)))))))

(defun feebleline--force-insert ()
  "Insert stuff into the echo area even if it's displaying something."
  (condition-case nil (feebleline--clear-echo-area)
	(error nil)))

(cl-defun feebleline--insert-func (func &key (face 'default) pre (post " ")
										(fmt "%s") (align 'left))
  "Format an element of \='feebleline-element-list\=' based on its properties.
- FUNC is the function used to generate the message.
- FACE is an optional face to apply to the whole string, defaults to \='default.
- PRE is an optional string to insert before the function output.
- POST is an optional string to insert after the function output,
  defaults to a single space.
- FMT is a format string, defaults to \"%s\".
- ALIGN is an optional symbol specifying alignment (\='left or \='right),
  defaults to \='left.

Returns a pair with align setting and the resulting string."
  (list align
		(let* ((msg (apply func nil))
			   (string (concat pre (format fmt msg) post)))
		  (if msg
			  (if face
				  (propertize string 'face face)
				string)
			""))))

(defun feebleline--insert ()
  "Insert stuff into the mini buffer."
  (unless (current-message)
	(let ((left ())
		  (right ()))
	  (dolist (idx feebleline-element-list)
		(let* ((fragment (apply 'feebleline--insert-func idx))
			   (align (car fragment))
			   (string (cadr fragment)))
		  (cond
		   ((eq align 'left)
			(push string left))
		   ((eq align 'right)
			(push string right))
		   (t
			(push string left))))) ; default to left if not specified
	  (with-current-buffer feebleline--minibuf
		(erase-buffer)
		(let* ((left-string (string-join (reverse left)))
			   (message-truncate-lines t)
			   (max-mini-window-height 1)
			   (right-string (string-join (reverse right)))
			   (free-space (- (frame-width)
							  (length left-string) (length right-string)))
			   (padding (make-string (max 0 free-space) ?\ )))
		  (insert (concat left-string
						  (if right-string (concat padding
												   right-string)))))))))

(defun feebleline--clear-echo-area ()
  "Erase the echo area."
  (with-current-buffer feebleline--minibuf
	(erase-buffer)))

;; ------------------------------ Feebleline Mode ------------------------------

;;;###autoload
(define-minor-mode feebleline-mode
  "Replace modeline with a slimmer proxy."
  :require 'feebleline
  :global t
  (if feebleline-mode
	  ;; Activation:
	  (progn
		(setq feebleline--home-dir (expand-file-name "~"))
		(setq feebleline--original-mode-line-format mode-line-format)
		(setq feebleline--msg-timer
			  (run-with-timer 0 feebleline-timer-interval
							  'feebleline--insert-ignore-errors))
		(feebleline-appearance-settings-on)
		(add-function :after after-focus-change-function
					  'feebleline--insert-ignore-errors))
	;; Deactivation:
	(window-divider-mode feebleline--original-window-divider-setting)
	(set-face-attribute 'mode-line nil :height 1.0)
	(setq-default mode-line-format feebleline--original-mode-line-format)
	(walk-windows (lambda (window)
					(with-selected-window window
					  (setq mode-line-format
							feebleline--original-mode-line-format)))
				  nil t)
	(cancel-timer feebleline--msg-timer)
	(remove-function after-focus-change-function
					 'feebleline--insert-ignore-errors)

	(force-mode-line-update)
	(redraw-display)
	(feebleline--clear-echo-area)))

;; ----------------------- Feebleline Appearance Settings ----------------------

(defun feebleline-appearance-settings-on ()
  "The appearance settings for feebleline.
Set window divider to 1 pixel and place at bottom. Save original settings to be
restored once feebleline mode is deactivated."
  (setq window-divider-default-bottom-width 1
		window-divider-default-places (quote bottom-only))
  (setq feebleline--original-window-divider-setting window-divider-mode)
  (window-divider-mode 1)
  (setq-default mode-line-format nil)
  (walk-windows (lambda (window)
				  (with-selected-window window
					(setq mode-line-format nil)))
				nil t))

;; ---------------------------- Disable And Reenable ---------------------------

(defun feebleline-disable ()
  "Disable \='feebleline-mode\='.
This is meant to be used in a hook, before issuing a conflicting command."
  (when feebleline-mode
	(cancel-timer feebleline--msg-timer)
	(remove-function after-focus-change-function
					 'feebleline--insert-ignore-errors)))

(defun feebleline-reenable ()
  "Re-enable \='feebleline-mode\='.
This is meant to be used in a hook, after issuing a conflicting command."
  (when feebleline-mode
	(setq feebleline--msg-timer
		  (run-with-timer 0 feebleline-timer-interval
						  'feebleline--insert-ignore-errors))
	(add-function :after after-focus-change-function
				  'feebleline--insert-ignore-errors)))

(provide 'feebleline)
;;; feebleline.el ends here

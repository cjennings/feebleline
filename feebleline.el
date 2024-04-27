;;; feebleline.el --- Replace modeline with a slimmer proxy -*- lexical-binding: t; -*-

;; Copyright 2018 Benjamin Lindqvist

;; Author: Benjamin Lindqvist <benjamin.lindqvist@gmail.com>
;; Maintainer: Benjamin Lindqvist <benjamin.lindqvist@gmail.com>
;; URL: https://github.com/tautologyclub/feebleline
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

;; Feebleline now has a much improved customization interface. Simply set
;; feebleline-msg-functions to whatever you want! Example:

;; (setq
;;  feebleline-msg-functions
;;  '((feebleline-line-number)
;;    (feebleline-column-number)
;;    (feebleline-file-directory)
;;    (feebleline-file-or-buffer-name)
;;    (feebleline-file-modified-star)
;;    (magit-get-current-branch)
;;    (projectile-project-name)))

;; The elements should be functions, accepting no arguments, returning either
;; nil or a valid string. Even lambda functions work (but don't forget to quote
;; them). Optionally, you can include keywords  after each function, like so:

;; (feebleline-line-number :post "" :fmt "%5s")

;; Accepted keys are pre, post, face, fmt and align.
;; See source code for inspiration.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(autoload 'magit-get-current-branch "magit")

;; tell byte-compiler this is a valid function defined at runtime
(declare-function tramp-tramp-file-p "tramp")

(defcustom feebleline-timer-interval 0.1
  "Refresh interval of feebleline mode-line proxy."
  :type  'float
  :group 'feebleline)

(defcustom feebleline-use-legacy-settings nil
  "Hacky settings only applicable to releases older than 25."
  :type  'boolean
  :group 'feebleline)

(defface feebleline-file-or-buffer-face
  '((t :inherit default :foreground "#2EDAFF"))
  "The default custom face for feebleline."
  :group 'feebleline)

(defface feebleline-git-face
  '((t :inherit default :foreground "#e6b400"))
  "Example face for git branch."
  :group 'feebleline)

(defface feebleline-dir-face
  '((t :inherit 'font-lock-variable-name-face))
  "Example face for dir face."
  :group 'feebleline)

(defvar feebleline--home-dir nil
  "The user's home directory, stored as an absolute file name.
This variable is used to abbreviate file paths in feebleline messages.")

(defvar-local feebleline--msg-timer nil
  "Timer object for mode line updates in the current buffer.
This variable is buffer-local.")

(defvar-local feebleline--mode-line-format-previous nil
  "Backup storage for the previous mode line format in the current buffer.
This variable is buffer-local.")

(defvar feebleline--window-divider-previous nil
  "Previous window divider setting before feebleline mode was enabled.
This variable is used to restore the old setting when feebleline mode is
disabled.")

(defvar feebleline-last-error-shown nil
  "The last error that was displayed by feebleline mode.
This variable is used to prevent the same error from being displayed
repeatedly.")

(defun feebleline-git-branch ()
  "Return current git branch, unless file is remote."
  (if (or (null (buffer-file-name))
          (file-remote-p (buffer-file-name))
          (tramp-tramp-file-p (buffer-file-name)))
      ""
    (let ((branch (shell-command-to-string
                   "git rev-parse --symbolic-full-name --abbrev-ref HEAD 2>/dev/null")))
      (string-trim (replace-regexp-in-string
                    "^HEAD" "(detached HEAD)"
					branch)))))

(defcustom feebleline-msg-functions
  '((feebleline-line-number         :post "" :fmt "%5s")
    (feebleline-column-number       :pre ":" :fmt "%-2s")
    (feebleline-file-directory      :face feebleline-dir-face :post "")
    (feebleline-file-or-buffer-name :face feebleline-file-or-buffer-face :post "")
    (feebleline-file-modified-star  :face feebleline-file-or-buffer-face :post "")
    (feebleline-git-branch          :face feebleline-git-face :pre " - ")
    ;; (feebleline-project-name        :align right)
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

:align \='right will align the output string to the right of the echo area."
  :type  '(repeat sexp)
  :group 'feebleline)

(defun feebleline-linecol-string ()
  "Return a formatted string displaying the current line and column number.
The line number is formatted to occupy exactly 4 spaces, and the
column number exactly 2 spaces, separated by a colon.
For example, the string for line 120 and column 15 would be ' 120:15'."
  (format "%4s:%-2s" (format-mode-line "%l") (current-column)))

(defun feebleline-previous-buffer-name ()
  "Get name of previous buffer."
  (buffer-name (other-buffer (current-buffer) 1)))

(defun feebleline-line-number ()
  "Line number as string."
  (format "%s" (line-number-at-pos)))

(defun feebleline-column-number ()
  "Column number as string."
  (format "%s" (current-column)))

(defun feebleline-file-directory ()
  "Current directory, if buffer is displaying a file."
  (when (buffer-file-name)
    (replace-regexp-in-string
     (concat "^" feebleline--home-dir) "~"
     default-directory)))

(defun feebleline-file-or-buffer-name ()
  "Current file, or just buffer name if not a file."
  (if (buffer-file-name)
      (file-name-nondirectory (buffer-file-name))
    (buffer-name)))

(defun feebleline-file-modified-star ()
  "Display star if buffer file was modified."
  (when (and (buffer-file-name) (buffer-modified-p)) "*"))

(defun feebleline-project-name ()
  "Return project name if exists, otherwise nil."
  (when (cdr (project-current))
    (file-name-nondirectory (directory-file-name (cdr (project-current))))))

(defmacro feebleline-append-msg-function (&rest b)
  "Macro for adding B to the feebleline mode-line, at the end."
  `(add-to-list 'feebleline-msg-functions ,@b t (lambda (x y) nil)))

(defmacro feebleline-prepend-msg-function (&rest b)
  "Macro for adding B to the feebleline mode-line, at the beginning."
  `(add-to-list 'feebleline-msg-functions ,@b nil (lambda (x y) nil)))

(defun feebleline-default-settings-on ()
  "Some default settings that works well with feebleline."
  (setq window-divider-default-bottom-width 1
        window-divider-default-places (quote bottom-only))
  (setq feebleline--window-divider-previous window-divider-mode)
  (window-divider-mode 1)
  (setq-default mode-line-format nil)
  (walk-windows (lambda (window)
                  (with-selected-window window
                    (setq mode-line-format nil)))
                nil t))

(defun feebleline-legacy-settings-on ()
  "Some default settings for EMACS < 25."
  (set-face-attribute 'mode-line nil :height 0.1))

(defun feebleline--insert-ignore-errors ()
  "Insert stuff into the echo area, ignoring potential errors."
  (unless (current-message)
    (condition-case err (feebleline--insert)
      (error (unless (equal feebleline-last-error-shown err)
               (setq feebleline-last-error-shown err)
               (message (format "feebleline error: %s" err)))))))

(defun feebleline--force-insert ()
  "Insert stuff into the echo area even if it's displaying something."
  (condition-case nil (feebleline--clear-echo-area)
    (error nil)))

(defvar feebleline--minibuf " *Minibuf-0*"
  "Buffer name used by feebleline for displaying messages.
This buffer is primarily used to overwrite the echo area.")

(cl-defun feebleline--insert-func (func &key (face 'default) pre (post " ")
                                        (fmt "%s") (align 'left))
  "Format an element of \='feebleline-msg-functions\=' based on its properties.
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
      (dolist (idx feebleline-msg-functions)
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
               (free-space (- (frame-width) (length left-string) (length right-string)))
               (padding (make-string (max 0 free-space) ?\ )))
          (insert (concat left-string (if right-string (concat padding right-string)))))))))

(defun feebleline--clear-echo-area ()
  "Erase echo area."
  (with-current-buffer feebleline--minibuf
	(erase-buffer)))

(defgroup feebleline nil
  "Feebleline customizations."
  :prefix "feebleline-"
  :group 'convenience)

;;;###autoload
(define-minor-mode feebleline-mode
  "Replace modeline with a slimmer proxy."
  :require 'feebleline
  :global t
  (if feebleline-mode
	  ;; Activation:
	  (progn
		(setq feebleline--home-dir (expand-file-name "~"))
		(setq feebleline--mode-line-format-previous mode-line-format)
		(setq feebleline--msg-timer
			  (run-with-timer 0 feebleline-timer-interval
							  'feebleline--insert-ignore-errors))
		(if feebleline-use-legacy-settings (feebleline-legacy-settings-on)
		  (feebleline-default-settings-on))
		(add-function :after after-focus-change-function 'feebleline--insert-ignore-errors))
	;; Deactivation:
	(window-divider-mode feebleline--window-divider-previous)
	(set-face-attribute 'mode-line nil :height 1.0)
	(setq-default mode-line-format feebleline--mode-line-format-previous)
	(walk-windows (lambda (window)
					(with-selected-window window
					  (setq mode-line-format feebleline--mode-line-format-previous)))
				  nil t)
    (cancel-timer feebleline--msg-timer)
	(remove-function after-focus-change-function 'feebleline--insert-ignore-errors)
	(force-mode-line-update)
	(redraw-display)
	(feebleline--clear-echo-area)))

(defun feebleline-disable ()
  "Neutralise \='feebleline-mode\='.
This is meant to be used in a hook, before a conflictual command."
  (when feebleline-mode
	(cancel-timer feebleline--msg-timer)
	(remove-function after-focus-change-function 'feebleline--insert-ignore-errors)))

(defun feebleline-reenable ()
  "Re-enable \='feebleline-mode\='.
This is meant to be used in a hook, after a conflictual command."
  (when feebleline-mode
	(setq feebleline--msg-timer
		  (run-with-timer 0 feebleline-timer-interval
						  'feebleline--insert-ignore-errors))
	(add-function :after after-focus-change-function 'feebleline--insert-ignore-errors)))

(provide 'feebleline)
;;; feebleline.el ends here

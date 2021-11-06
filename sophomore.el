;;; sophomore.el --- Fine-tuned command disabling -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Case Duckworth <acdw@acdw.net>

;; Maintainer: Case Duckworth
;; Homepage: https://github.com/duckwork/sophomore.el
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; Package-Version: 0.1

;; This file is NOT part of GNU Emacs.

;; Everyone is permitted to do whatever with this software, without
;; limitation.  This software comes without any warranty whatsoever,
;; but with two pieces of advice:
;; - Be kind to yourself.
;; - Make good choices.

;;; Commentary:

;; While novice.el is fine, but most people disable its disabling of commands at
;; some point.  While I can't speak for everyone's motives, I know a big one of
;; mine is the clunky UI of the default `disabled-command-function'.

;; sophomore.el attempts to add a better UI around disabled commands that's
;; better-suited to more experienced users.  Of course, my attempt might come
;; off as only a little better than novice, or even, dare I say, "sophomoric" in
;; nature.

;; This package trades in the knowledge that a command's disabled property need
;; not be a binary choice between "yes" or "no", "t" or "nil": indeed, there are
;; a number of ways in which a command might be considered "disabled," with
;; different safeguards against using it.  For intermediate and more advanced
;; users, a common issue is "fat-fingering" commands like `set-fill-column' when
;; `find-file' was meant, or accidentally killing Emacs when typing C-x too
;; quickly.  In addition, in my own usage I don't want to /disable/ a command
;; per se, but disallow too-easy invocation (which isn't possible with the
;; built-in novice.el except by unbinding or rebinding a number of keys).

;;; Code:

(require 'novice)

;;; Customization

(defgroup sophomore nil
  "A better interface for disabled commands."
  :prefix "sophomore-"
  :group 'convenience)

(defcustom sophomore-dispatch-alist '()
  "Alist of possible disabled property symbols and the functions to call.
In an entry (SYMBOL . FUNCTION), the symbol can be arbitrary; if
a command's disabled property is not found in
`sophomore-dispatch-alist',
`sophomore-dispatch-fallback-function' will be called instead.

Each entry's FUNCTION, however, will be passed two parameters: a
command and the keys used to call that command."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom sophomore-dispatch-fallback-function 'sophomore-disabled-M-x
  "What to do if a disabled property isn't in `sophomore-dispatch-alist'.
To emulate Emacs's default behavior, use the function
`disabled-command-function'."
  :type 'function)

;;; Utility functions

(defun sophomore-disable-with (prop &rest commands)
  "Disable COMMANDS by setting their disabled property to PROP."
  (mapc (lambda (c) (put command 'disabled prop)) commands))

;;;###autoload
(defun sophomore-enable-all ()
  "Enable all disabled commands."
  (mapatoms (lambda (symbol)
              (when (get symbol 'disabled)
                (put symbol 'disabled nil)))))

;;;###autoload
(defun sophomore-disable (&rest commands)
  "Disable COMMANDS by setting their disabled properties to t.
This is a drop-in replacement for `disable-command', but it
doesn't write to `user-emacs-file'."
  (apply 'sophomore-disable-with t commands))

;;;###autoload
(defun sophomore-enable (&rest commands)
  "Enable COMMANDS by setting their disabled properties to nil.
This is a drop-in replacement for `enable-command', but it
doesn't write to `user-emacs-file'."
  (apply 'sophomore-disable-with nil commands))

;;; Disabled command dispatcher

;;;###autoload
(defun sophomore-dispatch (&optional cmd keys)
  "If CMD is disabled, figure out what to do based on its disabled property.
CMD defaults to `this-command', and KEYS default to
`this-command-keys'.  CMD and KEYS are passed to a function in
`sophomore-dispatch-alist' depending on CMD's disabled property.
If the property isn't in `sophomore-dispatch-alist',
`sophomore-dispatch-fallback-function' is called with the same
parameters.  Dispatching is only done if the disabled property is
non-nil."
  (let* ((cmd (or cmd this-command))
         (keys (or keys (this-command-keys)))
         (disabled (get cmd 'disabled)))
    (when disabled
      (funcall (or (alist-get disabled sophomore-dispatch-alist)
                   sophomore-dispatch-fallback-function)
               cmd keys))))

;;; Functions for `sophomore-dispatch'

(defun sophomore-extended-command-p (keys)
  "Inspect a the KEYS that called a command for `execute-extended-command'."
  ;;  This logic is stolen from the original `disabled-command-function'.  I
  ;; don't like how "M-x" is hardcoded into the function here, but I can't
  ;; figure out how to generalize it.
  (or (eq (aref keys 0) (if (stringp keys)
                                  (aref "\M-x" 0)
                                ?\M-x))
            (and (>= (length keys) 2)
                 (eq (aref keys 0) meta-prefix-char)
                 (eq (aref keys 1) ?x))))

;;;###autoload
(defun sophomore-disabled-M-x (cmd keys)
  "Disable CMD, unless KEYS begins with \"M-x\"."
  (if (sophomore-extended-command-p keys)
      (call-interactively cmd)
    (message (substitute-command-keys
              (concat "Command `%s' has been disabled.  "
                      "Run with \\[execute-extended-command]."))
             cmd)))

;;;###autoload
(defun sophomore-fat-finger (cmd keys)
  "Prompt a user before executing CMD with KEYS not beginning with \"M-x\"."
  (when (or (sophomore-extended-command-p keys)
            (yes-or-no-p
             (format "Are you sure you want to execute `%s'? " cmd)))
      (call-interactively cmd)))

(provide 'sophomore)
;;; sophomore.el ends here

;;; easy-kill.el --- kill things easily              -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.5.0
;; Keywords: convenience
;; Created: 2013-08-12

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

;; Kill things easily in emacs.
;; To use: (global-set-key "\M-w" 'easy-kill)

;;; Code:

(eval-when-compile (require 'cl))
(require 'thingatpt)

(defcustom easy-kill-alist
  '((?w . word)
    (?s . sexp)
    (?l . list)
    (?f . filename)
    (?d . defun))
  "A list of (Key . THING)."
  :type '(repeat (cons character symbol))
  :group 'killing)

(defun easy-kill-message-nolog (format-string &rest args)
  "Same as `message' except not writing to *Messages* buffer."
  (let (message-log-max)
    (apply 'message format-string args)))

(defvar easy-kill-candidate nil)

(defun easy-kill-remember (str)
  (when str
    (setq easy-kill-candidate str)
    ;; Immediately put it in clipboard for other applications.
    (and interprogram-cut-function
         (funcall interprogram-cut-function str))
    (easy-kill-message-nolog "%s" str)))

(defun easy-kill-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "-" 'easy-kill-negative-argument)
    (mapc (lambda (d)
            (define-key map (number-to-string d) 'easy-kill-digit-argument))
          (number-sequence 0 9))
    (mapc (lambda (c)
            ;; (define-key map (vector meta-prefix-char c) 'easy-kill-select)
            (define-key map (char-to-string c) 'easy-kill-select))
          (mapcar 'car easy-kill-alist))
    map))

;;; `digit-argument' is incompatible with `set-temporary-overlay-map'
;;; becauses the former overrides the latter's keymap.

(defun easy-kill-digit-argument (arg)
  (interactive "P")
  (digit-argument arg)
  ;; Counter the effect of (save&set-overriding-map universal-argument-map).
  (restore-overriding-map))

(defun easy-kill-negative-argument (arg)
  (interactive "P")
  (negative-argument arg)
  (restore-overriding-map))

(defun easy-kill-bounds (thing &optional n)
  "Like `bounds-of-thing-at-point' but allow upto N things.
Return nil if no THING at point."
  (or n (setq n 1))
  (let* ((bounds (bounds-of-thing-at-point thing))
         (beg (car bounds))
         (end (cdr bounds))
         (count 0)
         (step (if (minusp n) -1 1)))
    (when bounds
      (save-excursion
        (if (minusp n)
            (goto-char end)
          (goto-char beg))
        (while (ignore-errors (forward-thing thing step)
                              (incf count)
                              (if (< count (abs n)) t nil)))
        ;; Don't update if point is located between BEG and END.
        (unless (and (<= (point) end) (<= beg (point)))
          (if (minusp n)
              (setq beg (point))
            (setq end (point)))))
      (cons beg end))))

(defun easy-kill-select (&optional n)
  (interactive "p")
  (let ((thing (cdr (assoc (car (last (listify-key-sequence
                                       (single-key-description last-command-event))))
                           easy-kill-alist)))
        bounds)
    (if (not (setq bounds (easy-kill-bounds thing n)))
        (easy-kill-message-nolog "No `%s' at point." thing)
      (easy-kill-remember (buffer-substring (car bounds) (cdr bounds))))))

(defun easy-kill-url-at-point ()
  "Get the url at point.
It inspects char properties `help-echo', `shr-url' and
`w3m-href-anchor'."
  (if (bounds-of-thing-at-point 'url)
      (thing-at-point 'url)
    (loop for prop in '(help-echo shr-url w3m-href-anchor)
          for data = (get-char-property-and-overlay (point) prop)
          for text = (car data)
          for overlay = (cdr data)
          when (and (stringp text) (with-temp-buffer
                                     (insert text)
                                     (easy-kill-url-at-point)))
          return it
          when (and overlay (overlay-get overlay prop))
          when (and (stringp it) (with-temp-buffer
                                   (insert it)
                                   (easy-kill-url-at-point)))
          return it)))

(defun easy-kill-guess (n)
  (or (and (use-region-p)
           (buffer-substring (region-beginning) (region-end)))
      (easy-kill-url-at-point)
      (save-restriction
        ;; Note (bounds-of-thing-at-point 'email) takes time
        ;; proportional to buffer size, so narrow buffer for
        ;; efficiency.
        (narrow-to-region (line-beginning-position (- (abs n)))
                          (line-end-position (abs n)))
        (loop for thing in '(email line)
              when (easy-kill-bounds thing n)
              return (buffer-substring (car it) (cdr it))))))

(defun easy-kill-activate-keymap ()
  (let ((map (easy-kill-map)))
    (set-temporary-overlay-map
     map
     (lambda ()
       ;; When any error happens the keymap is active forever.
       (with-demoted-errors
         (or (let ((cmd (lookup-key map (this-command-keys))))
               (eq this-command
                   (if (and (numberp cmd)
                            (> (length (this-command-keys))
                               universal-argument-num-events))
                       (lookup-key map (substring (this-command-keys)
                                                  universal-argument-num-events))
                     cmd)))
             (when easy-kill-candidate
               ;; Do not modify the clipboard here because it will
               ;; intercept pasting from other programs and
               ;; `easy-kill-remember' already did the work.
               (let ((interprogram-cut-function nil)
                     (interprogram-paste-function nil))
                 (kill-new easy-kill-candidate))
               (setq easy-kill-candidate nil)
               nil)))))))

;;;###autoload
(defun easy-kill (&optional n)
  (interactive "p")
  (easy-kill-remember (easy-kill-guess n))
  (setq deactivate-mark t)
  (easy-kill-activate-keymap))

(provide 'easy-kill)
;;; easy-kill.el ends here

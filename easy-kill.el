;;; easy-kill.el --- kill things easily              -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.6.0
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

;; `easy-kill' aims to be a drop-in replacement for `kill-ring-save'.
;;
;; To use: (global-set-key "\M-w" 'easy-kill)

;;; Code:

(eval-when-compile (require 'cl))
(require 'thingatpt)

(defcustom easy-kill-alist
  '((?w . word)
    (?s . sexp)
    (?l . list)
    (?f . filename)
    (?d . defun)
    (?b . buffer-file-name))
  "A list of (Key . THING)."
  :type '(repeat (cons character symbol))
  :group 'killing)

(defun easy-kill-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "-" 'easy-kill-shrink)
    (define-key map "+" 'easy-kill-enlarge)
    (mapc (lambda (d)
            (define-key map (number-to-string d) 'easy-kill-digit-argument))
          (number-sequence 0 9))
    (mapc (lambda (c)
            ;; (define-key map (vector meta-prefix-char c) 'easy-kill-select)
            (define-key map (char-to-string c) 'easy-kill-thing))
          (mapcar 'car easy-kill-alist))
    map))

(defface easy-kill-face '((t (:inherit 'secondary-selection)))
  "Faced used to highlight kill candidate."
  :group 'killing)

(defun easy-kill-message-nolog (format-string &rest args)
  "Same as `message' except not writing to *Messages* buffer."
  (let (message-log-max)
    (apply 'message format-string args)))

(defun easy-kill-strip-trailing (s)
  (if (string-match "[ \t\f\r\n]*\\'" s)
      (substring s 0 (match-beginning 0))
    (error "`string-match' failed in `easy-kill-strip'")))

(defvar easy-kill-candidate nil)

(defun easy-kill-candidate ()
  (easy-kill-strip-trailing
   (if (/= (overlay-start easy-kill-candidate)
           (overlay-end easy-kill-candidate))
       (buffer-substring (overlay-start easy-kill-candidate)
                         (overlay-end easy-kill-candidate))
     (overlay-get easy-kill-candidate 'candidate))))

(defun easy-kill-select-text ()
  "Make current kill candidate available to other programs."
  (and interprogram-cut-function
       (funcall interprogram-cut-function (easy-kill-candidate))))

(defun easy-kill-enlarge (n)
  (interactive "p")
  (let ((thing (overlay-get easy-kill-candidate 'thing)))
    (when thing
      (if (get thing 'easy-kill-enlarge)
          (funcall (get thing 'easy-kill-enlarge) n)
        (let ((direction (if (minusp n) -1 +1))
              (start (overlay-start easy-kill-candidate))
              (end (overlay-end easy-kill-candidate)))
          (when thing
            (save-excursion
              (goto-char end)
              (with-demoted-errors
                (dotimes (_ (abs n))
                  (forward-thing thing direction)
                  (when (<= (point) start)
                    (forward-thing thing 1)
                    (return))))
              (when (/= end (point))
                (move-overlay easy-kill-candidate start (point))
                (easy-kill-select-text)
                t))))))))

(defun easy-kill-shrink (n)
  (interactive "p")
  (easy-kill-enlarge (- n)))

(defun easy-kill-digit-argument (&optional n)
  (interactive
   (list (- (logand (if (integerp last-command-event)
                        last-command-event
                      (get last-command-event 'ascii-character))
                    ?\177)
            ?0)))
  (easy-kill-thing (overlay-get easy-kill-candidate 'thing) n))

(defun easy-kill-thing (thing &optional n)
  (interactive
   (list (cdr (assoc (car (last (listify-key-sequence
                                 (single-key-description last-command-event))))
                     easy-kill-alist))
         (prefix-numeric-value current-prefix-arg)))
  ;; Return non-nil if succeed
  (if (and thing
           (let ((n (or n 1)))
             (cond
              ((intern-soft (format "easy-kill-on-%s" thing))
               (funcall (intern-soft (format "easy-kill-on-%s" thing)) n))
              ((eq thing (overlay-get easy-kill-candidate 'thing))
               (easy-kill-enlarge n))
              (t (let ((bounds (bounds-of-thing-at-point thing)))
                   (when bounds
                     (move-overlay easy-kill-candidate (car bounds) (cdr bounds))
                     (overlay-put easy-kill-candidate 'thing thing)
                     (easy-kill-enlarge (1- n))
                     t))))))
      (progn
        (easy-kill-select-text)
        t)
    (ignore (when (called-interactively-p 'interact)
              (easy-kill-message-nolog "No `%s'" thing)))))

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
                            universal-argument-num-events
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
                 (kill-new (easy-kill-candidate)))
               (delete-overlay easy-kill-candidate)
               (setq easy-kill-candidate nil)
               nil)))))))

;;;###autoload
(defun easy-kill ()
  (interactive)
  (setq easy-kill-candidate (let ((o (make-overlay (point) (point))))
                              (overlay-put o 'face 'easy-kill-face)
                              o))
  (setq deactivate-mark t)
  (dolist (thing (if (use-region-p)
                     '(region url email line)
                   '(url email line)))
    (when (easy-kill-thing thing)
      (return)))
  (easy-kill-activate-keymap))

;;; Extended things

(put 'region 'bounds-of-thing-at-point
     (lambda () (cons (region-beginning) (region-end))))

(defun easy-kill-on-buffer-file-name (n)
  (let ((file (or buffer-file-name default-directory)))
    (when file
      (move-overlay easy-kill-candidate (point) (point))
      (overlay-put easy-kill-candidate 'thing 'buffer-file-name)
      (let* ((file (directory-file-name file))
             (text (cond
                    ((zerop n) (file-name-nondirectory file))
                    ((plusp n) file)
                    (t (file-name-directory file)))))
        (overlay-put easy-kill-candidate 'candidate text)
        (easy-kill-message-nolog "%s" text))
      t)))

(put 'buffer-file-name 'easy-kill-enlarge
     'easy-kill-on-buffer-file-name)

(provide 'easy-kill)
;;; easy-kill.el ends here

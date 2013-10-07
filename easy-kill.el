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
    (define-key map "=" 'easy-kill-enlarge)
    (define-key map "\C-w" 'easy-kill-region)
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
  (cond ((stringp s)
         (if (string-match "[ \t\f\r\n]*\\'" s)
             (substring s 0 (match-beginning 0))
           (error "`string-match' failed in `easy-kill-strip'")))
        (t s)))

(defvar easy-kill-exit nil
  "If non-nil tells `set-temporary-overlay-map' to exit.")

(defvar easy-kill-candidate nil)

(defun easy-kill-candidate ()
  "Get the kill candidate as a string.
If the overlay specified by variable `easy-kill-candidate' has
non-zero length, it is the string covered by the overlay.
Otherwise, it is the value of the overlay's candidate property."
  (easy-kill-strip-trailing
   (if (/= (overlay-start easy-kill-candidate)
           (overlay-end easy-kill-candidate))
       (buffer-substring (overlay-start easy-kill-candidate)
                         (overlay-end easy-kill-candidate))
     (overlay-get easy-kill-candidate 'candidate))))

(defun easy-kill-adjust-candidate (thing &optional beg end)
  "Adjust kill candidate to THING, BEG, END.
If BEG is a string, shring the overlay to zero length and set its
candidate property instead."
  (let ((o easy-kill-candidate))
    (overlay-put o 'thing thing)
    (if (stringp beg)
        (progn
          (move-overlay o (point) (point))
          (overlay-put o 'candidate beg)
          (easy-kill-message-nolog "%s" beg))
      (move-overlay o (or beg (overlay-start o)) (or end (overlay-end 0)))))
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
                (easy-kill-adjust-candidate thing nil (point))
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

(defun easy-kill-region ()
  "Kill current selection and exit."
  (interactive)
  (let ((beg (overlay-start easy-kill-candidate))
        (end (overlay-end easy-kill-candidate)))
    (if (/= beg end)
        (kill-region beg end)
      (easy-kill-message-nolog "Region empty")))
  (setq easy-kill-exit t))

(defun easy-kill-thing (thing &optional n inhibit-handler)
  (interactive
   (list (cdr (assoc (car (last (listify-key-sequence
                                 (single-key-description last-command-event))))
                     easy-kill-alist))
         (prefix-numeric-value current-prefix-arg)))
  (let ((n (or n 1)))
    (cond
     ((and (not inhibit-handler)
           (intern-soft (format "easy-kill-on-%s" thing)))
      (funcall (intern-soft (format "easy-kill-on-%s" thing)) n))
     ((eq thing (overlay-get easy-kill-candidate 'thing))
      (easy-kill-enlarge n))
     (t (let ((bounds (bounds-of-thing-at-point thing)))
          (if (not bounds)
              (when (called-interactively-p 'interact)
                (easy-kill-message-nolog "No `%s'" thing))
            (easy-kill-adjust-candidate thing (car bounds) (cdr bounds))
            (easy-kill-enlarge (1- n))))))))

(defun easy-kill-activate-keymap ()
  (let ((map (easy-kill-map)))
    (set-temporary-overlay-map
     map
     (lambda ()
       ;; When any error happens the keymap is active forever.
       (with-demoted-errors
         (or (and (not (prog1 easy-kill-exit
                         (setq easy-kill-exit nil)))
                  (eq this-command (lookup-key map (this-command-keys))))
             (when easy-kill-candidate
               ;; Do not modify the clipboard here because it will
               ;; intercept pasting from other programs and
               ;; `easy-kill-remember' already did the work.
               (let ((interprogram-cut-function nil)
                     (interprogram-paste-function nil)
                     (candidate (easy-kill-candidate)))
                 (unless(member candidate '(nil ""))
                   (kill-new candidate)))
               (delete-overlay easy-kill-candidate)
               (setq easy-kill-candidate nil)
               nil)))))))

;;;###autoload
(defun easy-kill (&optional n)
  "Kill thing at point in the order of region, url, email and line."
  (interactive "p")
  (setq easy-kill-candidate
        (let ((o (make-overlay (point) (point))))
          (overlay-put o 'face 'easy-kill-face)
          ;; Use higher priority to avoid shadowing by, for example,
          ;; `hl-line-mode'.
          (overlay-put o 'priority 999)
          o))
  (setq deactivate-mark t)
  (dolist (thing '(region url email line))
    (easy-kill-thing thing n)
    (when (overlay-get easy-kill-candidate 'thing)
      (return)))
  (easy-kill-activate-keymap))

;;; Extended things

(put 'region 'bounds-of-thing-at-point
     (lambda ()
       (when (use-region-p)
         (cons (region-beginning) (region-end)))))

(defun easy-kill-on-buffer-file-name (n)
  "Get `buffer-file-name' or `default-directory'.
If N is zero, remove the directory part; negative, remove the
file name party; positive, full path."
  (let ((file (or buffer-file-name default-directory)))
    (when file
      (let* ((file (directory-file-name file))
             (text (cond
                    ((zerop n) (file-name-nondirectory file))
                    ((plusp n) file)
                    (t (file-name-directory file)))))
        (easy-kill-adjust-candidate 'buffer-file-name text)))))

(put 'buffer-file-name 'easy-kill-enlarge 'easy-kill-on-buffer-file-name)

(defun easy-kill-on-url (&optional _n)
  "Get url at point or from char properties.
Char properties `help-echo', `shr-url' and `w3m-href-anchor' are
inspected."
  (if (bounds-of-thing-at-point 'url)
      (easy-kill-thing 'url nil t)
    (let ((get-url (lambda (text)
                     (when (stringp text)
                       (with-temp-buffer
                         (insert text)
                         (and (bounds-of-thing-at-point 'url)
                              (thing-at-point 'url)))))))
      (dolist (p '(help-echo shr-url w3m-href-anchor))
        (pcase-let* ((`(,text . ,ov)
                      (get-char-property-and-overlay (point) p))
                     (url (or (funcall get-url text)
                              (funcall get-url
                                       (and ov (overlay-get ov p))))))
          (when url
            (easy-kill-adjust-candidate 'url url)
            (return url)))))))

(provide 'easy-kill)
;;; easy-kill.el ends here

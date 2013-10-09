;;; easy-kill.el --- kill things easily              -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.7.0
;; Package-Requires: ((emacs "24"))
;; Keywords: convenience
;; Created: 2013-08-12
;; URL: https://github.com/leoliu/easy-kill

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
;; To use: (global-set-key [remap kill-ring-save] 'easy-kill)

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
  "A list of (CHAR . THING).
CHAR is used immediately following `easy-kill' to select THING."
  :type '(repeat (cons character symbol))
  :group 'killing)

(defun easy-kill-map ()
  "Build the keymap according to `easy-kill-alist'."
  (let ((map (make-sparse-keymap)))
    (define-key map "-" 'easy-kill-shrink)
    (define-key map "+" 'easy-kill-expand)
    (define-key map "=" 'easy-kill-expand)
    (define-key map "\C-w" 'easy-kill-region)
    (define-key map (kbd "C-SPC") 'easy-kill-mark-region)
    (define-key map (kbd "C-@") 'easy-kill-mark-region)
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
           (error "`string-match' failed in `easy-kill-strip-trailing'")))
        (t "")))

(defvar easy-kill-exit nil
  "Tell `set-temporary-overlay-map' to exit if non-nil.
Note: exit only happens right before next command per
`set-temporary-overlay-map'.")

(defvar easy-kill-candidate nil)

(defun easy-kill-candidate ()
  "Get the kill candidate as a string.
If the overlay specified by variable `easy-kill-candidate' has
non-zero length, it is the string covered by the overlay.
Otherwise, it is the value of the overlay's candidate property."
  (easy-kill-strip-trailing
   (with-current-buffer (overlay-buffer easy-kill-candidate)
     (if (/= (overlay-start easy-kill-candidate)
             (overlay-end easy-kill-candidate))
         (buffer-substring (overlay-start easy-kill-candidate)
                           (overlay-end easy-kill-candidate))
       (overlay-get easy-kill-candidate 'candidate)))))

(defun easy-kill-adjust-candidate (thing &optional beg end)
  "Adjust kill candidate to THING, BEG, END.
If BEG is a string, shrink the overlay to zero length and set its
candidate property instead."
  (let ((o easy-kill-candidate))
    (overlay-put o 'thing thing)
    (if (stringp beg)
        (progn
          (move-overlay o (point) (point))
          (overlay-put o 'candidate beg)
          (easy-kill-message-nolog "%s" beg))
      (move-overlay o (or beg (overlay-start o)) (or end (overlay-end o)))))
  (and interprogram-cut-function
       (not (string= (easy-kill-candidate) ""))
       (funcall interprogram-cut-function (easy-kill-candidate))))

(defun easy-kill-expand ()
  (interactive)
  (easy-kill-thing nil '+))

(defun easy-kill-digit-argument (n)
  (interactive
   (list (- (logand (if (integerp last-command-event)
                        last-command-event
                      (get last-command-event 'ascii-character))
                    ?\177)
            ?0)))
  (easy-kill-thing nil n))

(defun easy-kill-shrink ()
  (interactive)
  (easy-kill-thing nil '-))

;; helper for `easy-kill-thing'.
(defun easy-kill-thing-forward (n)
  (let ((thing (overlay-get easy-kill-candidate 'thing))
        (direction (if (minusp n) -1 +1))
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
          t)))))

(defun easy-kill-thing (&optional thing n nomsg inhibit-handler)
  ;; N can be -, + and digits
  (interactive
   (list (cdr (assoc (car (last (listify-key-sequence
                                 (single-key-description last-command-event))))
                     easy-kill-alist))
         (prefix-numeric-value current-prefix-arg)))
  (let ((thing (or thing (overlay-get easy-kill-candidate 'thing)))
        (n (or n 1)))
    (cond
     ((and (not inhibit-handler)
           (intern-soft (format "easy-kill-on-%s" thing)))
      (funcall (intern-soft (format "easy-kill-on-%s" thing)) n))
     ((or (eq thing (overlay-get easy-kill-candidate 'thing))
          (memq n '(+ -)))
      (easy-kill-thing-forward (pcase n
                                 (`+ 1)
                                 (`- -1)
                                 (n n))))
     (t (let ((bounds (bounds-of-thing-at-point thing)))
          (if (not bounds)
              (unless nomsg
                (easy-kill-message-nolog "No `%s'" thing))
            (easy-kill-adjust-candidate thing (car bounds) (cdr bounds))
            (easy-kill-thing-forward (1- n))))))))

(defun easy-kill-region ()
  "Kill current selection and exit."
  (interactive "*")
  (if (not easy-kill-candidate)         ; `easy-kill' has exited
      (push last-input-event unread-command-events)
    (let ((beg (overlay-start easy-kill-candidate))
          (end (overlay-end easy-kill-candidate)))
      (if (= beg end)
          (easy-kill-message-nolog "Empty region")
        (setq easy-kill-exit t)
        (easy-kill-adjust-candidate nil "")
        (kill-region beg end)))))

(defun easy-kill-mark-region ()
  (interactive)
  (if (not easy-kill-candidate)
      (push last-input-event unread-command-events)
    (let ((beg (overlay-start easy-kill-candidate))
          (end (overlay-end easy-kill-candidate)))
      (if (= beg end)
          (easy-kill-message-nolog "Empty region")
        (setq easy-kill-exit t)
        (easy-kill-adjust-candidate nil "")
        (set-mark beg)
        (goto-char end)
        (activate-mark)))))

(defun easy-kill-activate-keymap ()
  (let ((map (easy-kill-map)))
    (set-temporary-overlay-map
     map
     (lambda ()
       ;; When any error happens the keymap is active forever.
       (with-demoted-errors
         (or (and (not (prog1 easy-kill-exit
                         (setq easy-kill-exit nil)))
                  (eq this-command (lookup-key map (this-command-keys-vector))))
             (when easy-kill-candidate
               ;; Do not modify the clipboard here because it will
               ;; intercept pasting from other programs and
               ;; `easy-kill-adjust-candidate' already did the work.
               (let ((interprogram-cut-function nil)
                     (interprogram-paste-function nil))
                 (unless (string= (easy-kill-candidate) "")
                   (kill-new (easy-kill-candidate))))
               (delete-overlay easy-kill-candidate)
               (setq easy-kill-candidate nil)
               nil)))))))

;;;###autoload
(defun easy-kill (&optional n)
  "Kill thing at point in the order of region, url, email and line.
Temporally activate additional key bindings as follows:

  letters => select or expand things according to `easy-kill-alist';
  0..9    => expand current selection by that number;
  +,=/-   => expand or shrink current selection by 1;
  C-w     => kill current selection;
  C-SPC   => turn current selection into active region
  others  => save current selection to kill ring and exit."
  (interactive "p")
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (setq easy-kill-candidate
          (let ((o (make-overlay (point) (point))))
            (overlay-put o 'face 'easy-kill-face)
            ;; Use higher priority to avoid shadowing by, for example,
            ;; `hl-line-mode'.
            (overlay-put o 'priority 999)
            o))
    (setq deactivate-mark t)
    (dolist (thing '(url email line))
      (easy-kill-thing thing n 'nomsg)
      (or (string= (easy-kill-candidate) "")
          (return)))
    (when (zerop (buffer-size))
      (easy-kill-message-nolog "Warn: `easy-kill' activated in empty buffer"))
    (easy-kill-activate-keymap)))

;;; Extended things

(defun easy-kill-on-buffer-file-name (n)
  "Get `buffer-file-name' or `default-directory'.
If N is zero, remove the directory part; -, remove the file name
party; +, full path."
  (let ((file (or buffer-file-name default-directory)))
    (when file
      (let* ((file (directory-file-name file))
             (text (pcase n
                     (`- (file-name-directory file))
                     ((pred (eq 0)) (file-name-nondirectory file))
                     (_ file))))
        (easy-kill-adjust-candidate 'buffer-file-name text)))))

(defun easy-kill-on-url (&optional _n)
  "Get url at point or from char properties.
Char properties `help-echo', `shr-url' and `w3m-href-anchor' are
inspected."
  (if (bounds-of-thing-at-point 'url)
      (easy-kill-thing 'url nil nil t)
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

(defun easy-kill-backward-up ()
  (condition-case nil
      (up-list -1)
    (scan-error (let ((ppss (syntax-ppss)))
                  (and (nth 3 ppss) (goto-char (nth 8 ppss)))))))

(defun easy-kill-backward-down (point &optional bound)
  (condition-case nil
      (progn
        (easy-kill-backward-up)
        (if (and (or (not bound) (> (point) bound))
                 (/= point (point)))
            (easy-kill-backward-down (point) bound)
          point))
    (scan-error point)))

(defun easy-kill-bounds-of-list (n)
  (save-excursion
    (when (pcase n
            (`+ (let ((start (overlay-start easy-kill-candidate)))
                  (goto-char start)
                  (easy-kill-backward-up)
                  (/= start (point))))
            (`- (let ((pt (point)))
                  (goto-char (easy-kill-backward-down
                              (point) (overlay-start easy-kill-candidate)))
                  (/= pt (point))))
            (_ (error "Unsupported argument `%s'" n)))
      (cons (point) (progn (forward-sexp 1) (point))))))

(defun easy-kill-on-list (n)
  (if (memq n '(+ -))
      (let ((bounds (easy-kill-bounds-of-list n)))
        (when bounds
          (easy-kill-adjust-candidate 'list (car bounds) (cdr bounds))))
    (easy-kill-thing 'list n nil t)))

(defun easy-kill-on-sexp (n)
  (if (memq n '(+ -))
      (easy-kill-on-list n)
    (easy-kill-thing 'sexp n nil t)))

(provide 'easy-kill)
;;; easy-kill.el ends here

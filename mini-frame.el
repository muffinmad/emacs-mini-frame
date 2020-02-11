;;; mini-frame.el --- Show minibuffer in child frame on read-from-minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2020 Andrii Kolomoiets

;; Author: Andrii Kolomoiets <andreyk.mad@gmail.com>
;; Keywords: minibuffer
;; URL: https://github.com/muffinmad/emacs-mini-frame
;; Package-Version: 1.0
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Show minibuffer in child frame on read-from-minibuffer.
;; This is done by adding advice around `read-from-minibuffer' function.
;; In advice function child minibuffer-only frame is displayed.

;;; Code:

(defgroup mini-frame nil
  "Show minibuffer in child frame."
  :group 'minibuffer)

(defcustom mini-frame-ignore-commands '(eval-expression)
  "For this commands minibuffer will not be displayed in child frame."
  :type '(repeat function))

(defvar mini-frame-frame nil)
(defvar mini-frame-selected-frame nil)
(defvar mini-frame-completions-frame nil)

(defun mini-frame--shift-color (from to &optional by)
  "Move color FROM towards TO by BY."
  (let ((f (ash from -8))
        (by (or by 27)))
    (cond
     ((> from to) (- f by))
     ((< from to) (+ f by))
     (t f))))

(defun mini-frame-get-background-color (&optional frame)
  "Calculate background color for minibuffer frame from FRAME."
  (let* ((params (frame-parameters frame))
         (bg (color-values (alist-get 'background-color params)))
         (fg (color-values (alist-get 'foreground-color params))))
    (format "#%02x%02x%02x"
            (mini-frame--shift-color (car bg) (car fg))
            (mini-frame--shift-color (cadr bg) (cadr fg))
            (mini-frame--shift-color (caddr bg) (caddr fg)))))

(defun mini-frame--resize-mini-frame (frame)
  "Resize FRAME vertically only.
This function used as value for `resize-mini-frames' variable."
  (fit-frame-to-buffer frame nil nil nil nil 'vertically))

(defun mini-frame--hide-completions (&optional frame _force)
  "Hide completions FRAME."
  (make-frame-invisible frame)
  (when (and (frame-live-p mini-frame-frame) (frame-visible-p mini-frame-frame))
    (select-frame-set-input-focus mini-frame-frame)))

(defun mini-frame--ensure-completions-frame ()
  "Create frame to show completions."
  (unless (frame-live-p mini-frame-completions-frame)
    (setq mini-frame-completions-frame
          (make-frame `((height . 1)
                        (visibility . nil)
                        (parent-frame . ,mini-frame-selected-frame)
                        (delete-before . ,mini-frame-frame)
                        (auto-hide-function . mini-frame--hide-completions)
                        (user-position . t)
                        (user-size . t)
                        (keep-ratio . t)
                        (minibuffer . nil)
                        (undecorated . t)
                        (internal-border-width . 3)
                        (drag-internal-border . t))))))

(defun mini-frame--display-completions (buffer &rest _args)
  "Display completions BUFFER in another child frame."
  (mini-frame--ensure-completions-frame)
  (modify-frame-parameters
   mini-frame-completions-frame
   `((parent-frame . ,mini-frame-selected-frame)))
  (modify-frame-parameters
   mini-frame-completions-frame
   `(
     (background-color . ,(frame-parameter mini-frame-frame 'background-color))
     (top . ,(+ 7
                (frame-parameter mini-frame-frame 'top)
                (cdr (window-text-pixel-size (frame-selected-window mini-frame-frame)))))
     (height . 0.25)
     (width . 0.99)
     (left . 0.5)))
  (window--display-buffer buffer (frame-selected-window mini-frame-completions-frame) 'frame))

(defun mini-frame--ensure-frame ()
  "Create mini-frame."
  (unless (frame-live-p mini-frame-frame)
    (setq mini-frame-frame
          (make-frame '((height . 1)
                        (left . 0.5)
                        (visibility . nil)
                        (minibuffer . only)
                        (undecorated . t)
                        (keep-ratio . t)
                        (user-position . t)
                        (user-size . t)
                        (internal-border-width . 3)
                        (drag-internal-border . t))))))

(defun mini-frame--display ()
  "Show mini-frame."
  (let ((selected-frame (selected-frame)))
    (mini-frame--ensure-frame)
    (unless (eq selected-frame mini-frame-completions-frame)
      (setq mini-frame-selected-frame selected-frame)
      (mini-frame--ensure-completions-frame)
      (modify-frame-parameters
       mini-frame-frame
       `((parent-frame . ,mini-frame-selected-frame)))
      (modify-frame-parameters
       mini-frame-frame
       `((left . 0.5)
         (top . 0)
         (width . 0.99)
         (background-color . ,(mini-frame-get-background-color))
         (height . ,(if minibuffer-completion-table 2 1))))
      (when (frame-visible-p mini-frame-completions-frame)
        (make-frame-invisible mini-frame-completions-frame)))
    (make-frame-visible mini-frame-frame)
    (select-frame-set-input-focus mini-frame-frame)
    (fit-frame-to-buffer mini-frame-frame nil nil nil nil 'vertically)))

(defun mini-frame-read-from-minibuffer (fn &rest args)
  "Show minibuffer-only child frame and call FN with ARGS."
  (if (or (minibufferp)
          (memq this-command mini-frame-ignore-commands))
      (progn
        (when (and (frame-live-p mini-frame-frame)
                   (frame-visible-p mini-frame-frame)
                   (not (eq (selected-frame) mini-frame-frame)))
          (select-frame-set-input-focus mini-frame-frame))
        (apply fn args))
    (let (
          (dd default-directory)
          (visible (and (frame-live-p mini-frame-frame)
                        (frame-visible-p mini-frame-frame)))
          (resize-mini-frames #'mini-frame--resize-mini-frame)
          (display-buffer-alist `(("\\*Completions\\*" mini-frame--display-completions))))
      (mini-frame--display)
      (setq default-directory dd)
      (if visible
          (apply fn args)
        (unwind-protect
            (apply fn args)
          (progn
            (when (frame-live-p mini-frame-completions-frame)
              (make-frame-invisible mini-frame-completions-frame))
            (when (frame-live-p mini-frame-frame)
              (select-frame-set-input-focus mini-frame-selected-frame)
              (make-frame-invisible mini-frame-frame))))))))

;;;###autoload
(define-minor-mode mini-frame-mode
  "Show minibuffer in child frame on read-from-minibuffer."
  :global t
  (cond
   (mini-frame-mode
    (advice-add 'read-from-minibuffer :around #'mini-frame-read-from-minibuffer))
   (t
    (advice-remove 'read-from-minibuffer #'mini-frame-read-from-minibuffer)
    (when (frame-live-p mini-frame-frame)
      (delete-frame mini-frame-frame))
    (when (frame-live-p mini-frame-completions-frame)
      (delete-frame mini-frame-completions-frame)))))

(provide 'mini-frame)

;;; mini-frame.el ends here

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

(defun mini-frame--display-completions (buffer &rest _args)
  "Display completions BUFFER in another child frame."
  (let ((parent-frame-parameters `((parent-frame . ,mini-frame-selected-frame)))
        (show-parameters `((background-color . ,(frame-parameter mini-frame-frame 'background-color))
                           (top . ,(+ 6
                                      (frame-parameter mini-frame-frame 'top)
                                      (cdr (window-text-pixel-size (frame-selected-window mini-frame-frame)))))
                           (height . 0.25)
                           (width . 1.0)
                           (left . 0.5))))
    (if (frame-live-p mini-frame-completions-frame)
        (modify-frame-parameters mini-frame-completions-frame parent-frame-parameters)
      (setq mini-frame-completions-frame
            (make-frame (append '((visibility . nil)
                                  (auto-hide-function . mini-frame--hide-completions)
                                  (user-position . t)
                                  (user-size . t)
                                  (keep-ratio . t)
                                  (minibuffer . nil)
                                  (undecorated . t)
                                  (internal-border-width . 3)
                                  (drag-internal-border . t))
                                parent-frame-parameters
                                show-parameters))))
    (modify-frame-parameters mini-frame-completions-frame show-parameters)
    (window--display-buffer buffer (frame-selected-window mini-frame-completions-frame) 'frame)))

(defun mini-frame--display (fn args)
  "Show mini-frame and call FN with ARGS."
  (let* ((selected-frame (selected-frame))
         (selected-is-mini-frame (memq selected-frame
                                       (list mini-frame-frame
                                             mini-frame-completions-frame)))
         (dd default-directory)
         (parent-frame-parameters `((parent-frame . ,selected-frame)))
         (show-parameters `((left . 0.5)
                            (top . 0)
                            (width . 1.0)
                            (background-color . ,(mini-frame-get-background-color selected-frame))
                            (height . 1))))
    (if (frame-live-p mini-frame-frame)
        (unless selected-is-mini-frame
          (setq mini-frame-selected-frame selected-frame)
          (modify-frame-parameters mini-frame-frame parent-frame-parameters))
      (progn
        (setq mini-frame-selected-frame selected-frame)
        (setq mini-frame-frame
              (make-frame (append '((height . 1)
                                    (left . 0.5)
                                    (visibility . nil)
                                    (minibuffer . only)
                                    (undecorated . t)
                                    (keep-ratio . t)
                                    (user-position . t)
                                    (user-size . t)
                                    (internal-border-width . 3)
                                    (drag-internal-border . t))
                                  parent-frame-parameters
                                  show-parameters)))))
    (modify-frame-parameters mini-frame-frame show-parameters)
    (when (and (frame-live-p mini-frame-completions-frame)
               (frame-visible-p mini-frame-completions-frame))
      (make-frame-invisible mini-frame-completions-frame))
    (make-frame-visible mini-frame-frame)
    (select-frame-set-input-focus mini-frame-frame)
    (setq default-directory dd)
    (apply fn args)))

(defun mini-frame--delete-frame (frame)
  "Called before delete FRAME."
  (cond
   ((eq frame mini-frame-completions-frame)
    (when (and (frame-live-p mini-frame-frame)
               (frame-visible-p mini-frame-frame))
      (select-frame-set-input-focus mini-frame-frame)))
   ((eq frame mini-frame-frame)
    (when (frame-live-p mini-frame-completions-frame)
      (make-frame-invisible mini-frame-completions-frame))
    (select-frame-set-input-focus mini-frame-selected-frame))))

(defun mini-frame-read-from-minibuffer (fn &rest args)
  "Show minibuffer-only child frame (if needed) and call FN with ARGS."
  (cond
   ((or (minibufferp)
        (memq this-command mini-frame-ignore-commands))
    (apply fn args))
   ((and (frame-live-p mini-frame-frame)
         (frame-visible-p mini-frame-frame))
    (mini-frame--display fn args))
   (t
    (let ((after-make-frame-functions nil)
          (resize-mini-frames #'mini-frame--resize-mini-frame)
          (display-buffer-alist
           `(("\\*\\(Ido \\)?Completions\\*" mini-frame--display-completions)))
          (delete-frame-functions
           (cons #'mini-frame--delete-frame delete-frame-functions)))
      (unwind-protect
          (mini-frame--display fn args)
        (progn
          (when (frame-live-p mini-frame-completions-frame)
            (make-frame-invisible mini-frame-completions-frame))
          (when (frame-live-p mini-frame-selected-frame)
            (select-frame-set-input-focus mini-frame-selected-frame))
          (when (frame-live-p mini-frame-frame)
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

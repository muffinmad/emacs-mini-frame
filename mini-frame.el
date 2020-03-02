;;; mini-frame.el --- Show minibuffer in child frame on read-from-minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2020 Andrii Kolomoiets

;; Author: Andrii Kolomoiets <andreyk.mad@gmail.com>
;; Keywords: frames
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

;; Place minibuffer at the top of the current frame on `read-from-minibuffer`.
;;
;; While it's fine for me to have eldoc, flymake and other messages to appear
;; at the bottom of the screen, editing minibuffer (find file, create VC
;; branch, etc.) feels more comfortable in the upper area of the screen.
;;
;; `mini-frame-mode` makes an advice around `read-from-minibuffer` function to
;; create and show minibuffer-only child frame to accept input.
;;
;; By default mini-frame is placed at the top of the current frame and occupied
;; full width.  Those who use vertical completion candidates list may configure
;; mini-frame not to occupy full width:
;;
;;   (custom-set-variables
;;    '(mini-frame-show-parameters
;;      '((top . 10)
;;        (width . 0.7)
;;        (left . 0.5))))
;;
;; Users of Emacs 27 will benefits the most because of `resize-mini-frames`
;; variable: mini-frame will be resized vertically to fit content.
;;
;; Users of Emacs 26 will need to configure frame height explicitly, e.g.:
;;
;;   (custom-set-variables
;;    '(mini-frame-show-parameters
;;      '((top . 0)
;;        (width . 1.0)
;;        (left . 0.5)
;;        (height . 15))))
;;
;; One can configure the list of commands that must not be shown in the child
;; frame by customizing the `mini-frame-ignore-commands`.
;; The `eval-expression` command is there by default because mini-frame have no
;; modeline to display eldoc hints.  And because there must be some place to
;; turn `mini-frame-mode` off if something goes wrong (I hope not) :)

;;; Code:

(defgroup mini-frame nil
  "Show minibuffer in child frame."
  :group 'minibuffer)

(defcustom mini-frame-ignore-commands '(eval-expression)
  "For this commands minibuffer will not be displayed in child frame."
  :type '(repeat function))

(defcustom mini-frame-show-parameters '((left . 0.5)
                                        (top . 0)
                                        (width . 1.0)
                                        (height . 1))
  "Frame parameters wich will be applied to mini frame on show.
Unless background color is specified it will be set to result of
`mini-frame-background-color-function'."
  :type '(choice alist function))

(defcustom mini-frame-color-shift-step 27
  "Shift each of RGB channels of background color by this value.
Background color is \"moved\" towards foreground color of selected frame
to determine background color of mini frame."
  :type 'integer)

(defcustom mini-frame-background-color-function #'mini-frame-get-background-color
  "Function to calculate background color of mini frame.
Called if `mini-frame-show-paremeters' doesn't specify background color."
  :type 'function)

(defcustom mini-frame-handle-completions t
  "Create child frame to display completions buffer."
  :type 'boolean)

(defcustom mini-frame-completions-show-parameters '((height . 0.25)
                                                    (width . 1.0)
                                                    (left . 0.5))
  "Frame parameters wich will be applied to completions frame on show.
Unless background-color is specified it will be set to background color
of mini frame.
Unless top is specified it will be set to result of `mini-frame-completions-top-function'."
  :type '(choice alist function))

(defcustom mini-frame-completions-top-function #'mini-frame-get-completions-top
  "Function to calculate top parameter of completions frame."
  :type 'function)

(defcustom mini-frame-resize t
  "Resize mini frame.
If non-nil, set `resize-mini-frames' option to `mini-frame--resize-mini-frame'
which will fit frame to buffer vertically only.
Option `resize-mini-frames' is available on Emacs 27 and later."
  :type 'boolean)

(defcustom mini-frame-max-height nil
  " Set max frame height.
when non-nil and `mini-frame-resize' is non-nil ."
  :type 'boolean)


(defvar mini-frame-frame nil)
(defvar mini-frame-selected-frame nil)
(defvar mini-frame-completions-frame nil)

(defun mini-frame--shift-color (from to &optional by)
  "Move color FROM towards TO by BY.  If BY is ommited, `mini-frame-color-shift-step' is used."
  (let ((f (ash from -8))
        (by (or by mini-frame-color-shift-step)))
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
  (fit-frame-to-buffer frame mini-frame-max-height nil nil nil 'vertically))

(defun mini-frame--hide-completions (&optional frame _force)
  "Hide completions FRAME."
  (make-frame-invisible frame)
  (when (and (frame-live-p mini-frame-frame) (frame-visible-p mini-frame-frame))
    (select-frame-set-input-focus mini-frame-frame)))

(defun mini-frame-get-completions-top ()
  "Calculate top of completions frame to be just below mini frame."
  (+ (* 2 (frame-parameter mini-frame-frame 'internal-border-width))
     (frame-parameter mini-frame-frame 'top)
     (cdr (window-text-pixel-size (frame-selected-window mini-frame-frame)))))

(defun mini-frame--display-completions (buffer &rest _args)
  "Display completions BUFFER in another child frame."
  (let* ((parent-frame-parameters `((parent-frame . ,mini-frame-selected-frame)))
         (show-parameters (if (functionp mini-frame-completions-show-parameters)
                              (funcall mini-frame-completions-show-parameters)
                            mini-frame-completions-show-parameters))
         (show-parameters (append (unless (alist-get 'background-color show-parameters)
                                    `((background-color . ,(frame-parameter mini-frame-frame 'background-color))))
                                  (unless (alist-get 'top show-parameters)
                                    `((top . ,(funcall mini-frame-completions-top-function))))
                                  show-parameters)))
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
                                show-parameters)))
      (set-face-background 'fringe nil mini-frame-completions-frame))
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
         (show-parameters (if (functionp mini-frame-show-parameters)
                              (funcall mini-frame-show-parameters)
                            mini-frame-show-parameters))
         (show-parameters (append (unless (alist-get 'background-color show-parameters)
                                    `((background-color . ,(funcall mini-frame-background-color-function))))
                                  show-parameters)))
    (if (frame-live-p mini-frame-frame)
        (unless selected-is-mini-frame
          (setq mini-frame-selected-frame selected-frame)
          (modify-frame-parameters mini-frame-frame parent-frame-parameters))
      (progn
        (setq mini-frame-selected-frame selected-frame)
        (setq mini-frame-frame
              (make-frame (append '((visibility . nil)
                                    (minibuffer . only)
                                    (undecorated . t)
                                    (keep-ratio . t)
                                    (user-position . t)
                                    (user-size . t)
                                    (internal-border-width . 3)
                                    (drag-internal-border . t))
                                  parent-frame-parameters
                                  show-parameters)))
        (set-face-background 'fringe nil mini-frame-frame)))
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
          (resize-mini-frames (when mini-frame-resize
                                #'mini-frame--resize-mini-frame))
          (display-buffer-alist
           (if mini-frame-handle-completions
               (append
                '(("\\(\\*\\(Ido \\)?Completions\\)\\|\\(\\*Isearch completions\\)\\*" mini-frame--display-completions))
                display-buffer-alist)
             display-buffer-alist))
          (delete-frame-functions
           (cons #'mini-frame--delete-frame delete-frame-functions))
          ;; FIXME which-key is not working in mini frame
          (which-key-popup-type 'frame))
      (ignore resize-mini-frames)
      (ignore which-key-popup-type)
      (unwind-protect
          (mini-frame--display fn args)
        (when (frame-live-p mini-frame-completions-frame)
          (make-frame-invisible mini-frame-completions-frame))
        (when (frame-live-p mini-frame-selected-frame)
          (select-frame-set-input-focus mini-frame-selected-frame))
        (when (frame-live-p mini-frame-frame)
          (if (eq system-type 'windows-nt)
              ;; FIXME sometime buffer is not visible on windows
              (delete-frame mini-frame-frame)
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

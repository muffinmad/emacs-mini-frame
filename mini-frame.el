;;; mini-frame.el --- Show minibuffer in child frame on read-from-minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Andrii Kolomoiets

;; Author: Andrii Kolomoiets <andreyk.mad@gmail.com>
;; Keywords: frames
;; URL: https://github.com/muffinmad/emacs-mini-frame
;; Package-Version: 1.19
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

(defcustom mini-frame-advice-functions '(read-from-minibuffer read-string yes-or-no-p)
  "Functions to advice with `mini-frame-read-from-minibuffer'.
Set this variable before `mini-frame' mode activation."
  :type '(repeat function))

(defcustom mini-frame-ignore-commands '(eval-expression "edebug-eval-expression" debugger-eval-expression)
  "For this commands minibuffer will not be displayed in child frame."
  :type '(repeat (choice function regexp)))

(defcustom mini-frame-ignore-functions nil
  "This functions will be advised to not display minibuffer in child frame.
Set this variable before `mini-frame' mode activation."
  :type '(repeat function))

(defcustom mini-frame-show-parameters '((left . 0.5)
                                        (top . 0.0)
                                        (width . 1.0)
                                        (height . 1))
  "Frame parameters which will be applied to mini frame on show.
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
Called if `mini-frame-show-parameters' doesn't specify background color."
  :type 'function)

(defcustom mini-frame-internal-border-color nil
  "When set, set the internal border color of mini-frames to this color."
  :type '(choice (const :tag "Not set" nil)
                 (color :tag "Color")
                 (const :tag "Unspecified" unspecified)))

(defcustom mini-frame-handle-completions t
  "Create child frame to display completions buffer."
  :type 'boolean)

(defcustom mini-frame-completions-show-parameters '((height . 0.25)
                                                    (width . 1.0)
                                                    (left . 0.5))
  "Frame parameters which will be applied to completions frame on show.
Unless background-color is specified it will be set to background color
of mini frame.
Unless top is specified it will be set to result of the
`mini-frame-completions-top-function'."
  :type '(choice alist function))

(defcustom mini-frame-completions-focus 'minibuffer
  "Which frame will receive focus once completions frame is shown.
If nil, leave focus as is."
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Select completions frame" completions)
                 (const :tag "Select minibuffer frame" minibuffer)))

(defcustom mini-frame-completions-top-function #'mini-frame-get-completions-top
  "Function to calculate top parameter of completions frame."
  :type 'function)

(defcustom mini-frame-resize t
  "How to resize mini-frame.
A value of nil means don't autotomatically resize mini-frame.
A value of t means autotomatically resize mini-frame.
A value of `grow-only' means let mini-frame grow only.
A value of `not-set' means to not override `resize-mini-frames'.

If t or `grow-only', `resize-mini-frames' will be set to
`mini-frame--resize-mini-frame' function.

Option `resize-mini-frames' is available on Emacs 27 and later."
  :type '(choice (const :tag "Don't resize" nil)
                 (const :tag "Resize" t)
                 (const :tag "Grow only" grow-only)
                 (const :tag "Don't set" not-set)))

(defcustom mini-frame-resize-max-height nil
  "Max height boundary for mini-frame when `mini-frame-resize' is set."
  :type '(choice (const :tag "Not set" nil)
                 (integer :tag "Lines count")))

(defcustom mini-frame-resize-min-height nil
  "Min height boundary for mini-frame when `mini-frame-resize' is set."
  :type '(choice (const :tag "Not set" nil)
                 (integer :tag "Lines count")))

(defcustom mini-frame-create-lazy t
  "Create mini-frame lazily.
If non-nil, mini-frame will be created on first use.
If nil, mini-frame will be created on the mode activation."
  :type 'boolean)

(defcustom mini-frame-detach-on-hide t
  "Detach mini-frame from parent frame on mini-frame hide.
This allow to avoid mini-frame recreation in case its parent frame were deleted."
  :type 'boolean)

(defcustom mini-frame-standalone nil
  "Make mini-frame frame standalone instead of child-frame."
  :type 'boolean)


(defvar mini-frame-frame nil)
(defvar mini-frame-selected-frame nil)
(defvar mini-frame-selected-window nil)
(defvar mini-frame-completions-frame nil)
(defvar mini-frame-ignore-this nil)

(defun mini-frame--shift-color (from to &optional by)
  "Move color FROM towards TO by BY.  If BY is omitted, `mini-frame-color-shift-step' is used."
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

(defconst mini-frame--fit-frame-function
  (if (functionp 'fit-frame-to-buffer-1)
      'fit-frame-to-buffer-1
    'fit-frame-to-buffer)
  "Function used to fit mini-frame to buffer.")

(defun mini-frame--resize-mini-frame (frame)
  "Resize FRAME vertically only.
This function used as value for `resize-mini-frames' variable."
  (funcall mini-frame--fit-frame-function
           frame
           mini-frame-resize-max-height
           (if (eq mini-frame-resize 'grow-only)
               (max (frame-parameter frame 'height)
                    mini-frame-resize-min-height)
             mini-frame-resize-min-height)
           ;; A max-width must be included to work around a bug in Emacs which
           ;; causes wrapping to not be taken into account in some situations
           ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=56102
           (window-body-width)
           nil
           'vertically)
  (when (and (frame-live-p mini-frame-completions-frame)
             (frame-visible-p mini-frame-completions-frame))
    (let ((show-parameters (if (functionp mini-frame-completions-show-parameters)
                               (funcall mini-frame-completions-show-parameters)
                             mini-frame-completions-show-parameters)))
      (unless (alist-get 'top show-parameters)
        (modify-frame-parameters
         mini-frame-completions-frame
         `((top . ,(funcall mini-frame-completions-top-function))))))))

(defun mini-frame--hide-completions (&optional frame _force)
  "Hide completions FRAME."
  (make-frame-invisible frame)
  (when (and (frame-live-p mini-frame-frame) (frame-visible-p mini-frame-frame))
    (select-frame-set-input-focus mini-frame-frame)))

(defun mini-frame-get-completions-top ()
  "Calculate top of completions frame to be just below mini frame."
  (let* ((mini-frame-top (frame-parameter mini-frame-frame 'top))
         (comp-frame-top
          (+ (* 2 (or (frame-parameter mini-frame-frame
                                       'child-frame-border-width)
                      (frame-parameter mini-frame-frame
                                       'internal-border-width)))
             (if (consp mini-frame-top)
                 (cadr mini-frame-top)
               mini-frame-top)
             (cdr (window-text-pixel-size
                   (frame-selected-window mini-frame-frame))))))
    (if (consp mini-frame-top)
        `(,(car mini-frame-top) ,comp-frame-top)
      comp-frame-top)))

(defun mini-frame--completions-setup ()
  "Completion setup hook."
  (when mini-frame-completions-focus
    (let ((frame (if (eq mini-frame-completions-focus 'completions)
                     mini-frame-completions-frame
                   mini-frame-frame)))
      (when (and (frame-live-p frame) (frame-visible-p frame))
        (select-frame-set-input-focus frame)))))

(defun mini-frame--make-frame (parameters)
  "Make frame with common parameters and PARAMETERS."
  (let ((frame (make-frame (append parameters
                                   '((visibility . nil)
                                     (user-position . t)
                                     (user-size . t)
                                     (keep-ratio . t)
                                     (undecorated . t)
                                     (desktop-dont-save . t)
                                     (child-frame-border-width . 3)
                                     (internal-border-width . 3)
                                     (drag-internal-border . t)
                                     (z-group . above))))))
    (set-face-background 'fringe nil frame)
    (when mini-frame-internal-border-color
      (set-face-background 'child-frame-border mini-frame-internal-border-color frame)
      (set-face-background 'internal-border mini-frame-internal-border-color frame))
    frame))

(defun mini-frame--move-frame-to-frame (frame to-frame)
  "Move FRAME to the same monitor as TO-FRAME."
  (when (and mini-frame-standalone
               (not (equal (frame-monitor-workarea frame)
                           (frame-monitor-workarea to-frame))))
      (modify-frame-parameters frame
                               (mapcar (lambda (param)
                                         `(,param . ,(frame-parameter to-frame param)))
                                       '(left width top)))))

(defun mini-frame--display-completions (buffer alist)
  "Display completions BUFFER in another child frame.
ALIST is passed to `window--display-buffer'."
  (let* ((parent-frame-parameters `((parent-frame . ,(unless mini-frame-standalone
                                                       mini-frame-selected-frame))))
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
            (mini-frame--make-frame (append '((auto-hide-function . mini-frame--hide-completions)
                                              (minibuffer . nil))
                                            parent-frame-parameters
                                            show-parameters))))
    (mini-frame--move-frame-to-frame mini-frame-completions-frame mini-frame-frame)
    (modify-frame-parameters mini-frame-completions-frame show-parameters)
    (make-frame-visible mini-frame-completions-frame)
    (let ((w (frame-selected-window mini-frame-completions-frame)))
      (prog1 (window--display-buffer buffer w 'frame alist)
        (set-window-dedicated-p w 'soft)))))

(defun mini-frame--display (fn args)
  "Show mini-frame and call FN with ARGS."
  (let* ((selected-frame (selected-frame))
         (selected-window (selected-window))
         (selected-is-mini-frame (memq selected-frame
                                       (list mini-frame-frame
                                             mini-frame-completions-frame)))
         (dd default-directory)
         (parent-frame-parameters `((parent-frame . ,(unless mini-frame-standalone
                                                       selected-frame))))
         (show-parameters (if (functionp mini-frame-show-parameters)
                              (funcall mini-frame-show-parameters)
                            mini-frame-show-parameters))
         (show-parameters (append (unless (alist-get 'background-color show-parameters)
                                    `((background-color . ,(funcall mini-frame-background-color-function))))
                                  show-parameters)))
    (if (frame-live-p mini-frame-frame)
        (unless selected-is-mini-frame
          (setq mini-frame-selected-frame selected-frame)
          (setq mini-frame-selected-window selected-window)
          (modify-frame-parameters mini-frame-frame parent-frame-parameters))
      (setq mini-frame-selected-frame selected-frame)
      (setq mini-frame-selected-window selected-window)
      (setq mini-frame-frame
            (mini-frame--make-frame (append '((minibuffer . only))
                                            parent-frame-parameters
                                            show-parameters))))
    (mini-frame--move-frame-to-frame mini-frame-frame mini-frame-selected-frame)
    (modify-frame-parameters mini-frame-frame show-parameters)
    (when (and (frame-live-p mini-frame-completions-frame)
               (frame-visible-p mini-frame-completions-frame))
      (make-frame-invisible mini-frame-completions-frame))
    (make-frame-visible mini-frame-frame)
    (redirect-frame-focus mini-frame-selected-frame mini-frame-frame)
    (select-frame-set-input-focus mini-frame-frame)
    (setq default-directory dd)
    (apply fn args)))

(defun mini-frame--minibuffer-selected-window (fn &rest args)
  "Call FN with ARGS.  Return window selected just before mini-frame window was selected."
  (let ((window (apply fn args)))
    (if (and window
             (eq mini-frame-frame (window-frame window)))
        mini-frame-selected-window
      window)))

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

(defvar which-key-popup-type)
(defvar ivy-fixed-height-minibuffer)

(defun mini-frame--ignore-function (fn &rest args)
  "Let `mini-frame-ignore-this' and call FN with ARGS."
  (let ((mini-frame-ignore-this t))
    (apply fn args)))

(defun mini-frame-read-from-minibuffer (fn &rest args)
  "Show minibuffer-only child frame (if needed) and call FN with ARGS."
  (cond
   ((or mini-frame-ignore-this
        (not (display-graphic-p))
        (minibufferp)
        isearch-mode
        (and (symbolp this-command)
             (catch 'ignored
               (dolist (ignored-command mini-frame-ignore-commands)
                 (when (if (stringp ignored-command)
                           (string-match-p ignored-command (symbol-name this-command))
                         (eq ignored-command this-command))
                   (throw 'ignored t))))))
    (apply fn args))
   ((and (frame-live-p mini-frame-frame)
         (or mini-frame-standalone
             (frame-parameter mini-frame-frame 'parent-frame))
         (frame-visible-p mini-frame-frame))
    (mini-frame--display fn args))
   (t
    ;; On windows `frame-visible-p' can be t even if the frame is not visible so
    ;; calling `make-frame-visible' doesn't make frame actually visible.  Make frame
    ;; invisible one more time.
    (when (and (frame-live-p mini-frame-frame)
               (frame-visible-p mini-frame-frame))
      (make-frame-invisible mini-frame-frame))
    (let ((after-make-frame-functions nil)
          (resize-mini-frames (if (eq mini-frame-resize 'not-set)
                                  resize-mini-frames
                                (when mini-frame-resize
                                  #'mini-frame--resize-mini-frame)))
          (display-buffer-alist
           (if mini-frame-handle-completions
               (append
                '(("\\(\\*\\(Ido \\)?Completions\\)\\|\\(\\*Isearch completions\\)\\*" mini-frame--display-completions))
                display-buffer-alist)
             display-buffer-alist))
          (completion-setup-hook
           (cons #'mini-frame--completions-setup completion-setup-hook))
          (temp-buffer-window-show-hook
           (cons #'mini-frame--completions-setup temp-buffer-window-show-hook))
          (delete-frame-functions
           (cons #'mini-frame--delete-frame delete-frame-functions))
          ;; FIXME which-key is not working in mini frame
          (which-key-popup-type 'frame)
          (ivy-fixed-height-minibuffer nil))
      (ignore ivy-fixed-height-minibuffer)
      (ignore resize-mini-frames)
      (ignore which-key-popup-type)
      (save-current-buffer
        (save-window-excursion
          (unwind-protect
              (mini-frame--display fn args)
            (when (frame-live-p mini-frame-completions-frame)
              (make-frame-invisible mini-frame-completions-frame))
            (when (frame-live-p mini-frame-selected-frame)
              (select-frame-set-input-focus mini-frame-selected-frame))
            (when (frame-live-p mini-frame-frame)
              (make-frame-invisible mini-frame-frame)
              (when mini-frame-detach-on-hide
                (modify-frame-parameters mini-frame-frame '((parent-frame . nil))))))))))))

(defun mini-frame--advice (funcs func &optional remove)
  "Add advice FUNC around FUNCS.  If REMOVE, remove advice instead."
  (mapc (lambda (fn)
          (if remove (advice-remove fn func) (advice-add fn :around func)))
        funcs))

;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2ecbf4cfae
;; By default minibuffer is moved onto active frame leaving empty mini-frame.
;; Disable this behavior on mini-frame-mode.
(defvar minibuffer-follows-selected-frame)
(defvar mini-frame--minibuffer-follows-selected-frame)

;;;###autoload
(define-minor-mode mini-frame-mode
  "Show minibuffer in child frame on read-from-minibuffer."
  :global t
  (cond
   (mini-frame-mode
    (when (boundp 'minibuffer-follows-selected-frame)
      (setq mini-frame--minibuffer-follows-selected-frame minibuffer-follows-selected-frame)
      (setq minibuffer-follows-selected-frame nil))
    (mini-frame--advice mini-frame-advice-functions #'mini-frame-read-from-minibuffer)
    (mini-frame--advice mini-frame-ignore-functions #'mini-frame--ignore-function)
    (advice-add 'minibuffer-selected-window :around #'mini-frame--minibuffer-selected-window)
    (unless mini-frame-create-lazy
      (add-hook 'window-setup-hook
                (lambda ()
                  (let ((after-make-frame-functions nil))
                    (setq mini-frame-frame
                          (mini-frame--make-frame '((minibuffer . only)))))))))
   (t
    (when (boundp 'minibuffer-follows-selected-frame)
      (setq minibuffer-follows-selected-frame mini-frame--minibuffer-follows-selected-frame))
    (mini-frame--advice mini-frame-advice-functions #'mini-frame-read-from-minibuffer t)
    (mini-frame--advice mini-frame-ignore-functions #'mini-frame--ignore-function t)
    (advice-remove 'minibuffer-selected-window #'mini-frame--minibuffer-selected-window)
    (when (frame-live-p mini-frame-frame)
      (delete-frame mini-frame-frame))
    (when (frame-live-p mini-frame-completions-frame)
      (delete-frame mini-frame-completions-frame)))))

(provide 'mini-frame)

;;; mini-frame.el ends here

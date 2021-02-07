[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/copyleft/gpl.html)
[![MELPA](http://melpa.org/packages/mini-frame-badge.svg)](http://melpa.org/#/mini-frame)
# emacs-mini-frame

Place minibuffer at the top of the current frame on `read-from-minibuffer`.

While it's fine for me to have eldoc, flymake and other messages to appear at the bottom of the screen, editing minibuffer (find file, create VC branch, etc.) feels more comfortable in the upper area of the screen.

`mini-frame-mode` makes an advice around `read-from-minibuffer` function to create and show minibuffer-only child frame to accept input.

## How it looks like

By default mini-frame is placed at the top of the current frame and occupied full width. Here is `execute-extended-command` (<kbd>M-x</kbd>) with `icomplete-mode` enabled:

<p align="center">
  <img src="https://raw.githubusercontent.com/muffinmad/emacs-mini-frame/master/screenshots/icomplete-m-x.png" width="640">
</p>

Those who use vertical completion candidates list may configure mini-frame not to occupy full width:

```elisp
(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 10)
     (width . 0.7)
     (left . 0.5))))
```

Here is `switch-to-buffer` (<kbd>C-x b</kbd>) with `ido-vertical-mode`:

<p align="center">
  <img src="https://raw.githubusercontent.com/muffinmad/emacs-mini-frame/master/screenshots/ido-vertical-buffers.png" width="640">
</p>

And `execute-extended-command` (<kbd>M-x</kbd>) with `ivy-mode`:

<p align="center">
  <img src="https://raw.githubusercontent.com/muffinmad/emacs-mini-frame/master/screenshots/ivy-m-x.png" width="640">
</p>

`mini-frame-mode` also create separate child frame to display completions list:

<p align="center">
  <img src="https://raw.githubusercontent.com/muffinmad/emacs-mini-frame/master/screenshots/completions-frame.png" width="640">
</p>

## Mini-frame size

Users of Emacs 27 will benefits the most because of `resize-mini-frames` variable: mini-frame will be resized vertically to fit content.

Users of Emacs 26 will need to configure frame height explicitly, e.g.:

```elisp
(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 0)
     (width . 1.0)
     (left . 0.5)
     (height . 15))))
```

### Gnome shell does not resize Emacs child frames

Until [this issue](https://gitlab.gnome.org/GNOME/mutter/-/issues/840) will be solved, Gnome Shell users must also set mini-frame height.

Another option for Gnome Shell users is to use the following code in initialization file:

```elisp
(setq x-gtk-resize-child-frames 'resize-mode)
```

## Ignore commands

One can configure the list of commands that must not be shown in the child frame by customizing the `mini-frame-ignore-commands`. The `eval-expression` command is there by default because mini-frame have no modeline to display eldoc hints. And because there must be some place to turn `mini-frame-mode` off if something goes wrong (I hope not) :)

If you customize the `mini-frame-accept-commands` mini-buffer will always be set for this even if it was ignored in `mini-frame-ignore-commands`.

## Installation and usage

`mini-frame` is available on [MELPA](https://melpa.org/#/mini-frame).

Alternatively, you can download `mini-frame.el` and run:

<kbd>M-x</kbd> `package-install-file` <kbd>RET</kbd> `<path-to-mini-frame-el>` <kbd>RET</kbd>

<kbd>M-x</kbd> `mini-frame-mode` <kbd>RET</kbd>

<h1 align="center"> TopSpace </h1>
<p align="center">Scroll down & recenter top lines in Emacs.</p>

<!-- cursor -->

<p align="center">
   <a href="https://github.com/trevorpogue/topspace/actions/workflows/ci.yml/"><img src="https://github.com/trevorpogue/topspace/actions/workflows/ci.yml/badge.svg" height="20"/></a>
  <a href='https://coveralls.io/github/trevorpogue/topspace?branch=main'><img src='https://coveralls.io/repos/github/trevorpogue/topspace/badge.svg?branch=main&1' alt='Coverage Status' /></a>
  <a href="https://github.com/trevorpogue/topspace/blob/main/.github/workflows/ci.yml"><img src="https://img.shields.io/badge/Emacs-25.1+-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=whit" height="20"/></a>
  <a href="http://melpa.org/#/topspace"><img src="http://melpa.org/packages/topspace-badge.svg" height="20"/></a>
  <a href="http://stable.melpa.org/#/topspace"><img src="http://stable.melpa.org/packages/topspace-badge.svg" height="20"/></a>

</p>

<p align="center"><img src="https://user-images.githubusercontent.com/12535207/164986647-cdb35afa-de45-4e6f-ac16-fad765f9969e.gif"/></p>

<p align="center">
[ <a href="https://github.com/trevorpogue/topspace#installation"> Installation </a> |
<a href="https://github.com/trevorpogue/topspace#usage"> Usage</a> |
<a href="https://github.com/trevorpogue/topspace#customization"> Customization</a> |
<a href="https://github.com/trevorpogue/topspace#extra-functions"> Extra functions</a> |
   <a href="https://github.com/trevorpogue/topspace#how-it-works"> How it works </a> ]

TopSpace is an Emacs minor mode that lets you display a buffer's first line in the center of a window instead of just at the top.
This is done by automatically drawing an upper margin/padding above line 1
as you recenter and scroll it down.

### Features

* **Easier on the eyes**: Recenter or scroll down top text to a more comfortable eye level for reading, especially when in full-screen or on a large monitor.
* **Easy to use**: No new keybindings are required, keep using all your previous scrolling & recentering commands, except now you can also scroll down the first line. It also integrates seamlessly with  [centered-cursor-mode][1] to keep the cursor centered all the way to the first line.

# Installation

TopSpace is available on [MELPA](http://melpa.org).
After [installing MELPA](https://melpa.org/#/getting-started) you can install TopSpace with the following command:

&nbsp;&nbsp;&nbsp;&nbsp; <kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `topspace` <kbd>[RET]</kbd>

If `topspace` did not appear here in `package-install` try running <kbd>M-x</kbd> `package-refresh-contents` and repeating the above step.

Then enable TopSpace locally with

&nbsp;&nbsp;&nbsp;&nbsp; <kbd>M-x</kbd> `topspace-mode`,

or globally with

&nbsp;&nbsp;&nbsp;&nbsp; <kbd>M-x</kbd> `global-topspace-mode`.

To enable `topspace-mode` globally on startup, add the following to your Emacs config:
```
(global-topspace-mode 1)
```
# Usage
### Just enable and go
No new keybindings are required, keep using all your previous scrolling & recentering commands, except now you can also scroll down the first line.

# Customization
```elisp
(defcustom topspace-active #'topspace-default-active
  "Determine when `topspace-mode' mode is active / has any effect on buffer.
This is useful in particular when `global-topspace-mode' is enabled but you want
`topspace-mode' to be inactive in certain buffers or in any specific
circumstance.  When inactive, `topspace-mode' will still technically be on,
but will be effectively off and have no effect on the buffer.
Note that if `topspace-active' returns non-nil but `topspace-mode' is off,
`topspace-mode' will still be disabled.

With the default value, topspace will only be inactive in child frames.

If non-nil, then always be active.  If nil, never be active.
If set to a predicate function (function that returns a boolean value),
then be active only when that function returns a non-nil value."
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil)
                 (function :tag "predicate function")))

(defcustom topspace-autocenter-buffers #'topspace-default-autocenter-buffers
  "Center small buffers with top space when first opened or window sizes change.
This is done by automatically calling `topspace-recenter-buffer'
and the positioning can be customized with `topspace-center-position'.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height, or if `window-start' is greater
than 1.

With the default value, buffers will not be centered if in a child frame
or if the user has already scrolled or used `recenter' with buffer in the
selected window.

If non-nil, then always autocenter.  If nil, never autocenter.
If set to a predicate function (function that returns a boolean value),
then do auto-centering only when that function returns a non-nil value."
  :group 'topspace
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil)
                 (function :tag "predicate function")))

(defcustom topspace-center-position 0.4
  "Target position when centering buffers.

Used in `topspace-recenter-buffer' when called without an argument, or when
opening/resizing buffers if `topspace-autocenter-buffers' returns non-nil.

Can be set to a floating-point number, integer, or function that returns a
floating-point number or integer.

If a floating-point number, it represents the position to center buffers as a
ratio of frame height, and can be a value from 0.0 to 1.0 where lower values
center buffers higher up in the screen.

If a positive or negative integer value, buffers will be centered by putting
their center line at a distance of `topspace-center-position' lines away
from the top of the selected window when positive, or from the bottom
of the selected window when negative.
The distance will be in units of lines with height `default-line-height',
and the value should be less than the height of the window.

If a function, the same rules above apply to the function's return value."
  :group 'topspace
  :type '(choice float integer
                 (function :tag "float or integer function")))

(defcustom topspace-empty-line-indicator
  #'topspace-default-empty-line-indicator
  "Text that will appear in each empty topspace line above the top text line.
Can be set to either a constant string or a function that returns a string.

The conditions in which the indicator string is present are also customizable
by setting `topspace-empty-line-indicator' to a function, where the function
returns \"\" (an empty string) under any conditions in which you don't want
the indicator string to be shown.

By default it will show the empty-line bitmap in the left fringe
if `indicate-empty-lines' is non-nil, otherwise nothing.
This is done by adding a 'display property to the string (see
`topspace-default-empty-line-indicator' for more details).
The default bitmap is the one that the `empty-line' logical fringe indicator
maps to in `fringe-indicator-alist'.

You can alternatively show the string text in the body of each top space line by
having `topspace-empty-line-indicator' return a string without the 'display
property added.  If you do this you may be interested in also changing the
string's face like so: (propertize indicator-string 'face 'fringe)."
  :type '(choice 'string (function :tag "String function")))

(defcustom topspace-mode-line " T"
  "Mode line lighter for Topspace.
The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
more information.  Note that it should contain a _single_ mode
line construct only.
Set this variable to nil to disable the mode line completely."
  :group 'topspace
  :type 'sexp)

(defvar topspace-keymap (make-sparse-keymap)
  "Keymap for Topspace commands.
By default this is left empty for users to set with their own
preferred bindings.")
```

# Extra functions

```elisp
;;;###autoload
(defun topspace-height ()
  "Return the top space height in lines for current buffer in selected window.
The top space is the empty region in the buffer above the top text line.
The return value is of type float, and is equivalent to
the top space pixel height / `default-line-height'.

If the height does not exist yet, zero will be returned if
`topspace-autocenter-buffers' returns nil, otherwise a value that centers
the buffer will be returned according to `topspace-center-position'.

If the stored height is now invalid, it will first be corrected by
`topspace--correct-height' before being returned.
Valid top space line heights are:
- never negative,
- only positive when `window-start' equals 1,
  `topspace-active' returns non-nil, and `topspace-mode' is enabled,
- not larger than `topspace--window-height' minus `topspace--context-lines'."
...

;;;###autoload
(defun topspace-set-height (&optional total-lines)
  "Set and redraw the top space overlay to have a target height of TOTAL-LINES.
This sets the top space height for the current buffer in the selected window.
Integer or float values are accepted for TOTAL-LINES, and the value is
considered to be in units of `default-line-height'.

If argument TOTAL-LINES is not provided, the top space height will be set to
the value returned by `topspace-height', which can be useful when redrawing a
previously stored top space height in a window after a new buffer is
displayed in it, or when first setting the height to an initial default value
according to `topspace-height'.

If TOTAL-LINES is invalid, it will be corrected by `topspace--correct-height'.
Valid top space line heights are:
- never negative,
- only positive when `window-start' equals 1,
  `topspace-active' returns non-nil, and `topspace-mode' is enabled,
- not larger than `topspace--window-height' minus `topspace--context-lines'."
  (interactive "P")
...

;;;###autoload
(defun topspace-recenter-buffer (&optional position)
  "Add enough top space to center small buffers according to POSITION.
POSITION defaults to `topspace-center-position'.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height, or if `window-start' is greater
than 1.

If POSITION is a float, it represents the position to center buffer as a ratio
of frame height, and can be a value from 0.0 to 1.0 where lower values center
the buffer higher up in the screen.

If POSITION is a positive or negative integer value, buffer will be centered
by putting its center line at a distance of `topspace-center-position' lines
away from the top of the selected window when positive, or from the bottom
of the selected window when negative.
The distance will be in units of lines with height `default-line-height',
and the value should be less than the height of the window.

Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height, or if `window-start' is greater
than 1.

Customize `topspace-center-position' to adjust the default centering position.
Customize `topspace-autocenter-buffers' to run this command automatically
after first opening buffers and after window sizes change."
  (interactive)
...

;;;###autoload
(defun topspace-default-active ()
  "Default function that `topspace-active' is set to.
Return nil if the selected window is in a child-frame."
...

;;;###autoload
(defun topspace-default-autocenter-buffers ()
  "Default function that `topspace-autocenter-buffers' is set to.
Return nil if the selected window is in a child-frame or user has scrolled
buffer in selected window."
...

;;;###autoload
(defun topspace-default-empty-line-indicator ()
  "Default function that `topspace-empty-line-indicator' is set to.
Put the empty-line bitmap in fringe if `indicate-empty-lines' is non-nil.
This is done by adding a 'display property to the returned string.
The bitmap used is the one that the `empty-line' logical fringe indicator
maps to in `fringe-indicator-alist'."
...

;;;###autoload
(defun topspace-buffer-was-scrolled-p ()
  "Return t if current buffer has been scrolled in the selected window before.
This is provided since it is used in `topspace-default-autocenter-buffers'.
Scrolling is only recorded if topspace is active in the buffer at the time of
scrolling."
...
```

# How it works

The "upper margin" is created by drawing an [overlay](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html) before
window-start containing newline characters.  As you scroll down the
first line, more newline characters are added or removed accordingly.

No new keybindings are required as topspace automatically works for
any commands or subsequent function calls which use `scroll-up`,
`scroll-down`, or `recenter` as the underlying primitives for
scrolling.  This includes all scrolling commands/functions available
in Emacs as far as the author is aware. This is achieved by using
`advice-add` with the `scroll-up`, `scroll-down`, and `recenter`
commands so that custom topspace functions are called before or after
each time any of these other commands are called (interactively or
otherwise).

Fill out the [satisfaction survey](https://www.supersurvey.com/QRMU65MKE) to help the author know what you would like improved or added.

[1]: https://github.com/andre-r/centered-cursor-mode.el

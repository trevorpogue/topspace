<h1 align="center"> TopSpace </h1>
<p align="center">Scroll down & recenter top lines in Emacs.</p>

<!-- cursor -->

<p align="center">
  <a href="http://melpa.org/#/topspace"><img src="http://melpa.org/packages/topspace-badge.svg" height="20"/></a>
  <a href="http://stable.melpa.org/#/topspace"><img src="http://stable.melpa.org/packages/topspace-badge.svg" height="20"/></a>
  <a href="https://www.gnu.org/licenses/gpl-3.0"><img src="https://img.shields.io/badge/License-GPLv3-blue.svg" height="20"/></a>
</p>

<p align="center"><img src="https://user-images.githubusercontent.com/12535207/155176914-87390537-10f0-4ee5-9b37-cd798f07df27.gif"/></p>

TopSpace is an Emacs minor mode that allows you to scroll down and recenter top lines by automatically drawing an upper margin/padding above the top line
as you scroll down or recenter top text.

TopSpace is:

* **Easier on the eyes**: Recenter or scroll down top text to a more comfortable eye level for reading, especially when in full-screen or on a large monitor.
* **Easy to use**: No new keybindings are required, keep using all your previous scrolling & recentering commands, except now you can also scroll above the top lines. It also integrates seamlessly with  [centered-cursor-mode][1] to keep the cursor centered all the way to the top line.

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
### Just install and go
No new keybindings are required, keep using all your previous scrolling & recentering commands, except now you can also scroll above the top lines. 

# Extra commands

### `topspace-recenter-buffer`
* Add enough top space in the selected window to center small buffers.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.
Customize `topspace-center-position` to adjust the centering position.
Customize `topspace-autocenter-buffers` to run this command automatically
after first opening buffers and after window sizes change.

# Customization
```elisp
(defcustom topspace-autocenter-buffers t
  "Center small buffers with top space when first opened or window sizes change.
This is done by automatically calling `topspace-recenter-buffer'
and the positioning can be customized with `topspace-center-position'.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.
Customize `topspace-center-position' to adjust the centering position.

If non-nil, then always autocenter.  If nil, never autocenter.
If set to a predicate function (function that returns a boolean value),
then do auto-centering only when that function returns a non-nil value."
  :group 'topspace
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil)
                 (function :tag "predicate function")))

(defcustom topspace-center-position 0.4
  "Target position when centering buffers as a ratio of frame height.
A value from 0 to 1 where lower values center buffers higher up in the screen.
Used in `topspace-recenter-buffer' when called or when opening/resizing buffers
if `topspace-autocenter-buffers' is non-nil."
  :group 'topspace
  :type 'float)

(defcustom topspace-active t
  "Determine when `topspace-mode' mode is active / has any effect on buffer.
This is useful in particular when `global-topspace-mode' is enabled but you want
`topspace-mode' to be inactive in certain buffers or in any specific
circumstance.  When inactive, `topspace-mode' will still technically be on,
but will be effectively off and have no effect on the buffer.
Note that if `topspace-active' returns non-nil but `topspace-mode' is off,
`topspace-mode' will still be disabled.

If non-nil, then always be active.  If nil, never be active.
If set to a predicate function (function that returns a boolean value),
then be active only when that function returns a non-nil value."
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil)
                 (function :tag "predicate function")))

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

(defun topspace-default-empty-line-indicator ()
  "Put the empty-line bitmap in fringe if `indicate-empty-lines' is non-nil.
This is done by adding a 'display property to the returned string.
The bitmap used is the one that the `empty-line' logical fringe indicator
maps to in `fringe-indicator-alist'."
  (if indicate-empty-lines
      (let ((bitmap (catch 'tag (dolist (x fringe-indicator-alist)
                                  (when (eq (car x) 'empty-line)
                                    (throw 'tag (cdr x)))))))
        (propertize " " 'display (list `left-fringe bitmap `fringe)))
    ""))

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

# How it works under the hood

The "upper margin" is created by drawing an [overlay](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html) before
window-start containing newline characters.  As you scroll above the
top line, more newline characters are added or removed accordingly.

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

<h1 align="center"> TopSpace </h1>
<p align="center">Scroll down & recenter top lines in Emacs / get upper margins/padding.</p>

<!-- padding cursor -->

<p align="center">
  <a href="http://melpa.org/#/topspace"><img src="http://melpa.org/packages/topspace-badge.svg" height="20"/></a>
  <a href="http://stable.melpa.org/#/topspace"><img src="http://stable.melpa.org/packages/topspace-badge.svg" height="20"/></a>
  <a href="https://www.gnu.org/licenses/gpl-3.0"><img src="https://img.shields.io/badge/License-GPLv3-blue.svg" height="20"/></a>
</p>

<p align="center"><img src="https://user-images.githubusercontent.com/12535207/155176914-87390537-10f0-4ee5-9b37-cd798f07df27.gif"/></p>

TopSpace is an Emacs minor mode that allows you to scroll down and recenter top lines
by automatically drawing an upper margin/padding above the top line
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

# Customization
```elisp
(defcustom topspace-autocenter-buffers
  t
  "Center small buffers with top space when first opened or window sizes change.
This is done by automatically calling `topspace-recenter-buffer'
and the positioning can be customized with `topspace-center-position'.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.
Customize `topspace-center-position' to adjust the centering position.

If set to a predicate function (function that returns a boolean value),
then do auto-centering only when that function returns a non-nil value."
  :group 'topspace
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil)
                 (function :tag "predicate function")))

(defcustom topspace-center-position
  0.4
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

If t, then always be active.  If nil, never be active.
If set to a predicate function (function that returns a boolean value),
then be active only when that function returns a non-nil value."
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil)
                 (function :tag "predicate function")))

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

# Extra commands

### `topspace-recenter-buffer`
* Add enough top space in the selected window to center small buffers.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.
Customize `topspace-center-position` to adjust the centering position.
Customize `topspace-autocenter-buffers` to run this command automatically
after first opening buffers and after window sizes change.

# How it works under the hood

The "upper margin" is created by drawing an overlay before
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


[1]: https://github.com/andre-r/centered-cursor-mode.el

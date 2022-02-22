# TopSpace

[![MELPA](http://melpa.org/packages/topspace-badge.svg)](http://melpa.org/#/topspace)
[![MELPA Stable](http://stable.melpa.org/packages/topspace-badge.svg)](http://stable.melpa.org/#/topspace)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

**Scroll above the top line**

![topspace](https://user-images.githubusercontent.com/12535207/155176914-87390537-10f0-4ee5-9b37-cd798f07df27.gif)

TopSpace is an Emacs minor mode that lets you scroll above the top line to vertically center top text with a scrollable top margin/padding. In particular, it is useful when using Emacs in full-screen/on large monitors.

### Just install and go:

No new keybindings are required as `topspace` automatically works for any
commands or subsequent function calls which use `scroll-up`, `scroll-down`,
or `recenter` as the underlying primitives for scrolling. This includes all
scrolling commands/functions available in Emacs as far as the author is aware.

### How it works:

The top "margin" is created by drawing an [overlay](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html) before window-start which contains nothing but newline characters. As you scroll, more newline characters are added or removed accordingly.

Bonus: If you use [`centered-cursor-mode`][1], this means that `topspace` will automatically let you center the cursor all the way to the top line!

# Installation

TopSpace is available on [MELPA](http://melpa.org).
After [installing MELPA](https://melpa.org/#/getting-started) you can install TopSpace with the following command:

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `topspace` <kbd>[RET]</kbd>

If `topspace` did not appear here in `package-install` try running <kbd>M-x</kbd> `package-refresh-contents` and repeating the above step. Then enable TopSpace locally with <kbd>M-x</kbd> `topspace-mode`, or globally with <kbd>M-x</kbd> `global-topspace-mode`.
Alternatively, add `(global-topspace-mode 1)` to your Emacs config to enable `topspace-mode` globally on startup.

# Customization

```
(defcustom topspace-autocenter-buffers
  t
  "Vertically center small buffers when first opened or window sizes change.

This is done by automatically calling `topspace-recenter-buffer',
which adds enough top space to center small buffers.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.
Customize `topspace-center-position' to adjust the centering position."
  :group 'topspace
  :type 'boolean)

(defcustom topspace-center-position
  0.5
  "Target position when centering buffers as a ratio of frame height.
A value from 0 to 1 where lower values center buffers higher up in the screen.

Used in `topspace-recenter-buffer' when called or when opening/resizing buffers
if `topspace-autocenter-buffers' is non-nil."
  :group 'topspace
  :type 'float)
```

# Extra commands

### `topspace-recenter-buffer`:

Add enough top space in the selected window to center small buffers.

Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.

Customize `topspace-center-position` to adjust the centering position.
Customize `topspace-autocenter-buffers` to run this command automatically
after first opening buffers and after window sizes change.


  [1]: https://github.com/andre-r/centered-cursor-mode.el

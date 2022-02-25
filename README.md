<h1 align="center"> TopSpace </h1>
<p align="center">Scroll down and recenter the top lines in Emacs.</p>

<!-- padding cursor -->

<p align="center">
  <a href="http://melpa.org/#/topspace"><img src="http://melpa.org/packages/topspace-badge.svg" height="20"/></a>
  <a href="http://stable.melpa.org/#/topspace"><img src="http://stable.melpa.org/packages/topspace-badge.svg" height="20"/></a>
  <a href="https://www.gnu.org/licenses/gpl-3.0"><img src="https://img.shields.io/badge/License-GPLv3-blue.svg" height="20"/></a>
</p>

<p align="center"><img src="https://user-images.githubusercontent.com/12535207/155176914-87390537-10f0-4ee5-9b37-cd798f07df27.gif" /></a></p>

* **Easier on the eyes**: Recenter or scroll down top text to a more comfortable eye level for reading, especially when in full-screen or on a large monitor.
* **Easy to use**: No new keybindings are required, keep using all your previous scrolling & recentering commands, except now you can also scroll above the top lines. It also integrates seamlessly with  [centered-cursor-mode][1] to keep the cursor centered all the way to the top line.

# :hammer_and_wrench: Installation

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

# :gear: Customization
### `topspace-autocenter-buffer`
* Description: By default, small buffers will be vertically centered with top space when first opened by calling `topspace-recenter-buffer` (described below).
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.
Customize `topspace-center-position` (described below) to adjust the centering position.
* Default value: t
* Type: boolean
* How to modify: Disable this feature by adding the following to your Emacs config:
```
(custom-set-variables '(topspace-autocenter-buffers nil))
```

### `topspace-center-position`
* Description: Target position when centering buffers as a ratio of frame height. It must be a value from 0 to 1 where lower values center buffers higher up in the screen. Used in `topspace-recenter-buffer` (described below) when called or when opening/resizing buffers if `topspace-autocenter-buffers` is non-nil.
* Default value: 0.4
* Type: float
* How to modify: Add the following to your Emacs config:
```
(custom-set-variables '(topspace-center-position <custom value>))
```

# :hammer_and_pick: Extra commands

### `topspace-recenter-buffer`
* Add enough top space in the selected window to center small buffers.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.
Customize `topspace-center-position` to adjust the centering position.
Customize `topspace-autocenter-buffers` to run this command automatically
after first opening buffers and after window sizes change.

[1]: https://github.com/andre-r/centered-cursor-mode.el

<h1 align="center"> TopSpace </h1>
<p align="center">Scroll above the top line.</p>

<p align="center">
  <a href="http://melpa.org/#/topspace"><img src="http://melpa.org/packages/topspace-badge.svg" height="20"/></a>
  <a href="http://stable.melpa.org/#/topspace"><img src="http://stable.melpa.org/packages/topspace-badge.svg" height="20"/></a>
  <a href="https://www.gnu.org/licenses/gpl-3.0"><img src="https://img.shields.io/badge/License-GPLv3-blue.svg" height="20"/></a>
</p>

<p align="center"><img src="https://user-images.githubusercontent.com/12535207/155176914-87390537-10f0-4ee5-9b37-cd798f07df27.gif" /></a></p>

TopSpace is an Emacs minor mode that lets you scroll above the top line to vertically center the top text or cursor with a scrollable top margin/padding. In particular, it is useful when using Emacs in full-screen/on large monitors. TopSpace is:

* **Easy to setup**:
No new keybindings are required, keep using all your previous scrolling commands.

* **Compatible with [centered-cursor-mode][1]**:
Center the cursor all the way to the top line!

* **Using overlays**:
The top "margin" is created by drawing an [overlay](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html) before window-start which contains nothing but newline characters. As you scroll, more newline characters are added or removed accordingly.

# :hammer_and_wrench: Installation

TopSpace is available on [MELPA](http://melpa.org).
After [installing MELPA](https://melpa.org/#/getting-started) you can install TopSpace with the following command:

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `topspace` <kbd>[RET]</kbd>

If `topspace` did not appear here in `package-install` try running <kbd>M-x</kbd> `package-refresh-contents` and repeating the above step. Then enable TopSpace locally with <kbd>M-x</kbd> `topspace-mode`, or globally with <kbd>M-x</kbd> `global-topspace-mode`.
Alternatively, add `(global-topspace-mode 1)` to your Emacs config to enable `topspace-mode` globally on startup.

# :gear: Customization
By default, small buffers will be vertically centered with top space when first opened. To disable this feature, simply add the following to your Emacs config:
```
(custom-set-variables '(topspace-autocenter-buffers nil))
```
More options/detail:
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
  0.4
  "Target position when centering buffers as a ratio of frame height.
A value from 0 to 1 where lower values center buffers higher up in the screen.

Used in `topspace-recenter-buffer' when called or when opening/resizing buffers
if `topspace-autocenter-buffers' is non-nil."
  :group 'topspace
  :type 'float)
```

# :hammer_and_pick: Extra commands

### `topspace-recenter-buffer`:

Add enough top space in the selected window to center small buffers.

Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.

Customize `topspace-center-position` to adjust the centering position.
Customize `topspace-autocenter-buffers` to run this command automatically
after first opening buffers and after window sizes change.

# :chains:  Supporters
### &#8627; Stargazers
[![Stargazers repo roster for @trevorpogue/topspace](https://reporoster.com/stars/trevorpogue/topspace)](https://github.com/trevorpogue/topspace/stargazers)

### &#8627; Forkers
[![Forkers repo roster for @trevorpogue/topspace](https://reporoster.com/forks/trevorpogue/topspace)](https://github.com/trevorpogue/topspace/network/members)
<p align="center"><a href="https://github.com/trevorpogue/topspace#"><img src="http://randojs.com/images/barsSmallTransparentBackground.gif" alt="Animated footer bars" width="100%"/></a></p>
<br/>
<p align="center"><a href="https://github.com/trevorpogue/topspace#"><img src="http://randojs.com/images/backToTopButtonTransparentBackground.png" alt="Back to top" height="29"/></a></p>

  [1]: https://github.com/andre-r/centered-cursor-mode.el

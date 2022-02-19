# TopSpace
**Scroll above the top line**

![topspace](https://user-images.githubusercontent.com/12535207/154770200-0b3edcd8-8036-40c7-910f-d5b3a1c3b4df.gif)


TopSpace is an Emacs minor mode that lets you scroll above the top line to vertically center top text.

No new keybindings are required as `topspace` automatically works for any
commands or subsequent function calls which use `scroll-up`, `scroll-down`,
or `recenter` as the underlying primitives for scrolling. This includes all
scrolling commands/functions available in Emacs as far as the author is aware.

Bonus: If you use [`centered-cursor-mode`][1], this means that `topspace` will automatically let you center the cursor all the way to the top line!

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

# TopSpace
**Scroll above the top line**

![topspace](https://user-images.githubusercontent.com/12535207/154770200-0b3edcd8-8036-40c7-910f-d5b3a1c3b4df.gif)


TopSpace is an emacs minor mode that lets you scroll above the top line to vertically center top text.

No new keybindings are required as `topspace` automatically works for any
commands or subsequent function calls which use `scroll-up`, `scroll-down`,
or `recenter` as the underlying primitives for scrolling. This includes all
scrolling commands/functions available in Emacs that the author is aware of.

Bonus: If you use [`centered-cursor-mode`][3], this means that `topspace` will automatically let you center the cursor all the way to the top line!

# Customization

```
(defcustom topspace-autocenter-buffers
  t
  "Vertically center small buffers when first opened or window sizes change."
  :group 'topspace
  :type 'boolean)

(defcustom topspace-center-position
  0.5
  "Suggested position when centering buffers as a ratio of frame height.
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

<!-- # Installation -->
<!-- Save the file from this repository named `"topspace.el"` into a directory of your choice, then install it using [`use-package`][2] by adding the following lines to your [`init file`][1]: -->

<!-- ``` -->
<!-- (use-package topspace :load-path "<directory>") -->
<!-- ``` -->

<!-- Above, `<directory>` should be the path to the directory in which you saved the `"topspace.el"` file. -->

<!-- # Usage -->
<!-- After restarting Emacs, you can now activate or deactivate the mode by typing <kbd>M-x topspace-mode</kbd>. -->

<!-- You can also globally enable the minor mode either manually by typing <kbd>M-x global-topspace-mode</kbd>, or on init by changing the `use-package` code to the following to your init file: -->

<!-- ``` -->
<!-- (use-package topspace -->
<!--   :load-path "<directory>" -->
<!--   :config (global-topspace-mode)) -->
<!-- ``` -->


  [1]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
  [2]: https://github.com/jwiegley/use-package
  [3]: https://github.com/emacsmirror/centered-cursor-mode

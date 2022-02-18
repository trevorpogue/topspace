# TopSpace
**Scroll above the top line**

![topspace](https://user-images.githubusercontent.com/12535207/154770200-0b3edcd8-8036-40c7-910f-d5b3a1c3b4df.gif)


Topspace is an emacs minor mode that lets you scroll above the top line to vertically center top text.

No new keybindings are required as topspace automatically works for any
commands or subsequent function calls which use `scroll-up`, `scroll-down`,
or `recenter` as the underlying primitives for scrolling. This includes all
scrolling commands/functions available in Emacs that the author is aware of.

Bonus: If you use [`centered-cursor-mode`][3], this means that `topspace` will automatically let you center the cursor all the way to the top line!


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

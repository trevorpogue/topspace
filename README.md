# vertical-center-mode
### An Emacs minor-mode
Automatically center buffers vertically in the window after opening files and during editing. Users can also adjust the centering offset with scrolling to further scroll up or down by any amount **above** the top lines in a buffer.

Scrolling is currently supported when using the `scroll-down-line`/`scroll-up-line`, or `evil-scroll-line-up`/`evil-scroll-line-down`.

Scrolling also integrates well with `centered-cursor-mode`, allowing the cursor to stay centered all the way to the top line when moving the cursor with `previous-line` and `next-line`.

# Installation
Save the file from this repository named `"vertical-center-mode.el"` into a directory of your choice, then add the following lines to your [`.emacs`][1] file if not using Spacemacs:

	(setq load-path (append load-path "<directory>"))
	(require 'use-package)

If using Spacemacs, add the following to your `dotspacemacs/user-config`:

```
(eval-when-compile
	(add-to-list 'load-path "<directory>")
	(require 'use-package))
(use-package vertical-center-mode)
```

Above, `<directory>` should be the path to the directory in which you saved the `"vertical-center-mode.el"` file.

# Usage
After restarting Emacs, you can now activate or deactivate the mode by typing <kbd>M-x vertical-center-mode</kbd>.

You can also globally enable the minor mode either manually by typing <kbd>M-x global-vertical-center-mode</kbd>, or on init by adding the following to your init file.

`(global-vertical-center-mode 1)`

# Contributions
The initial implementation ideas and name for this minor-mode originated from [this answer](https://stackoverflow.com/a/66678124/3705784) in a Stack Overflow thread.

  [1]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

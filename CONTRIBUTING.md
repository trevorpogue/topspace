# Contributing

Contributions are welcome! Please [open an issue][1]
if you find a bug, have a feature request, or have any suggestions for
the project at all.
Also feel free to make pull requests yourself instead of issues,
or make pull requests for any existing unresolved issues.
Try to follow the following guidelines if you open an issue or pull request:

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `main`).
* Be clear, concise and precise in your description of the problem.
* Open an issue with a descriptive title and a summary in grammatically correct,
  complete sentences.
* Include any relevant code to the issue summary.
* If you're reporting performance issues it'd be nice if you added some profiling data (Emacs has a built-in profiler).

## Pull requests

* Add tests (if possible) to cover your change(s)
* Follow the [Emacs Lisp conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html) and the [Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide)
* Read [how to properly contribute to open source projects on Github][2].
* Use a topic branch to easily amend a pull request later, if necessary.
* Write [good commit messages][3].
* Mention related tickets in the commit messages (e.g. `[Fix #N] Add missing autoload cookies`)
* Use the same coding conventions as the rest of the project.
* Verify your Emacs Lisp code with `checkdoc` (<kbd>C-c ? d</kbd>).
* Open a [pull request][4] that relates to *only* one subject with a clear title
  and description in grammatically correct, complete sentences.

## Running Tests
Check [test.yml][5] to see the latest testing frameworks being used, and the command(s) required to run the tests. When last updating this file, cask and buttercup were being used to run the tests like so:

```
cask install
cask exec buttercup -L .
```

[1]: https://github.com/trevorpogue/topspace/issues
[2]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://help.github.com/articles/using-pull-requests
[5]: https://github.com/trevorpogue/topspace/blob/main/.github/workflows/test.yml

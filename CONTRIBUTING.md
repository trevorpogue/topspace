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
Check [ci.yml][5] to see all the details for the testing frameworks being used, the Emacs versions being tested, and the command(s) required to run the tests. 
TLDR: Cask and buttercup are used for testing and can be run like so:

```
cask install
cask exec buttercup -L .
```

## Copyright Assignment
This package is subject to the same [Copyright Assignment](https://www.gnu.org/prep/maintain/html_node/Copyright-Papers.html) policy as
GNU Emacs and all other packages in [GNU ELPA](https://elpa.gnu.org/packages/).

Any [legally significant](https://www.gnu.org/prep/maintain/html_node/Legally-Significant.html#Legally-Significant) contributions can only be accepted after the
author has completed their paperwork.  Please see [the request form](https://git.savannah.gnu.org/cgit/gnulib.git/tree/doc/Copyright/request-assign.future) if
you want to proceed with the assignment.

The copyright assignment isn't a big deal, it just says that the
copyright for your submitted changes to Emacs belongs to the FSF.
This assignment works for all projects related to Emacs.  To obtain it
you need to:
* Send one email
* Send one letter (if you live in the US, it's digital)
* Wait for some time (recently it's less than a week)

[1]: https://github.com/trevorpogue/topspace/issues
[2]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://help.github.com/articles/using-pull-requests
[5]: https://github.com/trevorpogue/topspace/blob/main/.github/workflows/ci.yml

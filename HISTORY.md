## [v0.1.2](https://github.com/trevorpogue/topspace/tree/v0.1.2) (2022-03-01)

[Full Changelog](https://github.com/trevorpogue/topspace/compare/v0.1.1...v0.1.2)

**Fixed bugs:**
* [#2](https://github.com/trevorpogue/topspace/pull/2): Make `recenter-top-bottom` act correctly when it moves point to bottom and top space is added to get there

**Other changes:**

* [2584138](https://github.com/trevorpogue/topspace/commit/25841387a5d0300ea49356b9781c357b84df20bd): Raise topspace-center-position default to a subjectively better position

## [v0.1.1](https://github.com/trevorpogue/topspace/tree/v0.1.1) (2022-02-22)

[Full Changelog](https://github.com/trevorpogue/topspace/compare/v0.1.0...v0.1.1)

**Fixed bugs:**

* [4a69b2e](https://github.com/trevorpogue/topspace/commit/4a69b2eb741f8db9d69169a03a6724af0f2ec7ac): Allow recenter and recenter-top-bottom to be called interactively without an error
* [4eb27ab](https://github.com/trevorpogue/topspace/commit/4eb27abaa182e856ba3f3c8e1e84fdd2e1f009af): Prevent top space from all suddenly disappearing when visual-line-mode is enabled and cursor scrolls bellow window-end when top space is present

## [v0.1.0](https://github.com/trevorpogue/topspace/tree/v0.1.0) (2022-02-19)

[Full Changelog](https://github.com/trevorpogue/topspace/compare/79aa4e78d3f5c90fc9db46d597f1680c7900b52a...v0.1.0)

**Implemented enhancements:**

* [#1](https://github.com/trevorpogue/topspace/pull/1): Make mode work for any scrolling command by using add-advice with scroll-up, scroll-down, and recenter


**Fixed bugs:**

* [#1](https://github.com/trevorpogue/topspace/pull/1): Stabilize, clean up, and add performance optimizations to code to make it ready for submission to MELPA

**Other changes:**

* [e5b65ec](https://github.com/trevorpogue/topspace/commit/e5b65eccf92571163aa1b6bd738be22d8e0ad1a5): Change project name from vertical-center-mode to topspace

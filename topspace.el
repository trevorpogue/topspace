;;; topspace.el --- Scroll down & recenter top lines / get upper margins/padding -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Trevor Edwin Pogue

;; Author: Trevor Edwin Pogue <trevor.pogue@gmail.com>
;; Maintainer: Trevor Edwin Pogue <trevor.pogue@gmail.com>
;; URL: https://github.com/trevorpogue/topspace
;; Keywords: convenience, scrolling, center, cursor, margin, padding
;; Version: 0.1.2
;; Package-Requires: ((emacs "25.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; TopSpace allows you to scroll down and recenter top lines
;; by automatically drawing an upper margin/padding above the top line
;; as you scroll down or recenter top text.

;; TopSpace is:

;; - Easier on the eyes: Recenter or scroll down top text to a more
;;   comfortable eye level for reading, especially when in full-screen
;;   or on a large monitor.

;; - Easy to use: No new keybindings are required, keep using all
;;   your previous scrolling & recentering commands, except now you
;;   can also scroll above the top lines.  It also integrates
;;   seamlessly with `centered-cursor-mode' to keep the cursor
;;   centered all the way to the top line.

;; How it works under the hood:
;; The "upper margin" is created by drawing an overlay before
;; window-start containing newline characters.  As you scroll above the
;; top line, more newline characters are added or removed accordingly.

;; No new keybindings are required as topspace automatically works for
;; any commands or subsequent function calls which use `scroll-up',
;; `scroll-down', or `recenter' as the underlying primitives for
;; scrolling.  This includes all scrolling commands/functions available
;; in Emacs as far as the author is aware.  This is achieved by using
;; `advice-add' with the `scroll-up', `scroll-down', and `recenter'
;; commands so that custom topspace functions are called before or after
;; each time any of these other commands are called (interactively or
;; otherwise).

;; See the readme at https://github.com/trevorpogue/topspace for more
;; information.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private variables

(defvar-local topspace--heights '()
  "Stores top space heights of each window that buffer has been selected in.")

(defvar-local topspace--autocenter-heights '()
  "Stores the top space heights needed to center small buffers.
A value is stored for each window that the buffer has been selected in.")

(defvar-local topspace--previous-window-heights '()
  "Stores the window heights of each window that buffer has been selected in.")

(defvar-local topspace--window-start-before-scroll 2
  "Helps to identify if more top space must be drawn after scrolling up.")

(defvar-local topspace--total-lines-scrolling 0
  "Stores the total lines that the user is scrolling until scroll is complete.")

(defvar-local topspace--pre-command-point 1
  "Used for performance improvement by abandoning extra calculations.
In the post command hook, this determines if point moved further than the
window height, in which case there is no point checking if the top space
should be reduced in size or not.")

(defvar-local topspace--pre-command-window-start 2
  "Used for performance improvement by abandoning extra calculations.
In the post command hook, this determines if any top space was present
before the command, otherwise there is no point checking if the top
space should be reduced in size or not")

(defvar-local topspace--got-first-window-configuration-change nil
  "Displaying top space before the first window config change can cause errors.
This flag signals to wait until then to display top space.")

(defvar topspace--advice-added nil "Keep track if `advice-add` done already.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization

(defgroup topspace nil
  "Scroll down & recenter top lines / get upper margins/padding."
  :group 'scrolling
  :group 'convenience
  :link '(emacs-library-link :tag "Source Lisp File" "topspace.el")
  :link '(url-link "https://github.com/trevorpogue/topspace")
  :link '(emacs-commentary-link :tag "Commentary" "topspace"))

(defcustom topspace-autocenter-buffers t
  "Center small buffers with top space when first opened or window sizes change.
This is done by automatically calling `topspace-recenter-buffer'
and the positioning can be customized with `topspace-center-position'.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.
Customize `topspace-center-position' to adjust the centering position.

If non-nil, then always autocenter.  If nil, never autocenter.
If set to a predicate function (function that returns a boolean value),
then do auto-centering only when that function returns a non-nil value."
  :group 'topspace
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil)
                 (function :tag "predicate function")))

(defcustom topspace-center-position 0.4
  "Target position when centering buffers as a ratio of frame height.
A value from 0 to 1 where lower values center buffers higher up in the screen.
Used in `topspace-recenter-buffer' when called or when opening/resizing buffers
if `topspace-autocenter-buffers' returns non-nil."
  :group 'topspace
  :type 'float)

(defcustom topspace-active t
  "Determine when `topspace-mode' mode is active / has any effect on buffer.
This is useful in particular when `global-topspace-mode' is enabled but you want
`topspace-mode' to be inactive in certain buffers or in any specific
circumstance.  When inactive, `topspace-mode' will still technically be on,
but will be effectively off and have no effect on the buffer.
Note that if `topspace-active' returns non-nil but `topspace-mode' is off,
`topspace-mode' will still be disabled.

If non-nil, then always be active.  If nil, never be active.
If set to a predicate function (function that returns a boolean value),
then be active only when that function returns a non-nil value."
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil)
                 (function :tag "predicate function")))

(defcustom topspace-empty-line-indicator
  #'topspace-default-empty-line-indicator
  "Text that will appear in each empty topspace line above the top text line.
Can be set to either a constant string or a function that returns a string.

By default it will show the empty-line bitmap in the left fringe
if `indicate-empty-lines' is non-nil, otherwise nothing.
The default bitmap is the one that the `empty-line' logical fringe indicator
maps to in `fringe-indicator-alist'.
This is done by adding a 'display property to the string (see
`topspace-default-empty-line-indicator' for more details).

 You can alternatively show a string in the body of each top space line by
having `topspace-empty-line-indicator' return a string without the 'display
property added.  If you do this you may be interested in also changing the
string's face like so: (propertize indicator-string 'face 'fringe)."
  :type '(choice 'string (function :tag "String function")))

(defun topspace-default-empty-line-indicator ()
  "Put the empty-line bitmap in fringe if `indicate-empty-lines' is non-nil.

The bitmap used is the one that the `empty-line' logical fringe indicator
maps to in `fringe-indicator-alist'."
  (if indicate-empty-lines
      (let ((bitmap (catch 'tag
                      (dolist (x fringe-indicator-alist)
                        (when (eq (car x) 'empty-line)
                          (throw 'tag (cdr x)))))))
      (propertize " " 'display (list `left-fringe bitmap `fringe)))
    ""))

(defcustom topspace-mode-line " T"
  "Mode line lighter for Topspace.
The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
more information.  Note that it should contain a _single_ mode
line construct only.
Set this variable to nil to disable the mode line completely."
  :group 'topspace
  :type 'sexp)

(defvar topspace-keymap (make-sparse-keymap)
  "Keymap for Topspace commands.
By default this is left empty for users to set with their own
preferred bindings.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advice for `scroll-up', `scroll-down', and `recenter'

(defun topspace--scroll (total-lines)
  "Run before `scroll-up'/`scroll-down' for scrolling above the top line.
TOTAL-LINES is used in the same way as in `scroll-down'."
  (let ((old-topspace-height (topspace--height))
        (new-topspace-height))
    (setq new-topspace-height (topspace--correct-height
                               (+ old-topspace-height total-lines)))
    (setq topspace--window-start-before-scroll (window-start))
    (topspace--draw new-topspace-height)
    (- total-lines (- new-topspace-height old-topspace-height))))

(defun topspace--filter-args-scroll-down (&optional total-lines)
  "Run before `scroll-down' for scrolling above the top line.
TOTAL-LINES is used in the same way as in `scroll-down'."
  (cond
   ((not (topspace--enabled)) total-lines)
   ((setq total-lines (car total-lines))
    (setq total-lines (or total-lines (- (topspace--window-height)
                                         next-screen-context-lines)))
    (setq topspace--total-lines-scrolling total-lines)
    (list (topspace--scroll total-lines)))))

(defun topspace--filter-args-scroll-up (&optional total-lines)
  "Run before `scroll-up' for scrolling above the top line.
TOTAL-LINES is used in the same way as in `scroll-up'."
  (cond
   ((not (topspace--enabled)) total-lines)
   ((setq total-lines (car total-lines))
    (setq total-lines (* (or total-lines (- (topspace--window-height)
                                            next-screen-context-lines)) -1))
    (setq topspace--total-lines-scrolling total-lines)
    (list (* (topspace--scroll total-lines) -1)))))

(defun topspace--after-scroll (&optional total-lines)
  "Run after `scroll-up'/`scroll-down' for scrolling above the top line.
TOTAL-LINES is used in the same way as in `scroll-down'.
This is needed when scrolling down (moving buffer text lower in the screen)
and no top space was present before scrolling but it should be after scrolling.
The reason this is needed is because `topspace--draw' only draws the overlay
when `window-start` equals 1, which can only be true after the scroll command is
run
in the described case above."
  (cond
   ((not (topspace--enabled)))
   ((setq total-lines topspace--total-lines-scrolling)
    (when (and (> topspace--window-start-before-scroll 1) (= (window-start) 1))
      (let ((lines-already-scrolled (count-screen-lines
                                     1 topspace--window-start-before-scroll)))
        (setq total-lines (abs total-lines))
        (set-window-start (selected-window) 1)
        (topspace--draw (- total-lines lines-already-scrolled)))))))

(defun topspace--after-recenter (&optional line-offset redisplay)
  "Recenter near the top of buffers by adding top space appropriately.
LINE-OFFSET and REDISPLAY are used in the same way as in `recenter'."
  ;; redisplay is unused but needed since this function
  ;; must take the same arguments as `recenter'
  redisplay  ; remove flycheck warning for unused argument (see above)
  (cond
   ((not (topspace--enabled)))
   ((when (= (window-start) 1)
      (unless line-offset
        (setq line-offset (round (/ (topspace--window-height) 2))))
      (when (< line-offset 0)
        ;; subtracting 3 below made `recenter-top-bottom' act correctly
        ;; when it moves point to bottom and top space is added to get there
        (setq line-offset (- (- (topspace--window-height) line-offset) 3)))
      (topspace--draw (- line-offset (topspace--count-screen-lines
                                      (window-start)
                                      (point))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top space line height calculation

(defun topspace--set-height (height)
  "Set the top space line height for the selected window to HEIGHT.
Will only set to HEIGHT if HEIGHT is a valid value based on (window-start)."
  (setf (alist-get (selected-window) topspace--heights)
        (topspace--correct-height height)))

(defun topspace--height ()
  "Get the top space line height for the selected window.
If the existing value is invalid, set and return a valid value.
If no previous value exists, return the appropriate value to
 center the buffer when `topspace-autocenter-buffers' returns non-nil, else 0."
  (let ((height) (window (selected-window)))
    (setq height (alist-get window topspace--heights))
    (unless (or height (topspace--recenter-buffers-p)) (setq height 0))
    (when height (topspace--set-height (topspace--correct-height height)))
    (when (and (not height) (topspace--recenter-buffers-p))
      (setq height (alist-get (selected-window) topspace--autocenter-heights))
      (unless height (setq height (topspace--height-to-make-buffer-centered)))
      (setq height (topspace--correct-height height))
      (setf (alist-get window topspace--heights) height))
    height))

(defun topspace--correct-height (height)
  "Return HEIGHT if a valid top space line height, else a valid value.
Valid top space line heights are:
- never negative,
- only positive when `window-start' equals 1,
- not larger than `topspace--window-height' minus `next-screen-context-lines'."
  (let ((max-height (- (topspace--window-height) next-screen-context-lines)))
    (when (> (window-start) 1) (setq height 0))
    (when (< height 0) (setq height 0))
    (when (> height max-height) (setq height max-height)))
  height)

(defun topspace--total-lines-past-max (&optional topspace-height)
  "Used when making sure top space height does not push cursor off-screen.
Return how many lines past the bottom of the window the cursor would get pushed
if setting the top space to the target value TOPSPACE-HEIGHT.
Any value above 0 flags that the target TOPSPACE-HEIGHT is too large."
  (- (topspace--current-line-plus-topspace topspace-height)
     (- (topspace--window-height) next-screen-context-lines)))

(defun topspace--current-line-plus-topspace (&optional topspace-height)
  "Used when making sure top space height does not push cursor off-screen.
Return the current line plus the top space height TOPSPACE-HEIGHT."
  (+ (count-screen-lines (window-start) (point))
     (or topspace-height (topspace--height))))

(defun topspace--height-to-make-buffer-centered ()
  "Return the necessary top space height to center selected window's buffer."
  (let ((buffer-height (count-screen-lines (window-start) (window-end)))
        (result)
        (window-height (topspace--window-height)))
    (setq result (- (- (topspace--center-frame-line)
                       (round (/ buffer-height 2)))
                    (window-top-line (selected-window))))
    (when (> (+ result buffer-height) (- window-height
                                         next-screen-context-lines))
      (setq result (- (- window-height buffer-height)
                      next-screen-context-lines)))
    result))

(defun topspace--center-frame-line ()
  "Return a center line number based on `topspace-center-position'.
The return value is only valid for windows starting at the top of the frame,
which must be accounted for in the calling functions."
  (round (* (frame-text-lines) topspace-center-position)))

(defun topspace--recenter-buffers-p ()
  "Return non-nil if buffer is allowed to be auto-centered.
Buffers will not be auto-centered if `topspace-autocenter-buffers' returns nil
or if the selected window is in a child-frame."
  (and (topspace--eval-choice topspace-autocenter-buffers)
       (or ;; frame-parent is only provided in Emacs 26.1, so first check
        ;; if fhat function is fboundp.
        (not (fboundp 'frame-parent))
        (not (frame-parent)))))

(defun topspace--window-height ()
  "Return the number of screen lines in the selected window rounded up."
  (ceiling (window-screen-lines)))

(defun topspace--count-screen-lines (start end)
  "Return screen lines between START and END.
Like `count-screen-lines' except `count-screen-lines' will
return unexpected value when END is in column 0. This fixes that issue."
  (let ((adjustment 0) (column 0))
    (save-excursion
      (goto-char end)
      (setq column (car (nth 6 (posn-at-point))))
      (unless (= column 0) (setq adjustment -1)))
    (+ (count-screen-lines start end) adjustment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay drawing

(defun topspace--text (height)
  "Return the topspace text that appears in the top overlay with height HEIGHT."
  (let ((text "")
        (indicator-line (topspace--eval-choice
                         topspace-empty-line-indicator)))
    (setq indicator-line (cl-concatenate 'string indicator-line "\n"))
    (when (> height 0)
      (dotimes (n height)
        n ;; remove flycheck warning
        (setq text (cl-concatenate 'string text indicator-line)))
      text)))

(defun topspace--draw (&optional height)
  "Draw top space as an overlay with the target line height HEIGHT."
  (let ((old-height))
    (unless (topspace--enabled) (setq height 0) (setq old-height 0))
    (unless old-height (setq old-height (topspace--height)))
    (when height (setq height (topspace--set-height height)))
    (when (not height) (setq height old-height))
    (when (and (> height 0) (> height old-height))
      (let ((lines-past-max (topspace--total-lines-past-max height)))
        (when (> lines-past-max 0) (forward-line (* lines-past-max -1)))))
    (let ((topspace (make-overlay 1 1)))
      (remove-overlays 1 1 'topspace--remove-from-window-tag
                       (selected-window))
      (overlay-put topspace 'window (selected-window))
      (overlay-put topspace 'topspace--remove-from-window-tag
                   (selected-window))
      (overlay-put topspace 'topspace--remove-from-buffer-tag t)
      (overlay-put topspace 'before-string (topspace--text height)))
    height))

(defun topspace--draw-increase-height (total-lines)
  "Increase the top space line height by the target amount of TOTAL-LINES."
  (topspace--draw (+ (topspace--height) total-lines)))

(defun topspace--draw-decrease-height (total-lines)
  "Decrease the top space line height by the target amount of TOTAL-LINES."
  (topspace--draw (- (topspace--height) total-lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities

(defun topspace--eval-choice (variable-or-function)
  "Evaluate VARIABLE-OR-FUNCTION which is either var or func'n of type var.
If it is a variable, return its value, if it is a function,
evaluate the function and return its return value.
VARIABLE-OR-FUNCTION is most likely a user customizable variable of choice
type."
  (cond ((fboundp variable-or-function)
         (funcall variable-or-function))
        (t variable-or-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hooks

(defun topspace--window-configuration-change ()
  "Update top spaces when window buffers change or windows are resized."
  (setq topspace--got-first-window-configuration-change t)
  (let ((current-height (topspace--window-height)) (window (selected-window)))
    (let ((previous-height (alist-get window topspace--previous-window-heights
                                      current-height)))
      (if (and (topspace--recenter-buffers-p)
               (not (= previous-height current-height)))
          (topspace-recenter-buffer)
        (topspace--draw))
      (setf (alist-get window topspace--previous-window-heights)
            current-height))))

(defun topspace--pre-command ()
  "Reduce the amount of code that must execute in `topspace--post-command'."
  (setq-local topspace--pre-command-point (window-start))
  (setq-local topspace--pre-command-window-start (window-start)))

(defun topspace--post-command ()
  "Gradually reduce top space before the cursor will move past the bottom."
  (when (and (= topspace--pre-command-window-start 1)
             (< (- (line-number-at-pos (point))
                   (line-number-at-pos topspace--pre-command-point))
                (topspace--window-height)))
    (let ((topspace-height (topspace--height)) (total-lines-past-max))
      (when (> topspace-height 0)
        (setq total-lines-past-max (topspace--total-lines-past-max
                                    topspace-height))
        (when (> total-lines-past-max 0)
          (topspace--draw-decrease-height total-lines-past-max)))))
  (when (and (= (window-start) 1)
             topspace--got-first-window-configuration-change)
    (topspace--draw)))

(defvar topspace--hook-alist
  '((window-configuration-change-hook . topspace--window-configuration-change)
    (pre-command-hook . topspace--pre-command)
    (post-command-hook . topspace--post-command))
  "A list of hooks to add/remove in the format (hook-variable . function).")

(defun topspace--add-hooks ()
  "Add hooks defined in `topspace--hook-alist'."
  (dolist (hook-func-pair topspace--hook-alist)
    (add-hook (car hook-func-pair) (cdr hook-func-pair) 0 t)))

(defun topspace--remove-hooks ()
  "Remove hooks defined in `topspace--hook-alist'."
  (dolist (hook-func-pair topspace--hook-alist)
    (remove-hook (car hook-func-pair) (cdr hook-func-pair) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User functions

;;;###autoload
(defun topspace-recenter-buffer ()
  "Add enough top space in the selected window to center small buffers.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height.
Customize `topspace-center-position' to adjust the centering position.
Customize `topspace-autocenter-buffers' to run this command automatically
after first opening buffers and after window sizes change."
  (interactive)
  (let ((center-height (topspace--height-to-make-buffer-centered)))
    (setf (alist-get (selected-window) topspace--autocenter-heights)
          center-height)
    (topspace--draw center-height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definition and setup

(defun topspace--enable-p ()
  "Return non-nil if buffer is allowed to enable `topspace-mode.'.
Topspace will not be enabled for:
- minibuffers
- ephemeral buffers (See Info node `(elisp)Buffer Names')
- if `topspace-mode' is already enabled"
  (not (or (minibufferp) (string-prefix-p " " (buffer-name)))))

(defun topspace--enable ()
  "Enable `topspace-mode' and do mode setup."
  (when (topspace--enable-p)
    (topspace--add-hooks)
    (unless topspace--advice-added
      (setq topspace--advice-added t)
      (advice-add #'scroll-up :filter-args #'topspace--filter-args-scroll-up)
      (advice-add #'scroll-down :filter-args
                  #'topspace--filter-args-scroll-down)
      (advice-add #'scroll-up :after #'topspace--after-scroll)
      (advice-add #'scroll-down :after #'topspace--after-scroll)
      (advice-add #'recenter :after #'topspace--after-recenter))
    (dolist (window (get-buffer-window-list))
      (with-selected-window window (topspace--draw)))))

(defun topspace--disable ()
  "Disable `topspace-mode' and do mode cleanup."
  (remove-overlays 1 1 'topspace--remove-from-buffer-tag t)
  (topspace--remove-hooks))

;;;###autoload
(define-minor-mode topspace-mode
  "Scroll down & recenter top lines / get upper margins/padding.

TopSpace allows you to scroll down and recenter top lines
by automatically drawing an upper margin/padding above the top line
as you scroll down or recenter top text.

TopSpace is:

- Easier on the eyes: Recenter or scroll down top text to a more
  comfortable eye level for reading, especially when in full-screen
  or on a large monitor.

- Easy to use: No new keybindings are required, keep using all
  your previous scrolling & recentering commands, except now you
  can also scroll above the top lines.  It also integrates
  seamlessly with `centered-cursor-mode' to keep the cursor
  centered all the way to the top line.

How it works under the hood:
The \"upper margin\" is created by drawing an overlay before
window-start containing newline characters.  As you scroll above the
top line, more newline characters are added or removed accordingly.

No new keybindings are required as topspace automatically works for
any commands or subsequent function calls which use `scroll-up',
`scroll-down', or `recenter' as the underlying primitives for
scrolling.  This includes all scrolling commands/functions available
in Emacs as far as the author is aware.  This is achieved by using
`advice-add' with the `scroll-up', `scroll-down', and `recenter'
commands so that custom topspace functions are called before or after
each time any of these other commands are called (interactively or
otherwise).

See the readme at https://github.com/trevorpogue/topspace for more
information.

Enabling/disabling:
When called interactively, toggle `topspace-mode'.

With prefix ARG, enable `topspace-mode' if
ARG is positive, otherwise disable it.

When called from Lisp, enable `topspace-mode' if
ARG is omitted, nil or positive.

If ARG is `toggle', toggle `topspace-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :ligher topspace-mode-line
  :keymap topspace-keymap
  :group 'topspace
  (if topspace-mode (topspace--enable) (topspace--disable)))

;;;###autoload
(define-globalized-minor-mode global-topspace-mode topspace-mode
  topspace-mode
  :group 'topspace)

(defun topspace--enabled ()
  "Return t only if both `topspace-mode' and `topspace-active' are non-nil."
  (and (topspace--eval-choice topspace-active) topspace-mode))

(provide 'topspace)

;;; topspace.el ends here

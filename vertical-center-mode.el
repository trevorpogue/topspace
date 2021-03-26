;;; vertical-center-mode.el --- Center buffers vertically in their window and scroll above the top line -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Trevor Pogue, ...

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
;; Automatically center buffers vertically in the window after opening files and
;; during editing. Users can also adjust the centering offset with scrolling to
;; further scroll up or down by any amount above the top lines in a buffer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO:
;; - support scrolling above top line with page scrolling as well
;; - wrapped lines being counted as single lines when centering
;; - cannot scroll above top line if buffer open in multiple windows and
;; one or more windows is scrolled above beginning of buffer
;; - recentering on window resize only occurs in selected buffer
;; - issues if enabling when top line in window is > line 1
;; - submit to MELPA? (after optimizing/cleaning up code more)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Mode definition and setup

;;;###autoload
(define-global-minor-mode global-vertical-center-mode vertical-center-mode
  vcm--turn-on-from-global)

(defun vcm--turn-on-from-global ()
  "Try to turn on vertical-center-mode from global call.
Called when calling command `global-vertical-center-mode'.
vertical-center-mode will not start in minibuffer or hidden buffers, or helm."
  (unless (or (bound-and-true-p vcm-on)
              (string-match " \\*.*\\*" (buffer-name))
              (string-match "helm" (buffer-name))
              (minibufferp))
    (vertical-center-mode 1)))

;;;###autoload
(define-minor-mode vertical-center-mode
  "Allows vertical padding or scrolling above the top line of a buffer.
When opening a buffer, the contents are initially vertically centered with
respect to the window height. The user can also scroll as well to adjust the
centering offset. The buffer also recenters if transfered to
another window unless user has previously adjusted its height with scrolling.
"
  :init-value nil
  :ligher " vc"
  :keymap nil
  ;; only turn on if mode was previously off
  (if (and vertical-center-mode (not (bound-and-true-p vcm-on)))
      (vcm--turn-on))
  ;; only turn off if mode was previously on
  (if (and (not vertical-center-mode) (bound-and-true-p vcm-on))
      (vcm--turn-off)))

(defun vcm--turn-on ()
  (setq-local vcm-on t)
  (setq-local vcm-overlay (make-overlay (point-min) (point-max)))
  ;; vcm-scroll-offset: the overlay portion that the user adjusts with scrolling
  (setq-local vcm-scroll-offset 0)
  (setq-local vcm-user-scrolled nil)
  (vcm--add-hooks)
  (if (not (boundp 'vcm-first-recenter-done))
      ;; vcm-first-recenter-done is used to block too many recenterings occuring
      ;; that are triggered by window-based hooks,
      (setq-local vcm-first-recenter-done nil))
  ;; Below: let user turn the mode off then on again to recenter while preventing
  ;; recentering here on initial mode turn-on. This avoids a bug with buffer
  ;; not being centered on emacs startup, but need to investigate further to
  ;; understand the root cause behind this bug/solution relationship.
  (if vcm-first-recenter-done (vcm--recenter-reset-scroll)))

(defun vcm--turn-off ()
  "Delete/unset data structures when the mode is turned off."
  (vcm--remove-hooks)
  (makunbound 'vcm-on)
  (delete-overlay vcm-overlay)
  (makunbound 'vcm-overlay)
  (makunbound 'vcm-scroll-offset)
  (makunbound 'vcm-user-scrolled))

(defun vcm--kill-buffer ()
  (makunbound 'vcm-first-recenter-done)
  (vcm--turn-off))

(provide 'vertical-center-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Properties (inspired specifically by definition of properties from Python)

(defun vcm--center-offset ()
  "Portion of the overlay that makes small buffers centered."
  ;; dividing by slightly less than 2 here made buffers more dead centered
  (let ((center-offset (/ (* (- (window-height) (vcm--buf-size)) 31) 64)))
    (when (< center-offset 0) (setq center-offset 0))
    center-offset)) ; return center-offset

(defun vcm--add-to-scroll-offset (direction)
  (let ((pos (+ (- (line-number-at-pos) (vcm--top-line)) (vcm--overlay-size)))
        (bottom (- (window-height) 5)))
    ;; avoids a bug with cursor suddenly scrolling up
    (when (> pos bottom) (previous-line)))
  ;; only put overlay when top line is 1
  (when (= (vcm--top-line) 1)
    ;; block scrolling text fully below bottom of window
    (unless (and (> direction 0)
                 (>= (vcm--overlay-size) (- (window-height) 5)))
      (setq vcm-scroll-offset (+ vcm-scroll-offset direction)))))

(defun vcm--overlay-size ()
  "The total overlay size."
  (+ vcm-scroll-offset (vcm--center-offset)))

(defun vcm--buf-size ()
  "Size of the buffer text in lines."
  (count-lines (point-min) (point-max)))

(defun vcm--top-line ()
  "Line number of the top line of text shown in the window."
  (line-number-at-pos (window-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Overlay dislaying and recentering hooks

;; the optional unused args in this section are just for hook compatibility

(defun vcm--recenter-keep-scroll (&optional arg0 arg1 arg2)
  "Use an overlay to display empty lines at the beginning of the buffer.
This emulates the ability to scroll above the top line."
  (let ((overlay-size (vcm--overlay-size)))
    (overlay-put vcm-overlay 'before-string
                 (when (> overlay-size 0) (make-string overlay-size ?\n))))
  (setq vcm-first-recenter-done t))

(defun vcm--recenter-reset-scroll (&optional arg0 arg1 arg2)
  (setq vcm-scroll-offset 0)
  (vcm--recenter-keep-scroll))

(defun vcm--recenter-reset-scroll-conditional (&optional arg0 arg1 arg2)
  (unless (and vcm-user-scrolled) (vcm--recenter-reset-scroll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Scrolling hooks

(defun vcm--scroll (scroll-list ccm-list scroll-direction)
  "Emulate scrolling if user command was a scrolling command."
  (when (member this-command '(comment-line))
    ;; this resolves a bug with the overlay changing size when commenting code
    (vcm--recenter-keep-scroll))
  (let ((user-is-scrolling (member this-command scroll-list))
        (centering-cursor (member this-command ccm-list)))
    ;; shouldn't scroll from moving cursor unless in centered-cursor-mode
    (unless (bound-and-true-p centered-cursor-mode) (setq centering-cursor nil))
    (when (or user-is-scrolling centering-cursor)
      (vcm--add-to-scroll-offset scroll-direction)
      (setq vcm-user-scrolled t)
      (vcm--recenter-keep-scroll))))

(defun vcm--scroll-increase-overlay ()
  "Check if user command should initiate scrolling down."
  (vcm--scroll '(scroll-down-line evil-scroll-line-up)
               '(previous-line evil-previous-visual-line) 1))

(defun vcm--scroll-decrease-overlay ()
  "Check if user command should initiate scrolling up."
  (vcm--scroll '(scroll-up-line evil-scroll-line-down)
               '(next-line evil-next-visual-line) -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Hooks

(defvar vcm--hook-alist
  '(
    (window-configuration-change-hook . vcm--recenter-reset-scroll-conditional)
    (window-size-change-functions . vcm--recenter-reset-scroll)
    (kill-buffer-hook . vcm--kill-buffer)
    (after-change-functions . vcm--recenter-keep-scroll)
    (pre-command-hook . vcm--scroll-increase-overlay)
    (post-command-hook . vcm--scroll-decrease-overlay))
  "A list of hooks so they only need to be written in one spot.
List of cons cells in format (hook-variable . function).")

(defun vcm--add-hooks ()
  "Add hooks defined in variable `vcm-hook-alist'."
  (mapc (lambda (entry) (add-hook (car entry) (cdr entry) t t))
        vcm--hook-alist))

(defun vcm--remove-hooks ()
  "Remove hooks defined in variable `vcm-hook-alist'."
  (mapc (lambda (entry) (remove-hook (car entry) (cdr entry) t))
        vcm--hook-alist))

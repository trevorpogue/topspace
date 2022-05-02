;;; test-topspace.el --- Main test file  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Trevor Edwin Pogue

;; Author: Trevor Edwin Pogue

;;; Code:

(setq topspace--log-target '(file . "~/topspace/topspace.log"))
(setq topspace--start-time (float-time))

(defun topspace--log (message)
  "Log MESSAGE."
  (when topspace--log-target
    (let ((log-line (format "%06d %s\n"
                            (round (- (* 1000 (float-time))
                                      (* 1000 topspace--start-time)))
                            message))
          (target-type (car topspace--log-target))
          (target-name (cdr topspace--log-target)))
      (pcase target-type
        ('buffer
         (with-current-buffer (get-buffer-create target-name)
           (goto-char (point-max))
           (insert log-line)))
        ('file
         (let ((save-silently t))
           (append-to-file log-line nil target-name)))
        (_
         (error "Unrecognized log target type: %S" target-type))))))

(defmacro topspace--cmds (&rest cmds)
  "Run CMDS with command hooks."
  (let ((result '(progn)))
    (dolist (cmd cmds)
      (setq result
            (append result
                    `((run-hooks 'pre-command-hook)
                      (eval ',cmd)
                      (run-hooks 'post-command-hook)
                      ))))
    result))

(describe
 "topspace"
 :var (prev-height)

 (before-all
  (topspace--cmds (set-frame-size (selected-frame) 90 24))
  (switch-to-buffer (find-file-noselect "./topspace.el" t))
  (global-topspace-mode)
  )

 (before-each (switch-to-buffer "topspace.el"))

 (it "reduces top space height before cursor can move below window-end"
     (goto-char 1)
     (topspace--draw 0)
     (topspace--cmds
      (scroll-down)
      (scroll-up)
      (scroll-down)
      )
     (setq prev-height (topspace-height))
     (topspace--cmds
      (next-line))
     (expect (topspace-height) :to-equal (1- prev-height))
     (topspace--cmds (next-line 4))
     (expect (topspace-height) :to-equal (- prev-height 5))
     (topspace--cmds (scroll-down 2)))

 (it "moves cursor up before cursor is scrolled below window-end"
     (topspace--cmds (scroll-down-line))
     (expect (topspace-height) :to-equal (- prev-height 2))
     (topspace--cmds
      (scroll-down-line)
      (scroll-down-line))
     (expect (topspace-height) :to-equal prev-height)
     (topspace--cmds (scroll-up-line))
     (expect (topspace-height) :to-equal (1- prev-height)))

 (describe
  "topspace--after-scroll"
  (it "is needed when first scrolling above the top line"
      (goto-char 1)
      (topspace--draw 0)
      (scroll-up-line)
      (scroll-down 2)
      (expect (round (topspace-height)) :to-equal 1)))

 (describe
  "topspace--window-configuration-change"

  (it "autocenters buffer when window size changes"
      (switch-to-buffer "*scratch*")
      (run-hooks 'window-configuration-change-hook)
      (expect (round (* (topspace-height) 10)) :to-equal 86)
      (topspace--cmds (set-frame-size (selected-frame) 90 22))
      (run-hooks 'window-configuration-change-hook)
      (expect (round (* (topspace-height) 10)) :to-equal 78)
      (topspace--cmds (set-frame-size (selected-frame) 90 24)))

  (it "will redraw topspace even if window height didn't change
in case topspace-autocenter-buffers changed return value"
      (spy-on 'topspace--draw)
      (topspace--window-configuration-change)
      (expect 'topspace--draw :to-have-been-called)))

 (describe
  "topspace-mode"
  (it "can be enabled and disabled locally"
      (topspace-mode -1)
      (expect topspace-mode :to-equal nil)
      (scroll-up-line)
      (topspace--draw 1)
      (expect (topspace-height) :to-equal 0)
      (ignore-errors (scroll-down-line))
      (topspace-mode 1)
      (expect topspace-mode :to-equal t)))

 (describe
  "topspace--draw-increase-height"
  (it "increases top space height"
      (goto-char 1)
      (recenter)
      (setq prev-height (topspace-height))
      (topspace--draw-increase-height 1)
      (expect (topspace-height) :to-equal (1+ prev-height))))

 (describe
  "topspace--after-recenter"
  (it "adds top space if recentering near top of buffer"
      (goto-char 1)
      (recenter)
      (expect (round (topspace-height)) :to-equal (/ (window-height) 2))
      (recenter -1)
      (expect (round (topspace-height)) :to-equal (- (window-height) 2))))

 (describe
  "topspace--previous-line"
  (it "is to be used like previous-line but non-interactively"
      (goto-char 1)
      (next-line)
      (topspace--previous-line)
      (expect (line-number-at-pos) :to-equal 1)
      (should-error (topspace--previous-line))))

 (describe
  "topspace--smooth-scroll-lines-above-point"
  (it "allows smooth-scrolling package to work with topspace"
      :to-equal (smooth-scroll-lines-above-point)
      (progn (goto-char 1)
             (topspace--draw 0)
             (goto-line smooth-scroll-margin)
             (set-window-start (selected-window) (point))
             (scroll-down smooth-scroll-margin)
             (setq smooth-scrolling-mode nil)
             (call-interactively 'smooth-scrolling-mode))
      (previous-line)
      (previous-line)
      (expect (round (topspace-height)) :to-equal 2)
      (setq smooth-scrolling-mode nil)
      ))

 (describe
  "topspace-default-empty-line-indicator"
  (it "can return a string with an indicator in left-fringe"
      (setq indicate-empty-lines t)
      (let ((bitmap (catch 'tag (dolist (x fringe-indicator-alist)
                                  (when (eq (car x) 'empty-line)
                                    (throw 'tag (cdr x)))))))
        (expect (topspace-default-empty-line-indicator) :to-equal
                (propertize " " 'display (list `left-fringe bitmap
                                               `fringe))))))
 (describe
  "topspace--count-lines"
  ;; TODO: figure out how to test cask on a graphical emacs frame with display
  ;;   (it "can count lines if window-absolute-pixel-position returns non-nil"
  ;;       (expect (display-graphic-p) :to-equal nil)
  ;;       (make-frame-on-display ":0")
  ;;       (topspace--log (frame-list))
  ;;       (sit-for 1)
  ;;       (with-selected-window
  ;;           ;; (switch-to-buffer "topspace.el")
  ;;           (frame-selected-window (car (frames-on-display-list)))
  ;;       (expect (round (topspace--count-lines (point-min) (point-max)))
  ;;               :to-equal
  ;;               (line-number-at-pos (point-max)))))

  (it "can count lines if window-absolute-pixel-position returns nil"
      (expect (round (topspace--count-lines (point-min) (point-max)))
              :to-equal
              (line-number-at-pos (point-max))
              )))

 (describe
  "topspace--correct-height"
  (it "fixes topspace height when larger than max valid value"
      (let ((max-height
             (- (topspace--window-height) (topspace--context-lines))))
        (expect (topspace--correct-height (1+ max-height))
                :to-equal max-height))))

 (describe
  "topspace-height"
  (it "by default returns 0 for new buffer when topspace-autocenter-buffers
returns nil"
      (let ((prev-autocenter-val topspace-autocenter-buffers))
        (setq topspace--heights '())
        (setq topspace-autocenter-buffers nil)
        (expect (topspace-height) :to-equal 0)
        (setq topspace-autocenter-buffers prev-autocenter-val))))

 (describe
  "topspace--current-line-plus-topspace"
  (it "can accept an arg or no args"
      (expect (topspace--current-line-plus-topspace)
              :to-equal (topspace--current-line-plus-topspace
                         (topspace-height))))))

;;; test-topspace.el ends here

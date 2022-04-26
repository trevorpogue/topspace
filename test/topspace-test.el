;;; test-topspace.el --- Main test file  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Trevor Edwin Pogue

;; Author: Trevor Edwin Pogue

;;; Code:

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

(describe "topspace"
  :var (prev-height)

  (before-all
    (topspace--cmds (set-frame-size (selected-frame) 101 24))
    (switch-to-buffer (find-file-noselect "./topspace.el" t))
    (global-topspace-mode 1))

  (before-each (switch-to-buffer "topspace.el")
               (setq smooth-scrolling-mode nil))

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
    (topspace--cmds (next-line) (next-line))
    (expect (topspace-height) :to-equal (- prev-height 3)))

  (it "moves cursor up before cursor is scrolled below window-end"
    (topspace--cmds (scroll-down-line))
    (expect (topspace-height) :to-equal (- prev-height 2))
    (topspace--cmds
     (scroll-down-line)
     (scroll-down-line))
    (expect (topspace-height) :to-equal prev-height)
    (topspace--cmds (scroll-up-line))
    (expect (topspace-height) :to-equal (1- prev-height)))

  (describe "topspace--after-scroll"
    (it "is needed when first scrolling above the top line"
      (progn (topspace--cmds (goto-char 1)
                             (topspace--draw 0))
             (topspace--cmds (scroll-up-line))
             (condition-case nil (scroll-down 2)
               (error (print 'wtf))))
      (expect (topspace-height) :to-equal 0)))

  (describe "topspace--window-configuration-change"
    (it "autocenters buffer when window size changes"
      (switch-to-buffer "*scratch*")
      (topspace--cmds (set-frame-size (selected-frame) 101 24))
      (run-hooks 'window-configuration-change-hook)
      (expect (round (* (topspace-height) 10)) :to-equal 86)
      (topspace--cmds (set-frame-size (selected-frame) 101 22))
      (run-hooks 'window-configuration-change-hook)
      (expect (round (* (topspace-height) 10)) :to-equal 78)))

  (describe "topspace-mode"
    (it "can be enabled and disabled locally"
      (topspace-mode -1)
      (expect topspace-mode :to-equal nil)
      (scroll-up-line)
      (topspace--draw 1)
      (expect (topspace-height) :to-equal 0)
      (ignore-errors (scroll-down-line))
      (topspace-mode 1)
      (expect topspace-mode :to-equal t)
      ))

  (describe "topspace--draw-increase-height"
    (it "increases top space height"
      (goto-char 1)
      (recenter)
      (setq prev-height (topspace-height))
      (topspace--draw-increase-height 1)
      (expect (topspace-height) :to-equal (1+ prev-height))))

  (describe "topspace--draw-increase-height"
    (it "increases top space height"
      (goto-char 1)
      (recenter)
      (setq prev-height (topspace-height))
      (topspace--draw-increase-height 1)
      (expect (topspace-height) :to-equal (1+ prev-height))))

  (describe "topspace--after-recenter"
    (it "adds top space if recentering near top of buffer"
      (goto-char 1)
      (recenter)
      (expect (round (topspace-height)) :to-equal (/ (window-height) 2))
      (recenter -1)
      (expect (round (topspace-height)) :to-equal (- (window-height) 2))))

  (describe "topspace--previous-line"
    (it "is to be used like previous-line but non-interactively"
      (goto-char 1)
      (next-line)
      (topspace--previous-line)
      (expect (line-number-at-pos) :to-equal 1)))

  (describe "topspace--smooth-scroll-lines-above-point"
    (it "allows smooth-scrolling package to work with topspace"
      (expect (topspace--smooth-scroll-lines-above-point)
              :to-equal (smooth-scroll-lines-above-point))))

  (describe "topspace-default-empty-line-indicator"
    (it "can return a string with an indicator in left-fringe"
      (setq indicate-empty-lines t)
      (let ((bitmap (catch 'tag (dolist (x fringe-indicator-alist)
                                  (when (eq (car x) 'empty-line)
                                    (throw 'tag (cdr x)))))))
        (expect (topspace-default-empty-line-indicator) :to-equal
                (propertize " " 'display (list `left-fringe bitmap
                                               `fringe)))))))

;;; test-topspace.el ends here

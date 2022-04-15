;; Run with:
;;
;;   emacs -Q -nw -l ../../util/director-bootstrap.el -l demo.el

(director-bootstrap
 :user-dir "/tmp/director-demo"
 :packages '()
 :load-path '("../.."))

(director-run
 :version 1
 :before-start (lambda ()
                 (global-set-key (kbd "C-M-n") 'scroll-down-line)
                 (global-set-key (kbd "C-M-p") 'scroll-up-line)
                 (global-set-key (kbd "C-M-e") 'end-of-buffer)
                 (switch-to-buffer (find-file-noselect "../topspace.el" t))
                 (global-topspace-mode)
                 )
 :steps '(
          ;; Test scrolling using key commands
          (:type "\M-v")     ;; page down
          (:type "\C-\M-n")  ;; scroll down line
          (:assert (setq topspace--tests-prev-height (topspace--height)))
          (:type "\C-n")     ;; next-line
          (:assert (= (topspace--height) (1- topspace--tests-prev-height)))
          (:type "\C-u2\C-n");; next-line x2
          (:assert (= (topspace--height) (- topspace--tests-prev-height 3)))
          (:type "\C-\M-n")  ;; scroll down line
          (:assert (= (topspace--height) (- topspace--tests-prev-height 2)))
          (:type "\C-u2\C-\M-n")  ;; scroll down line x2
          (:assert (= (topspace--height) topspace--tests-prev-height))
          ;; reset top line to top of window:
          (:type "\C-v")     ;; page up
          (:assert (= (topspace--height) 1))
          (:type "\C-\M-p")  ;; scroll up line
          (:assert (= (topspace--height) 0))
          (:assert (= (window-start) 1))

          ;; Test mouse scrolling
          (:type "\M-v")     ;; page down
          (:run (mwheel-scroll mouse-wheel-down-event)) ;; scroll down line
          (:assert (setq topspace--tests-prev-height (topspace--height)))
          (:type "\C-n")     ;; next-line
          (:assert (= (topspace--height) (1- topspace--tests-prev-height)))
          (:type "\C-u2\C-n");; next-line x2
          (:assert (= (topspace--height) (- topspace--tests-prev-height 3)))
          (:run (mwheel-scroll mouse-wheel-down-event)) ;; scroll down line
          (:run (mwheel-scroll mouse-wheel-up-event))   ;; scroll up line
          (:run (mwheel-scroll mouse-wheel-down-event)) ;; scroll down line
          (:assert (= (topspace--height) (- topspace--tests-prev-height 2)))
          (:run (mwheel-scroll mouse-wheel-down-event)) ;; scroll down line
          (:run (mwheel-scroll mouse-wheel-down-event)) ;; scroll down line
          (:assert (= (topspace--height) topspace--tests-prev-height))
          )
 :typing-style 'human
 :delay-between-steps 0.1
 :after-end (lambda () (kill-emacs 0))
 :on-failure (lambda () (kill-emacs 1))
 :on-error (lambda () (kill-emacs 1)))

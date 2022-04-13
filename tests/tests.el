;; Run with:
;;
;;   emacs -Q -nw -l ../../util/director-bootstrap.el -l demo.el
;; (require 'topspace)

(director-bootstrap
 :user-dir "/tmp/director-demo"
 :packages '()
 :load-path '("./"))

(defun t-t () (kill-emacs 1))

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
          (:type "\M-v")
          (:type "\C-\M-n")
          (:assert (setq topspace--tests-prev-height (topspace--height)))
          (:type "\C-n")
          (:assert (= (topspace--height) (1- topspace--tests-prev-height)))
          (:type "\C-u2\C-n")
          (:assert (= (topspace--height) (- topspace--tests-prev-height 3)))
          (:type "\C-\M-n")
          (:assert (= (topspace--height) (- topspace--tests-prev-height 2)))
          (:type "\C-u2\C-\M-n")
          (:assert (= (topspace--height) topspace--tests-prev-height))
          )
 :typing-style 'human
 :delay-between-steps 0.1
 :after-end (lambda () (kill-emacs 0))
 :on-failure (lambda () (kill-emacs 1))
 :on-error (lambda () (kill-emacs 1))
 )

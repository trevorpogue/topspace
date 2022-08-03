;;; test-helper.el --- Helper for tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Trevor Edwin Pogue

;; Author: Trevor Edwin Pogue

;;; Code:

(when (require 'undercover nil t)
  (setq undercover-force-coverage t)
  (undercover "*.el"
              ;; (:report-file "coverage/.resultset.json")
              ;; (:report-format 'simplecov)
              ;; (:report-format 'text)
              ))

(require 'smooth-scrolling)
(require 'linum)
(require 'topspace)

;;; test-helper.el ends here

;;; key-layout-mapper-test.el --- tests for key-layout-mapper -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.0.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: keybind keymap layout

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'key-layout-mapper)

;; define tests

(ert-deftest split-modifiers ()
  (should (equal '("k" . ()) (key-layout-mapper--split-modifiers "k")))
  (should (equal '("shift" . ()) (key-layout-mapper--split-modifiers "shift")))
  (should (equal '("k" . ("C-")) (key-layout-mapper--split-modifiers "C-k")))
  (should (equal '("d" . ("M-")) (key-layout-mapper--split-modifiers "M-d")))

  (should (equal '("k" . ("C-" "M-")) (key-layout-mapper--split-modifiers "C-M-k")))
  (should (equal '("k" . ("M-" "C-")) (key-layout-mapper--split-modifiers "M-C-k")))
  (should (equal '("k" . ("M-" "C-" "A-")) (key-layout-mapper--split-modifiers "M-C-A-k")))
  (should (equal '("k" . ("M-" "C-" "A-" "H-")) (key-layout-mapper--split-modifiers "M-C-A-H-k")))
  )

(ert-deftest convert-key-between-layouts ()
  (should (equal "s" (key-layout-mapper--convert-key 'sturdy "a")))
  (should (equal "R" (key-layout-mapper--convert-key 'sturdy "D")))
  (should (equal "C-s" (key-layout-mapper--convert-key 'sturdy "C-a")))
  (should (equal "g l y" (key-layout-mapper--convert-key 'sturdy "v e g"))))

;; Run test
(ert t)

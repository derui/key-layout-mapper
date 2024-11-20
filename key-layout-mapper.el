;;; key-layout-mapper.el -- define key layout and keybind -*- lexical-binding: t -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: https://github.com/derui/key-layout-mapper
;; Version: 0.0.1
;; Created: 2024
;; Package-Requires: ((emacs "29.1"))
;; Keywords: keybind keymap layout

;;; Commentary:
;;
;; key-layout-mapper defines some macro and tiny function to
;; convert different key layout such as qwerty and dvorak and
;; define those layouts.
;;
;; Define keymap like this:
;;
;; (key-layout-mapper-deflayout "sample"
;;   (1 2 3 4 5 6 7 8 9 0
;;    a b c d e f))
;;
;; and then, you use this layout for conversion between base layout.
;; Using base layout is `qwerty' always.
;;
;; (key-layout-mapper-set-layout "sample")
;; ;; this code set binding `a' on sample layout.
;; (key-layout-mapper-keymap-set keymap "q" #'command)

;;; Code:

(defgroup key-layout-mapper nil
  "Key layout define and mapper group"
  :prefix "key-layout-mapper-")

(defvar key-layout-mapper-current-layout "qwerty"
  "current layout for key conversion. Default is `qwerty'.

Set this variable `BEFORE' load this library if you want to use other layout
without waiting this library is loaded.")

(defvar key-layout-mapper--layouts (make-hash-table)
  "layouts for conversion")

;;;###autoload
(defmacro key-layout-mapper-deflayout (name layout)
  "define and register new layout. If other layout is already registered with `name',
it will overwrite with new one."

  `(puthash ,name (vconcat ',(mapcar (lambda (v)
                                       (cond
                                        ((stringp v) v)
                                        (t (prin1-to-string v))))
                                     layout))
            key-layout-mapper--layouts))

(defun key-layout-mapper--get-layout (layout)
  "Get `layout' from registered layouts. Return `NIL' if `layout' is not defined"
  (gethash layout key-layout-mapper--layouts))

;; helper function
(defun key-layout-mapper--convert-key (layout key)
  "Convert `key' from standard qwerty layout to `layout'.

If no convertable key contains base layout, use given `key' as-is."
  (let ((result '())
        (base (key-layout-mapper--get-layout "qwerty"))
        (layout (key-layout-mapper--get-layout layout)))
    (seq-each (lambda (k)
                (let* ((base-index (seq-position base (char-to-string k)))
                       (to-char (and base-index
                                     layout
                                     (seq-elt layout base-index))))
                  (setq result (cons (or to-char
                                         (char-to-string k))) result)))
              (split-string key " "))
    (string-join result " ")))

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

  `(puthash ',(intern name) (vconcat ',(split-string layout "[ \n]" t))
            key-layout-mapper--layouts))

(defun key-layout-mapper--get-layout (layout)
  "Get `layout' from registered layouts. Return `NIL' if `layout' is not defined"
  (gethash layout key-layout-mapper--layouts))

;; helper function
(defun key-layout-mapper--split-modifiers (s)
  "Split modifiers from given key sequence"

  (let (mods
        key
        (target s)
        (target-modifiers '("C-" "M-" "A-" "H-")))
    (while (not key)
      (let ((previous target))
        (dolist (m target-modifiers)
          (when (string-prefix-p m target)
            (setq target (substring target (seq-length m)))
            (setq mods (cons m mods))))
        (when (string= target previous)
          (setq key target)))
      )

    (cons key (seq-reverse mods))))

(defun key-layout-mapper--convert-key (layout key)
  "Convert `key' from standard qwerty layout to `layout'.

If no convertable key contains base layout, use given `key' as-is."
  (let ((result '())
        (base (key-layout-mapper--get-layout 'qwerty))
        (layout (key-layout-mapper--get-layout layout)))
    (seq-each (lambda (k)
                (let* ((key (key-layout-mapper--split-modifiers k))
                       (mods (string-join (cdr key) ""))
                       (key (car key))
                       (base-index (seq-position base key))
                       (to-char (and base-index
                                     layout
                                     (seq-elt layout base-index))))
                  (setq result (cons (string-join (list mods (or to-char
                                                                 key))
                                                  "")
                                     result))))
              (split-string key " "))
    (string-join (seq-reverse result) " ")))

;;;###autoload
(defun key-layout-mapper-keymap-set (keymap key fn)
  "Wrapping function for keymap-set with converted key
with `key-layout-mapper-current-layout'"

  (keymap-set keymap (key-layout-mapper--convert-key
                      key-layout-mapper-current-layout
                      key)
              fn))

;;;###autoload
(defun key-layout-mapper-set-layout (layout)
  "Set current layout for key conversion"
  (when (symbolp layout)
    (setq key-layout-mapper-current-layout layout)))

;; define layouts
(key-layout-mapper-deflayout
 "qwerty"
 "
` 1 2 3 4 5 6 7 8 9 0 - =
~ ! @ # $ % ^ & * ( ) _ +

[ ] \
{ } |

q w e r t y u i o p
a s d f g h j k l ; '
z x c v b n m , . /

Q W E R T Y U I O P
A S D F G H J K L : \"
Z X C V B N M < > ?
")

(key-layout-mapper-deflayout
 "sturdy"
 "
` 1 2 3 4 5 6 7 8 9 0 - =
~ ! @ # $ % ^ & * ( ) _ +

[ ] \
{ } |

v m l c p x f o u j
s t r d y ; n a e i '
z k q g w b h , . /

V M L C P X F O U J
S T R D Y > N A E I ?
Z K Q G W B H \" : <
")

(provide 'key-layout-mapper)
;;; key-layout-mapper.el ends here

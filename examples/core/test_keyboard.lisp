(require :cl-raylib)

(defpackage :test-kb
  (:use :cl :cl-raylib))

(in-package :test-kb)

(with-window (800 450 "Keyboard Test")
  (set-target-fps 60)
  (loop until (window-should-close) do
    (with-drawing
      (clear-background :raywhite)
      (draw-text (text-format "W key: %s" (if (is-key-down :key-w) "DOWN" "UP")) 20 20 20 :black)
      (draw-text (text-format "A key: %s" (if (is-key-down :key-a) "DOWN" "UP")) 20 50 20 :black)
      (draw-text (text-format "S key: %s" (if (is-key-down :key-s) "DOWN" "UP")) 20 80 20 :black)
      (draw-text (text-format "D key: %s" (if (is-key-down :key-d) "DOWN" "UP")) 20 110 20 :black))))

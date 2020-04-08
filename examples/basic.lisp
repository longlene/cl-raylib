(defpackage :raylib-user
 (:use :cl :raylib))

(in-package :raylib-user)

(defun basic ()
  (with-window (640 480 "basic")
               (set-target-fps 60)
               (loop
                 (if (window-should-close) (return))
                 (with-drawing
                  (clear-background +raywhite+)
                   (draw-fps 20 20)
                   (draw-text "hello, lisper" 320 200 40 +green+)))))

(basic)

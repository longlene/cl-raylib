(defpackage :raylib-user
 (:use :cl :raylib))

(in-package :raylib-user)

(show-logo)

(with-window (640 480 "basic")
	       (loop
		 (if (window-should-close) (return))
		 (with-drawing
		   (draw-fps 20 20)
		   (draw-text "hello, lisper" 320 200 40 +green+))))

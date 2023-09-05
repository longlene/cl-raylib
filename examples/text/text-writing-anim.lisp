(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  (let ((screen-width 800)
        (screen-height 450)
	(frame-counter 0)
	(message (format nil "This sample illustrates a text writing~%animation effect! Check it out!")))
    (with-window (screen-width screen-height "raylib [text] example - text writing anim")
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop
        until (window-should-close) ; detect window close button or ESC key
        do
	   (setf frame-counter
		 (cond
		   ((is-key-down :key-space) (+ frame-counter 8))
		   ((is-key-down :key-enter) 0)
		   (t (1+ frame-counter))))
	   (with-drawing
	     (incf frame-counter)
             (clear-background :raywhite)
             (draw-text
	      (text-subtext message 0 (floor frame-counter 10))
	      ;;message
	      210 160 20 :maroon))))))

(main)

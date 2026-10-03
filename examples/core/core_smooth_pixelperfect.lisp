;;;; core_smooth_pixelperfect.lisp - Smooth Pixel-perfect camera
;;;; Translated from raylib/examples/core/core_smooth_pixelperfect.c

(require :cl-raylib)

(defpackage :core-smooth-pixelperfect
  (:use :cl :cl-raylib))

(in-package :core-smooth-pixelperfect)

(defun main ()
  "Main function - smooth pixel-perfect camera"
  (let ((screen-width 800)
        (screen-height 450)
        (virtual-screen-width 160)
        (virtual-screen-height 90))

    ;; Initialization
    (init-window screen-width screen-height "raylib [core] example - smooth pixel-perfect camera")

    (let ((virtual-ratio (/ screen-width virtual-screen-width)))

      ;; Game world camera
      (let ((world-space-camera (make-camera-2d :offset (vec2 0.0 0.0)
                                                :target (vec2 0.0 0.0)
                                                :rotation 0.0
                                                :zoom 1.0))
            ;; Smoothing camera
            (screen-space-camera (make-camera-2d :offset (vec2 0.0 0.0)
                                                :target (vec2 0.0 0.0)
                                                :rotation 0.0
                                                :zoom 1.0)))

        ;; This is where we'll draw all our objects
        (let ((target (load-render-texture virtual-screen-width virtual-screen-height))
              (rec01 (make-rectangle :x 70.0 :y 35.0 :width 20.0 :height 20.0))
              (rec02 (make-rectangle :x 90.0 :y 55.0 :width 30.0 :height 10.0))
              (rec03 (make-rectangle :x 80.0 :y 65.0 :width 15.0 :height 25.0)))

          ;; The target's height is flipped (in the source Rectangle), due to OpenGL reasons
          (let ((source-rec (make-rectangle :x 0.0 :y 0.0 
                                           :width (float (texture-width (render-texture-texture target)))
                                           :height (float (- (texture-height (render-texture-texture target))))))
                (dest-rec (make-rectangle :x (- virtual-ratio) :y (- virtual-ratio)
                                         :width (+ screen-width (* virtual-ratio 2))
                                         :height (+ screen-height (* virtual-ratio 2))))
                (origin (vec2 0.0 0.0))
                (rotation 0.0)
                (camera-x 0.0)
                (camera-y 0.0))

            (set-target-fps 60)

            ;; Main game loop
            (loop until (window-should-close) do
              ;; Update
              ;; Rotate the rectangles, 60 degrees per second
              (incf rotation (* 60.0 (get-frame-time)))

              ;; Make the camera move to demonstrate the effect
              (setf camera-x (- (* (sin (get-time)) 50.0) 10.0))
              (setf camera-y (* (cos (get-time)) 30.0))

              ;; Set the camera's target to the values computed above
              (setf (camera-2d-target screen-space-camera) (vec2 camera-x camera-y))

              ;; Round worldSpace coordinates, keep decimals into screenSpace coordinates
              (setf (vx (camera-2d-target world-space-camera)) (truncate (vx (camera-2d-target screen-space-camera))))
              (decf (vx (camera-2d-target screen-space-camera)) (vx (camera-2d-target world-space-camera)))
              (setf (vx (camera-2d-target screen-space-camera)) (* (vx (camera-2d-target screen-space-camera)) virtual-ratio))

              (setf (vy (camera-2d-target world-space-camera)) (truncate (vy (camera-2d-target screen-space-camera))))
              (decf (vy (camera-2d-target screen-space-camera)) (vy (camera-2d-target world-space-camera)))
              (setf (vy (camera-2d-target screen-space-camera)) (* (vy (camera-2d-target screen-space-camera)) virtual-ratio))

              ;; Draw
              (begin-texture-mode target)
                (clear-background +raywhite+)

                (begin-mode-2d world-space-camera)
                  (draw-rectangle-pro rec01 origin rotation +black+)
                  (draw-rectangle-pro rec02 origin (- rotation) +red+)
                  (draw-rectangle-pro rec03 origin (+ rotation 45.0) +blue+)
                (end-mode-2d)
              (end-texture-mode)

              (begin-drawing)
                (clear-background +red+)

                (begin-mode-2d screen-space-camera)
                  (draw-texture-pro (render-texture-texture target) source-rec dest-rec origin 0.0 +white+)
                (end-mode-2d)

                (draw-text (format nil "Screen resolution: ~dx~d" screen-width screen-height) 10 10 20 +darkblue+)
                (draw-text (format nil "World resolution: ~dx~d" virtual-screen-width virtual-screen-height) 10 40 20 +darkgreen+)
                (draw-fps (- (get-screen-width) 95) 10)
              (end-drawing))

            ;; De-Initialization
            (unload-render-texture target)))) ; Unload render texture

    ;; Close window
    (close-window)))

;; Run the example
(main)
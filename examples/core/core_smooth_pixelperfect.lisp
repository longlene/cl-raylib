;;;; raylib [core] example - smooth pixelperfect
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 3.7, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Giancamillo Alessandroni (@NotManyIdeasDev) and
;;;; reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2021-2025 Giancamillo Alessandroni (@NotManyIdeasDev) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_smooth_pixelperfect.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-smooth-pixelperfect
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-smooth-pixelperfect)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let* ((screen-width 800)
         (screen-height 450)
         (virtual-screen-width 160)
         (virtual-screen-height 90)
         (virtual-ratio (/ (float screen-width) (float virtual-screen-width))))

    (init-window screen-width screen-height "raylib [core] example - smooth pixelperfect")

    (let* ((world-space-camera (make-camera2d :zoom 1.0)) ; Game world camera
           (screen-space-camera (make-camera2d :zoom 1.0)) ; Smoothing camera
           ;; Load render texture to draw all our objects
           (target (load-render-texture virtual-screen-width virtual-screen-height))
           (rec01 (make-rectangle :x 70.0 :y 35.0 :width 20.0 :height 20.0))
           (rec02 (make-rectangle :x 90.0 :y 55.0 :width 30.0 :height 10.0))
           (rec03 (make-rectangle :x 80.0 :y 65.0 :width 15.0 :height 25.0))
           ;; The target's height is flipped (in the source Rectangle), due to OpenGL reasons
           (source-rec (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width (render-texture-texture target)))
                                       :height (- (float (texture-height (render-texture-texture target))))))
           (dest-rec (make-rectangle :x (/ (- screen-width (/ screen-width 1.25)) 2.0) :y (/ (- screen-height (/ screen-height 1.25)) 2.0)
                                     :width (/ screen-width 1.25) :height (/ screen-height 1.25)))
           (origin (vec2 0.0 0.0))
           (rotation 0.0)
           (camera-x 0.0)
           (camera-y 0.0)
           (smooth-on t)
           (overscan nil))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf rotation (* 60.0 (get-frame-time))) ; Rotate the rectangles, 60 degrees per second

               ;; Make the camera move to demonstrate the effect
               (setf camera-x (- (* (sin (float (get-time) 1.0)) 50.0) 10.0)
                     camera-y (* (cos (float (get-time) 1.0)) 30.0))

               ;; Set the camera's target to the values computed above
               (setf (camera2d-target screen-space-camera) (vec2 camera-x camera-y))

               ;; Round worldSpace coordinates, keep decimals into screenSpace coordinates
               (setf (vx (camera2d-target world-space-camera)) (ftruncate (vx (camera2d-target screen-space-camera))))
               (decf (vx (camera2d-target screen-space-camera)) (vx (camera2d-target world-space-camera)))
               (setf (vx (camera2d-target screen-space-camera)) (* (vx (camera2d-target screen-space-camera)) virtual-ratio))

               (setf (vy (camera2d-target world-space-camera)) (ftruncate (vy (camera2d-target screen-space-camera))))
               (decf (vy (camera2d-target screen-space-camera)) (vy (camera2d-target world-space-camera)))
               (setf (vy (camera2d-target screen-space-camera)) (* (vy (camera2d-target screen-space-camera)) virtual-ratio))

               (when (is-key-pressed +key-s+) (setf smooth-on (not smooth-on)))
               (when (is-key-pressed +key-o+) (setf overscan (not overscan)))

               (if overscan
                   (setf dest-rec (make-rectangle :x (- virtual-ratio) :y (- virtual-ratio)
                                                  :width (+ screen-width (* virtual-ratio 2)) :height (+ screen-height (* virtual-ratio 2))))
                   (setf dest-rec (make-rectangle :x (/ (- screen-width (/ screen-width 1.25)) 2.0) :y (/ (- screen-height (/ screen-height 1.25)) 2.0)
                                                  :width (/ screen-width 1.25) :height (/ screen-height 1.25))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-texture-mode target)
               (clear-background +raywhite+)

               (begin-mode-2d world-space-camera)
               (draw-rectangle-pro rec01 origin rotation +black+)
               (draw-rectangle-pro rec02 origin (- rotation) +red+)
               (draw-rectangle-pro rec03 origin (+ rotation 45.0) +blue+)
               (end-mode-2d)
               (end-texture-mode)

               (begin-drawing)
               (clear-background +lightgray+)

               (if smooth-on
                   (progn
                     (begin-mode-2d screen-space-camera)
                     (draw-texture-pro (render-texture-texture target) source-rec dest-rec origin 0.0 +white+)
                     (end-mode-2d))
                   (draw-texture-pro (render-texture-texture target) source-rec dest-rec origin 0.0 +white+))

               (draw-text (text-format "Screen resolution: %ix%i" screen-width screen-height) 10 10 20 +darkblue+)
               (draw-text (text-format "World resolution: %ix%i" virtual-screen-width virtual-screen-height) 10 40 20 +darkgreen+)
               (draw-text (text-format "Smooth: %s" (if smooth-on "ON" "OFF")) 10 (- screen-height 60) 20 +red+)
               (draw-text (text-format "Overscan: %s" (if overscan "ON" "OFF")) 10 (- screen-height 30) 20 +red+)
               (draw-fps (- (get-screen-width) 95) 10)
               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture target)    ; Unload render texture

      (close-window))))                 ; Close window and OpenGL context

(main)

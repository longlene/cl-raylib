;;;; raylib [models] example - point rendering
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 5.0
;;;;
;;;; Example contributed by Reese Gallagher (@satchelfrost) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2024-2025 Reese Gallagher (@satchelfrost)
;;;; Common Lisp port of raylib/examples/models/models_point_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-point-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-point-rendering)

(defconstant +max-points+ 10000000)     ; 10 million
(defconstant +min-points+ 1000)         ; 1 thousand

;; NOTE: The point cloud uses the C library rand() like the C example, so it is the same
;; pseudo-random sequence (unseeded: seed 1)
(defun crand () (cffi:foreign-funcall "rand" :int))
(defconstant +rand-max+ 2147483647)     ; glibc RAND_MAX

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Generate a spherical point cloud
(defun gen-mesh-points (num-points)
  (let ((mesh (make-mesh :triangle-count 1
                         :vertex-count num-points
                         :vertices (make-array (* num-points 3) :element-type 'single-float :initial-element 0.0)
                         :colors (make-array (* num-points 4) :element-type '(unsigned-byte 8) :initial-element 0))))

    ;; REF: https://en.wikipedia.org/wiki/Spherical_coordinate_system
    (dotimes (i num-points)
      (let* ((theta (/ (* (float +pi+) (crand)) (float +rand-max+)))
             (phi (/ (* 2.0 +pi+ (crand)) (float +rand-max+)))
             (r (/ (* 10.0 (crand)) (float +rand-max+)))
             (vertices (mesh-vertices mesh))
             (colors (mesh-colors mesh)))
        (setf (aref vertices (+ (* i 3) 0)) (* r (sin theta) (cos phi))
              (aref vertices (+ (* i 3) 1)) (* r (sin theta) (sin phi))
              (aref vertices (+ (* i 3) 2)) (* r (cos theta)))

        (destructuring-bind (cr cg cb ca) (color-from-hsv (* r 360.0) 1.0 1.0)
          (setf (aref colors (+ (* i 4) 0)) cr
                (aref colors (+ (* i 4) 1)) cg
                (aref colors (+ (* i 4) 2)) cb
                (aref colors (+ (* i 4) 3)) ca))))

    ;; Upload mesh data from CPU (RAM) to GPU (VRAM) memory
    (upload-mesh mesh nil)

    mesh))

;; Draw a model points
;; WARNING: OpenGL ES 2.0 does not support point mode drawing
(defun draw-model-points (model position scale tint)
  (rl-enable-point-mode)
  (rl-disable-backface-culling)

  (draw-model model position scale tint)

  (rl-enable-backface-culling)
  (rl-disable-point-mode))

;; Draw a model points
;; WARNING: OpenGL ES 2.0 does not support point mode drawing
(defun draw-model-points-ex (model position rotation-axis rotation-angle scale tint)
  (rl-enable-point-mode)
  (rl-disable-backface-culling)

  (draw-model-ex model position rotation-axis rotation-angle scale tint)

  (rl-enable-backface-culling)
  (rl-disable-point-mode))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - point rendering")

    (let* ((camera (make-camera3d :position (vec3 3.0 3.0 3.0)
                                  :target (vec3 0.0 0.0 0.0)
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 45.0
                                  :projection +camera-perspective+))

           (position (vec3 0.0 0.0 0.0))
           (use-draw-model-points t)
           (num-points-changed nil)
           (num-points 1000)

           (mesh (gen-mesh-points num-points))
           (model (load-model-from-mesh mesh)))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close)
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               (when (is-key-pressed +key-space+) (setf use-draw-model-points (not use-draw-model-points)))
               (when (is-key-pressed +key-up+)
                 (setf num-points (if (> (* num-points 10) +max-points+) +max-points+ (* num-points 10)))
                 (setf num-points-changed t))
               (when (is-key-pressed +key-down+)
                 (setf num-points (if (< (floor num-points 10) +min-points+) +min-points+ (floor num-points 10)))
                 (setf num-points-changed t))

               ;; Upload a different point cloud size
               (when num-points-changed
                 (unload-model model)

                 (setf mesh (gen-mesh-points num-points))
                 (setf model (load-model-from-mesh mesh))
                 (setf num-points-changed nil))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +black+)

               (begin-mode-3d camera)

               ;; The new method only uploads the points once to the GPU
               (if use-draw-model-points
                   (draw-model-points model position 1.0 +white+)
                   ;; The old method must continually draw the "points" (lines)
                   (dotimes (i num-points)
                     (let ((pos (vec3 (aref (mesh-vertices mesh) (+ (* i 3) 0))
                                      (aref (mesh-vertices mesh) (+ (* i 3) 1))
                                      (aref (mesh-vertices mesh) (+ (* i 3) 2))))
                           (color (list (aref (mesh-colors mesh) (+ (* i 4) 0))
                                        (aref (mesh-colors mesh) (+ (* i 4) 1))
                                        (aref (mesh-colors mesh) (+ (* i 4) 2))
                                        (aref (mesh-colors mesh) (+ (* i 4) 3)))))
                       (draw-point-3d pos color))))

               ;; Draw a unit sphere for reference
               (draw-sphere-wires position 1.0 10 10 +yellow+)

               (end-mode-3d)

               ;; Draw UI text
               (draw-text (text-format "Point Count: %d" num-points) 10 (- screen-height 50) 40 +white+)
               (draw-text "UP - Increase points" 10 40 20 +white+)
               (draw-text "DOWN - Decrease points" 10 70 20 +white+)
               (draw-text "SPACE - Drawing function" 10 100 20 +white+)

               (if use-draw-model-points
                   (draw-text "Using: DrawModelPoints()" 10 130 20 +green+)
                   (draw-text "Using: DrawPoint3D()" 10 130 20 +red+))

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model model)

      (close-window))))

(main)

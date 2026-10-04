;;;; raylib [models] example - tesseract view
;;;;
;;;; NOTE: This example only works on platforms that support drag & drop (Windows, Linux, OSX, Html5?)
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Timothy van der Valk (@arceryz) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2024-2025 Timothy van der Valk (@arceryz) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_tesseract_view.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-tesseract-view
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-tesseract-view)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - tesseract view")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 4.0 4.0 4.0)  ; Camera position
                                 :target (vec3 0.0 0.0 0.0)    ; Camera looking at point
                                 :up (vec3 0.0 0.0 1.0)        ; Camera up vector (rotation towards target)
                                 :fovy 50.0                    ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera mode type

          ;; Find the coordinates by setting XYZW to +-1
          (tesseract (map 'vector (lambda (p) (apply #'vec4 (mapcar #'float p)))
                          '((1 1 1 1) (1 1 1 -1)
                            (1 1 -1 1) (1 1 -1 -1)
                            (1 -1 1 1) (1 -1 1 -1)
                            (1 -1 -1 1) (1 -1 -1 -1)
                            (-1 1 1 1) (-1 1 1 -1)
                            (-1 1 -1 1) (-1 1 -1 -1)
                            (-1 -1 1 1) (-1 -1 1 -1)
                            (-1 -1 -1 1) (-1 -1 -1 -1))))

          (rotation 0.0)
          (transformed (make-array 16 :initial-element (vec3 0.0 0.0 0.0)))
          (w-values (make-array 16 :initial-element 0.0)))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf rotation (* +deg2rad+ 45.0 (float (get-time) 1.0)))

               (dotimes (i 16)
                 (let* ((p (vcopy (aref tesseract i)))
                        ;; Rotate the XW part of the vector
                        (rot-xw (vector2-rotate (vec2 (vx p) (vw p)) rotation)))
                   (setf (vx p) (vx rot-xw)
                         (vw p) (vy rot-xw))

                   ;; Projection from XYZW to XYZ from perspective point (0, 0, 0, 3)
                   ;; NOTE: Trace a ray from (0, 0, 0, 3) > p and continue until W = 0
                   (let ((c (/ 3.0 (- 3.0 (vw p)))))
                     (setf (vx p) (* c (vx p))
                           (vy p) (* c (vy p))
                           (vz p) (* c (vz p))))

                   ;; Split XYZ coordinate and W values later for drawing
                   (setf (aref transformed i) (vec3 (vx p) (vy p) (vz p))
                         (aref w-values i) (vw p))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (dotimes (i 16)
                 ;; Draw spheres to indicate the W value
                 (draw-sphere (aref transformed i) (abs (* (aref w-values i) 0.1)) +red+)

                 (dotimes (j 16)
                   ;; Two lines are connected if they differ by 1 coordinate
                   ;; This way we dont have to keep an edge list
                   (let* ((v1 (aref tesseract i))
                          (v2 (aref tesseract j))
                          (diff (+ (if (= (vx v1) (vx v2)) 1 0) (if (= (vy v1) (vy v2)) 1 0)
                                   (if (= (vz v1) (vz v2)) 1 0) (if (= (vw v1) (vw v2)) 1 0))))

                     ;; Draw only differing by 1 coordinate and the lower index only (duplicate lines)
                     (when (and (= diff 3) (< i j)) (draw-line-3d (aref transformed i) (aref transformed j) +maroon+)))))

               (end-mode-3d)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)

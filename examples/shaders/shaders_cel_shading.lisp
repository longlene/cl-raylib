;;;; raylib [shaders] example - cel shading
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; NOTE: Shaders used in this example are #version 330 (OpenGL 3.3)
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Gleb A (@ggrizzly) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 Gleb A (@ggrizzly)
;;;; Common Lisp port of raylib/examples/shaders/shaders_cel_shading.c

(require :cl-raylib)
(load (merge-pathnames "rlights.lisp" *load-truename*)) ; Required for: lights

(defpackage #:raylib-examples/shaders-cel-shading
  (:use #:cl #:raylib #:rlights))
(in-package #:raylib-examples/shaders-cel-shading)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shaders] example - cel shading")

    (let* ((camera (make-camera3d :position (vec3 9.0 6.0 9.0)
                                  :target (vec3 0.0 1.0 0.0)
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 45.0
                                  :projection +camera-perspective+))

           ;; Load model
           (model (load-model "resources/models/old_car_new.glb"))

           ;; Load cel shader
           (cel-shader (load-shader (text-format "resources/shaders/glsl%i/cel.vs" +glsl-version+)
                                    (text-format "resources/shaders/glsl%i/cel.fs" +glsl-version+)))

           ;; Apply cel shader to model, keep copy of default shader
           (default-shader (material-shader (aref (model-materials model) 0)))

           ;; numBands: controls toon quantization steps (2 = hard binary, 20 = near-smooth)
           (num-bands 10.0)
           (num-bands-loc (get-shader-location cel-shader "numBands"))

           ;; Inverted-hull outline shader: draws back faces extruded along normals
           (outline-shader (load-shader (text-format "resources/shaders/glsl%i/outline_hull.vs" +glsl-version+)
                                        (text-format "resources/shaders/glsl%i/outline_hull.fs" +glsl-version+)))
           (outline-thickness-loc (get-shader-location outline-shader "outlineThickness"))

           ;; Single directional white light, angled so toon bands are visible on the model sides.
           ;; Spins opposite to CAMERA_ORBITAL (0.5 rad/s) so lighting changes as you watch.
           (lights (make-array +max-lights+ :initial-element nil))

           (cel-enabled t)
           (outline-enabled t))

      (setf (aref (shader-locs cel-shader) +shader-loc-vector-view+) (get-shader-location cel-shader "viewPos"))
      (setf (material-shader (aref (model-materials model) 0)) cel-shader)
      (set-shader-value cel-shader num-bands-loc num-bands +shader-uniform-float+)

      (dotimes (i +max-lights+) (setf (aref lights i) (make-light)))
      (setf (aref lights 0) (create-light +light-directional+ (vec3 50.0 50.0 50.0) (vector3-zero) +white+ cel-shader))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close)
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               (let ((camera-pos (list (vx (camera3d-position camera)) (vy (camera3d-position camera)) (vz (camera3d-position camera)))))
                 (set-shader-value cel-shader (aref (shader-locs cel-shader) +shader-loc-vector-view+) camera-pos +shader-uniform-vec3+))

               ;; [Z] Toggle cel shading on/off
               (when (is-key-pressed +key-z+)
                 (setf cel-enabled (not cel-enabled))
                 (setf (material-shader (aref (model-materials model) 0))
                       (if cel-enabled cel-shader default-shader))) ; Apply cel shader or default shader to model

               ;; [C] Toggle outline on/off
               (when (is-key-pressed +key-c+) (setf outline-enabled (not outline-enabled)))

               ;; [Q/E] Decrease/increase toon band count (press or hold to repeat)
               (when (or (is-key-pressed +key-e+) (is-key-pressed-repeat +key-e+)) (setf num-bands (clamp (+ num-bands 1.0) 2.0 20.0)))
               (when (or (is-key-pressed +key-q+) (is-key-pressed-repeat +key-q+)) (setf num-bands (clamp (- num-bands 1.0) 2.0 20.0)))

               (set-shader-value cel-shader num-bands-loc num-bands +shader-uniform-float+)

               ;; Spin light opposite to CAMERA_ORBITAL (0.5 rad/s), angled 45 degrees off vertical
               (let ((tt (float (get-time) 1.0)))
                 (setf (light-position (aref lights 0)) (vec3 (* (sin (* (- tt) 0.3)) 5.0) 5.0 (* (cos (* (- tt) 0.3)) 5.0))))

               (dotimes (i +max-lights+) (update-light-values cel-shader (aref lights i)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (when outline-enabled
                 ;; Outline pass: cull front faces, draw extruded back faces as silhouette
                 (let ((thickness 0.005))
                   (set-shader-value outline-shader outline-thickness-loc thickness +shader-uniform-float+))

                 (rl-set-cull-face +rl-cull-face-front+)
                 (setf (material-shader (aref (model-materials model) 0)) outline-shader)

                 (draw-model model (vector3-zero) 0.75 +white+)

                 (setf (material-shader (aref (model-materials model) 0))
                       (if cel-enabled cel-shader default-shader)) ; Apply cel shader or default shader to model

                 (rl-set-cull-face +rl-cull-face-back+))

               (draw-model model (vector3-zero) 0.75 +white+)
               (draw-sphere-ex (light-position (aref lights 0)) 0.2 50 50 +yellow+) ; Light position indicator

               (draw-grid 10 10.0)

               (end-mode-3d)

               (draw-fps 10 10)

               (draw-text (text-format "Cel: %s  [Z]" (if cel-enabled "ON" "OFF")) 10 65 20 (if cel-enabled +darkgreen+ +darkgray+))
               (draw-text (text-format "Outline: %s  [C]" (if outline-enabled "ON" "OFF")) 10 90 20 (if outline-enabled +darkgreen+ +darkgray+))
               (draw-text (text-format "Bands: %.0f  [Q/E]" num-bands) 10 115 20 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model model)
      (unload-shader cel-shader)
      (unload-shader outline-shader)

      (close-window))))

(main)

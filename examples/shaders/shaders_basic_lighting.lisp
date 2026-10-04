;;;; raylib [shaders] example - basic lighting
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; NOTE: Shaders used in this example are #version 330 (OpenGL 3.3)
;;;;
;;;; Example originally created with raylib 3.0, last time updated with raylib 4.2
;;;;
;;;; Example contributed by Chris Camacho (@chriscamacho) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Chris Camacho (@chriscamacho) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_basic_lighting.c

(require :cl-raylib)
(load (merge-pathnames "rlights.lisp" *load-truename*)) ; Required for: lights

(defpackage #:raylib-examples/shaders-basic-lighting
  (:use #:cl #:raylib #:rlights))
(in-package #:raylib-examples/shaders-basic-lighting)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+) ; Enable Multi Sampling Anti Aliasing 4x (if available)
    (init-window screen-width screen-height "raylib [shaders] example - basic lighting")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 2.0 4.0 6.0) ; Camera position
                                  :target (vec3 0.0 0.5 0.0)   ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                  :fovy 45.0                   ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Load basic lighting shader
           (shader (load-shader (text-format "resources/shaders/glsl%i/lighting.vs" +glsl-version+)
                                (text-format "resources/shaders/glsl%i/lighting.fs" +glsl-version+)))
           (ambient-loc 0)
           (lights (make-array +max-lights+)))

      ;; Get some required shader locations
      (setf (aref (shader-locs shader) +shader-loc-vector-view+) (get-shader-location shader "viewPos"))
      ;; NOTE: "matModel" location name is automatically assigned on shader loading,
      ;; no need to get the location again if using that uniform name
      ;;(setf (aref (shader-locs shader) +shader-loc-matrix-model+) (get-shader-location shader "matModel"))

      ;; Ambient light level (some basic lighting)
      (setf ambient-loc (get-shader-location shader "ambient"))
      (set-shader-value shader ambient-loc '(0.1 0.1 0.1 1.0) +shader-uniform-vec4+)

      ;; Create lights
      (setf (aref lights 0) (create-light +light-point+ (vec3 -2.0 1.0 -2.0) (vector3-zero) +yellow+ shader)
            (aref lights 1) (create-light +light-point+ (vec3 2.0 1.0 2.0) (vector3-zero) +red+ shader)
            (aref lights 2) (create-light +light-point+ (vec3 -2.0 1.0 2.0) (vector3-zero) +green+ shader)
            (aref lights 3) (create-light +light-point+ (vec3 2.0 1.0 -2.0) (vector3-zero) +blue+ shader))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               ;; Update the shader with the camera view vector (points towards { 0.0f, 0.0f, 0.0f })
               (let ((camera-pos (list (vx (camera3d-position camera)) (vy (camera3d-position camera)) (vz (camera3d-position camera)))))
                 (set-shader-value shader (aref (shader-locs shader) +shader-loc-vector-view+) camera-pos +shader-uniform-vec3+))

               ;; Check key inputs to enable/disable lights
               (when (is-key-pressed +key-y+) (setf (light-enabled (aref lights 0)) (not (light-enabled (aref lights 0)))))
               (when (is-key-pressed +key-r+) (setf (light-enabled (aref lights 1)) (not (light-enabled (aref lights 1)))))
               (when (is-key-pressed +key-g+) (setf (light-enabled (aref lights 2)) (not (light-enabled (aref lights 2)))))
               (when (is-key-pressed +key-b+) (setf (light-enabled (aref lights 3)) (not (light-enabled (aref lights 3)))))

               ;; Update light values (actually, only enable/disable them)
               (dotimes (i +max-lights+) (update-light-values shader (aref lights i)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (begin-shader-mode shader)

               (draw-plane (vector3-zero) (vec2 10.0 10.0) +white+)
               (draw-cube (vector3-zero) 2.0 4.0 2.0 +white+)

               (end-shader-mode)

               ;; Draw spheres to show where the lights are
               (dotimes (i +max-lights+)
                 (let ((light (aref lights i)))
                   (if (light-enabled light)
                       (draw-sphere-ex (light-position light) 0.2 8 8 (light-color light))
                       (draw-sphere-wires (light-position light) 0.2 8 8 (color-alpha (light-color light) 0.3)))))

               (draw-grid 10 1.0)

               (end-mode-3d)

               (draw-fps 10 10)

               (draw-text "Use keys [Y][R][G][B] to toggle lights" 10 40 20 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader

      (close-window))))                 ; Close window and OpenGL context

(main)

;;;; raylib [shaders] example - raymarching rendering
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 for shaders support and only #version 330
;;;;       is currently supported. OpenGL ES 2.0 platforms are not supported at the moment
;;;;
;;;; Example originally created with raylib 2.0, last time updated with raylib 4.2
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2018-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_raymarching_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-raymarching-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-raymarching-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-window-resizable+)
    (init-window screen-width screen-height "raylib [shaders] example - raymarching rendering")

    (let* ((camera (make-camera3d :position (vec3 2.5 2.5 3.0)  ; Camera position
                                  :target (vec3 0.0 0.0 0.7)    ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)        ; Camera up vector (rotation towards target)
                                  :fovy 65.0                    ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Load raymarching shader
           ;; NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
           (shader (load-shader nil (text-format "resources/shaders/glsl%i/raymarching.fs" +glsl-version+)))

           ;; Get shader locations for required uniforms
           (view-eye-loc (get-shader-location shader "viewEye"))
           (view-center-loc (get-shader-location shader "viewCenter"))
           (run-time-loc (get-shader-location shader "runTime"))
           (resolution-loc (get-shader-location shader "resolution"))

           (resolution (list (float screen-width) (float screen-height)))

           (run-time 0.0))

      (set-shader-value shader resolution-loc resolution +shader-uniform-vec2+)

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-first-person+)

               (let ((camera-pos (list (vx (camera3d-position camera)) (vy (camera3d-position camera)) (vz (camera3d-position camera))))
                     (camera-target (list (vx (camera3d-target camera)) (vy (camera3d-target camera)) (vz (camera3d-target camera))))
                     (delta-time (get-frame-time)))
                 (incf run-time delta-time)

                 ;; Set shader required uniform values
                 (set-shader-value shader view-eye-loc camera-pos +shader-uniform-vec3+)
                 (set-shader-value shader view-center-loc camera-target +shader-uniform-vec3+)
                 (set-shader-value shader run-time-loc run-time +shader-uniform-float+))

               ;; Check if screen is resized
               (when (is-window-resized)
                 (setf resolution (list (float (get-screen-width)) (float (get-screen-height))))
                 (set-shader-value shader resolution-loc resolution +shader-uniform-vec2+))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; We only draw a white full-screen rectangle,
               ;; frame is generated in shader using raymarching
               (begin-shader-mode shader)
               (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +white+)
               (end-shader-mode)

               (draw-text "(c) Raymarching shader by Iñigo Quilez. MIT License." (- (get-screen-width) 280) (- (get-screen-height) 20) 10 +black+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader

      (close-window))))                 ; Close window and OpenGL context

(main)

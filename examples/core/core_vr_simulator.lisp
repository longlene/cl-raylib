;;;; raylib [core] example - vr simulator
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 4.0
;;;;
;;;; Copyright (c) 2017-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_vr_simulator.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-vr-simulator
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-vr-simulator)

(defconstant +glsl-version+ 330)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    ;; NOTE: screenWidth/screenHeight should match VR device aspect ratio
    (init-window screen-width screen-height "raylib [core] example - vr simulator")

    ;; VR device parameters definition
    (let* ((device (make-vr-device-info
                    ;; Oculus Rift CV1 parameters for simulator
                    :h-resolution 2160                ; Horizontal resolution in pixels
                    :v-resolution 1200                ; Vertical resolution in pixels
                    :h-screen-size 0.133793           ; Horizontal size in meters
                    :v-screen-size 0.0669             ; Vertical size in meters
                    :eye-to-screen-distance 0.041     ; Distance between eye and display in meters
                    :lens-separation-distance 0.07    ; Lens separation distance in meters
                    :interpupillary-distance 0.07     ; IPD (distance between pupils) in meters

                    ;; NOTE: CV1 uses fresnel-hybrid-asymmetric lenses with specific compute shaders
                    ;; Following parameters are just an approximation to CV1 distortion stereo rendering
                    :lens-distortion-values (make-array 4 :element-type 'single-float
                                                          :initial-contents '(1.0 0.22 0.24 0.0)) ; Lens distortion constant parameters
                    :chroma-ab-correction (make-array 4 :element-type 'single-float
                                                        :initial-contents '(0.996 -0.004 1.014 0.0)))) ; Chromatic aberration correction parameters
           ;; Load VR stereo config for VR device parameteres (Oculus Rift CV1 parameters)
           (config (load-vr-stereo-config device))
           ;; Distortion shader (uses device lens distortion and chroma)
           (distortion (load-shader nil (text-format "resources/shaders/glsl%i/distortion.fs" +glsl-version+))))

      ;; Update distortion shader with lens and distortion-scale parameters
      (set-shader-value distortion (get-shader-location distortion "leftLensCenter")
                        (vr-stereo-config-left-lens-center config) +shader-uniform-vec2+)
      (set-shader-value distortion (get-shader-location distortion "rightLensCenter")
                        (vr-stereo-config-right-lens-center config) +shader-uniform-vec2+)
      (set-shader-value distortion (get-shader-location distortion "leftScreenCenter")
                        (vr-stereo-config-left-screen-center config) +shader-uniform-vec2+)
      (set-shader-value distortion (get-shader-location distortion "rightScreenCenter")
                        (vr-stereo-config-right-screen-center config) +shader-uniform-vec2+)

      (set-shader-value distortion (get-shader-location distortion "scale")
                        (vr-stereo-config-scale config) +shader-uniform-vec2+)
      (set-shader-value distortion (get-shader-location distortion "scaleIn")
                        (vr-stereo-config-scale-in config) +shader-uniform-vec2+)
      (set-shader-value distortion (get-shader-location distortion "deviceWarpParam")
                        (vr-device-info-lens-distortion-values device) +shader-uniform-vec4+)
      (set-shader-value distortion (get-shader-location distortion "chromaAbParam")
                        (vr-device-info-chroma-ab-correction device) +shader-uniform-vec4+)

      ;; Initialize framebuffer for stereo rendering
      ;; NOTE: Screen size should match HMD aspect ratio
      (let* ((target (load-render-texture (vr-device-info-h-resolution device) (vr-device-info-v-resolution device)))
             ;; The target's height is flipped (in the source Rectangle), due to OpenGL reasons
             (source-rec (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width (render-texture-texture target)))
                                         :height (- (float (texture-height (render-texture-texture target))))))
             (dest-rec (make-rectangle :x 0.0 :y 0.0 :width (float (get-screen-width)) :height (float (get-screen-height))))
             ;; Define the camera to look into our 3d world
             (camera (make-camera3d :position (vec3 5.0 2.0 5.0) ; Camera position
                                    :target (vec3 0.0 2.0 0.0)   ; Camera looking at point
                                    :up (vec3 0.0 1.0 0.0)       ; Camera up vector
                                    :fovy 60.0                   ; Camera field-of-view Y
                                    :projection +camera-perspective+)) ; Camera projection type
             (cube-position (vec3 0.0 0.0 0.0)))

        (disable-cursor)                ; Limit cursor to relative movement inside the window

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (update-camera camera +camera-first-person+)
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-texture-mode target)
                 (clear-background +raywhite+)
                 (begin-vr-stereo-mode config)
                 (begin-mode-3d camera)

                 (draw-cube cube-position 2.0 2.0 2.0 +red+)
                 (draw-cube-wires cube-position 2.0 2.0 2.0 +maroon+)
                 (draw-grid 40 1.0)

                 (end-mode-3d)
                 (end-vr-stereo-mode)
                 (end-texture-mode)

                 (begin-drawing)
                 (clear-background +raywhite+)
                 (begin-shader-mode distortion)
                 (draw-texture-pro (render-texture-texture target) source-rec dest-rec (vec2 0.0 0.0) 0.0 +white+)
                 (end-shader-mode)
                 (draw-fps 10 10)
                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-vr-stereo-config config) ; Unload stereo config

        (unload-render-texture target)  ; Unload stereo render fbo
        (unload-shader distortion)      ; Unload distortion shader

        (close-window)))))              ; Close window and OpenGL context

(main)

;;;; raylib [shaders] example - lights bloom
;;;;
;;;; Example demonstrates forward multi-point lighting (8 point lights, attenuation and
;;;; Blinn-Phong specular) combined with a threshold-based bloom pass and Reinhard tone
;;;; mapping, applied as a full-screen post-process pass
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example uses resources/shaders/glsl100 and resources/shaders/glsl330, shared with the
;;;; rest of the shaders examples
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by PanicTitan (@PanicTitan) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 PanicTitan (@PanicTitan)
;;;; Common Lisp port of raylib/examples/shaders/shaders_lights_bloom.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-lights-bloom
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-lights-bloom)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;----------------------------------------------------------------------------------
;; Global Definitions
;;----------------------------------------------------------------------------------
(defconstant +max-lights+ 8)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags (logior +flag-msaa-4x-hint+ +flag-vsync-hint+))
    (init-window screen-width screen-height "raylib [shaders] example - lights bloom")

    (let* ((camera (make-camera3d :position (vec3 0.0 5.0 9.0)
                                  :target (vec3 0.0 0.5 0.0)
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 45.0
                                  :projection +camera-perspective+))

           (target (load-render-texture screen-width screen-height))

           ;; Forward multi-light shader and bloom post-process shader, both loaded from the
           ;; resources folder shared with the rest of the shaders examples. If the GLSL version
           ;; guessed from the PLATFORM_DESKTOP build flag turns out to be wrong for whatever
           ;; raylib was built against, linking fails and silently falls back to the default
           ;; shader - so this retries once with the other version, and either way the result
           ;; is checked explicitly and reported on screen rather than staying silently wrong
           (light-shader (load-shader (text-format "resources/shaders/glsl%i/lights_bloom.vs" +glsl-version+)
                                      (text-format "resources/shaders/glsl%i/lights_bloom.fs" +glsl-version+)))
           (bloom-shader (load-shader nil (text-format "resources/shaders/glsl%i/lights_bloom_post.fs" +glsl-version+)))

           ;; Load models from generated cube mesh and plane
           ;; NOTE: Meshes are automatically unloaded on UnloadModel()
           (cube (load-model-from-mesh (gen-mesh-cube 2.0 2.0 2.0)))
           (floor (load-model-from-mesh (gen-mesh-plane 14.0 14.0 1 1)))

           (light-pos-loc (get-shader-location light-shader "lightPositions"))
           (light-col-loc (get-shader-location light-shader "lightColors"))
           (view-pos-loc (get-shader-location light-shader "viewPos"))

           (light-positions (make-array +max-lights+))
           (light-colors (vector (vec3 1.0 0.2 0.2)   ; Red
                                 (vec3 0.2 1.0 0.3)   ; Green
                                 (vec3 0.2 0.5 1.0)   ; Blue
                                 (vec3 1.0 0.8 0.1)   ; Yellow
                                 (vec3 1.0 0.1 0.8)   ; Magenta
                                 (vec3 0.1 1.0 1.0)   ; Cyan
                                 (vec3 1.0 0.4 0.1)   ; Orange
                                 (vec3 0.7 0.2 1.0)))) ; Purple

      (setf (material-shader (aref (model-materials cube) 0)) light-shader
            (material-shader (aref (model-materials floor) 0)) light-shader)

      (dotimes (i +max-lights+) (setf (aref light-positions i) (vec3 0.0 0.0 0.0)))

      (set-shader-value-v light-shader light-col-loc light-colors +shader-uniform-vec3+ +max-lights+)

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               (let ((time (float (get-time) 1.0)))

                 ;; Orbital path for each point light, its own radius/height offset by index
                 (dotimes (i +max-lights+)
                   (let* ((angle (+ (* (/ i (float +max-lights+)) 2.0 +pi+) (* time 0.6)))
                          (radius (+ 4.2 (* (sin (+ (* time 1.2) i)) 0.4)))
                          (height (+ 1.0 (* (sin (+ (* time 1.8) i)) 0.6))))
                     (setf (aref light-positions i) (vec3 (* (sin angle) radius) height (* (cos angle) radius))))))

               (set-shader-value-v light-shader light-pos-loc light-positions +shader-uniform-vec3+ +max-lights+)
               (set-shader-value light-shader view-pos-loc (camera3d-position camera) +shader-uniform-vec3+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               ;; Render the lit scene to an offscreen texture
               (begin-texture-mode target)
               (clear-background '(12 12 18 255))

               (begin-mode-3d camera)

               (draw-model cube (vec3 0.0 1.0 0.0) 1.0 +gray+)
               (draw-model floor (vec3 0.0 0.0 0.0) 1.0 +darkgray+)
               (draw-grid 10 1.0)

               ;; Glowing bulbs mark each light's position
               (dotimes (i +max-lights+)
                 (let* ((c (aref light-colors i))
                        (bulb-color (list (truncate (* (vx c) 255))
                                          (truncate (* (vy c) 255))
                                          (truncate (* (vz c) 255))
                                          255)))
                   (draw-sphere (aref light-positions i) 0.15 bulb-color)))

               (end-mode-3d)
               (end-texture-mode)

               ;; Present the offscreen texture through the bloom + tone mapping shader
               (begin-drawing)
               (clear-background +black+)

               (let* ((tex (render-texture-texture target))
                      (src-rec (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width tex)) :height (float (- (texture-height tex))))))
                 (begin-shader-mode bloom-shader)
                 ;; NOTE: Render texture must be y-flipped due to default OpenGL coordinates (left-bottom)
                 (draw-texture-rec tex src-rec (vec2 0.0 0.0) +white+)
                 (end-shader-mode))

               (draw-text "BALANCED MULTI-LIGHT + REINHARD TONE MAPPED BLOOM" 20 20 20 +green+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model cube)
      (unload-model floor)
      (unload-shader light-shader)
      (unload-shader bloom-shader)
      (unload-render-texture target)

      (close-window))))                 ; Close window and OpenGL context

(main)

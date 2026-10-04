;;;; raylib [shaders] example - normalmap rendering
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;      OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Jeremy Montgomery (@Sir_Irk) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Jeremy Montgomery (@Sir_Irk) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_normalmap_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-normalmap-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-normalmap-rendering)

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
    (init-window screen-width screen-height "raylib [shaders] example - normalmap rendering")

    (let* ((camera (make-camera3d :position (vec3 0.0 2.0 -4.0)
                                  :target (vec3 0.0 0.0 0.0)
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 45.0
                                  :projection +camera-perspective+))

           ;; Load basic normal map lighting shader
           (shader (load-shader (text-format "resources/shaders/glsl%i/normalmap.vs" +glsl-version+)
                                (text-format "resources/shaders/glsl%i/normalmap.fs" +glsl-version+)))

           ;; This example uses just 1 point light
           (light-position (vec3 0.0 1.0 0.0))
           (light-pos-loc 0)

           ;; Load a plane model that has proper normals and tangents
           (plane (load-model "resources/models/plane.glb"))
           (material (aref (model-materials plane) 0))

           ;; Specular exponent AKA shininess of the material
           (specular-exponent 8.0)
           (specular-exponent-loc 0)

           ;; Allow toggling the normal map on and off for comparison purposes
           (use-normal-map 1)
           (use-normal-map-loc 0))

      ;; Get some required shader locations
      (setf (aref (shader-locs shader) +shader-loc-map-normal+) (get-shader-location shader "normalMap")
            (aref (shader-locs shader) +shader-loc-vector-view+) (get-shader-location shader "viewPos"))
      ;; NOTE: "matModel" location name is automatically assigned on shader loading,
      ;; no need to get the location again if using that uniform name
      ;; (setf (aref (shader-locs shader) +shader-loc-matrix-model+) (get-shader-location shader "matModel"))

      (setf light-pos-loc (get-shader-location shader "lightPos"))

      ;; Set the plane model's shader and texture maps
      (setf (material-shader material) shader)
      (setf (material-map-texture (aref (material-maps material) +material-map-diffuse+)) (load-texture "resources/tiles_diffuse.png")
            (material-map-texture (aref (material-maps material) +material-map-normal+)) (load-texture "resources/tiles_normal.png"))

      ;; Generate Mipmaps and use TRILINEAR filtering to help with texture aliasing
      (gen-texture-mipmaps (material-map-texture (aref (material-maps material) +material-map-diffuse+)))
      (gen-texture-mipmaps (material-map-texture (aref (material-maps material) +material-map-normal+)))

      (set-texture-filter (material-map-texture (aref (material-maps material) +material-map-diffuse+)) +texture-filter-trilinear+)
      (set-texture-filter (material-map-texture (aref (material-maps material) +material-map-normal+)) +texture-filter-trilinear+)

      (setf specular-exponent-loc (get-shader-location shader "specularExponent")
            use-normal-map-loc (get-shader-location shader "useNormalMap"))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Move the light around on the X and Z axis using WASD keys
               (let ((direction (vec3 0.0 0.0 0.0)))
                 (when (is-key-down +key-w+) (setf direction (vector3-add direction (vec3 0.0 0.0 1.0))))
                 (when (is-key-down +key-s+) (setf direction (vector3-add direction (vec3 0.0 0.0 -1.0))))
                 (when (is-key-down +key-d+) (setf direction (vector3-add direction (vec3 -1.0 0.0 0.0))))
                 (when (is-key-down +key-a+) (setf direction (vector3-add direction (vec3 1.0 0.0 0.0))))

                 (setf direction (vector3-normalize direction))
                 (setf light-position (vector3-add light-position (vector3-scale direction (* (get-frame-time) 3.0)))))

               ;; Increase/Decrease the specular exponent(shininess)
               (when (is-key-down +key-up+) (setf specular-exponent (clamp (+ specular-exponent (* 40.0 (get-frame-time))) 2.0 128.0)))
               (when (is-key-down +key-down+) (setf specular-exponent (clamp (- specular-exponent (* 40.0 (get-frame-time))) 2.0 128.0)))

               ;; Toggle normal map on and off
               (when (is-key-pressed +key-n+) (setf use-normal-map (if (= use-normal-map 0) 1 0)))

               ;; Spin plane model at a constant rate
               (setf (model-transform plane) (matrix-rotate-y (* (float (get-time) 1.0) 0.5)))

               ;; Update shader values
               (let ((light-pos (list (vx light-position) (vy light-position) (vz light-position)))
                     (cam-pos (list (vx (camera3d-position camera)) (vy (camera3d-position camera)) (vz (camera3d-position camera)))))
                 (set-shader-value shader light-pos-loc light-pos +shader-uniform-vec3+)
                 (set-shader-value shader (aref (shader-locs shader) +shader-loc-vector-view+) cam-pos +shader-uniform-vec3+))

               (set-shader-value shader specular-exponent-loc specular-exponent +shader-uniform-float+)
               (set-shader-value shader use-normal-map-loc use-normal-map +shader-uniform-int+)
               ;;--------------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (begin-shader-mode shader)

               (draw-model plane (vector3-zero) 2.0 +white+)

               (end-shader-mode)

               ;; Draw sphere to show light position
               (draw-sphere-wires light-position 0.2 8 8 +orange+)

               (end-mode-3d)

               (let ((text-color (if (/= use-normal-map 0) +darkgreen+ +red+))
                     (toggle-str (if (/= use-normal-map 0) "On" "Off"))
                     (y-offset 24))
                 (draw-text (text-format "Use key [N] to toggle normal map: %s" toggle-str) 10 10 10 text-color)

                 (draw-text "Use keys [W][A][S][D] to move the light" 10 (+ 10 (* y-offset 1)) 10 +black+)
                 (draw-text "Use keys [Up][Down] to change specular exponent" 10 (+ 10 (* y-offset 2)) 10 +black+)
                 (draw-text (text-format "Specular Exponent: %.2f" specular-exponent) 10 (+ 10 (* y-offset 3)) 10 +blue+))

               (draw-fps (- screen-width 90) 10)

               (end-drawing))
      ;;--------------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)
      (unload-model plane)

      (close-window))))                 ; Close window and OpenGL context

(main)

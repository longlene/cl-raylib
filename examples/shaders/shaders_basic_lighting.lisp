;;;; shaders_basic_lighting.lisp - Basic lighting shader example
;;;; Translated from raylib/examples/shaders/shaders_basic_lighting.c

(require :cl-raylib)

(defpackage :shaders-basic-lighting
  (:use :cl :cl-raylib))

(in-package :shaders-basic-lighting)

(defconstant +glsl-version+ 330)  ; Desktop OpenGL version
(defconstant +max-lights+ 4)

;; Temporary implementation of draw-plane if not available
(defun draw-plane (center-pos size color)
  "Draw a plane (horizontal by default)"
  (set-gl-color color)
  (let* ((cx (vx center-pos))
         (cy (vy center-pos))
         (cz (vz center-pos))
         (sx (vx size))
         (sz (vy size))
         (half-x (/ sx 2.0))
         (half-z (/ sz 2.0)))
    (gl:with-primitive :triangles
      (gl:normal 0.0 1.0 0.0)
      
      ;; Triangle 1
      (gl:vertex (- cx half-x) cy (- cz half-z))
      (gl:vertex (+ cx half-x) cy (- cz half-z))
      (gl:vertex (+ cx half-x) cy (+ cz half-z))
      
      ;; Triangle 2
      (gl:vertex (- cx half-x) cy (- cz half-z))
      (gl:vertex (+ cx half-x) cy (+ cz half-z))
      (gl:vertex (- cx half-x) cy (+ cz half-z)))))

;; Light structure
(defstruct light
  enabled
  type
  position
  target
  color
  intensity)

(defun create-light (light-type position target color shader)
  "Create a light with given parameters"
  (let ((light (make-light :enabled t
                          :type light-type
                          :position position
                          :target target
                          :color color
                          :intensity 1.0)))
    ;; In a full implementation, this would set up shader uniforms
    ;; For now, we'll return a basic light structure
    light))

(defun update-light-values (shader light)
  "Update light values in shader (simplified)"
  ;; In a full implementation, this would update shader uniforms
  ;; For now, this is a placeholder
  (declare (ignore shader light))
  nil)

(defun main ()
  "Main function - basic lighting example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (set-config-flags +flag-msaa-4x-hint+)  ; Enable Multi Sampling Anti Aliasing 4x
    (init-window screen-width screen-height "raylib [shaders] example - basic lighting")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 2.0 4.0 6.0)
                                :target (vec3 0.0 0.5 0.0)
                                :up (vec3 0.0 1.0 0.0)
                                :fovy 45.0
                                :projection :camera-perspective)))

      ;; Load basic lighting shader
      (let ((shader (load-shader (format nil "resources/shaders/glsl~a/lighting.vs" +glsl-version+)
                                (format nil "resources/shaders/glsl~a/lighting.fs" +glsl-version+))))

        ;; Get shader locations
        (let ((view-pos-loc (get-shader-location shader "viewPos"))
              (ambient-loc (get-shader-location shader "ambient")))

          ;; Set ambient light level
          (set-shader-value shader ambient-loc (vector 0.1 0.1 0.1 1.0) +shader-uniform-vec4+)

          ;; Create lights (simplified structure)
          (let ((lights (vector
                         (create-light :light-point (vec3 -2.0 1.0 -2.0) (vec3 0.0 0.0 0.0) +yellow+ shader)
                         (create-light :light-point (vec3 2.0 1.0 2.0) (vec3 0.0 0.0 0.0) +red+ shader)
                         (create-light :light-point (vec3 -2.0 1.0 2.0) (vec3 0.0 0.0 0.0) +green+ shader)
                         (create-light :light-point (vec3 2.0 1.0 -2.0) (vec3 0.0 0.0 0.0) +blue+ shader))))

            (set-target-fps 60)

            ;; Main game loop
            (loop until (window-should-close) do
              ;; Update
              (update-camera camera +camera-orbital+)

              ;; Update the shader with the camera view vector
              (let ((camera-pos (vector (vx (camera3d-position camera))
                                       (vy (camera3d-position camera))
                                       (vz (camera3d-position camera)))))
                (set-shader-value shader view-pos-loc camera-pos +shader-uniform-vec3+))

              ;; Check key inputs to enable/disable lights
              (when (is-key-pressed +key-y+)
                (setf (light-enabled (aref lights 0)) (not (light-enabled (aref lights 0)))))
              (when (is-key-pressed +key-r+)
                (setf (light-enabled (aref lights 1)) (not (light-enabled (aref lights 1)))))
              (when (is-key-pressed +key-g+)
                (setf (light-enabled (aref lights 2)) (not (light-enabled (aref lights 2)))))
              (when (is-key-pressed +key-b+)
                (setf (light-enabled (aref lights 3)) (not (light-enabled (aref lights 3)))))

              ;; Update light values
              (dotimes (i +max-lights+)
                (update-light-values shader (aref lights i)))

              ;; Draw
              (begin-drawing)
                (clear-background +raywhite+)

                (begin-mode-3d camera)
                  (begin-shader-mode shader)
                    ;; Draw plane and cube with lighting
                    (draw-plane (vec3 0.0 0.0 0.0) (vec2 10.0 10.0) +white+)
                    (draw-cube-v (vec3 0.0 0.0 0.0) (vec3 2.0 4.0 2.0) +white+)
                  (end-shader-mode)

                  ;; Draw spheres to show where the lights are
                  (dotimes (i +max-lights+)
                    (let ((light (aref lights i)))
                      (if (light-enabled light)
                          (draw-sphere-ex (light-position light) 0.2 8 8 (light-color light))
                          (draw-sphere-wires (light-position light) 0.2 8 8 
                                           (color-alpha (light-color light) 0.3)))))

                  (draw-grid 10 1.0)
                (end-mode-3d)

                (draw-fps 10 10)
                (draw-text "Use keys [Y][R][G][B] to toggle lights" 10 40 20 +darkgray+)
              (end-drawing)))

          ;; De-Initialization
          (unload-shader shader))))

    ;; Close window
    (close-window)))

;; Run the example
(main)
;;;; raylib [textures] example - framebuffer rendering
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.6, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Jack Boakes (@jackboakes) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 Jack Boakes (@jackboakes)
;;;; Common Lisp port of raylib/examples/textures/textures_framebuffer_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-framebuffer-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-framebuffer-rendering)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
(defun draw-camera-prism (camera aspect color)
  (let* ((length (vector3-distance (camera3d-position camera) (camera3d-target camera)))
         ;; Define the 4 corners of the camera's prism plane sliced at the target in Normalized Device Coordinates
         (plane-ndc (vector (vec3 -1.0 -1.0 1.0)   ; Bottom Left
                            (vec3 1.0 -1.0 1.0)    ; Bottom Right
                            (vec3 1.0 1.0 1.0)     ; Top Right
                            (vec3 -1.0 1.0 1.0)))  ; Top Left

         ;; Build the matrices
         (view (get-camera-matrix camera))
         (proj (matrix-perspective (* (camera3d-fovy camera) +deg2rad+) aspect 0.05 length))
         ;; Combine view and projection so we can reverse the full camera transform
         (view-proj (matrix-multiply view proj))
         ;; Invert the view-projection matrix to unproject points from NDC space back into world space
         (inverse-view-proj (matrix-to-float-v (matrix-invert view-proj))) ; m0..m15

         ;; Transform the 4 plane corners from NDC into world space
         (corners (make-array 4)))

    (flet ((m (i) (aref inverse-view-proj i)))
      (dotimes (i 4)
        (let* ((x (vx (aref plane-ndc i)))
               (y (vy (aref plane-ndc i)))
               (z (vz (aref plane-ndc i)))

               ;; Multiply NDC position by the inverse view-projection matrix
               ;; This produces a homogeneous (x, y, z, w) position in world space
               (vx (+ (* (m 0) x) (* (m 4) y) (* (m 8) z) (m 12)))
               (vy (+ (* (m 1) x) (* (m 5) y) (* (m 9) z) (m 13)))
               (vz (+ (* (m 2) x) (* (m 6) y) (* (m 10) z) (m 14)))
               (vw (+ (* (m 3) x) (* (m 7) y) (* (m 11) z) (m 15))))

          (setf (aref corners i) (vec3 (/ vx vw) (/ vy vw) (/ vz vw))))))

    ;; Draw the far plane sliced at the target
    (draw-line-3d (aref corners 0) (aref corners 1) color)
    (draw-line-3d (aref corners 1) (aref corners 2) color)
    (draw-line-3d (aref corners 2) (aref corners 3) color)
    (draw-line-3d (aref corners 3) (aref corners 0) color)

    ;; Draw the prism lines from the far plane to the camera position
    (dotimes (i 4)
      (draw-line-3d (camera3d-position camera) (aref corners i) color))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let* ((screen-width 800)
         (screen-height 450)
         (split-width (truncate screen-width 2)))

    (init-window screen-width screen-height "raylib [textures] example - framebuffer rendering")

    (let* (;; Camera to look at the 3D world
           (subject-camera (make-camera3d :position (vec3 5.0 5.0 5.0)
                                          :target (vec3 0.0 0.0 0.0)
                                          :up (vec3 0.0 1.0 0.0)
                                          :fovy 45.0
                                          :projection +camera-perspective+))

           ;; Camera to observe the subject camera and 3D world
           (observer-camera (make-camera3d :position (vec3 10.0 10.0 10.0)
                                           :target (vec3 0.0 0.0 0.0)
                                           :up (vec3 0.0 1.0 0.0)
                                           :fovy 45.0
                                           :projection +camera-perspective+))

           ;; Set up render textures
           (observer-target (load-render-texture split-width screen-height))
           (observer-source (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width (render-texture-texture observer-target)))
                                            :height (- (float (texture-height (render-texture-texture observer-target))))))
           (observer-dest (make-rectangle :x 0.0 :y 0.0 :width (float split-width) :height (float screen-height)))

           (subject-target (load-render-texture split-width screen-height))
           (subject-texture (render-texture-texture subject-target))
           (subject-source (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width subject-texture))
                                           :height (- (float (texture-height subject-texture)))))
           (subject-dest (make-rectangle :x (float split-width) :y 0.0 :width (float split-width) :height (float screen-height)))
           (texture-aspect-ratio (/ (float (texture-width subject-texture)) (float (texture-height subject-texture))))

           ;; Rectangles for cropping render texture
           (capture-size 128.0)
           (crop-source (make-rectangle :x (/ (- (texture-width subject-texture) capture-size) 2.0)
                                        :y (/ (- (texture-height subject-texture) capture-size) 2.0)
                                        :width capture-size :height (- capture-size)))
           (crop-dest (make-rectangle :x (+ split-width 20.0) :y 20.0 :width capture-size :height capture-size)))

      (set-target-fps 60)
      (disable-cursor)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera observer-camera +camera-free+)
               (update-camera subject-camera +camera-orbital+)

               (when (is-key-pressed +key-r+) (setf (camera3d-target observer-camera) (vec3 0.0 0.0 0.0)))

               ;; Build LHS observer view texture
               (begin-texture-mode observer-target)

               (clear-background +raywhite+)

               (begin-mode-3d observer-camera)

               (draw-grid 10 1.0)
               (draw-cube (vec3 0.0 0.0 0.0) 2.0 2.0 2.0 +gold+)
               (draw-cube-wires (vec3 0.0 0.0 0.0) 2.0 2.0 2.0 +pink+)
               (draw-camera-prism subject-camera texture-aspect-ratio +green+)

               (end-mode-3d)

               (draw-text "Observer View" 10 (- (texture-height (render-texture-texture observer-target)) 30) 20 +black+)
               (draw-text "WASD + Mouse to Move" 10 10 20 +darkgray+)
               (draw-text "Scroll to Zoom" 10 30 20 +darkgray+)
               (draw-text "R to Reset Observer Target" 10 50 20 +darkgray+)

               (end-texture-mode)

               ;; Build RHS subject view texture
               (begin-texture-mode subject-target)

               (clear-background +raywhite+)

               (begin-mode-3d subject-camera)

               (draw-cube (vec3 0.0 0.0 0.0) 2.0 2.0 2.0 +gold+)
               (draw-cube-wires (vec3 0.0 0.0 0.0) 2.0 2.0 2.0 +pink+)
               (draw-grid 10 1.0)

               (end-mode-3d)

               (draw-rectangle-lines (truncate (/ (- (texture-width subject-texture) capture-size) 2.0))
                                     (truncate (/ (- (texture-height subject-texture) capture-size) 2.0))
                                     (truncate capture-size) (truncate capture-size) +green+)
               (draw-text "Subject View" 10 (- (texture-height subject-texture) 30) 20 +black+)

               (end-texture-mode)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +black+)

               ;; Draw observer texture LHS
               (draw-texture-pro (render-texture-texture observer-target) observer-source observer-dest (vec2 0.0 0.0) 0.0 +white+)

               ;; Draw subject texture RHS
               (draw-texture-pro subject-texture subject-source subject-dest (vec2 0.0 0.0) 0.0 +white+)

               ;; Draw the small crop overlay on top
               (draw-texture-pro subject-texture crop-source crop-dest (vec2 0.0 0.0) 0.0 +white+)
               (draw-rectangle-lines-ex crop-dest 2.0 +black+)

               ;; Draw split screen divider line
               (draw-line split-width 0 split-width screen-height +black+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture observer-target)
      (unload-render-texture subject-target)

      (close-window))))                 ; Close window and OpenGL context

(main)

;;;; raylib [models] example - textured cube
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 4.5, last time updated with raylib 4.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2022-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_textured_cube.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-textured-cube
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-textured-cube)

;;------------------------------------------------------------------------------------
;; Custom Functions Definition
;;------------------------------------------------------------------------------------
;; Draw cube textured
;; NOTE: Cube position is the center position
(defun draw-cube-texture (texture position width height length color)
  (let ((x (vx position))
        (y (vy position))
        (z (vz position)))

    ;; Set desired texture to be enabled while drawing following vertex data
    (rl-set-texture (texture-id texture))

    ;; Vertex data transformation can be defined with the commented lines,
    ;; but in this example we calculate the transformed vertex data directly when calling rlVertex3f()
    ;;(rl-push-matrix)
    ;; NOTE: Transformation is applied in inverse order (scale -> rotate -> translate)
    ;;(rl-translatef 2.0 0.0 0.0)
    ;;(rl-rotatef 45 0 1 0)
    ;;(rl-scalef 2.0 2.0 2.0)

    (rl-begin +rl-quads+)
    (rl-color4ub (first color) (second color) (third color) (fourth color))
    ;; Front Face
    (rl-normal3f 0.0 0.0 1.0)           ; Normal Pointing Towards Viewer
    (rl-tex-coord2f 0.0 0.0) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Left Of The Texture and Quad
    (rl-tex-coord2f 1.0 0.0) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Right Of The Texture and Quad
    (rl-tex-coord2f 1.0 1.0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Right Of The Texture and Quad
    (rl-tex-coord2f 0.0 1.0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left Of The Texture and Quad
    ;; Back Face
    (rl-normal3f 0.0 0.0 -1.0)          ; Normal Pointing Away From Viewer
    (rl-tex-coord2f 1.0 0.0) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Right Of The Texture and Quad
    (rl-tex-coord2f 1.0 1.0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Right Of The Texture and Quad
    (rl-tex-coord2f 0.0 1.0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Left Of The Texture and Quad
    (rl-tex-coord2f 0.0 0.0) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Left Of The Texture and Quad
    ;; Top Face
    (rl-normal3f 0.0 1.0 0.0)           ; Normal Pointing Up
    (rl-tex-coord2f 0.0 1.0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Left Of The Texture and Quad
    (rl-tex-coord2f 0.0 0.0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Bottom Left Of The Texture and Quad
    (rl-tex-coord2f 1.0 0.0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Bottom Right Of The Texture and Quad
    (rl-tex-coord2f 1.0 1.0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Right Of The Texture and Quad
    ;; Bottom Face
    (rl-normal3f 0.0 -1.0 0.0)          ; Normal Pointing Down
    (rl-tex-coord2f 1.0 1.0) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Top Right Of The Texture and Quad
    (rl-tex-coord2f 0.0 1.0) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Top Left Of The Texture and Quad
    (rl-tex-coord2f 0.0 0.0) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Left Of The Texture and Quad
    (rl-tex-coord2f 1.0 0.0) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Right Of The Texture and Quad
    ;; Right face
    (rl-normal3f 1.0 0.0 0.0)           ; Normal Pointing Right
    (rl-tex-coord2f 1.0 0.0) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Right Of The Texture and Quad
    (rl-tex-coord2f 1.0 1.0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Right Of The Texture and Quad
    (rl-tex-coord2f 0.0 1.0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left Of The Texture and Quad
    (rl-tex-coord2f 0.0 0.0) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Left Of The Texture and Quad
    ;; Left Face
    (rl-normal3f -1.0 0.0 0.0)          ; Normal Pointing Left
    (rl-tex-coord2f 0.0 0.0) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Left Of The Texture and Quad
    (rl-tex-coord2f 1.0 0.0) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Right Of The Texture and Quad
    (rl-tex-coord2f 1.0 1.0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Right Of The Texture and Quad
    (rl-tex-coord2f 0.0 1.0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Left Of The Texture and Quad
    (rl-end)
    ;;(rl-pop-matrix)

    (rl-set-texture 0)))

;; Draw cube with texture piece applied to all faces
(defun draw-cube-texture-rec (texture source position width height length color)
  (let* ((x (vx position))
         (y (vy position))
         (z (vz position))
         (tex-width (float (texture-width texture)))
         (tex-height (float (texture-height texture)))
         ;; We calculate the normalized texture coordinates for the desired texture-source-rectangle
         ;; It means converting from (tex.width, tex.height) coordinates to [0.0f, 1.0f] equivalent
         (u0 (/ (rectangle-x source) tex-width))
         (u1 (/ (+ (rectangle-x source) (rectangle-width source)) tex-width))
         (v0 (/ (rectangle-y source) tex-height))
         (v1 (/ (+ (rectangle-y source) (rectangle-height source)) tex-height)))

    ;; Set desired texture to be enabled while drawing following vertex data
    (rl-set-texture (texture-id texture))

    (rl-begin +rl-quads+)
    (rl-color4ub (first color) (second color) (third color) (fourth color))

    ;; Front face
    (rl-normal3f 0.0 0.0 1.0)
    (rl-tex-coord2f u0 v1) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2)))
    (rl-tex-coord2f u1 v1) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2)))
    (rl-tex-coord2f u1 v0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2)))
    (rl-tex-coord2f u0 v0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2)))

    ;; Back face
    (rl-normal3f 0.0 0.0 -1.0)
    (rl-tex-coord2f u1 v1) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2)))
    (rl-tex-coord2f u1 v0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2)))
    (rl-tex-coord2f u0 v0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2)))
    (rl-tex-coord2f u0 v1) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2)))

    ;; Top face
    (rl-normal3f 0.0 1.0 0.0)
    (rl-tex-coord2f u0 v0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2)))
    (rl-tex-coord2f u0 v1) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2)))
    (rl-tex-coord2f u1 v1) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2)))
    (rl-tex-coord2f u1 v0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2)))

    ;; Bottom face
    (rl-normal3f 0.0 -1.0 0.0)
    (rl-tex-coord2f u1 v0) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2)))
    (rl-tex-coord2f u0 v0) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2)))
    (rl-tex-coord2f u0 v1) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2)))
    (rl-tex-coord2f u1 v1) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2)))

    ;; Right face
    (rl-normal3f 1.0 0.0 0.0)
    (rl-tex-coord2f u1 v1) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2)))
    (rl-tex-coord2f u1 v0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2)))
    (rl-tex-coord2f u0 v0) (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2)))
    (rl-tex-coord2f u0 v1) (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2)))

    ;; Left face
    (rl-normal3f -1.0 0.0 0.0)
    (rl-tex-coord2f u0 v1) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2)))
    (rl-tex-coord2f u1 v1) (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2)))
    (rl-tex-coord2f u1 v0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2)))
    (rl-tex-coord2f u0 v0) (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2)))

    (rl-end)

    (rl-set-texture 0)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - textured cube")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 0.0 10.0 10.0)
                                 :target (vec3 0.0 0.0 0.0)
                                 :up (vec3 0.0 1.0 0.0)
                                 :fovy 45.0
                                 :projection +camera-perspective+))

          ;; Load texture to be applied to the cubes sides
          (texture (load-texture "resources/cubicmap_atlas.png")))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; TODO: Update your variables here
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               ;; Draw cube with an applied texture
               (draw-cube-texture texture (vec3 -2.0 2.0 0.0) 2.0 4.0 2.0 +white+)

               ;; Draw cube with an applied texture, but only a defined rectangle piece of the texture
               (draw-cube-texture-rec texture (make-rectangle :x 0.0 :y (/ (texture-height texture) 2.0) :width (/ (texture-width texture) 2.0) :height (/ (texture-height texture) 2.0))
                                      (vec3 2.0 1.0 0.0) 2.0 2.0 2.0 +white+)

               (draw-grid 10 1.0)        ; Draw a grid

               (end-mode-3d)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture texture)          ; Unload texture

      (close-window))))                 ; Close window and OpenGL context

(main)

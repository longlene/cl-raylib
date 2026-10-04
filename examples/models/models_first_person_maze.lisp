;;;; raylib [models] example - first person maze
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_first_person_maze.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-first-person-maze
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-first-person-maze)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - first person maze")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 0.2 0.4 0.2)   ; Camera position
                                  :target (vec3 0.185 0.4 0.0)   ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)         ; Camera up vector (rotation towards target)
                                  :fovy 45.0                     ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           (im-map (load-image "resources/cubicmap.png"))  ; Load cubicmap image (RAM)
           (cubicmap (load-texture-from-image im-map))     ; Convert image to texture to display (VRAM)
           (mesh (gen-mesh-cubicmap im-map (vec3 1.0 1.0 1.0)))
           (model (load-model-from-mesh mesh))

           ;; NOTE: By default each cube is mapped to one part of texture atlas
           (texture (load-texture "resources/cubicmap_atlas.png")) ; Load map texture

           ;; Get map image data to be used for collision detection
           (map-pixels (load-image-colors im-map))

           (map-position (vec3 -16.0 0.0 -8.0))) ; Set model position

      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture) ; Set map diffuse texture

      (unload-image im-map)             ; Unload image from RAM

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((old-cam-pos (vcopy (camera3d-position camera)))) ; Store old camera position

                 (update-camera camera +camera-first-person+)

                 ;; Check player collision (we simplify to 2D collision detection)
                 (let* ((player-pos (vec2 (vx (camera3d-position camera)) (vz (camera3d-position camera))))
                        (player-radius 0.1)     ; Collision radius (player is modelled as a cilinder for collision)

                        (player-cell-x (truncate (+ (- (vx player-pos) (vx map-position)) 0.5)))
                        (player-cell-y (truncate (+ (- (vy player-pos) (vz map-position)) 0.5))))

                   ;; Out-of-limits security check
                   (cond ((< player-cell-x 0) (setf player-cell-x 0))
                         ((>= player-cell-x (texture-width cubicmap)) (setf player-cell-x (1- (texture-width cubicmap)))))

                   (cond ((< player-cell-y 0) (setf player-cell-y 0))
                         ((>= player-cell-y (texture-height cubicmap)) (setf player-cell-y (1- (texture-height cubicmap)))))

                   ;; Check map collisions using image data and player position against surrounding cells only
                   (loop for y from (1- player-cell-y) to (1+ player-cell-y)
                         ;; Avoid map accessing out of bounds
                         when (and (>= y 0) (< y (texture-height cubicmap)))
                           do (loop for x from (1- player-cell-x) to (1+ player-cell-x)
                                    ;; NOTE: Collision: Only checking R channel for white pixel
                                    do (when (and (and (>= x 0) (< x (texture-width cubicmap)))
                                                  (= (first (aref map-pixels (+ (* y (texture-width cubicmap)) x))) 255)
                                                  (check-collision-circle-rec player-pos player-radius
                                                                              (make-rectangle :x (+ (- (vx map-position) 0.5) (* x 1.0)) :y (+ (- (vz map-position) 0.5) (* y 1.0)) :width 1.0 :height 1.0)))
                                         ;; Collision detected, reset camera position
                                         (setf (camera3d-position camera) old-cam-pos))))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (begin-mode-3d camera)
                   (draw-model model map-position 1.0 +white+) ; Draw maze map
                   (end-mode-3d)

                   (draw-texture-ex cubicmap (vec2 (- (get-screen-width) (* (texture-width cubicmap) 4.0) 20) 20.0) 0.0 4.0 +white+)
                   (draw-rectangle-lines (- (get-screen-width) (* (texture-width cubicmap) 4) 20) 20 (* (texture-width cubicmap) 4) (* (texture-height cubicmap) 4) +green+)

                   ;; Draw player position radar
                   (draw-rectangle (+ (- (get-screen-width) (* (texture-width cubicmap) 4) 20) (* player-cell-x 4)) (+ 20 (* player-cell-y 4)) 4 4 +red+)

                   (draw-fps 10 10)

                   (end-drawing))))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-image-colors map-pixels)  ; Unload color array
      (unload-texture cubicmap)         ; Unload cubicmap texture
      (unload-texture texture)          ; Unload map texture
      (unload-model model)              ; Unload map model

      (close-window))))                 ; Close window and OpenGL context

(main)

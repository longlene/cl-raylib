;;;; models_first_person_maze.lisp - First person maze
;;;; Translated from raylib/examples/models/models_first_person_maze.c

(require :cl-raylib)

(defpackage :models-first-person-maze
  (:use :cl :cl-raylib))

(in-package :models-first-person-maze)

(defun main ()
  "Main function - first person maze"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [models] example - first person maze")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera-3d :position (vec3 0.2 0.4 0.2)    ; Camera position
                                  :target (vec3 0.185 0.4 0.0)    ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                  :fovy 45.0                      ; Camera field-of-view Y
                                  :projection +camera-perspective+))) ; Camera projection type

      (let ((im-map (load-image "examples/models/resources/cubicmap.png"))) ; Load cubicmap image (RAM)
        (let ((cubicmap (load-texture-from-image im-map)) ; Convert image to texture to display (VRAM)
              (mesh (gen-mesh-cubicmap im-map (vec3 1.0 1.0 1.0)))
              (model (load-model-from-mesh mesh)))

          ;; NOTE: By default each cube is mapped to one part of texture atlas
          (let ((texture (load-texture "examples/models/resources/cubicmap_atlas.png"))) ; Load map texture
            ;; Set map diffuse texture
            (setf (material-map-texture (aref (model-materials model) 0) +material-map-diffuse+) texture)

            ;; Get map image data to be used for collision detection
            (let ((map-pixels (load-image-colors im-map)))
              (unload-image im-map) ; Unload image from RAM

              (let ((map-position (vec3 -16.0 0.0 -8.0))) ; Set model position

                (disable-cursor) ; Limit cursor to relative movement inside the window

                (set-target-fps 60) ; Set game to run at 60 frames-per-second

                ;; Main game loop
                (loop until (window-should-close) do
                  ;; Update
                  (let ((old-cam-pos (copy-vec3 (camera-3d-position camera)))) ; Store old camera position

                    (update-camera camera +camera-first-person+)

                    ;; Check player collision (we simplify to 2D collision detection)
                    (let* ((player-pos (vec2 (vx (camera-3d-position camera)) 
                                            (vz (camera-3d-position camera))))
                           (player-radius 0.1) ; Collision radius (player is modelled as a cylinder for collision)
                           (player-cell-x (truncate (+ (- (vx player-pos) (vx map-position)) 0.5)))
                           (player-cell-y (truncate (+ (- (vy player-pos) (vz map-position)) 0.5))))

                      ;; Out-of-limits security check
                      (when (< player-cell-x 0) (setf player-cell-x 0))
                      (when (>= player-cell-x (texture-width cubicmap)) 
                        (setf player-cell-x (1- (texture-width cubicmap))))

                      (when (< player-cell-y 0) (setf player-cell-y 0))
                      (when (>= player-cell-y (texture-height cubicmap)) 
                        (setf player-cell-y (1- (texture-height cubicmap))))

                      ;; Check map collisions using image data and player position
                      ;; TODO: Improvement: Just check player surrounding cells for collision
                      (loop for y from 0 below (texture-height cubicmap) do
                        (loop for x from 0 below (texture-width cubicmap) do
                          (let ((pixel (aref map-pixels (+ (* y (texture-width cubicmap)) x))))
                            (when (and (= (color-r pixel) 255) ; Collision: white pixel, only check R channel
                                       (check-collision-circle-rec 
                                        player-pos 
                                        player-radius
                                        (make-rectangle :x (+ (vx map-position) -0.5 (* x 1.0))
                                                       :y (+ (vz map-position) -0.5 (* y 1.0))
                                                       :width 1.0
                                                       :height 1.0)))
                              ;; Collision detected, reset camera position
                              (setf (camera-3d-position camera) old-cam-pos)))))))

                  ;; Draw
                  (begin-drawing)
                    (clear-background +raywhite+)

                    (begin-mode-3d camera)
                      (draw-model model map-position 1.0 +white+) ; Draw maze map
                    (end-mode-3d)

                    (draw-texture-ex cubicmap 
                                    (vec2 (- screen-width (* (texture-width cubicmap) 4.0) 20) 20.0)
                                    0.0 4.0 +white+)
                    (draw-rectangle-lines (- screen-width (* (texture-width cubicmap) 4) 20) 20 
                                         (* (texture-width cubicmap) 4) 
                                         (* (texture-height cubicmap) 4) +green+)

                    ;; Draw player position radar
                    (draw-rectangle (+ (- screen-width (* (texture-width cubicmap) 4) 20) (* player-cell-x 4))
                                   (+ 20 (* player-cell-y 4)) 4 4 +red+)

                    (draw-fps 10 10)

                  (end-drawing))

                ;; De-Initialization
                (unload-image-colors map-pixels))) ; Unload color array

            (unload-texture cubicmap) ; Unload cubicmap texture
            (unload-texture texture) ; Unload map texture
            (unload-model model))))) ; Unload map model

    ;; Close window
    (close-window)))

;; Run the example
(main)
;;;; raylib [textures] example - fog of war
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 4.2, last time updated with raylib 4.2
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2018-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_fog_of_war.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-fog-of-war
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-fog-of-war)

(defconstant +map-tile-size+ 32)        ; Tiles size 32x32 pixels
(defconstant +player-size+ 16)          ; Player size
(defconstant +player-tile-visibility+ 2) ; Player can see 2 tiles around its position

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Map data type
(defstruct map-data
  (tiles-x 0)                           ; Number of tiles in X axis
  (tiles-y 0)                           ; Number of tiles in Y axis
  (tile-ids nil)                        ; Tile ids (tilesX*tilesY), defines type of tile to draw
  (tile-fog nil))                       ; Tile fog state (tilesX*tilesY), defines if a tile has fog or half-fog

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - fog of war")

    (let ((map (make-map-data)))
      (setf (map-data-tiles-x map) 25
            (map-data-tiles-y map) 15)

      ;; NOTE: We can have up to 256 values for tile ids and for tile fog state,
      ;; probably we don't need that many values for fog state, it can be optimized
      ;; to use only 2 bits per fog state (reducing size by 4) but logic will be a bit more complex
      (setf (map-data-tile-ids map) (make-array (* (map-data-tiles-x map) (map-data-tiles-y map)) :element-type '(unsigned-byte 8) :initial-element 0)
            (map-data-tile-fog map) (make-array (* (map-data-tiles-x map) (map-data-tiles-y map)) :element-type '(unsigned-byte 8) :initial-element 0))

      ;; Load map tiles (generating 2 random tile ids for testing)
      ;; NOTE: Map tile ids should be probably loaded from an external map file
      (dotimes (i (* (map-data-tiles-y map) (map-data-tiles-x map))) (setf (aref (map-data-tile-ids map) i) (get-random-value 0 1)))

      (let* ((tiles-x (map-data-tiles-x map))
             (tiles-y (map-data-tiles-y map))
             (tile-ids (map-data-tile-ids map))
             (tile-fog (map-data-tile-fog map))
             ;; Player position on the screen (pixel coordinates, not tile coordinates)
             (player-position (vec2 180.0 130.0))
             (player-tile-x 0)
             (player-tile-y 0)

             ;; Render texture to render fog of war
             ;; NOTE: To get an automatic smooth-fog effect we use a render texture to render fog
             ;; at a smaller size (one pixel per tile) and scale it on drawing with bilinear filtering
             (fog-of-war (load-render-texture tiles-x tiles-y)))
        (set-texture-filter (render-texture-texture fog-of-war) +texture-filter-bilinear+)
        (set-texture-wrap (render-texture-texture fog-of-war) +texture-wrap-clamp+)

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 ;; Move player around
                 (when (is-key-down +key-right+) (incf (vx player-position) 5))
                 (when (is-key-down +key-left+) (decf (vx player-position) 5))
                 (when (is-key-down +key-down+) (incf (vy player-position) 5))
                 (when (is-key-down +key-up+) (decf (vy player-position) 5))

                 ;; Check player position to avoid moving outside tilemap limits
                 (cond ((< (vx player-position) 0) (setf (vx player-position) 0.0))
                       ((> (+ (vx player-position) +player-size+) (* tiles-x +map-tile-size+))
                        (setf (vx player-position) (- (* (float tiles-x) +map-tile-size+) +player-size+))))
                 (cond ((< (vy player-position) 0) (setf (vy player-position) 0.0))
                       ((> (+ (vy player-position) +player-size+) (* tiles-y +map-tile-size+))
                        (setf (vy player-position) (- (* (float tiles-y) +map-tile-size+) +player-size+))))

                 ;; Previous visited tiles are set to partial fog
                 (dotimes (i (* tiles-x tiles-y)) (when (= (aref tile-fog i) 1) (setf (aref tile-fog i) 2)))

                 ;; Get current tile position from player pixel position
                 (setf player-tile-x (truncate (/ (+ (vx player-position) (/ (float +map-tile-size+) 2)) +map-tile-size+))
                       player-tile-y (truncate (/ (+ (vy player-position) (/ (float +map-tile-size+) 2)) +map-tile-size+)))

                 ;; Check visibility and update fog
                 ;; NOTE: We check tilemap limits to avoid processing tiles out-of-array-bounds (it could crash program)
                 (loop for y from (- player-tile-y +player-tile-visibility+) below (+ player-tile-y +player-tile-visibility+)
                       do (loop for x from (- player-tile-x +player-tile-visibility+) below (+ player-tile-x +player-tile-visibility+)
                                do (when (and (>= x 0) (< x tiles-x) (>= y 0) (< y tiles-y)) (setf (aref tile-fog (+ (* y tiles-x) x)) 1))))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 ;; Draw fog of war to a small render texture for automatic smoothing on scaling
                 (begin-texture-mode fog-of-war)
                 (clear-background +blank+)
                 (dotimes (y tiles-y)
                   (dotimes (x tiles-x)
                     (case (aref tile-fog (+ (* y tiles-x) x))
                       (0 (draw-rectangle x y 1 1 +black+))
                       (2 (draw-rectangle x y 1 1 (fade +black+ 0.8))))))
                 (end-texture-mode)

                 (begin-drawing)

                 (clear-background +raywhite+)

                 (dotimes (y tiles-y)
                   (dotimes (x tiles-x)
                     ;; Draw tiles from id (and tile borders)
                     (draw-rectangle (* x +map-tile-size+) (* y +map-tile-size+) +map-tile-size+ +map-tile-size+
                                     (if (= (aref tile-ids (+ (* y tiles-x) x)) 0) +blue+ (fade +blue+ 0.9)))
                     (draw-rectangle-lines (* x +map-tile-size+) (* y +map-tile-size+) +map-tile-size+ +map-tile-size+ (fade +darkblue+ 0.5))))

                 ;; Draw player
                 (draw-rectangle-v player-position (vec2 (float +player-size+) (float +player-size+)) +red+)

                 ;; Draw fog of war (scaled to full map, bilinear filtering)
                 (let ((texture (render-texture-texture fog-of-war)))
                   (draw-texture-pro texture (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width texture)) :height (float (- (texture-height texture))))
                                     (make-rectangle :x 0.0 :y 0.0 :width (* (float tiles-x) +map-tile-size+) :height (* (float tiles-y) +map-tile-size+))
                                     (vec2 0.0 0.0) 0.0 +white+))

                 ;; Draw player current tile
                 (draw-text (text-format "Current tile: [%i,%i]" player-tile-x player-tile-y) 10 10 20 +raywhite+)
                 (draw-text "ARROW KEYS to move" 10 (- screen-height 25) 20 +raywhite+)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-render-texture fog-of-war) ; Unload render texture

        (close-window)))))              ; Close window and OpenGL context

(main)

;;;; raylib [textures] example - tiled drawing
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 3.0, last time updated with raylib 4.2
;;;;
;;;; Example contributed by Vlad Adrian (@demizdor) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2020-2025 Vlad Adrian (@demizdor) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_tiled_drawing.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-tiled-drawing
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-tiled-drawing)

(defconstant +opt-width+ 220)           ; Max width for the options container
(defconstant +margin-size+ 8)           ; Size for the margins
(defconstant +color-size+ 16)           ; Size of the color select buttons

;; Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest
(defun draw-texture-tiled (texture source dest origin rotation scale tint)
  (when (or (<= (texture-id texture) 0) (<= scale 0.0)) (return-from draw-texture-tiled)) ; Wanna see a infinite loop?!...just delete this line!
  (when (or (= (rectangle-width source) 0) (= (rectangle-height source) 0)) (return-from draw-texture-tiled))

  (let* ((sx (rectangle-x source)) (sy (rectangle-y source)) (sw (rectangle-width source)) (sh (rectangle-height source))
         (x (rectangle-x dest)) (y (rectangle-y dest)) (w (rectangle-width dest)) (h (rectangle-height dest))
         (tile-width (truncate (* sw scale))) (tile-height (truncate (* sh scale))))
    (flet ((draw (src-w src-h dst-x dst-y dst-w dst-h)
             (draw-texture-pro texture (make-rectangle :x sx :y sy :width src-w :height src-h)
                               (make-rectangle :x dst-x :y dst-y :width dst-w :height dst-h) origin rotation tint)))
      (cond ((and (< w tile-width) (< h tile-height))
             ;; Can fit only one tile
             (draw (* (/ w tile-width) sw) (* (/ h tile-height) sh) x y w h))
            ((<= w tile-width)
             ;; Tiled vertically (one column)
             (let ((dy 0))
               (loop while (< (+ dy tile-height) h)
                     do (draw (* (/ w tile-width) sw) sh x (+ y dy) w (float tile-height))
                        (incf dy tile-height))

               ;; Fit last tile
               (when (< dy h)
                 (draw (* (/ w tile-width) sw) (* (/ (- h dy) tile-height) sh) x (+ y dy) w (- h dy)))))
            ((<= h tile-height)
             ;; Tiled horizontally (one row)
             (let ((dx 0))
               (loop while (< (+ dx tile-width) w)
                     do (draw sw (* (/ h tile-height) sh) (+ x dx) y (float tile-width) h)
                        (incf dx tile-width))

               ;; Fit last tile
               (when (< dx w)
                 (draw (* (/ (- w dx) tile-width) sw) (* (/ h tile-height) sh) (+ x dx) y (- w dx) h))))
            (t
             ;; Tiled both horizontally and vertically (rows and columns)
             (let ((dx 0))
               (loop while (< (+ dx tile-width) w)
                     do (let ((dy 0))
                          (loop while (< (+ dy tile-height) h)
                                do (draw-texture-pro texture source (make-rectangle :x (+ x dx) :y (+ y dy) :width (float tile-width) :height (float tile-height))
                                                     origin rotation tint)
                                   (incf dy tile-height))

                          (when (< dy h)
                            (draw sw (* (/ (- h dy) tile-height) sh) (+ x dx) (+ y dy) (float tile-width) (- h dy))))
                        (incf dx tile-width))

               ;; Fit last column of tiles
               (when (< dx w)
                 (let ((dy 0))
                   (loop while (< (+ dy tile-height) h)
                         do (draw (* (/ (- w dx) tile-width) sw) sh (+ x dx) (+ y dy) (- w dx) (float tile-height))
                            (incf dy tile-height))

                   ;; Draw final tile in the bottom right corner
                   (when (< dy h)
                     (draw (* (/ (- w dx) tile-width) sw) (* (/ (- h dy) tile-height) sh) (+ x dx) (+ y dy) (- w dx) (- h dy)))))))))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-window-resizable+) ; Make the window resizable
    (init-window screen-width screen-height "raylib [textures] example - tiled drawing")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let* ((tex-pattern (load-texture "resources/patterns.png"))

           ;; Coordinates for all patterns inside the texture
           (rec-pattern (vector (make-rectangle :x 3.0 :y 3.0 :width 66.0 :height 66.0)
                                (make-rectangle :x 75.0 :y 3.0 :width 100.0 :height 100.0)
                                (make-rectangle :x 3.0 :y 75.0 :width 66.0 :height 66.0)
                                (make-rectangle :x 7.0 :y 156.0 :width 50.0 :height 50.0)
                                (make-rectangle :x 85.0 :y 106.0 :width 90.0 :height 45.0)
                                (make-rectangle :x 75.0 :y 154.0 :width 100.0 :height 60.0)))

           ;; Setup colors
           (colors (vector +black+ +maroon+ +orange+ +blue+ +purple+ +beige+ +lime+ +red+ +darkgray+ +skyblue+))
           (max-colors (length colors))
           (color-rec (make-array max-colors))

           (active-pattern 0) (active-col 0)
           (scale 1.0) (rotation 0.0))

      (set-texture-filter tex-pattern +texture-filter-bilinear+) ; Makes the texture smoother when upscaled

      ;; Calculate rectangle for each color
      (let ((x 0) (y 0))
        (dotimes (i max-colors)
          (setf (aref color-rec i) (make-rectangle :x (+ 2.0 +margin-size+ x)
                                                   :y (+ 22.0 256.0 +margin-size+ y)
                                                   :width (* +color-size+ 2.0)
                                                   :height (float +color-size+)))

          (if (= i (1- (truncate max-colors 2)))
              (setf x 0
                    y (+ y +color-size+ +margin-size+))
              (incf x (+ (* +color-size+ 2) +margin-size+)))))

      (set-target-fps 60)
      ;;---------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Handle mouse
               (when (is-mouse-button-pressed +mouse-button-left+)
                 (let ((mouse (get-mouse-position)))

                   ;; Check which pattern was clicked and set it as the active pattern
                   (dotimes (i (length rec-pattern))
                     (let ((rec (aref rec-pattern i)))
                       (when (check-collision-point-rec mouse (make-rectangle :x (+ 2 +margin-size+ (rectangle-x rec)) :y (+ 40 +margin-size+ (rectangle-y rec))
                                                                              :width (rectangle-width rec) :height (rectangle-height rec)))
                         (setf active-pattern i)
                         (return))))

                   ;; Check to see which color was clicked and set it as the active color
                   (dotimes (i max-colors)
                     (when (check-collision-point-rec mouse (aref color-rec i))
                       (setf active-col i)
                       (return)))))

               ;; Handle keys: change scale
               (when (is-key-pressed +key-up+) (incf scale 0.25))
               (when (is-key-pressed +key-down+) (decf scale 0.25))
               (cond ((> scale 10.0) (setf scale 10.0))
                     ((<= scale 0.0) (setf scale 0.25)))

               ;; Handle keys: change rotation
               (when (is-key-pressed +key-left+) (decf rotation 25.0))
               (when (is-key-pressed +key-right+) (incf rotation 25.0))

               ;; Handle keys: reset
               (when (is-key-pressed +key-space+) (setf rotation 0.0 scale 1.0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)
               (clear-background +raywhite+)

               ;; Draw the tiled area
               (draw-texture-tiled tex-pattern (aref rec-pattern active-pattern)
                                   (make-rectangle :x (float (+ +opt-width+ +margin-size+)) :y (float +margin-size+)
                                                   :width (- (get-screen-width) +opt-width+ (* 2.0 +margin-size+))
                                                   :height (- (get-screen-height) (* 2.0 +margin-size+)))
                                   (vec2 0.0 0.0) rotation scale (aref colors active-col))

               ;; Draw options
               (draw-rectangle +margin-size+ +margin-size+ (- +opt-width+ +margin-size+) (- (get-screen-height) (* 2 +margin-size+)) (color-alpha +lightgray+ 0.5))

               (draw-text "Select Pattern" (+ 2 +margin-size+) (+ 30 +margin-size+) 10 +black+)
               (draw-texture tex-pattern (+ 2 +margin-size+) (+ 40 +margin-size+) +black+)
               (let ((rec (aref rec-pattern active-pattern)))
                 (draw-rectangle (+ 2 +margin-size+ (truncate (rectangle-x rec))) (+ 40 +margin-size+ (truncate (rectangle-y rec)))
                                 (truncate (rectangle-width rec)) (truncate (rectangle-height rec)) (color-alpha +darkblue+ 0.3)))

               (draw-text "Select Color" (+ 2 +margin-size+) (+ 10 256 +margin-size+) 10 +black+)
               (dotimes (i max-colors)
                 (draw-rectangle-rec (aref color-rec i) (aref colors i))
                 (when (= active-col i) (draw-rectangle-lines-ex (aref color-rec i) 3.0 (color-alpha +white+ 0.5))))

               (draw-text "Scale (UP/DOWN to change)" (+ 2 +margin-size+) (+ 80 256 +margin-size+) 10 +black+)
               (draw-text (text-format "%.2fx" scale) (+ 2 +margin-size+) (+ 92 256 +margin-size+) 20 +black+)

               (draw-text "Rotation (LEFT/RIGHT to change)" (+ 2 +margin-size+) (+ 122 256 +margin-size+) 10 +black+)
               (draw-text (text-format "%.0f degrees" rotation) (+ 2 +margin-size+) (+ 134 256 +margin-size+) 20 +black+)

               (draw-text "Press [SPACE] to reset" (+ 2 +margin-size+) (+ 164 256 +margin-size+) 10 +darkblue+)

               ;; Draw FPS
               (draw-text (text-format "%i FPS" (get-fps)) (+ 2 +margin-size+) (+ 2 +margin-size+) 20 +black+)
               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture tex-pattern)      ; Unload texture

      (close-window))))                 ; Close window and OpenGL context

(main)

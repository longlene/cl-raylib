;;;; textures_draw_tiled.lisp - Draw part of texture tiled
;;;; Translated from raylib/examples/textures/textures_draw_tiled.c

(require :cl-raylib)

(defpackage :textures-draw-tiled
  (:use :cl :cl-raylib))

(in-package :textures-draw-tiled)

(defconstant +opt-width+ 220)      ; Max width for the options container
(defconstant +margin-size+ 8)      ; Size for the margins
(defconstant +color-size+ 16)      ; Size of the color select buttons

(defun draw-texture-tiled (texture source dest origin rotation scale tint)
  "Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest"
  (when (or (<= (texture-id texture) 0) (<= scale 0.0))
    (return-from draw-texture-tiled))
  (when (or (= (rectangle-width source) 0) (= (rectangle-height source) 0))
    (return-from draw-texture-tiled))

  (let ((tile-width (truncate (* (rectangle-width source) scale)))
        (tile-height (truncate (* (rectangle-height source) scale))))
    
    (cond
      ;; Can fit only one tile
      ((and (< (rectangle-width dest) tile-width) (< (rectangle-height dest) tile-height))
       (draw-texture-pro texture
                        (make-rectangle :x (rectangle-x source)
                                       :y (rectangle-y source)
                                       :width (* (/ (rectangle-width dest) tile-width) (rectangle-width source))
                                       :height (* (/ (rectangle-height dest) tile-height) (rectangle-height source)))
                        (make-rectangle :x (rectangle-x dest)
                                       :y (rectangle-y dest)
                                       :width (rectangle-width dest)
                                       :height (rectangle-height dest))
                        origin rotation tint))
      
      ;; Tiled vertically (one column)
      ((<= (rectangle-width dest) tile-width)
       (let ((dy 0))
         (loop while (< (+ dy tile-height) (rectangle-height dest)) do
           (draw-texture-pro texture
                            (make-rectangle :x (rectangle-x source)
                                           :y (rectangle-y source)
                                           :width (* (/ (rectangle-width dest) tile-width) (rectangle-width source))
                                           :height (rectangle-height source))
                            (make-rectangle :x (rectangle-x dest)
                                           :y (+ (rectangle-y dest) dy)
                                           :width (rectangle-width dest)
                                           :height (float tile-height))
                            origin rotation tint)
           (incf dy tile-height))
         ;; Fit last tile
         (when (< dy (rectangle-height dest))
           (draw-texture-pro texture
                            (make-rectangle :x (rectangle-x source)
                                           :y (rectangle-y source)
                                           :width (* (/ (rectangle-width dest) tile-width) (rectangle-width source))
                                           :height (* (/ (- (rectangle-height dest) dy) tile-height) (rectangle-height source)))
                            (make-rectangle :x (rectangle-x dest)
                                           :y (+ (rectangle-y dest) dy)
                                           :width (rectangle-width dest)
                                           :height (- (rectangle-height dest) dy))
                            origin rotation tint))))
      
      ;; Tiled horizontally (one row)
      ((<= (rectangle-height dest) tile-height)
       (let ((dx 0))
         (loop while (< (+ dx tile-width) (rectangle-width dest)) do
           (draw-texture-pro texture
                            (make-rectangle :x (rectangle-x source)
                                           :y (rectangle-y source)
                                           :width (rectangle-width source)
                                           :height (* (/ (rectangle-height dest) tile-height) (rectangle-height source)))
                            (make-rectangle :x (+ (rectangle-x dest) dx)
                                           :y (rectangle-y dest)
                                           :width (float tile-width)
                                           :height (rectangle-height dest))
                            origin rotation tint)
           (incf dx tile-width))
         ;; Fit last tile
         (when (< dx (rectangle-width dest))
           (draw-texture-pro texture
                            (make-rectangle :x (rectangle-x source)
                                           :y (rectangle-y source)
                                           :width (* (/ (- (rectangle-width dest) dx) tile-width) (rectangle-width source))
                                           :height (* (/ (rectangle-height dest) tile-height) (rectangle-height source)))
                            (make-rectangle :x (+ (rectangle-x dest) dx)
                                           :y (rectangle-y dest)
                                           :width (- (rectangle-width dest) dx)
                                           :height (rectangle-height dest))
                            origin rotation tint))))
      
      ;; Tiled both horizontally and vertically (rows and columns)
      (t
       (let ((dx 0))
         (loop while (< (+ dx tile-width) (rectangle-width dest)) do
           (let ((dy 0))
             (loop while (< (+ dy tile-height) (rectangle-height dest)) do
               (draw-texture-pro texture source
                                (make-rectangle :x (+ (rectangle-x dest) dx)
                                               :y (+ (rectangle-y dest) dy)
                                               :width (float tile-width)
                                               :height (float tile-height))
                                origin rotation tint)
               (incf dy tile-height))
             (when (< dy (rectangle-height dest))
               (draw-texture-pro texture
                                (make-rectangle :x (rectangle-x source)
                                               :y (rectangle-y source)
                                               :width (rectangle-width source)
                                               :height (* (/ (- (rectangle-height dest) dy) tile-height) (rectangle-height source)))
                                (make-rectangle :x (+ (rectangle-x dest) dx)
                                               :y (+ (rectangle-y dest) dy)
                                               :width (float tile-width)
                                               :height (- (rectangle-height dest) dy))
                                origin rotation tint)))
           (incf dx tile-width))
         ;; Fit last column of tiles
         (when (< dx (rectangle-width dest))
           (let ((dy 0))
             (loop while (< (+ dy tile-height) (rectangle-height dest)) do
               (draw-texture-pro texture
                                (make-rectangle :x (rectangle-x source)
                                               :y (rectangle-y source)
                                               :width (* (/ (- (rectangle-width dest) dx) tile-width) (rectangle-width source))
                                               :height (rectangle-height source))
                                (make-rectangle :x (+ (rectangle-x dest) dx)
                                               :y (+ (rectangle-y dest) dy)
                                               :width (- (rectangle-width dest) dx)
                                               :height (float tile-height))
                                origin rotation tint)
               (incf dy tile-height))
             ;; Draw final tile in the bottom right corner
             (when (< dy (rectangle-height dest))
               (draw-texture-pro texture
                                (make-rectangle :x (rectangle-x source)
                                               :y (rectangle-y source)
                                               :width (* (/ (- (rectangle-width dest) dx) tile-width) (rectangle-width source))
                                               :height (* (/ (- (rectangle-height dest) dy) tile-height) (rectangle-height source)))
                                (make-rectangle :x (+ (rectangle-x dest) dx)
                                               :y (+ (rectangle-y dest) dy)
                                               :width (- (rectangle-width dest) dx)
                                               :height (- (rectangle-height dest) dy))
                                origin rotation tint)))))))))

(defun main ()
  "Main function - draw tiled texture example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (set-config-flags +flag-window-resizable+)  ; Make the window resizable
    (init-window screen-width screen-height "raylib [textures] example - Draw part of a texture tiled")

    ;; Load texture
    (let ((tex-pattern (load-texture "resources/patterns.png")))
      (set-texture-filter tex-pattern +texture-filter-trilinear+)  ; Makes the texture smoother when upscaled

      ;; Coordinates for all patterns inside the texture
      (let ((rec-pattern (vector (make-rectangle :x 3 :y 3 :width 66 :height 66)
                                (make-rectangle :x 75 :y 3 :width 100 :height 100)
                                (make-rectangle :x 3 :y 75 :width 66 :height 66)
                                (make-rectangle :x 7 :y 156 :width 50 :height 50)
                                (make-rectangle :x 85 :y 106 :width 90 :height 45)
                                (make-rectangle :x 75 :y 154 :width 100 :height 60)))
            
            ;; Setup colors
            (colors (vector +black+ +maroon+ +orange+ +blue+ +purple+ +beige+ +lime+ +red+ +darkgray+ +skyblue+))
            (color-rec (make-array 10))
            (active-pattern 0)
            (active-col 0)
            (scale 1.0)
            (rotation 0.0))

        ;; Calculate rectangle for each color
        (let ((x 0) (y 0))
          (dotimes (i 10)
            (setf (aref color-rec i)
                  (make-rectangle :x (+ 2.0 +margin-size+ x)
                                 :y (+ 22.0 256.0 +margin-size+ y)
                                 :width (* +color-size+ 2.0)
                                 :height (float +color-size+)))
            (if (= i 4)  ; (MAX_COLORS/2 - 1)
                (setf x 0 y (+ +color-size+ +margin-size+))
                (incf x (+ (* +color-size+ 2) +margin-size+)))))

        (set-target-fps 60)

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          ;; Handle mouse
          (when (is-mouse-button-pressed +mouse-button-left+)
            (let ((mouse (get-mouse-position)))
              ;; Check which pattern was clicked
              (dotimes (i (length rec-pattern))
                (let ((pattern-rect (aref rec-pattern i)))
                  (when (check-collision-point-rec 
                         mouse
                         (make-rectangle :x (+ 2 +margin-size+ (rectangle-x pattern-rect))
                                        :y (+ 40 +margin-size+ (rectangle-y pattern-rect))
                                        :width (rectangle-width pattern-rect)
                                        :height (rectangle-height pattern-rect)))
                    (setf active-pattern i)
                    (return))))
              ;; Check which color was clicked
              (dotimes (i 10)
                (when (check-collision-point-rec mouse (aref color-rec i))
                  (setf active-col i)
                  (return)))))

          ;; Handle keys
          ;; Change scale
          (when (is-key-pressed +key-up+) (incf scale 0.25))
          (when (is-key-pressed +key-down+) (decf scale 0.25))
          (when (> scale 10.0) (setf scale 10.0))
          (when (<= scale 0.0) (setf scale 0.25))

          ;; Change rotation
          (when (is-key-pressed +key-left+) (decf rotation 25.0))
          (when (is-key-pressed +key-right+) (incf rotation 25.0))

          ;; Reset
          (when (is-key-pressed +key-space+) (setf rotation 0.0 scale 1.0))

          ;; Draw
          (begin-drawing)
            (clear-background +raywhite+)

            ;; Draw the tiled area
            (draw-texture-tiled tex-pattern
                               (aref rec-pattern active-pattern)
                               (make-rectangle :x (+ +opt-width+ +margin-size+)
                                              :y (float +margin-size+)
                                              :width (- (get-screen-width) +opt-width+ (* 2.0 +margin-size+))
                                              :height (- (get-screen-height) (* 2.0 +margin-size+)))
                               (vec2 0.0 0.0)
                               rotation
                               scale
                               (aref colors active-col))

            ;; Draw options
            (draw-rectangle +margin-size+ +margin-size+ 
                           (- +opt-width+ +margin-size+) 
                           (- (get-screen-height) (* 2 +margin-size+))
                           (color-alpha +lightgray+ 0.5))

            (draw-text "Select Pattern" (+ 2 +margin-size+) (+ 30 +margin-size+) 10 +black+)
            (draw-texture tex-pattern (+ 2 +margin-size+) (+ 40 +margin-size+) +black+)
            (let ((pattern (aref rec-pattern active-pattern)))
              (draw-rectangle (+ 2 +margin-size+ (truncate (rectangle-x pattern)))
                             (+ 40 +margin-size+ (truncate (rectangle-y pattern)))
                             (truncate (rectangle-width pattern))
                             (truncate (rectangle-height pattern))
                             (color-alpha +darkblue+ 0.3)))

            (draw-text "Select Color" (+ 2 +margin-size+) (+ 10 256 +margin-size+) 10 +black+)
            (dotimes (i 10)
              (draw-rectangle-rec (aref color-rec i) (aref colors i))
              (when (= active-col i)
                (draw-rectangle-lines-ex (aref color-rec i) 3 (color-alpha +white+ 0.5))))

            (draw-text "Scale (UP/DOWN to change)" (+ 2 +margin-size+) (+ 80 256 +margin-size+) 10 +black+)
            (draw-text (format nil "~,2fx" scale) (+ 2 +margin-size+) (+ 92 256 +margin-size+) 20 +black+)

            (draw-text "Rotation (LEFT/RIGHT to change)" (+ 2 +margin-size+) (+ 122 256 +margin-size+) 10 +black+)
            (draw-text (format nil "~,0f degrees" rotation) (+ 2 +margin-size+) (+ 134 256 +margin-size+) 20 +black+)

            (draw-text "Press [SPACE] to reset" (+ 2 +margin-size+) (+ 164 256 +margin-size+) 10 +darkblue+)

            ;; Draw FPS
            (draw-text (format nil "~a FPS" (get-fps)) (+ 2 +margin-size+) (+ 2 +margin-size+) 20 +black+)

          (end-drawing))

        ;; De-Initialization
        (unload-texture tex-pattern)))

    ;; Close window
    (close-window)))

;; Run the example
(main)
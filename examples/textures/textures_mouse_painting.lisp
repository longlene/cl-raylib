;;;; textures_mouse_painting.lisp - Mouse painting example  
;;;; Translated from raylib/examples/textures/textures_mouse_painting.c

(require :cl-raylib)

(defpackage :textures-mouse-painting
  (:use :cl :cl-raylib))

(in-package :textures-mouse-painting)

(defconstant +max-colors-count+ 23) ; Number of colors available

(defun main ()
  "Main function - mouse painting example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [textures] example - mouse painting")

    ;; Colors to choose from
    (let ((colors (vector +raywhite+ +yellow+ +gold+ +orange+ +pink+ +red+ +maroon+ +green+ +lime+ +darkgreen+
                          +skyblue+ +blue+ +darkblue+ +purple+ +violet+ +darkpurple+ +beige+ +brown+ +darkbrown+
                          +lightgray+ +gray+ +darkgray+ +black+)))

      ;; Define colorsRecs data (for every rectangle)
      (let ((colors-recs (make-array +max-colors-count+)))
        (loop for i from 0 below +max-colors-count+ do
          (setf (aref colors-recs i)
                (make-rectangle :x (+ 10 (* 30.0 i) (* 2 i))
                               :y 10.0
                               :width 30.0
                               :height 30.0)))

        (let ((color-selected 0)
              (color-selected-prev 0)
              (color-mouse-hover -1)
              (brush-size 20.0)
              (mouse-was-pressed nil)
              (btn-save-rec (make-rectangle :x 750.0 :y 10.0 :width 40.0 :height 30.0))
              (btn-save-mouse-hover nil)
              (show-save-message nil)
              (save-message-counter 0))

          ;; Create a RenderTexture2D to use as a canvas
          (let ((target (load-render-texture screen-width screen-height)))

            ;; Clear render texture before entering the game loop
            (begin-texture-mode target)
            (clear-background (aref colors 0))
            (end-texture-mode)

            (set-target-fps 120) ; Set game to run at 120 frames-per-second

            ;; Main game loop
            (loop until (window-should-close) do
              ;; Update
              (let ((mouse-pos (get-mouse-position)))

                ;; Move between colors with keys
                (when (is-key-pressed +key-right+) (incf color-selected))
                (when (is-key-pressed +key-left+) (decf color-selected))

                (when (>= color-selected +max-colors-count+) 
                  (setf color-selected (1- +max-colors-count+)))
                (when (< color-selected 0) 
                  (setf color-selected 0))

                ;; Choose color with mouse
                (setf color-mouse-hover -1)
                (loop for i from 0 below +max-colors-count+ do
                  (when (check-collision-point-rec mouse-pos (aref colors-recs i))
                    (setf color-mouse-hover i)
                    (return)))

                (when (and (>= color-mouse-hover 0) (is-mouse-button-pressed +mouse-button-left+))
                  (setf color-selected color-mouse-hover)
                  (setf color-selected-prev color-selected))

                ;; Change brush size
                (incf brush-size (* (get-mouse-wheel-move) 5))
                (when (< brush-size 2) (setf brush-size 2.0))
                (when (> brush-size 50) (setf brush-size 50.0))

                ;; Clear canvas
                (when (is-key-pressed +key-c+)
                  (begin-texture-mode target)
                  (clear-background (aref colors 0))
                  (end-texture-mode))

                ;; Paint with left mouse button
                (when (is-mouse-button-down +mouse-button-left+)
                  ;; Paint circle into render texture
                  ;; NOTE: To avoid discontinuous circles, we could store
                  ;; previous-next mouse points and just draw a line using brush size
                  (begin-texture-mode target)
                  (when (> (vy mouse-pos) 50)
                    (draw-circle (truncate (vx mouse-pos)) (truncate (vy mouse-pos)) 
                                brush-size (aref colors color-selected)))
                  (end-texture-mode))

                ;; Erase with right mouse button
                (when (is-mouse-button-down +mouse-button-right+)
                  (unless mouse-was-pressed
                    (setf color-selected-prev color-selected)
                    (setf color-selected 0))

                  (setf mouse-was-pressed t)

                  ;; Erase circle from render texture
                  (begin-texture-mode target)
                  (when (> (vy mouse-pos) 50)
                    (draw-circle (truncate (vx mouse-pos)) (truncate (vy mouse-pos)) 
                                brush-size (aref colors 0)))
                  (end-texture-mode))

                (when (and (is-mouse-button-released +mouse-button-right+) mouse-was-pressed)
                  (setf color-selected color-selected-prev)
                  (setf mouse-was-pressed nil))

                ;; Check mouse hover save button
                (setf btn-save-mouse-hover (check-collision-point-rec mouse-pos btn-save-rec))

                ;; Image saving logic
                ;; NOTE: Saving painted texture to a default named image
                (when (or (and btn-save-mouse-hover (is-mouse-button-released +mouse-button-left+))
                          (is-key-pressed +key-s+))
                  (let ((image (load-image-from-texture (render-texture-texture target))))
                    (image-flip-vertical image)
                    (export-image image "my_amazing_texture_painting.png")
                    (unload-image image)
                    (setf show-save-message t)))

                (when show-save-message
                  ;; On saving, show a full screen message for 2 seconds
                  (incf save-message-counter)
                  (when (> save-message-counter 240)
                    (setf show-save-message nil)
                    (setf save-message-counter 0))))

              ;; Draw
              (begin-drawing)
                (clear-background +raywhite+)

                ;; NOTE: Render texture must be y-flipped due to default OpenGL coordinates (left-bottom)
                (draw-texture-rec (render-texture-texture target)
                                 (make-rectangle :x 0.0 :y 0.0 
                                               :width (float (texture-width (render-texture-texture target)))
                                               :height (float (- (texture-height (render-texture-texture target)))))
                                 (vec2 0.0 0.0) 
                                 +white+)

                ;; Draw drawing circle for reference
                (when (> (vy (get-mouse-position)) 50)
                  (if (is-mouse-button-down +mouse-button-right+)
                      (draw-circle-lines (get-mouse-x) (get-mouse-y) brush-size +gray+)
                      (draw-circle (get-mouse-x) (get-mouse-y) brush-size (aref colors color-selected))))

                ;; Draw top panel
                (draw-rectangle 0 0 screen-width 50 +raywhite+)
                (draw-line 0 50 screen-width 50 +lightgray+)

                ;; Draw color selection rectangles
                (loop for i from 0 below +max-colors-count+ do
                  (draw-rectangle-rec (aref colors-recs i) (aref colors i)))
                (draw-rectangle-lines 10 10 30 30 +lightgray+)

                (when (>= color-mouse-hover 0)
                  (draw-rectangle-rec (aref colors-recs color-mouse-hover) (fade +white+ 0.6)))

                (draw-rectangle-lines-ex (make-rectangle :x (- (rectangle-x (aref colors-recs color-selected)) 2)
                                                        :y (- (rectangle-y (aref colors-recs color-selected)) 2)
                                                        :width (+ (rectangle-width (aref colors-recs color-selected)) 4)
                                                        :height (+ (rectangle-height (aref colors-recs color-selected)) 4))
                                        2.0 +black+)

                ;; Draw save image button
                (draw-rectangle-lines-ex btn-save-rec 2.0 (if btn-save-mouse-hover +red+ +black+))
                (draw-text "SAVE!" 755 20 10 (if btn-save-mouse-hover +red+ +black+))

                ;; Draw save image message
                (when show-save-message
                  (draw-rectangle 0 0 screen-width screen-height (fade +raywhite+ 0.8))
                  (draw-rectangle 0 150 screen-width 80 +black+)
                  (draw-text "IMAGE SAVED:  my_amazing_texture_painting.png" 150 180 20 +raywhite+))

              (end-drawing))

            ;; De-Initialization
            (unload-render-texture target))))

    ;; Close window
    (close-window)))

;; Run the example
(main)
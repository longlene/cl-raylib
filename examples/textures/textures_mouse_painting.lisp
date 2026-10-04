;;;; raylib [textures] example - mouse painting
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 3.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Chris Dill (@MysteriousSpace) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Chris Dill (@MysteriousSpace) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_mouse_painting.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-mouse-painting
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-mouse-painting)

(defconstant +max-colors-count+ 23)     ; Number of colors available

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - mouse painting")

    (let* (;; Colors to choose from
           (colors (vector +raywhite+ +yellow+ +gold+ +orange+ +pink+ +red+ +maroon+ +green+ +lime+ +darkgreen+
                           +skyblue+ +blue+ +darkblue+ +purple+ +violet+ +darkpurple+ +beige+ +brown+ +darkbrown+
                           +lightgray+ +gray+ +darkgray+ +black+))

           ;; Define colorsRecs data (for every rectangle)
           (colors-recs (let ((a (make-array +max-colors-count+)))
                          (dotimes (i +max-colors-count+ a)
                            (setf (aref a i) (make-rectangle :x (+ 10 (* 30.0 i) (* 2 i)) :y 10.0 :width 30.0 :height 30.0)))))

           (color-selected 0)
           (color-selected-prev color-selected)
           (color-mouse-hover 0)
           (brush-size 20.0)
           (mouse-was-pressed nil)

           (btn-save-rec (make-rectangle :x 750.0 :y 10.0 :width 40.0 :height 30.0))
           (btn-save-mouse-hover nil)
           (show-save-message nil)
           (save-message-counter 0)

           ;; Create a RenderTexture2D to use as a canvas
           (target (load-render-texture screen-width screen-height)))

      ;; Clear render texture before entering the game loop
      (begin-texture-mode target)
      (clear-background (aref colors 0))
      (end-texture-mode)

      (set-target-fps 120)              ; Set our game to run at 120 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((mouse-pos (get-mouse-position)))

                 ;; Move between colors with keys
                 (cond ((is-key-pressed +key-right+) (incf color-selected))
                       ((is-key-pressed +key-left+) (decf color-selected)))

                 (cond ((>= color-selected +max-colors-count+) (setf color-selected (1- +max-colors-count+)))
                       ((< color-selected 0) (setf color-selected 0)))

                 ;; Choose color with mouse
                 (dotimes (i +max-colors-count+)
                   (if (check-collision-point-rec mouse-pos (aref colors-recs i))
                       (progn
                         (setf color-mouse-hover i)
                         (return))
                       (setf color-mouse-hover -1)))

                 (when (and (>= color-mouse-hover 0) (is-mouse-button-pressed +mouse-button-left+))
                   (setf color-selected color-mouse-hover
                         color-selected-prev color-selected))

                 ;; Change brush size
                 (incf brush-size (* (get-mouse-wheel-move) 5))
                 (when (< brush-size 2) (setf brush-size 2.0))
                 (when (> brush-size 50) (setf brush-size 50.0))

                 (when (is-key-pressed +key-c+)
                   ;; Clear render texture to clear color
                   (begin-texture-mode target)
                   (clear-background (aref colors 0))
                   (end-texture-mode))

                 (when (or (is-mouse-button-down +mouse-button-left+) (is-gesture-detected +gesture-drag+))
                   ;; Paint circle into render texture
                   ;; NOTE: To avoid discontinuous circles, we could store
                   ;; previous-next mouse points and just draw a line using brush size
                   (begin-texture-mode target)
                   (when (> (vy mouse-pos) 50) (draw-circle (truncate (vx mouse-pos)) (truncate (vy mouse-pos)) brush-size (aref colors color-selected)))
                   (end-texture-mode))

                 (cond ((is-mouse-button-down +mouse-button-right+)
                        (unless mouse-was-pressed
                          (setf color-selected-prev color-selected
                                color-selected 0))

                        (setf mouse-was-pressed t)

                        ;; Erase circle from render texture
                        (begin-texture-mode target)
                        (when (> (vy mouse-pos) 50) (draw-circle (truncate (vx mouse-pos)) (truncate (vy mouse-pos)) brush-size (aref colors 0)))
                        (end-texture-mode))
                       ((and (is-mouse-button-released +mouse-button-right+) mouse-was-pressed)
                        (setf color-selected color-selected-prev
                              mouse-was-pressed nil)))

                 ;; Check mouse hover save button
                 (if (check-collision-point-rec mouse-pos btn-save-rec)
                     (setf btn-save-mouse-hover t)
                     (setf btn-save-mouse-hover nil))

                 ;; Image saving logic
                 ;; NOTE: Saving painted texture to a default named image
                 (when (or (and btn-save-mouse-hover (is-mouse-button-released +mouse-button-left+)) (is-key-pressed +key-s+))
                   (let ((image (load-image-from-texture (render-texture-texture target))))
                     (image-flip-vertical image)
                     (export-image image "my_amazing_texture_painting.png")
                     (unload-image image))
                   (setf show-save-message t))

                 (when show-save-message
                   ;; On saving, show a full screen message for 2 seconds
                   (incf save-message-counter)
                   (when (> save-message-counter 240)
                     (setf show-save-message nil
                           save-message-counter 0)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 ;; NOTE: Render texture must be y-flipped due to default OpenGL coordinates (left-bottom)
                 (let ((texture (render-texture-texture target)))
                   (draw-texture-rec texture (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width texture)) :height (float (- (texture-height texture))))
                                     (vec2 0.0 0.0) +white+))

                 ;; Draw drawing circle for reference
                 (when (> (vy mouse-pos) 50)
                   (if (is-mouse-button-down +mouse-button-right+)
                       (draw-circle-lines (truncate (vx mouse-pos)) (truncate (vy mouse-pos)) brush-size +gray+)
                       (draw-circle (get-mouse-x) (get-mouse-y) brush-size (aref colors color-selected))))

                 ;; Draw top panel
                 (draw-rectangle 0 0 (get-screen-width) 50 +raywhite+)
                 (draw-line 0 50 (get-screen-width) 50 +lightgray+)

                 ;; Draw color selection rectangles
                 (dotimes (i +max-colors-count+) (draw-rectangle-rec (aref colors-recs i) (aref colors i)))
                 (draw-rectangle-lines 10 10 30 30 +lightgray+)

                 (when (>= color-mouse-hover 0) (draw-rectangle-rec (aref colors-recs color-mouse-hover) (fade +white+ 0.6)))

                 (let ((rec (aref colors-recs color-selected)))
                   (draw-rectangle-lines-ex (make-rectangle :x (- (rectangle-x rec) 2) :y (- (rectangle-y rec) 2)
                                                            :width (+ (rectangle-width rec) 4) :height (+ (rectangle-height rec) 4))
                                            2.0 +black+))

                 ;; Draw save image button
                 (draw-rectangle-lines-ex btn-save-rec 2.0 (if btn-save-mouse-hover +red+ +black+))
                 (draw-text "SAVE!" 755 20 10 (if btn-save-mouse-hover +red+ +black+))

                 ;; Draw save image message
                 (when show-save-message
                   (draw-rectangle 0 0 (get-screen-width) (get-screen-height) (fade +raywhite+ 0.8))
                   (draw-rectangle 0 150 (get-screen-width) 80 +black+)
                   (draw-text "IMAGE SAVED!" 150 180 20 +raywhite+))

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture target)    ; Unload render texture

      (close-window))))                 ; Close window and OpenGL context

(main)

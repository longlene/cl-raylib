;;;; textures_npatch_drawing.lisp - N-patch drawing
;;;; Translated from raylib/examples/textures/textures_npatch_drawing.c

(require :cl-raylib)

(defpackage :textures-npatch-drawing
  (:use :cl :cl-raylib))

(in-package :textures-npatch-drawing)

(defun main ()
  "Main function - N-patch drawing"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [textures] example - N-patch drawing")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let ((n-patch-texture (load-texture "examples/textures/resources/ninepatch_button.png")))

      (let ((mouse-position (vec2 0.0 0.0))
            (origin (vec2 0.0 0.0))
            ;; Position and size of the n-patches
            (dst-rec1 (make-rectangle :x 480.0 :y 160.0 :width 32.0 :height 32.0))
            (dst-rec2 (make-rectangle :x 160.0 :y 160.0 :width 32.0 :height 32.0))
            (dst-rec-h (make-rectangle :x 160.0 :y 93.0 :width 32.0 :height 32.0))
            (dst-rec-v (make-rectangle :x 92.0 :y 160.0 :width 32.0 :height 32.0)))

        ;; A 9-patch (NPATCH_NINE_PATCH) changes its sizes in both axis
        (let ((nine-patch-info1 (make-npatch-info :source (make-rectangle :x 0.0 :y 0.0 :width 64.0 :height 64.0)
                                                  :left 12 :top 40 :right 12 :bottom 12
                                                  :layout +npatch-nine-patch+))
              (nine-patch-info2 (make-npatch-info :source (make-rectangle :x 0.0 :y 128.0 :width 64.0 :height 64.0)
                                                  :left 16 :top 16 :right 16 :bottom 16
                                                  :layout +npatch-nine-patch+))
              ;; A horizontal 3-patch (NPATCH_THREE_PATCH_HORIZONTAL) changes its sizes along the x axis only
              (h3-patch-info (make-npatch-info :source (make-rectangle :x 0.0 :y 64.0 :width 64.0 :height 64.0)
                                               :left 8 :top 8 :right 8 :bottom 8
                                               :layout +npatch-three-patch-horizontal+))
              ;; A vertical 3-patch (NPATCH_THREE_PATCH_VERTICAL) changes its sizes along the y axis only
              (v3-patch-info (make-npatch-info :source (make-rectangle :x 0.0 :y 192.0 :width 64.0 :height 64.0)
                                               :left 6 :top 6 :right 6 :bottom 6
                                               :layout +npatch-three-patch-vertical+)))

          (set-target-fps 60)

          ;; Main game loop
          (loop until (window-should-close) do
            ;; Update
            (setf mouse-position (get-mouse-position))

            ;; Resize the n-patches based on mouse position
            (setf (rectangle-width dst-rec1) (- (vx mouse-position) (rectangle-x dst-rec1)))
            (setf (rectangle-height dst-rec1) (- (vy mouse-position) (rectangle-y dst-rec1)))
            (setf (rectangle-width dst-rec2) (- (vx mouse-position) (rectangle-x dst-rec2)))
            (setf (rectangle-height dst-rec2) (- (vy mouse-position) (rectangle-y dst-rec2)))
            (setf (rectangle-width dst-rec-h) (- (vx mouse-position) (rectangle-x dst-rec-h)))
            (setf (rectangle-height dst-rec-v) (- (vy mouse-position) (rectangle-y dst-rec-v)))

            ;; Set a minimum width and/or height
            (when (< (rectangle-width dst-rec1) 1.0) (setf (rectangle-width dst-rec1) 1.0))
            (when (> (rectangle-width dst-rec1) 300.0) (setf (rectangle-width dst-rec1) 300.0))
            (when (< (rectangle-height dst-rec1) 1.0) (setf (rectangle-height dst-rec1) 1.0))
            (when (< (rectangle-width dst-rec2) 1.0) (setf (rectangle-width dst-rec2) 1.0))
            (when (> (rectangle-width dst-rec2) 300.0) (setf (rectangle-width dst-rec2) 300.0))
            (when (< (rectangle-height dst-rec2) 1.0) (setf (rectangle-height dst-rec2) 1.0))
            (when (< (rectangle-width dst-rec-h) 1.0) (setf (rectangle-width dst-rec-h) 1.0))
            (when (< (rectangle-height dst-rec-v) 1.0) (setf (rectangle-height dst-rec-v) 1.0))

            ;; Draw
            (begin-drawing)
              (clear-background +raywhite+)

              ;; Draw the n-patches
              (draw-texture-npatch n-patch-texture nine-patch-info2 dst-rec2 origin 0.0 +white+)
              (draw-texture-npatch n-patch-texture nine-patch-info1 dst-rec1 origin 0.0 +white+)
              (draw-texture-npatch n-patch-texture h3-patch-info dst-rec-h origin 0.0 +white+)
              (draw-texture-npatch n-patch-texture v3-patch-info dst-rec-v origin 0.0 +white+)

              ;; Draw the source texture
              (draw-rectangle-lines 5 88 74 266 +blue+)
              (draw-texture n-patch-texture 10 93 +white+)
              (draw-text "TEXTURE" 15 360 10 +darkgray+)

              (draw-text "Move the mouse to stretch or shrink the n-patches" 10 20 20 +darkgray+)

            (end-drawing)))

        ;; De-Initialization
        (unload-texture n-patch-texture))) ; Texture unloading

    ;; Close window
    (close-window)))

;; Run the example
(main)
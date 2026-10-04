;;;; raylib [textures] example - screen buffer
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Agnis Aldiņš (@nezvers) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Agnis Aldiņš (@nezvers)
;;;; Common Lisp port of raylib/examples/textures/textures_screen_buffer.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-screen-buffer
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-screen-buffer)

(defconstant +max-colors+ 256)
(defconstant +scale-factor+ 2)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - screen buffer")

    (let* ((image-width (truncate screen-width +scale-factor+))
           (image-height (truncate screen-height +scale-factor+))
           (flame-width (truncate screen-width +scale-factor+))

           (palette (make-array +max-colors+))
           (index-buffer (make-array (* image-width image-width) :element-type '(unsigned-byte 8) :initial-element 0))
           (flame-root-buffer (make-array flame-width :element-type '(unsigned-byte 8) :initial-element 0))

           (screen-image (gen-image-color image-width image-height +black+))
           (screen-texture (load-texture-from-image screen-image)))

      ;; Generate flame color palette
      (dotimes (i +max-colors+)
        (let* ((tt (/ (float i) (float (1- +max-colors+))))
               (hue (* tt tt))
               (saturation tt)
               (value tt))
          (setf (aref palette i) (color-from-hsv (+ 250.0 (* 150.0 hue)) saturation value))))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Grow flameRoot
               (loop for x from 2 below flame-width
                     do (let ((flame (aref flame-root-buffer x)))
                          (incf flame (get-random-value 0 2))
                          (setf (aref flame-root-buffer x) (if (> flame 255) 255 flame))))

               ;; Transfer flameRoot to indexBuffer
               (dotimes (x flame-width)
                 (let ((i (+ x (* (1- image-height) image-width))))
                   (setf (aref index-buffer i) (aref flame-root-buffer x))))

               ;; Clear top row, because it can't move any higher
               (dotimes (x image-width)
                 (when (/= (aref index-buffer x) 0) (setf (aref index-buffer x) 0)))

               ;; Skip top row, it is already cleared
               (loop for y from 1 below image-height
                     do (dotimes (x image-width)
                          (let* ((i (+ x (* y image-width)))
                                 (color-index (aref index-buffer i)))
                            (when (/= color-index 0)
                              ;; Move pixel a row above
                              (setf (aref index-buffer i) 0)
                              (let* ((move-x (- (get-random-value 0 2) 1))
                                     (new-x (+ x move-x)))
                                (when (and (> new-x 0) (< new-x image-width))
                                  (let ((iabove (+ (- i image-width) move-x))
                                        (decay (get-random-value 0 3)))
                                    (decf color-index (if (< decay color-index) decay color-index))
                                    (setf (aref index-buffer iabove) color-index))))))))

               ;; Update screenImage with palette colors
               (loop for y from 1 below image-height
                     do (dotimes (x image-width)
                          (let* ((i (+ x (* y image-width)))
                                 (color-index (aref index-buffer i))
                                 (col (aref palette color-index)))
                            (image-draw-pixel screen-image x y col))))

               (update-texture screen-texture (image-data screen-image))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-texture-ex screen-texture (vec2 0.0 0.0) 0.0 2.0 +white+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture screen-texture)
      (unload-image screen-image)

      (close-window))))                 ; Close window and OpenGL context

(main)

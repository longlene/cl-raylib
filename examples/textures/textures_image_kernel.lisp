;;;; raylib [textures] example - image kernel
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; NOTE: Images are loaded in CPU memory (RAM); textures are loaded in GPU memory (VRAM)
;;;;
;;;; Example contributed by Karim Salem (@kimo-s) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 1.3
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Karim Salem (@kimo-s)
;;;; Common Lisp port of raylib/examples/textures/textures_image_kernel.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-image-kernel
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-image-kernel)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
(defun normalize-kernel (kernel size)
  (let ((sum 0.0))
    (dotimes (i size) (incf sum (aref kernel i)))

    (when (/= sum 0.0)
      (dotimes (i size) (setf (aref kernel i) (/ (aref kernel i) sum))))))

(defun make-kernel (&rest values)
  (make-array (length values) :element-type 'single-float :initial-contents values))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - image kernel")

    (let ((image (load-image "resources/cat.png")) ; Loaded in CPU memory (RAM)

          (gaussiankernel (make-kernel 1.0 2.0 1.0
                                       2.0 4.0 2.0
                                       1.0 2.0 1.0))

          (sobelkernel (make-kernel 1.0 0.0 -1.0
                                    2.0 0.0 -2.0
                                    1.0 0.0 -1.0))

          (sharpenkernel (make-kernel 0.0 -1.0 0.0
                                      -1.0 5.0 -1.0
                                      0.0 -1.0 0.0)))

      (normalize-kernel gaussiankernel 9)
      (normalize-kernel sharpenkernel 9)
      (normalize-kernel sobelkernel 9)

      (let ((cat-sharpend (image-copy image))
            (cat-sobel nil)
            (cat-gaussian nil))
        (image-kernel-convolution cat-sharpend sharpenkernel 9)

        (setf cat-sobel (image-copy image))
        (image-kernel-convolution cat-sobel sobelkernel 9)

        (setf cat-gaussian (image-copy image))

        (dotimes (i 6)
          (image-kernel-convolution cat-gaussian gaussiankernel 9))

        (image-crop image (make-rectangle :x 0.0 :y 0.0 :width 200.0 :height 450.0))
        (image-crop cat-gaussian (make-rectangle :x 0.0 :y 0.0 :width 200.0 :height 450.0))
        (image-crop cat-sobel (make-rectangle :x 0.0 :y 0.0 :width 200.0 :height 450.0))
        (image-crop cat-sharpend (make-rectangle :x 0.0 :y 0.0 :width 200.0 :height 450.0))

        ;; Images converted to texture, GPU memory (VRAM)
        (let ((texture (load-texture-from-image image))
              (cat-sharpend-texture (load-texture-from-image cat-sharpend))
              (cat-sobel-texture (load-texture-from-image cat-sobel))
              (cat-gaussian-texture (load-texture-from-image cat-gaussian)))

          ;; Once images have been converted to texture and uploaded to VRAM,
          ;; they can be unloaded from RAM
          (unload-image image)
          (unload-image cat-gaussian)
          (unload-image cat-sobel)
          (unload-image cat-sharpend)

          (set-target-fps 60)           ; Set our game to run at 60 frames-per-second
          ;;---------------------------------------------------------------------------------------

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

                   (draw-texture cat-sharpend-texture 0 0 +white+)
                   (draw-texture cat-sobel-texture 200 0 +white+)
                   (draw-texture cat-gaussian-texture 400 0 +white+)
                   (draw-texture texture 600 0 +white+)

                   (end-drawing))
          ;;----------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (unload-texture texture)
          (unload-texture cat-gaussian-texture)
          (unload-texture cat-sobel-texture)
          (unload-texture cat-sharpend-texture)

          (close-window))))))           ; Close window and OpenGL context

(main)

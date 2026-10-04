;;;; raylib [textures] example - image channel
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Bruno Cabral (@brccabral) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2024-2025 Bruno Cabral (@brccabral) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_image_channel.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-image-channel
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-image-channel)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - image channel")

    (let* ((fudesumi-image (load-image "resources/fudesumi.png"))

           (image-alpha (image-from-channel fudesumi-image 3))
           (image-red (image-from-channel fudesumi-image 0))
           (image-green (image-from-channel fudesumi-image 1))
           (image-blue (image-from-channel fudesumi-image 2)))

      (image-alpha-mask image-alpha image-alpha)
      (image-alpha-mask image-red image-alpha)
      (image-alpha-mask image-green image-alpha)
      (image-alpha-mask image-blue image-alpha)

      (let* ((background-image (gen-image-checked screen-width screen-height (truncate screen-width 20) (truncate screen-height 20) +orange+ +yellow+))

             (fudesumi-texture (load-texture-from-image fudesumi-image))
             (texture-alpha (load-texture-from-image image-alpha))
             (texture-red (load-texture-from-image image-red))
             (texture-green (load-texture-from-image image-green))
             (texture-blue (load-texture-from-image image-blue))
             (background-texture (load-texture-from-image background-image))

             (fudesumi-rec (make-rectangle :x 0.0 :y 0.0 :width (float (image-width fudesumi-image)) :height (float (image-height fudesumi-image))))

             (fudesumi-pos (make-rectangle :x 50.0 :y 10.0 :width (* (image-width fudesumi-image) 0.8) :height (* (image-height fudesumi-image) 0.8)))
             (red-pos (make-rectangle :x 410.0 :y 10.0 :width (/ (rectangle-width fudesumi-pos) 2.0) :height (/ (rectangle-height fudesumi-pos) 2.0)))
             (green-pos (make-rectangle :x 600.0 :y 10.0 :width (/ (rectangle-width fudesumi-pos) 2.0) :height (/ (rectangle-height fudesumi-pos) 2.0)))
             (blue-pos (make-rectangle :x 410.0 :y 230.0 :width (/ (rectangle-width fudesumi-pos) 2.0) :height (/ (rectangle-height fudesumi-pos) 2.0)))
             (alpha-pos (make-rectangle :x 600.0 :y 230.0 :width (/ (rectangle-width fudesumi-pos) 2.0) :height (/ (rectangle-height fudesumi-pos) 2.0))))

        (unload-image fudesumi-image)
        (unload-image image-alpha)
        (unload-image image-red)
        (unload-image image-green)
        (unload-image image-blue)
        (unload-image background-image)

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 ;; Nothing to update...
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (draw-texture background-texture 0 0 +white+)
                 (draw-texture-pro fudesumi-texture fudesumi-rec fudesumi-pos (vec2 0.0 0.0) 0.0 +white+)

                 (draw-texture-pro texture-red fudesumi-rec red-pos (vec2 0.0 0.0) 0.0 +red+)
                 (draw-texture-pro texture-green fudesumi-rec green-pos (vec2 0.0 0.0) 0.0 +green+)
                 (draw-texture-pro texture-blue fudesumi-rec blue-pos (vec2 0.0 0.0) 0.0 +blue+)
                 (draw-texture-pro texture-alpha fudesumi-rec alpha-pos (vec2 0.0 0.0) 0.0 +white+)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-texture background-texture)
        (unload-texture fudesumi-texture)
        (unload-texture texture-red)
        (unload-texture texture-green)
        (unload-texture texture-blue)
        (unload-texture texture-alpha)

        (close-window)))))              ; Close window and OpenGL context

(main)

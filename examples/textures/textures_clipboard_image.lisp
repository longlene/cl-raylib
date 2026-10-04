;;;; raylib [textures] example - clipboard image
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Maicon Santana (@maiconpintoabreu) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 Maicon Santana (@maiconpintoabreu)
;;;; Common Lisp port of raylib/examples/textures/textures_clipboard_image.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-clipboard-image
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-clipboard-image)

(defconstant +max-texture-collection+ 20)

(defstruct texture-collection
  (texture (make-texture))
  (position (vec2 0.0 0.0)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - clipboard image")

    (let ((collection (let ((a (make-array +max-texture-collection+)))
                        (dotimes (i +max-texture-collection+ a) (setf (aref a i) (make-texture-collection)))))
          (current-collection-index 0))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-r+) ; Reset image collection
                 ;; Unload textures to avoid memory leaks
                 (dotimes (i +max-texture-collection+) (unload-texture (texture-collection-texture (aref collection i))))

                 (setf current-collection-index 0))

               (when (and (is-key-down +key-left-control+) (is-key-pressed +key-v+)
                          (< current-collection-index +max-texture-collection+))
                 (let ((image (get-clipboard-image)))
                   (if (is-image-valid image)
                       (let ((item (aref collection current-collection-index)))
                         (setf (texture-collection-texture item) (load-texture-from-image image)
                               (texture-collection-position item) (get-mouse-position))
                         (incf current-collection-index)
                         (unload-image image))
                       (trace-log +log-info+ "IMAGE: Could not retrieve image from clipboard"))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (dotimes (i current-collection-index)
                 (let* ((item (aref collection i))
                        (texture (texture-collection-texture item))
                        (position (texture-collection-position item)))
                   (when (is-texture-valid texture)
                     (draw-texture-pro texture
                                       (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width texture)) :height (float (texture-height texture)))
                                       (make-rectangle :x (vx position) :y (vy position) :width (float (texture-width texture)) :height (float (texture-height texture)))
                                       (vec2 (* (texture-width texture) 0.5) (* (texture-height texture) 0.5))
                                       0.0 +white+))))

               (draw-rectangle 0 0 screen-width 40 +black+)
               (draw-text "Clipboard Image - Ctrl+V to Paste and R to Reset " 120 10 20 +lightgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (dotimes (i +max-texture-collection+)
        (unload-texture (texture-collection-texture (aref collection i))))

      (close-window))))                 ; Close window and OpenGL context

(main)

;;;; raylib [shaders] example - rlgl compute
;;;;
;;;; WARNING: This example requires raylib compiled with OpenGL 4.3 version for
;;;;       compute shaders support, shaders used in this example are #version 430
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 4.0, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Teddy Astie (@tsnake41) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2021-2025 Teddy Astie (@tsnake41)
;;;; Common Lisp port of raylib/examples/shaders/shaders_rlgl_compute.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-rlgl-compute
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-rlgl-compute)

;; IMPORTANT: This must match gol*.glsl GOL_WIDTH constant
;; This must be a multiple of 16 (check golLogic compute dispatch)
(defconstant +gol-width+ 768)

;; Maximum amount of queued draw commands (squares draw from mouse down events)
(defconstant +max-buffered-transferts+ 48)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Game Of Life Update Commands SSBO, laid out like the C struct (uint32 fields):
;;   unsigned int count;
;;   GolUpdateCmd commands[MAX_BUFFERED_TRANSFERTS]; // { x, y, w, enabled } each
(defun make-gol-update-ssbo ()
  (make-array (1+ (* 4 +max-buffered-transferts+)) :element-type '(unsigned-byte 32) :initial-element 0))

(defmacro gol-update-ssbo-count (ssbo) `(aref ,ssbo 0))

(defun set-gol-update-cmd (ssbo index x y w enabled)
  (let ((base (+ 1 (* 4 index))))
    (setf (aref ssbo (+ base 0)) (ldb (byte 32 0) x) ; x coordinate of the gol command
          (aref ssbo (+ base 1)) (ldb (byte 32 0) y) ; y coordinate of the gol command
          (aref ssbo (+ base 2)) (ldb (byte 32 0) w) ; width of the filled zone
          (aref ssbo (+ base 3)) enabled)))          ; whether to enable or disable zone

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width +gol-width+)
        (screen-height +gol-width+))

    (init-window screen-width screen-height "raylib [shaders] example - rlgl compute")

    (let* ((resolution (vec2 (float screen-width) (float screen-height)))
           (brush-size 8)

           ;; Game of Life logic compute shader
           (gol-logic-code (load-file-text "resources/shaders/glsl430/gol.glsl"))
           (gol-logic-shader (rl-load-shader gol-logic-code +rl-compute-shader+))
           (gol-logic-program (rl-load-shader-program-compute gol-logic-shader))

           ;; Game of Life logic render shader
           (gol-render-shader (load-shader nil "resources/shaders/glsl430/gol_render.glsl"))
           (res-uniform-loc (get-shader-location gol-render-shader "resolution"))

           ;; Game of Life transfert shader (CPU<->GPU download and upload)
           (gol-transfert-code (load-file-text "resources/shaders/glsl430/gol_transfert.glsl"))
           (gol-transfert-shader (rl-load-shader gol-transfert-code +rl-compute-shader+))
           (gol-transfert-program (rl-load-shader-program-compute gol-transfert-shader))

           ;; Load shader storage buffer object (SSBO), id returned
           (ssbo-a (rl-load-shader-buffer (* +gol-width+ +gol-width+ 4) nil +rl-dynamic-copy+))
           (ssbo-b (rl-load-shader-buffer (* +gol-width+ +gol-width+ 4) nil +rl-dynamic-copy+))
           (transfert-buffer (make-gol-update-ssbo))
           (ssbo-transfert (rl-load-shader-buffer (* 4 (length transfert-buffer)) nil +rl-dynamic-copy+))

           ;; Create a white texture of the size of the window to update
           ;; each pixel of the window using the fragment shader: golRenderShader
           (white-image (gen-image-color +gol-width+ +gol-width+ +white+))
           (white-tex (load-texture-from-image white-image)))

      (unload-file-text gol-logic-code)
      (unload-file-text gol-transfert-code)
      (unload-image white-image)

      (set-target-fps 0)                ; Set our game to run with an uncapped framerate
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close)
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf brush-size (truncate (get-mouse-wheel-move)))

               (cond ((and (or (is-mouse-button-down +mouse-button-left+) (is-mouse-button-down +mouse-button-right+))
                           (< (gol-update-ssbo-count transfert-buffer) +max-buffered-transferts+))
                      ;; Buffer a new command
                      (set-gol-update-cmd transfert-buffer (gol-update-ssbo-count transfert-buffer)
                                          (- (get-mouse-x) (floor brush-size 2))
                                          (- (get-mouse-y) (floor brush-size 2))
                                          brush-size
                                          (if (is-mouse-button-down +mouse-button-left+) 1 0))
                      (incf (gol-update-ssbo-count transfert-buffer)))
                     ((> (gol-update-ssbo-count transfert-buffer) 0) ; Process transfert buffer
                      ;; Send SSBO buffer to GPU
                      (rl-update-shader-buffer ssbo-transfert transfert-buffer (* 4 (length transfert-buffer)) 0)

                      ;; Process SSBO commands on GPU
                      (rl-enable-shader gol-transfert-program)
                      (rl-bind-shader-buffer ssbo-a 1)
                      (rl-bind-shader-buffer ssbo-transfert 3)
                      (rl-compute-shader-dispatch (gol-update-ssbo-count transfert-buffer) 1 1) ; Each GPU unit will process a command!
                      (rl-disable-shader)

                      (setf (gol-update-ssbo-count transfert-buffer) 0))
                     (t
                      ;; Process game of life logic
                      (rl-enable-shader gol-logic-program)
                      (rl-bind-shader-buffer ssbo-a 1)
                      (rl-bind-shader-buffer ssbo-b 2)
                      (rl-compute-shader-dispatch (floor +gol-width+ 16) (floor +gol-width+ 16) 1)
                      (rl-disable-shader)

                      ;; ssboA <-> ssboB
                      (rotatef ssbo-a ssbo-b)))

               (rl-bind-shader-buffer ssbo-a 1)
               (set-shader-value gol-render-shader res-uniform-loc resolution +shader-uniform-vec2+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +blank+)

               (begin-shader-mode gol-render-shader)
               (draw-texture white-tex 0 0 +white+)
               (end-shader-mode)

               (draw-rectangle-lines (- (get-mouse-x) (floor brush-size 2)) (- (get-mouse-y) (floor brush-size 2)) brush-size brush-size +red+)

               (draw-text "Use Mouse wheel to increase/decrease brush size" 10 10 20 +white+)
               (draw-fps (- (get-screen-width) 100) 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      ;; Unload shader buffers objects
      (rl-unload-shader-buffer ssbo-a)
      (rl-unload-shader-buffer ssbo-b)
      (rl-unload-shader-buffer ssbo-transfert)

      ;; Unload compute shader
      (rl-unload-shader gol-logic-shader)
      (rl-unload-shader gol-transfert-shader)
      (rl-unload-shader-program gol-transfert-program)
      (rl-unload-shader-program gol-logic-program)

      (unload-texture white-tex)        ; Unload white texture
      (unload-shader gol-render-shader) ; Unload rendering fragment shader

      (close-window))))                 ; Close window and OpenGL context

(main)

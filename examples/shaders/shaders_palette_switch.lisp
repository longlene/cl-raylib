;;;; raylib [shaders] example - palette switch
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; NOTE: Shaders used in this example are #version 330 (OpenGL 3.3), to test this example
;;;;       on OpenGL ES 2.0 platforms (Android, Raspberry Pi, HTML5), use #version 100 shaders
;;;;       raylib comes with shaders ready for both versions, check raylib/shaders install folder
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.7
;;;;
;;;; Example contributed by Marco Lizza (@MarcoLizza) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Marco Lizza (@MarcoLizza) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_palette_switch.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-palette-switch
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-palette-switch)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

(defconstant +max-palettes+ 3)
(defconstant +colors-per-palette+ 8)
(defconstant +values-per-color+ 3)

;;------------------------------------------------------------------------------------
;; Global Variables Definition
;;------------------------------------------------------------------------------------
(defparameter *palettes*
  (vector (vector                       ; 3-BIT RGB
           0 0 0
           255 0 0
           0 255 0
           0 0 255
           0 255 255
           255 0 255
           255 255 0
           255 255 255)
          (vector                       ; AMMO-8 (GameBoy-like)
           4 12 6
           17 35 24
           30 58 41
           48 93 66
           77 128 97
           137 162 87
           190 220 127
           238 255 204)
          (vector                       ; RKBV (2-strip film)
           21 25 26
           138 76 88
           217 98 117
           230 184 193
           69 107 115
           75 151 166
           165 189 194
           255 245 247)))

(defparameter *palette-text* #("3-BIT RGB"
                               "AMMO-8 (GameBoy-like)"
                               "RKBV (2-strip film)"))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - palette switch")

    ;; Load shader to be used on some parts drawing
    ;; NOTE 1: Using GLSL 330 shader version, on OpenGL ES 2.0 use GLSL 100 shader version
    ;; NOTE 2: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
    (let* ((shader (load-shader nil (text-format "resources/shaders/glsl%i/palette_switch.fs" +glsl-version+)))

           ;; Get variable (uniform) location on the shader to connect with the program
           ;; NOTE: If uniform variable could not be found in the shader, function returns -1
           (palette-loc (get-shader-location shader "palette"))

           (current-palette 0)
           (line-height (floor screen-height +colors-per-palette+)))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (cond ((is-key-pressed +key-right+) (incf current-palette))
                     ((is-key-pressed +key-left+) (decf current-palette)))

               (cond ((>= current-palette +max-palettes+) (setf current-palette 0))
                     ((< current-palette 0) (setf current-palette (1- +max-palettes+))))

               ;; Send palette data to the shader to be used on drawing
               ;; NOTE: We are sending RGB triplets w/o the alpha channel
               (set-shader-value-v shader palette-loc (aref *palettes* current-palette) +shader-uniform-ivec3+ +colors-per-palette+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-shader-mode shader)

               (dotimes (i +colors-per-palette+)
                 ;; Draw horizontal screen-wide rectangles with increasing "palette index"
                 ;; The used palette index is encoded in the RGB components of the pixel
                 (draw-rectangle 0 (* line-height i) (get-screen-width) line-height (list i i i 255)))

               (end-shader-mode)

               (draw-text "< >" 10 10 30 +darkblue+)
               (draw-text "CURRENT PALETTE:" 60 15 20 +raywhite+)
               (draw-text (aref *palette-text* current-palette) 300 15 20 +red+)

               (draw-fps 700 15)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader

      (close-window))))                 ; Close window and OpenGL context

(main)

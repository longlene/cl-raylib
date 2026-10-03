;;;; raylib [core] example - window web
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 5.5
;;;;
;;;; This example has been adapted to compile for PLATFORM_WEB and PLATFORM_DESKTOP
;;;; As you will notice, code structure is slightly different to the other examples
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_window_web.c
;;;; (only the PLATFORM_DESKTOP path applies to this port)

(require :cl-raylib)

(defpackage #:raylib-examples/core-window-web
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-window-web)

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
(defparameter *screen-width* 800)
(defparameter *screen-height* 450)

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
(defun update-draw-frame ()
  ;; Update
  ;;----------------------------------------------------------------------------------
  ;; TODO: Update your variables here
  ;;----------------------------------------------------------------------------------

  ;; Draw
  ;;----------------------------------------------------------------------------------
  (begin-drawing)

  (clear-background +raywhite+)

  (draw-text "Welcome to raylib web structure!" 220 200 20 +skyblue+)

  (end-drawing))
  ;;----------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------
;; Program main entry point
;;----------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (init-window *screen-width* *screen-height* "raylib [core] example - window web")

  (set-target-fps 60)                   ; Set our game to run at 60 frames-per-second
  ;;--------------------------------------------------------------------------------------

  ;; Main game loop
  (loop until (window-should-close)     ; Detect window close button or ESC key
        do (update-draw-frame))

  ;; De-Initialization
  ;;--------------------------------------------------------------------------------------
  (close-window))                       ; Close window and OpenGL context

(main)

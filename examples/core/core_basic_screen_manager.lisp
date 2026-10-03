;;;; raylib [core] example - basic screen manager
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; NOTE: This example illustrates a very simple screen manager based on a states machines
;;;;
;;;; Example originally created with raylib 4.0, last time updated with raylib 4.0
;;;;
;;;; Copyright (c) 2021-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_basic_screen_manager.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-basic-screen-manager
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-basic-screen-manager)

;;------------------------------------------------------------------------------------------
;; Types and Structures Definition
;;------------------------------------------------------------------------------------------
;; GameScreen: :logo, :title, :gameplay, :ending

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - basic screen manager")

    (let ((current-screen :logo)
          ;; TODO: Initialize all required variables and load all required data here!
          (frames-counter 0))           ; Useful to count frames

      (set-target-fps 60)               ; Set desired framerate (frames-per-second)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (ecase current-screen
                 (:logo
                  ;; TODO: Update LOGO screen variables here!
                  (incf frames-counter) ; Count frames

                  ;; Wait for 2 seconds (120 frames) before jumping to TITLE screen
                  (when (> frames-counter 120)
                    (setf current-screen :title)))
                 (:title
                  ;; TODO: Update TITLE screen variables here!

                  ;; Press enter to change to GAMEPLAY screen
                  (when (or (is-key-pressed +key-enter+) (is-gesture-detected +gesture-tap+))
                    (setf current-screen :gameplay)))
                 (:gameplay
                  ;; TODO: Update GAMEPLAY screen variables here!

                  ;; Press enter to change to ENDING screen
                  (when (or (is-key-pressed +key-enter+) (is-gesture-detected +gesture-tap+))
                    (setf current-screen :ending)))
                 (:ending
                  ;; TODO: Update ENDING screen variables here!

                  ;; Press enter to return to TITLE screen
                  (when (or (is-key-pressed +key-enter+) (is-gesture-detected +gesture-tap+))
                    (setf current-screen :title))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (ecase current-screen
                 (:logo
                  ;; TODO: Draw LOGO screen here!
                  (draw-text "LOGO SCREEN" 20 20 40 +lightgray+)
                  (draw-text "WAIT for 2 SECONDS..." 290 220 20 +gray+))
                 (:title
                  ;; TODO: Draw TITLE screen here!
                  (draw-rectangle 0 0 screen-width screen-height +green+)
                  (draw-text "TITLE SCREEN" 20 20 40 +darkgreen+)
                  (draw-text "PRESS ENTER or TAP to JUMP to GAMEPLAY SCREEN" 120 220 20 +darkgreen+))
                 (:gameplay
                  ;; TODO: Draw GAMEPLAY screen here!
                  (draw-rectangle 0 0 screen-width screen-height +purple+)
                  (draw-text "GAMEPLAY SCREEN" 20 20 40 +maroon+)
                  (draw-text "PRESS ENTER or TAP to JUMP to ENDING SCREEN" 130 220 20 +maroon+))
                 (:ending
                  ;; TODO: Draw ENDING screen here!
                  (draw-rectangle 0 0 screen-width screen-height +blue+)
                  (draw-text "ENDING SCREEN" 20 20 40 +darkblue+)
                  (draw-text "PRESS ENTER or TAP to RETURN to TITLE SCREEN" 120 220 20 +darkblue+)))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------

      ;; TODO: Unload all loaded data (textures, fonts, audio) here!

      (close-window))))                 ; Close window and OpenGL context

(main)

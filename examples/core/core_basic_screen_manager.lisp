;;;; core_basic_screen_manager.lisp
;;;; 
;;;; cl-raylib [core] example - Basic screen manager
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Translation of raylib's core_basic_screen_manager.c example
;;;; This example demonstrates a simple screen manager based on state machines
;;;; Useful pattern for organizing different game states (menu, gameplay, etc.)
;;;;
;;;; This example has been created using cl-raylib
;;;; cl-raylib is a Common Lisp implementation of raylib
;;;; https://github.com/longlene/cl-raylib
;;;;
;;;; Copyright (c) 2025 cl-raylib contributors
;;;; Licensed under the unmodified zlib/libpng license

(require :cl-raylib)

(defpackage :cl-raylib-example-core-basic-screen-manager
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-basic-screen-manager)

;; Game screen states enumeration
(defparameter +logo+ 0)
(defparameter +title+ 1)
(defparameter +gameplay+ 2)
(defparameter +ending+ 3)

(defun core-basic-screen-manager ()
  "Basic screen manager example - demonstrate state machine pattern for game screens"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - basic screen manager")
      (let ((current-screen +logo+)
            (frames-counter 0))
        
        ;; TODO: Initialize all required variables and load all required data here!
        
        (set-target-fps 60)  ; Set desired framerate (frames-per-second)
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          
          ;; Update
          (cond
            ;; LOGO screen logic
            ((= current-screen +logo+)
             ;; TODO: Update LOGO screen variables here!
             (incf frames-counter)  ; Count frames
             
             ;; Wait for 2 seconds (120 frames) before jumping to TITLE screen
             (when (> frames-counter 120)
               (setf current-screen +title+)))
            
            ;; TITLE screen logic
            ((= current-screen +title+)
             ;; TODO: Update TITLE screen variables here!
             
             ;; Press enter to change to GAMEPLAY screen
             ;; Note: IsGestureDetected not implemented yet, so only using keyboard
             (when (is-key-pressed :key-enter)
               (setf current-screen +gameplay+)))
            
            ;; GAMEPLAY screen logic
            ((= current-screen +gameplay+)
             ;; TODO: Update GAMEPLAY screen variables here!
             
             ;; Press enter to change to ENDING screen
             (when (is-key-pressed :key-enter)
               (setf current-screen +ending+)))
            
            ;; ENDING screen logic
            ((= current-screen +ending+)
             ;; TODO: Update ENDING screen variables here!
             
             ;; Press enter to return to TITLE screen
             (when (is-key-pressed :key-enter)
               (setf current-screen +title+))))
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (cond
              ;; Draw LOGO screen
              ((= current-screen +logo+)
               ;; TODO: Draw LOGO screen here!
               (draw-text "LOGO SCREEN" 20 20 40 +lightgray+)
               (draw-text "WAIT for 2 SECONDS..." 290 220 20 +gray+))
              
              ;; Draw TITLE screen
              ((= current-screen +title+)
               ;; TODO: Draw TITLE screen here!
               (draw-rectangle 0 0 screen-width screen-height +green+)
               (draw-text "TITLE SCREEN" 20 20 40 +darkgreen+)
               (draw-text "PRESS ENTER to JUMP to GAMEPLAY SCREEN" 150 220 20 +darkgreen+))
              
              ;; Draw GAMEPLAY screen
              ((= current-screen +gameplay+)
               ;; TODO: Draw GAMEPLAY screen here!
               (draw-rectangle 0 0 screen-width screen-height +purple+)
               (draw-text "GAMEPLAY SCREEN" 20 20 40 +maroon+)
               (draw-text "PRESS ENTER to JUMP to ENDING SCREEN" 170 220 20 +maroon+))
              
              ;; Draw ENDING screen
              ((= current-screen +ending+)
               ;; TODO: Draw ENDING screen here!
               (draw-rectangle 0 0 screen-width screen-height +blue+)
               (draw-text "ENDING SCREEN" 20 20 40 +darkblue+)
               (draw-text "PRESS ENTER to RETURN to TITLE SCREEN" 150 220 20 +darkblue+)))))
        
        ;; De-Initialization
        ;; TODO: Unload all loaded data (textures, fonts, audio) here!
        ))))

;; Run the example
(core-basic-screen-manager)

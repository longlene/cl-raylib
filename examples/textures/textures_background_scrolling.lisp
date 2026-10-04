;;;; raylib [textures] example - background scrolling
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 2.0, last time updated with raylib 2.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_background_scrolling.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-background-scrolling
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-background-scrolling)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - background scrolling")

    ;; NOTE: Be careful, background width must be equal or bigger than screen width
    ;; if not, texture should be draw more than two times for scrolling effect
    (let ((background (load-texture "resources/cyberpunk_street_background.png"))
          (midground (load-texture "resources/cyberpunk_street_midground.png"))
          (foreground (load-texture "resources/cyberpunk_street_foreground.png"))

          (scrolling-back 0.0)
          (scrolling-mid 0.0)
          (scrolling-fore 0.0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (decf scrolling-back 0.1)
               (decf scrolling-mid 0.5)
               (decf scrolling-fore 1.0)

               ;; NOTE: Texture is scaled twice its size, so it sould be considered on scrolling
               (when (<= scrolling-back (* (- (texture-width background)) 2)) (setf scrolling-back 0.0))
               (when (<= scrolling-mid (* (- (texture-width midground)) 2)) (setf scrolling-mid 0.0))
               (when (<= scrolling-fore (* (- (texture-width foreground)) 2)) (setf scrolling-fore 0.0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background (get-color #x052c46ff))

               ;; Draw background image twice
               ;; NOTE: Texture is scaled twice its size
               (draw-texture-ex background (vec2 scrolling-back 20.0) 0.0 2.0 +white+)
               (draw-texture-ex background (vec2 (+ (* (texture-width background) 2) scrolling-back) 20.0) 0.0 2.0 +white+)

               ;; Draw midground image twice
               (draw-texture-ex midground (vec2 scrolling-mid 20.0) 0.0 2.0 +white+)
               (draw-texture-ex midground (vec2 (+ (* (texture-width midground) 2) scrolling-mid) 20.0) 0.0 2.0 +white+)

               ;; Draw foreground image twice
               (draw-texture-ex foreground (vec2 scrolling-fore 70.0) 0.0 2.0 +white+)
               (draw-texture-ex foreground (vec2 (+ (* (texture-width foreground) 2) scrolling-fore) 70.0) 0.0 2.0 +white+)

               (draw-text "BACKGROUND SCROLLING & PARALLAX" 10 10 20 +red+)
               (draw-text "(c) Cyberpunk Street Environment by Luis Zuno (@ansimuz)" (- screen-width 330) (- screen-height 20) 10 +raywhite+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture background)       ; Unload background texture
      (unload-texture midground)        ; Unload midground texture
      (unload-texture foreground)       ; Unload foreground texture

      (close-window))))                 ; Close window and OpenGL context

(main)

;;;; textures_background_scrolling.lisp - Background scrolling with parallax effect
;;;; Translated from raylib/examples/textures/textures_background_scrolling.c

(require :cl-raylib)

(defpackage :textures-background-scrolling
  (:use :cl :cl-raylib))

(in-package :textures-background-scrolling)

(defun get-color (hex-value)
  "Get Color structure from hexadecimal value (raylib GetColor equivalent)"
  (let ((r (logand (ash hex-value -24) #xFF))
        (g (logand (ash hex-value -16) #xFF))
        (b (logand (ash hex-value -8) #xFF))
        (a (logand hex-value #xFF)))
    (make-color r g b a)))

(defun main ()
  "Main function - background scrolling with parallax effect"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [textures] example - background scrolling")

    ;; Load background textures
    ;; NOTE: Be careful, background width must be equal or bigger than screen width
    ;; if not, texture should be drawn more than two times for scrolling effect
    (let ((background (load-texture "resources/cyberpunk_street_background.png"))
          (midground (load-texture "resources/cyberpunk_street_midground.png"))
          (foreground (load-texture "resources/cyberpunk_street_foreground.png")))
      
      (let ((scrolling-back 0.0)
            (scrolling-mid 0.0)
            (scrolling-fore 0.0))

        (set-target-fps 60) ; Set game to run at 60 frames-per-second

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          ;; Different scrolling speeds for parallax effect
          (decf scrolling-back 0.1)
          (decf scrolling-mid 0.5)
          (decf scrolling-fore 1.0)

          ;; Reset scrolling when texture has completely scrolled
          ;; NOTE: Texture is scaled twice its size, so it should be considered on scrolling
          (when (<= scrolling-back (- (* (texture-width background) 2)))
            (setf scrolling-back 0.0))
          (when (<= scrolling-mid (- (* (texture-width midground) 2)))
            (setf scrolling-mid 0.0))
          (when (<= scrolling-fore (- (* (texture-width foreground) 2)))
            (setf scrolling-fore 0.0))

          ;; Draw
          (begin-drawing)
            ;; Custom background color (dark blue from the C example)
            (clear-background (get-color #x052c46ff))

            ;; Draw background layer twice for seamless scrolling
            ;; NOTE: Texture is scaled twice its size (2.0f scale)
            (draw-texture-ex background 
                             (vec2 scrolling-back 20) 
                             0.0 2.0 +white+)
            (draw-texture-ex background 
                             (vec2 (+ (* (texture-width background) 2) scrolling-back) 20) 
                             0.0 2.0 +white+)

            ;; Draw midground layer twice
            (draw-texture-ex midground 
                             (vec2 scrolling-mid 20) 
                             0.0 2.0 +white+)
            (draw-texture-ex midground 
                             (vec2 (+ (* (texture-width midground) 2) scrolling-mid) 20) 
                             0.0 2.0 +white+)

            ;; Draw foreground layer twice
            (draw-texture-ex foreground 
                             (vec2 scrolling-fore 70) 
                             0.0 2.0 +white+)
            (draw-texture-ex foreground 
                             (vec2 (+ (* (texture-width foreground) 2) scrolling-fore) 70) 
                             0.0 2.0 +white+)

            ;; Draw title and credits
            (draw-text "BACKGROUND SCROLLING & PARALLAX" 10 10 20 +red+)
            (draw-text "(c) Cyberpunk Street Environment by Luis Zuno (@ansimuz)" 
                       (- screen-width 330) (- screen-height 20) 10 +raywhite+)

          (end-drawing))

        ;; De-Initialization
        (unload-texture background)
        (unload-texture midground)
        (unload-texture foreground)))

    ;; Close window
    (close-window)))

;; Run the example
(main)
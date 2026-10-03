;;;; raylib [core] example - window letterbox
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Anata (@anatagawa) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Anata (@anatagawa) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_window_letterbox.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-window-letterbox
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-window-letterbox)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  (let ((screen-width 800)
        (screen-height 450))

    ;; Enable config flags for resizable window and vertical synchro
    (set-config-flags (logior +flag-window-resizable+ +flag-vsync-hint+))
    (init-window screen-width screen-height "raylib [core] example - window letterbox")
    (set-window-min-size 320 240)

    (let* ((game-screen-width 640)
           (game-screen-height 480)
           ;; Render texture initialization, used to hold the rendering result so we can easily resize it
           (target (load-render-texture game-screen-width game-screen-height))
           (colors (make-array 10)))
      (set-texture-filter (render-texture-texture target) +texture-filter-bilinear+) ; Texture scale filter to use

      (flet ((random-colors ()
               (dotimes (i 10)
                 (setf (aref colors i) (list (get-random-value 100 250) (get-random-value 50 150)
                                             (get-random-value 10 100) 255)))))
        (random-colors)

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 ;; Compute required framebuffer scaling
                 (let ((scale (min (/ (float (get-screen-width)) game-screen-width)
                                   (/ (float (get-screen-height)) game-screen-height))))

                   (when (is-key-pressed +key-space+)
                     ;; Recalculate random colors for the bars
                     (random-colors))

                   ;; Update virtual mouse (clamped mouse value behind game screen)
                   (let ((mouse (get-mouse-position))
                         (virtual-mouse (vec2 0.0 0.0)))
                     (setf (vx virtual-mouse) (/ (- (vx mouse) (* (- (get-screen-width) (* game-screen-width scale)) 0.5)) scale))
                     (setf (vy virtual-mouse) (/ (- (vy mouse) (* (- (get-screen-height) (* game-screen-height scale)) 0.5)) scale))
                     (setf virtual-mouse (vector2-clamp virtual-mouse (vec2 0.0 0.0)
                                                        (vec2 (float game-screen-width) (float game-screen-height))))

                     ;; Apply the same transformation as the virtual mouse to the real mouse (i.e. to work with raygui)
                     ;;(set-mouse-offset (- (* (- (get-screen-width) (* game-screen-width scale)) 0.5)) (- (* (- (get-screen-height) (* game-screen-height scale)) 0.5)))
                     ;;(set-mouse-scale (/ 1 scale) (/ 1 scale))
                     ;;----------------------------------------------------------------------------------

                     ;; Draw
                     ;;----------------------------------------------------------------------------------
                     ;; Draw everything in the render texture, note this will not be rendered on screen, yet
                     (begin-texture-mode target)
                     (clear-background +raywhite+) ; Clear render texture background color

                     (dotimes (i 10)
                       (draw-rectangle 0 (* (truncate game-screen-height 10) i) game-screen-width
                                       (truncate game-screen-height 10) (aref colors i)))

                     (draw-text (format nil "If executed inside a window,~%you can resize the window,~%and see the screen scaling!") 10 25 20 +white+)
                     (draw-text (text-format "Default Mouse: [%i , %i]" (truncate (vx mouse)) (truncate (vy mouse))) 350 25 20 +green+)
                     (draw-text (text-format "Virtual Mouse: [%i , %i]" (truncate (vx virtual-mouse)) (truncate (vy virtual-mouse))) 350 55 20 +yellow+)
                     (end-texture-mode))

                   (begin-drawing)
                   (clear-background +black+) ; Clear screen background

                   ;; Draw render texture to screen, properly scaled
                   (let ((texture (render-texture-texture target)))
                     (draw-texture-pro texture
                                       (make-rectangle :x 0.0 :y 0.0
                                                       :width (float (texture-width texture))
                                                       :height (float (- (texture-height texture))))
                                       (make-rectangle :x (* (- (get-screen-width) (* (float game-screen-width) scale)) 0.5)
                                                       :y (* (- (get-screen-height) (* (float game-screen-height) scale)) 0.5)
                                                       :width (* (float game-screen-width) scale)
                                                       :height (* (float game-screen-height) scale))
                                       (vec2 0.0 0.0) 0.0 +white+))
                   (end-drawing)))
        ;;--------------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-render-texture target)  ; Unload render texture

        (close-window)))))              ; Close window and OpenGL context

(main)

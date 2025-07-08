(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun vector2-clamp (v min-v max-v)
  "Clamp vector2 between min and max vectors"
  (vec2 (max (vx min-v) (min (vx max-v) (vx v)))
        (max (vy min-v) (min (vy max-v) (vy v)))))

(defun main ()
  "raylib [core] example - window scale letterbox (and virtual mouse)"
  (let ((window-width 800)
        (window-height 450))
    
    ;; Enable config flags for resizable window and vertical synchro
    (set-config-flags (list :flag-window-resizable :flag-vsync-hint))
    (with-window (window-width window-height "raylib [core] example - window scale letterbox")
      (set-window-min-size 320 240)
      (set-target-fps 60) ; Set our game to run at 60 FPS

      (let ((game-screen-width 640)
            (game-screen-height 480)
            (colors (make-array 10)))

        ;; Initialize colors
        (loop for i from 0 below 10 do
          (setf (aref colors i) 
                (make-color (get-random-value 100 250)
                           (get-random-value 50 150)
                           (get-random-value 10 100)
                           255)))

        ;; Render texture initialization, used to hold the rendering result so we can easily resize it
        (let ((target (load-render-texture game-screen-width game-screen-height)))
          (set-texture-filter (render-texture-texture target) :texture-filter-bilinear)

          (loop
            until (window-should-close) ; Detect window close button or ESC key
            do (progn
                 ;; Update
                 ;; Compute required framebuffer scaling
                 (let ((scale (min (/ (get-screen-width) game-screen-width)
                                   (/ (get-screen-height) game-screen-height))))

                   (when (is-key-pressed :key-space)
                     ;; Recalculate random colors for the bars
                     (loop for i from 0 below 10 do
                       (setf (aref colors i) 
                             (make-color (get-random-value 100 250)
                                        (get-random-value 50 150)
                                        (get-random-value 10 100)
                                        255))))

                   ;; Update virtual mouse (clamped mouse value behind game screen)
                   (let* ((mouse (get-mouse-position))
                          (virtual-mouse-x (/ (- (vx mouse) 
                                                 (* (- (get-screen-width) (* game-screen-width scale)) 0.5))
                                             scale))
                          (virtual-mouse-y (/ (- (vy mouse) 
                                                 (* (- (get-screen-height) (* game-screen-height scale)) 0.5))
                                             scale))
                          (virtual-mouse (vector2-clamp (vec2 virtual-mouse-x virtual-mouse-y)
                                                        (vec2 0.0 0.0)
                                                        (vec2 (float game-screen-width) 
                                                              (float game-screen-height)))))

                     ;; Draw everything in the render texture
                     (with-texture-mode (target)
                       (clear-background :raywhite)

                       ;; Draw colored bars
                       (loop for i from 0 below 10 do
                         (draw-rectangle 0 (* (floor (/ game-screen-height 10)) i) 
                                        game-screen-width (floor (/ game-screen-height 10)) 
                                        (aref colors i)))

                       (draw-text "If executed inside a window,
you can resize the window,
and see the screen scaling!" 10 25 20 :white)
                       (draw-text (format nil "Default Mouse: [~d , ~d]" 
                                         (floor (vx mouse)) (floor (vy mouse))) 
                                 350 25 20 :green)
                       (draw-text (format nil "Virtual Mouse: [~d , ~d]" 
                                         (floor (vx virtual-mouse)) (floor (vy virtual-mouse))) 
                                 350 55 20 :yellow))

                     ;; Draw to screen
                     (with-drawing
                       (clear-background :black)

                       ;; Draw render texture to screen, properly scaled
                       (let ((source-rect (make-rectangle :x 0.0 :y 0.0 
                                                         :width (texture-width (render-texture-texture target))
                                                         :height (- (texture-height (render-texture-texture target)))))
                             (dest-rect (make-rectangle 
                                        :x (* (- (get-screen-width) (* game-screen-width scale)) 0.5)
                                        :y (* (- (get-screen-height) (* game-screen-height scale)) 0.5)
                                        :width (* game-screen-width scale)
                                        :height (* game-screen-height scale))))
                         (draw-texture-pro (render-texture-texture target) source-rect dest-rect 
                                          (vec2 0.0 0.0) 0.0 :white)))))))

          ;; Cleanup
          (unload-render-texture target))))))

(main)
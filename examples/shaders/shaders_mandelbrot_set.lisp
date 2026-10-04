;;;; raylib [shaders] example - mandelbrot set
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; NOTE: Shaders used in this example are #version 330 (OpenGL 3.3)
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Jordi Santonja (@JordSant)
;;;; Based on previous work by Josh Colclough (@joshcol9232)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Jordi Santonja (@JordSant)
;;;; Common Lisp port of raylib/examples/shaders/shaders_mandelbrot_set.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-mandelbrot-set
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-mandelbrot-set)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;; A few good interesting places
(defparameter *points-of-interest* #((-1.76826775 -0.00422996283 28435.9238)
                                     (0.322004497 -0.0357099883 56499.7266)
                                     (-0.748880744 -0.0562955774 9237.59082)
                                     (-1.78385007 -0.0156200649 14599.5283)
                                     (-0.0985441282 -0.924688697 26259.8535)
                                     (0.317785531 -0.0322612226 29297.9258)))

(defparameter *screen-width* 800)
(defparameter *screen-height* 450)
(defparameter *zoom-speed* 1.01)
(defparameter *offset-speed-mul* 2.0)
(defparameter *starting-zoom* 0.6)
(defparameter *starting-offset* '(-0.5 0.0))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (init-window *screen-width* *screen-height* "raylib [shaders] example - mandelbrot set")

  ;; Load mandelbrot set shader
  ;; NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
  (let* ((shader (load-shader nil (text-format "resources/shaders/glsl%i/mandelbrot_set.fs" +glsl-version+)))

         ;; Create a RenderTexture2D to be used for render to texture
         (target (load-render-texture (get-screen-width) (get-screen-height)))

         ;; Offset and zoom to draw the mandelbrot set at. (centered on screen and default size)
         (offset (copy-list *starting-offset*))
         (zoom *starting-zoom*)

         ;; Depending on the zoom the mximum number of iterations must be adapted to get more detail as we zzoom in
         ;; The solution is not perfect, so a control has been added to increase/decrease the number of iterations with UP/DOWN keys
         ;; NOTE: PLATFORM_DESKTOP values
         (max-iterations 333)
         (max-iterations-multiplier 166.5)

         ;; Get variable (uniform) locations on the shader to connect with the program
         ;; NOTE: If uniform variable could not be found in the shader, function returns -1
         (zoom-loc (get-shader-location shader "zoom"))
         (offset-loc (get-shader-location shader "offset"))
         (max-iterations-loc (get-shader-location shader "maxIterations"))

         (show-controls t))             ; Show controls

    ;; Upload the shader uniform values!
    (set-shader-value shader zoom-loc zoom +shader-uniform-float+)
    (set-shader-value shader offset-loc offset +shader-uniform-vec2+)
    (set-shader-value shader max-iterations-loc max-iterations +shader-uniform-int+)

    (set-target-fps 60)                 ; Set our game to run at 60 frames-per-second
    ;;--------------------------------------------------------------------------------------

    ;; Main game loop
    (loop until (window-should-close)   ; Detect window close button or ESC key
          do ;; Update
             ;;----------------------------------------------------------------------------------
             (let ((update-shader nil))

               ;; Press [1 - 6] to reset c to a point of interest
               (when (or (is-key-pressed +key-one+)
                         (is-key-pressed +key-two+)
                         (is-key-pressed +key-three+)
                         (is-key-pressed +key-four+)
                         (is-key-pressed +key-five+)
                         (is-key-pressed +key-six+))
                 (let ((interest-index (cond ((is-key-pressed +key-one+) 0)
                                             ((is-key-pressed +key-two+) 1)
                                             ((is-key-pressed +key-three+) 2)
                                             ((is-key-pressed +key-four+) 3)
                                             ((is-key-pressed +key-five+) 4)
                                             ((is-key-pressed +key-six+) 5)
                                             (t 0))))
                   (destructuring-bind (x y z) (aref *points-of-interest* interest-index)
                     (setf offset (list x y)
                           zoom z))
                   (setf update-shader t)))

               ;; If "R" is pressed, reset zoom and offset
               (when (is-key-pressed +key-r+)
                 (setf offset (copy-list *starting-offset*)
                       zoom *starting-zoom*
                       update-shader t))

               (when (is-key-pressed +key-f1+) (setf show-controls (not show-controls))) ; Toggle whether or not to show controls

               ;; Change number of max iterations with UP and DOWN keys
               ;; WARNING: Increasing the number of max iterations greatly impacts performance
               (cond ((is-key-pressed +key-up+)
                      (setf max-iterations-multiplier (* max-iterations-multiplier 1.4)
                            update-shader t))
                     ((is-key-pressed +key-down+)
                      (setf max-iterations-multiplier (/ max-iterations-multiplier 1.4)
                            update-shader t)))

               ;; If either left or right button is pressed, zoom in/out
               (when (or (is-mouse-button-down +mouse-button-left+) (is-mouse-button-down +mouse-button-right+))
                 ;; Change zoom. If Mouse left -> zoom in. Mouse right -> zoom out
                 (setf zoom (* zoom (if (is-mouse-button-down +mouse-button-left+) *zoom-speed* (/ 1.0 *zoom-speed*))))

                 (let* ((mouse-pos (get-mouse-position))
                        ;; Find the velocity at which to change the camera. Take the distance of the mouse
                        ;; From the center of the screen as the direction, and adjust magnitude based on the current zoom
                        (offset-velocity-x (/ (* (- (/ (vx mouse-pos) (float *screen-width*)) 0.5) *offset-speed-mul*) zoom))
                        (offset-velocity-y (/ (* (- (/ (vy mouse-pos) (float *screen-height*)) 0.5) *offset-speed-mul*) zoom)))

                   ;; Apply move velocity to camera
                   (setf offset (list (+ (first offset) (* (get-frame-time) offset-velocity-x))
                                      (+ (second offset) (* (get-frame-time) offset-velocity-y)))))

                 (setf update-shader t))

               ;; In case a parameter has been changed, update the shader values
               (when update-shader
                 ;; As we zoom in, increase the number of max iterations to get more detail
                 ;; Aproximate formula, but it works-ish
                 (setf max-iterations (truncate (* (sqrt (* 2.0 (sqrt (abs (- 1.0 (sqrt (* 37.5 zoom))))))) max-iterations-multiplier)))

                 ;; Update the shader uniform values!
                 (set-shader-value shader zoom-loc zoom +shader-uniform-float+)
                 (set-shader-value shader offset-loc offset +shader-uniform-vec2+)
                 (set-shader-value shader max-iterations-loc max-iterations +shader-uniform-int+)))
             ;;----------------------------------------------------------------------------------

             ;; Draw
             ;;----------------------------------------------------------------------------------
             ;; Using a render texture to draw Mandelbrot set
             (begin-texture-mode target)        ; Enable drawing to texture
             (clear-background +black+)         ; Clear the render texture

             ;; Draw a rectangle in shader mode to be used as shader canvas
             ;; NOTE: Rectangle uses font white character texture coordinates,
             ;; So shader can not be applied here directly because input vertexTexCoord
             ;; Do not represent full screen coordinates (space where want to apply shader)
             (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +black+)
             (end-texture-mode)

             (begin-drawing)
             (clear-background +black+)         ; Clear screen background

             ;; Draw the saved texture and rendered mandelbrot set with shader
             ;; NOTE: We do not invert texture on Y, already considered inside shader
             (begin-shader-mode shader)
             ;; WARNING: If FLAG_WINDOW_HIGHDPI is enabled, HighDPI monitor scaling should be considered
             ;; When rendering the RenderTexture2D to fit in the HighDPI scaled Window
             (draw-texture-ex (render-texture-texture target) (vec2 0.0 0.0) 0.0 1.0 +white+)
             (end-shader-mode)

             (when show-controls
               (draw-text "Press Mouse buttons right/left to zoom in/out and move" 10 15 10 +raywhite+)
               (draw-text "Press F1 to toggle these controls" 10 30 10 +raywhite+)
               (draw-text "Press [1 - 6] to change point of interest" 10 45 10 +raywhite+)
               (draw-text "Press UP | DOWN to change number of iterations" 10 60 10 +raywhite+)
               (draw-text "Press R to recenter the camera" 10 75 10 +raywhite+))

             (end-drawing))
    ;;----------------------------------------------------------------------------------

    ;; De-Initialization
    ;;--------------------------------------------------------------------------------------
    (unload-shader shader)              ; Unload shader
    (unload-render-texture target)      ; Unload render texture

    (close-window)))                    ; Close window and OpenGL context

(main)

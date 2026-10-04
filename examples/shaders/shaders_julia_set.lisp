;;;; raylib [shaders] example - julia set
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; NOTE: Shaders used in this example are #version 330 (OpenGL 3.3)
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Josh Colclough (@joshcol9232) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Josh Colclough (@joshcol9232) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_julia_set.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-julia-set
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-julia-set)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;; A few good julia sets
(defparameter *points-of-interest* #((-0.348827 0.607167)
                                     (-0.786268 0.169728)
                                     (-0.8 0.156)
                                     (0.285 0.0)
                                     (-0.835 -0.2321)
                                     (-0.70176 -0.3842)))

(defparameter *screen-width* 800)
(defparameter *screen-height* 450)
(defparameter *zoom-speed* 1.01)
(defparameter *offset-speed-mul* 2.0)
(defparameter *starting-zoom* 0.75)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (init-window *screen-width* *screen-height* "raylib [shaders] example - julia set")

  ;; Load julia set shader
  ;; NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
  (let* ((shader (load-shader nil (text-format "resources/shaders/glsl%i/julia_set.fs" +glsl-version+)))

         ;; Create a RenderTexture2D to be used for render to texture
         (target (load-render-texture (get-screen-width) (get-screen-height)))

         ;; c constant to use in z^2 + c
         (c (copy-list (aref *points-of-interest* 0)))

         ;; Offset and zoom to draw the julia set at. (centered on screen and default size)
         (offset (list 0.0 0.0))
         (zoom *starting-zoom*)

         ;; Get variable (uniform) locations on the shader to connect with the program
         ;; NOTE: If uniform variable could not be found in the shader, function returns -1
         (c-loc (get-shader-location shader "c"))
         (zoom-loc (get-shader-location shader "zoom"))
         (offset-loc (get-shader-location shader "offset"))

         (increment-speed 0)            ; Multiplier of speed to change c value
         (show-controls t))             ; Show controls

    ;; Upload the shader uniform values!
    (set-shader-value shader c-loc c +shader-uniform-vec2+)
    (set-shader-value shader zoom-loc zoom +shader-uniform-float+)
    (set-shader-value shader offset-loc offset +shader-uniform-vec2+)

    (set-target-fps 60)                 ; Set our game to run at 60 frames-per-second
    ;;--------------------------------------------------------------------------------------

    ;; Main game loop
    (loop until (window-should-close)   ; Detect window close button or ESC key
          do ;; Update
             ;;----------------------------------------------------------------------------------
             ;; Press [1 - 6] to reset c to a point of interest
             (when (or (is-key-pressed +key-one+)
                       (is-key-pressed +key-two+)
                       (is-key-pressed +key-three+)
                       (is-key-pressed +key-four+)
                       (is-key-pressed +key-five+)
                       (is-key-pressed +key-six+))
               (cond ((is-key-pressed +key-one+) (setf c (copy-list (aref *points-of-interest* 0))))
                     ((is-key-pressed +key-two+) (setf c (copy-list (aref *points-of-interest* 1))))
                     ((is-key-pressed +key-three+) (setf c (copy-list (aref *points-of-interest* 2))))
                     ((is-key-pressed +key-four+) (setf c (copy-list (aref *points-of-interest* 3))))
                     ((is-key-pressed +key-five+) (setf c (copy-list (aref *points-of-interest* 4))))
                     ((is-key-pressed +key-six+) (setf c (copy-list (aref *points-of-interest* 5)))))
               (set-shader-value shader c-loc c +shader-uniform-vec2+))

             ;; If "R" is pressed, reset zoom and offset
             (when (is-key-pressed +key-r+)
               (setf zoom *starting-zoom*
                     offset (list 0.0 0.0))
               (set-shader-value shader zoom-loc zoom +shader-uniform-float+)
               (set-shader-value shader offset-loc offset +shader-uniform-vec2+))

             (when (is-key-pressed +key-space+) (setf increment-speed 0)) ; Pause animation (c change)
             (when (is-key-pressed +key-f1+) (setf show-controls (not show-controls))) ; Toggle whether or not to show controls

             (cond ((is-key-pressed +key-right+) (incf increment-speed))
                   ((is-key-pressed +key-left+) (decf increment-speed)))

             ;; If either left or right button is pressed, zoom in/out
             (when (or (is-mouse-button-down +mouse-button-left+) (is-mouse-button-down +mouse-button-right+))
               ;; Change zoom. If Mouse left -> zoom in. Mouse right -> zoom out
               (setf zoom (* zoom (if (is-mouse-button-down +mouse-button-left+) *zoom-speed* (/ 1.0 *zoom-speed*))))

               (let* ((mouse-pos (get-mouse-position))
                      ;; Find the velocity at which to change the camera. Take the distance of the mouse
                      ;; from the center of the screen as the direction, and adjust magnitude based on the current zoom
                      (offset-velocity-x (/ (* (- (/ (vx mouse-pos) (float *screen-width*)) 0.5) *offset-speed-mul*) zoom))
                      (offset-velocity-y (/ (* (- (/ (vy mouse-pos) (float *screen-height*)) 0.5) *offset-speed-mul*) zoom)))

                 ;; Apply move velocity to camera
                 (setf offset (list (+ (first offset) (* (get-frame-time) offset-velocity-x))
                                    (+ (second offset) (* (get-frame-time) offset-velocity-y)))))

               ;; Update the shader uniform values!
               (set-shader-value shader zoom-loc zoom +shader-uniform-float+)
               (set-shader-value shader offset-loc offset +shader-uniform-vec2+))

             ;; Increment c value with time
             (let ((dc (* (get-frame-time) (float increment-speed) 0.0005)))
               (setf c (list (+ (first c) dc) (+ (second c) dc))))
             (set-shader-value shader c-loc c +shader-uniform-vec2+)
             ;;----------------------------------------------------------------------------------

             ;; Draw
             ;;----------------------------------------------------------------------------------
             ;; Using a render texture to draw Julia set
             (begin-texture-mode target)        ; Enable drawing to texture
             (clear-background +black+)         ; Clear the render texture

             ;; Draw a rectangle in shader mode to be used as shader canvas
             ;; NOTE: Rectangle uses font white character texture coordinates,
             ;; so shader can not be applied here directly because input vertexTexCoord
             ;; do not represent full screen coordinates (space where want to apply shader)
             (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +black+)
             (end-texture-mode)

             (begin-drawing)
             (clear-background +black+)         ; Clear screen background

             ;; Draw the saved texture and rendered julia set with shader
             ;; NOTE: We do not invert texture on Y, already considered inside shader
             (begin-shader-mode shader)
             ;; WARNING: If FLAG_WINDOW_HIGHDPI is enabled, HighDPI monitor scaling should be considered
             ;; when rendering the RenderTexture2D to fit in the HighDPI scaled Window
             (draw-texture-ex (render-texture-texture target) (vec2 0.0 0.0) 0.0 1.0 +white+)
             (end-shader-mode)

             (when show-controls
               (draw-text "Press Mouse buttons right/left to zoom in/out and move" 10 15 10 +raywhite+)
               (draw-text "Press KEY_F1 to toggle these controls" 10 30 10 +raywhite+)
               (draw-text "Press KEYS [1 - 6] to change point of interest" 10 45 10 +raywhite+)
               (draw-text "Press KEY_LEFT | KEY_RIGHT to change speed" 10 60 10 +raywhite+)
               (draw-text "Press KEY_SPACE to stop movement animation" 10 75 10 +raywhite+)
               (draw-text "Press KEY_R to recenter the camera" 10 90 10 +raywhite+))

             (end-drawing))
    ;;----------------------------------------------------------------------------------

    ;; De-Initialization
    ;;--------------------------------------------------------------------------------------
    (unload-shader shader)              ; Unload shader
    (unload-render-texture target)      ; Unload render texture

    (close-window)))                    ; Close window and OpenGL context

(main)

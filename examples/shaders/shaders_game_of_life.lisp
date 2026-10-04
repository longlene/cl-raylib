;;;; raylib [shaders] example - game of life
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Jordi Santonja (@JordSant) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Jordi Santonja (@JordSant)
;;;; Common Lisp port of raylib/examples/shaders/shaders_game_of_life.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-game-of-life
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/shaders-game-of-life)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Interaction mode
(defconstant +mode-run+ 0)
(defconstant +mode-pause+ 1)
(defconstant +mode-draw+ 2)

;; Struct to store example preset patterns
(defstruct preset-pattern
  (name "")
  (position (vec2 0.0 0.0)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - game of life")

    (let* ((menu-width 100)
           (window-width (- screen-width menu-width))
           (window-height screen-height)

           (world-width 2048)
           (world-height 2048)

           (random-tiles 8)             ; Random preset: divide the world to compute random points in each tile

           (world-rect-source (make-rectangle :x 0.0 :y 0.0 :width (float world-width) :height (float (- world-height))))
           (world-rect-dest (make-rectangle :x 0.0 :y 0.0 :width (float world-width) :height (float world-height)))
           (texture-on-screen (make-rectangle :x 0.0 :y 0.0 :width (float window-width) :height (float window-height)))

           (preset-patterns (vector (make-preset-pattern :name "Glider" :position (vec2 0.5 0.5))
                                    (make-preset-pattern :name "R-pentomino" :position (vec2 0.5 0.5))
                                    (make-preset-pattern :name "Acorn" :position (vec2 0.5 0.5))
                                    (make-preset-pattern :name "Spaceships" :position (vec2 0.1 0.5))
                                    (make-preset-pattern :name "Still lifes" :position (vec2 0.5 0.5))
                                    (make-preset-pattern :name "Oscillators" :position (vec2 0.5 0.5))
                                    (make-preset-pattern :name "Puffer train" :position (vec2 0.1 0.5))
                                    (make-preset-pattern :name "Glider Gun" :position (vec2 0.2 0.2))
                                    (make-preset-pattern :name "Breeder" :position (vec2 0.1 0.5))
                                    (make-preset-pattern :name "Random" :position (vec2 0.5 0.5))))
           (number-of-presets (length preset-patterns))

           (zoom 1)
           (offset-x (/ (- world-width window-width) 2.0)) ; Centered on window
           (offset-y (/ (- world-height window-height) 2.0)) ; Centered on window
           (frames-per-step 1)
           (frame 0)

           (preset -1)                  ; No button pressed for preset
           (mode +mode-run+)            ; Starting mode: running
           (button-zoom-in 0)           ; Button states: false not pressed
           (button-zom-out 0)
           (button-faster 0)
           (button-slower 0)

           ;; Load shader
           (shdr-game-of-life (load-shader nil (text-format "resources/shaders/glsl%i/game_of_life.fs" +glsl-version+)))

           ;; Set shader uniform size of the world
           (resolution-loc (get-shader-location shdr-game-of-life "resolution"))
           (resolution (list (float world-width) (float world-height)))

           ;; Define two textures: the current world and the previous world
           (world1 (load-render-texture world-width world-height))
           (world2 (load-render-texture world-width world-height))

           ;; Pointers to the two textures, to be swapped
           (current-world world2)
           (previous-world world1)

           ;; Image to be used in DRAW mode, to be changed with mouse input
           (image-to-draw nil)

           ;; NOTE: C static locals
           (previous-mouse-position (vec2 0.0 0.0))
           (first-color -1))

      (set-shader-value shdr-game-of-life resolution-loc resolution +shader-uniform-vec2+)

      (begin-texture-mode world2)
      (clear-background +raywhite+)
      (end-texture-mode)

      (let ((start-pattern (load-image "resources/game_of_life/r_pentomino.png")))
        (update-texture-rec (render-texture-texture world2) (make-rectangle :x (/ world-width 2.0) :y (/ world-height 2.0)
                                                                            :width (float (image-width start-pattern)) :height (float (image-height start-pattern)))
                            (image-data start-pattern))
        (unload-image start-pattern))

      (set-target-fps 60)               ; Set at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      (flet ((free-image-to-draw ()
               (when image-to-draw
                 (unload-image image-to-draw)
                 (setf image-to-draw nil))))

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (incf frame)

                 ;; Change zoom: both by buttons or by mouse wheel
                 (let ((mouse-wheel-move (get-mouse-wheel-move)))
                   (when (or (/= button-zoom-in 0) (and (/= button-zom-out 0) (> zoom 1)) (/= mouse-wheel-move 0.0))
                     (free-image-to-draw)       ; Zoom change: free the image to draw to be recreated again

                     (let ((center-x (+ offset-x (/ (/ window-width 2.0) zoom)))
                           (center-y (+ offset-y (/ (/ window-height 2.0) zoom))))
                       (when (or (/= button-zoom-in 0) (> mouse-wheel-move 0.0)) (setf zoom (* zoom 2)))
                       (when (and (or (/= button-zom-out 0) (< mouse-wheel-move 0.0)) (> zoom 1)) (setf zoom (truncate zoom 2)))
                       (setf offset-x (- center-x (/ (/ window-width 2.0) zoom))
                             offset-y (- center-y (/ (/ window-height 2.0) zoom))))))

                 ;; Change speed: number of frames per step
                 (when (and (/= button-faster 0) (> frames-per-step 1)) (decf frames-per-step))
                 (when (/= button-slower 0) (incf frames-per-step))

                 ;; Mouse management
                 (if (or (= mode +mode-run+) (= mode +mode-pause+))
                     (progn
                       (free-image-to-draw)     ; Free the image to draw: no longer needed in these modes

                       ;; Pan with mouse left button
                       (let ((mouse-position (get-mouse-position)))
                         (when (and (is-mouse-button-down +mouse-button-left+) (< (vx mouse-position) window-width))
                           (decf offset-x (/ (- (vx mouse-position) (vx previous-mouse-position)) zoom))
                           (decf offset-y (/ (- (vy mouse-position) (vy previous-mouse-position)) zoom)))
                         (setf previous-mouse-position mouse-position)))
                     ;; MODE_DRAW
                     (let* ((offset-decimal-x (- offset-x (ffloor offset-x)))
                            (offset-decimal-y (- offset-y (ffloor offset-y)))
                            (size-in-world-x (truncate (fceiling (/ (float (+ window-width (* offset-decimal-x zoom))) zoom))))
                            (size-in-world-y (truncate (fceiling (/ (float (+ window-height (* offset-decimal-y zoom))) zoom)))))
                       (when (>= (+ offset-x size-in-world-x) world-width) (setf size-in-world-x (- world-width (floor offset-x))))
                       (when (>= (+ offset-y size-in-world-y) world-height) (setf size-in-world-y (- world-height (floor offset-y))))

                       ;; Create image to draw if not created yet
                       (unless image-to-draw
                         (let ((world-on-screen (load-render-texture size-in-world-x size-in-world-y)))
                           (begin-texture-mode world-on-screen)
                           (draw-texture-pro (render-texture-texture current-world)
                                             (make-rectangle :x (ffloor offset-x) :y (ffloor offset-y) :width (float size-in-world-x) :height (- (float size-in-world-y)))
                                             (make-rectangle :x 0.0 :y 0.0 :width (float size-in-world-x) :height (float size-in-world-y))
                                             (vec2 0.0 0.0) 0.0 +white+)
                           (end-texture-mode)
                           (setf image-to-draw (load-image-from-texture (render-texture-texture world-on-screen)))
                           (unload-render-texture world-on-screen)))

                       (let ((mouse-position (get-mouse-position)))
                         (if (and (is-mouse-button-down +mouse-button-left+) (< (vx mouse-position) window-width))
                             (let ((mouse-x (truncate (truncate (+ (vx mouse-position) (* offset-decimal-x zoom))) zoom))
                                   (mouse-y (truncate (truncate (+ (vy mouse-position) (* offset-decimal-y zoom))) zoom)))
                               (when (>= mouse-x size-in-world-x) (setf mouse-x (1- size-in-world-x)))
                               (when (>= mouse-y size-in-world-y) (setf mouse-y (1- size-in-world-y)))
                               (when (= first-color -1) (setf first-color (if (< (first (get-image-color image-to-draw mouse-x mouse-y)) 5) 0 1)))
                               (let ((prev-color (if (< (first (get-image-color image-to-draw mouse-x mouse-y)) 5) 0 1)))
                                 (image-draw-pixel image-to-draw mouse-x mouse-y (if (/= first-color 0) +black+ +raywhite+))
                                 (when (/= prev-color first-color)
                                   (update-texture-rec (render-texture-texture current-world)
                                                       (make-rectangle :x (ffloor offset-x) :y (ffloor offset-y) :width (float size-in-world-x) :height (float size-in-world-y))
                                                       (image-data image-to-draw)))))
                             (setf first-color -1)))))

                 ;; Load selected preset
                 (when (>= preset 0)
                   (let ((pattern nil)
                         (preset-position (preset-pattern-position (aref preset-patterns preset))))
                     (if (< preset (1- number-of-presets)) ; Preset with pattern image lo load
                         (progn
                           (setf pattern (load-image (case preset
                                                       (0 "resources/game_of_life/glider.png")
                                                       (1 "resources/game_of_life/r_pentomino.png")
                                                       (2 "resources/game_of_life/acorn.png")
                                                       (3 "resources/game_of_life/spaceships.png")
                                                       (4 "resources/game_of_life/still_lifes.png")
                                                       (5 "resources/game_of_life/oscillators.png")
                                                       (6 "resources/game_of_life/puffer_train.png")
                                                       (7 "resources/game_of_life/glider_gun.png")
                                                       (8 "resources/game_of_life/breeder.png"))))

                           (begin-texture-mode current-world)
                           (clear-background +raywhite+)
                           (end-texture-mode)

                           (update-texture-rec (render-texture-texture current-world)
                                               (make-rectangle :x (- (* world-width (vx preset-position)) (/ (image-width pattern) 2.0))
                                                               :y (- (* world-height (vy preset-position)) (/ (image-height pattern) 2.0))
                                                               :width (float (image-width pattern)) :height (float (image-height pattern)))
                                               (image-data pattern)))
                         ;; Last preset: Random values
                         (progn
                           (setf pattern (gen-image-color (floor world-width random-tiles) (floor world-height random-tiles) +raywhite+))
                           (dotimes (i random-tiles)
                             (dotimes (j random-tiles)
                               (image-clear-background pattern +raywhite+)
                               (dotimes (x (image-width pattern))
                                 (dotimes (y (image-height pattern))
                                   (when (< (get-random-value 0 100) 15) (image-draw-pixel pattern x y +black+))))
                               (update-texture-rec (render-texture-texture current-world)
                                                   (make-rectangle :x (float (* (image-width pattern) i)) :y (float (* (image-height pattern) j))
                                                                   :width (float (image-width pattern)) :height (float (image-height pattern)))
                                                   (image-data pattern))))))

                     (unload-image pattern)

                     (setf mode +mode-pause+)
                     (setf offset-x (- (* world-width (vx preset-position)) (/ (/ (float window-width) zoom) 2.0))
                           offset-y (- (* world-height (vy preset-position)) (/ (/ (float window-height) zoom) 2.0)))))

                 ;; Check window draw inside world limits
                 (when (< offset-x 0) (setf offset-x 0.0))
                 (when (< offset-y 0) (setf offset-y 0.0))
                 (when (> offset-x (- world-width (/ (float window-width) zoom))) (setf offset-x (- world-width (/ (float window-width) zoom))))
                 (when (> offset-y (- world-height (/ (float window-height) zoom))) (setf offset-y (- world-height (/ (float window-height) zoom))))

                 ;; Rectangles for drawing texture portion to screen
                 (let ((texture-source-to-screen (make-rectangle :x offset-x :y offset-y :width (/ (float window-width) zoom) :height (/ (float window-height) zoom))))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw to texture
                   ;;----------------------------------------------------------------------------------
                   (when (and (= mode +mode-run+) (= (mod frame frames-per-step) 0))
                     ;; Swap worlds
                     (rotatef current-world previous-world)

                     ;; Draw to texture
                     (begin-texture-mode current-world)
                     (begin-shader-mode shdr-game-of-life)
                     (draw-texture-pro (render-texture-texture previous-world) world-rect-source world-rect-dest (vec2 0.0 0.0) 0.0 +raywhite+)
                     (end-shader-mode)
                     (end-texture-mode))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw to screen
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (draw-texture-pro (render-texture-texture current-world) texture-source-to-screen texture-on-screen (vec2 0.0 0.0) 0.0 +white+))

                 (draw-line window-width 0 window-width screen-height '(218 218 218 255))
                 (draw-rectangle window-width 0 (- screen-width window-width) screen-height '(232 232 232 255))

                 (draw-text "Conway's" 704 4 20 +darkblue+)
                 (draw-text " game of" 704 19 20 +darkblue+)
                 (draw-text "  life" 708 34 20 +darkblue+)
                 (draw-text "in raylib" 757 42 6 +black+)

                 (draw-text "Presets" 710 58 8 +gray+)
                 (setf preset -1)
                 (dotimes (i number-of-presets)
                   (when (/= (gui-button (make-rectangle :x 710.0 :y (+ 70.0 (* 18 i)) :width 80.0 :height 16.0) (preset-pattern-name (aref preset-patterns i))) 0)
                     (setf preset i)))

                 (setf mode (nth-value 1 (gui-toggle-group (make-rectangle :x 710.0 :y 258.0 :width 80.0 :height 16.0) (format nil "Run~%Pause~%Draw") mode)))

                 (draw-text (text-format "Zoom: %ix" zoom) 710 316 8 +gray+)
                 (setf button-zoom-in (gui-button (make-rectangle :x 710.0 :y 328.0 :width 80.0 :height 16.0) "Zoom in"))
                 (setf button-zom-out (gui-button (make-rectangle :x 710.0 :y 346.0 :width 80.0 :height 16.0) "Zoom out"))

                 (draw-text (text-format "Speed: %i frame%s" frames-per-step (if (> frames-per-step 1) "s" "")) 710 370 8 +gray+)
                 (setf button-faster (gui-button (make-rectangle :x 710.0 :y 382.0 :width 80.0 :height 16.0) "Faster"))
                 (setf button-slower (gui-button (make-rectangle :x 710.0 :y 400.0 :width 80.0 :height 16.0) "Slower"))

                 (draw-fps 712 426)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-shader shdr-game-of-life)
        (unload-render-texture world1)
        (unload-render-texture world2)
        (free-image-to-draw)

        (close-window)))))              ; Close window and OpenGL context

(main)

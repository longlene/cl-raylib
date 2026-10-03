;;;; raylib [shapes] example - bullet hell
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 5.6, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Zero (@zerohorsepower) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Zero (@zerohorsepower)
;;;; Common Lisp port of raylib/examples/shapes/shapes_bullet_hell.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-bullet-hell
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-bullet-hell)

(defconstant +max-bullets+ 500000)      ; Max bullets to be processed

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
(defstruct bullet
  (position (vec2 0.0 0.0))             ; Bullet position on screen
  (acceleration (vec2 0.0 0.0))         ; Amount of pixels to be incremented to position every frame
  (disabled nil)                        ; Skip processing and draw case out of screen
  (color +blank+))                      ; Bullet color

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - bullet hell")

    ;; Bullets definition
    ;; NOTE: Bullet structs are created the first time each slot is used
    (let* ((bullets (make-array +max-bullets+ :initial-element nil)) ; Bullets array
           (bullet-count 0)
           (bullet-disabled-count 0)    ; Used to calculate how many bullets are on screen
           (bullet-radius 10)
           (bullet-speed 3.0)
           (bullet-rows 6)
           (bullet-color (vector +red+ +blue+))

           ;; Spawner variables
           (base-direction 0.0)
           (angle-increment 5)          ; After spawn all bullet rows, increment this value on the baseDirection for next the frame
           (spawn-cooldown 2.0)
           (spawn-cooldown-timer spawn-cooldown)

           ;; Magic circle
           (magic-circle-rotation 0.0)

           ;; Used on performance drawing
           (bullet-texture (load-render-texture 24 24))
           (draw-in-performance-mode t)) ; Switch between DrawCircle() and DrawTexture()

      ;; Draw circle to bullet texture, then draw bullet using DrawTexture()
      ;; NOTE: This is done to improve the performance, since DrawCircle() is very slow
      (begin-texture-mode bullet-texture)
      (draw-circle 12 12 (float bullet-radius) +white+)
      (draw-circle-lines 12 12 (float bullet-radius) +black+)
      (end-texture-mode)

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Reset the bullet index
               ;; New bullets will replace the old ones that are already disabled due to out-of-screen
               (when (>= bullet-count +max-bullets+)
                 (setf bullet-count 0
                       bullet-disabled-count 0))

               (decf spawn-cooldown-timer)
               (when (< spawn-cooldown-timer 0)
                 (setf spawn-cooldown-timer spawn-cooldown)

                 ;; Spawn bullets
                 (let ((degrees-per-row (/ 360.0 bullet-rows)))
                   (dotimes (row bullet-rows)
                     (when (< bullet-count +max-bullets+)
                       (let ((bullet (or (aref bullets bullet-count)
                                         (setf (aref bullets bullet-count) (make-bullet))))
                             (bullet-direction (+ base-direction (* degrees-per-row row))))
                         (setf (bullet-position bullet) (vec2 (/ (float screen-width) 2) (/ (float screen-height) 2))
                               (bullet-disabled bullet) nil
                               (bullet-color bullet) (aref bullet-color (mod row 2)))

                         ;; Bullet speed*bullet direction, this will determine how much pixels will be incremented/decremented
                         ;; from the bullet position every frame. Since the bullets doesn't change its direction and speed,
                         ;; only need to calculate it at the spawning time
                         ;; 0 degrees = right, 90 degrees = down, 180 degrees = left and 270 degrees = up, basically clockwise
                         ;; Case you want it to be anti-clockwise, add "* -1" at the y acceleration
                         (setf (bullet-acceleration bullet)
                               (vec2 (* bullet-speed (cos (* bullet-direction +deg2rad+)))
                                     (* bullet-speed (sin (* bullet-direction +deg2rad+)))))

                         (incf bullet-count)))))

                 (incf base-direction angle-increment))

               ;; Update bullets position based on its acceleration
               (dotimes (i bullet-count)
                 (let ((bullet (aref bullets i)))
                   ;; Only update bullet if inside the screen
                   (unless (bullet-disabled bullet)
                     (let ((position (bullet-position bullet))
                           (acceleration (bullet-acceleration bullet)))
                       (incf (vx position) (vx acceleration))
                       (incf (vy position) (vy acceleration))

                       ;; Disable bullet if out of screen
                       (when (or (< (vx position) (* (- bullet-radius) 2))
                                 (> (vx position) (+ screen-width (* bullet-radius 2)))
                                 (< (vy position) (* (- bullet-radius) 2))
                                 (> (vy position) (+ screen-height (* bullet-radius 2))))
                         (setf (bullet-disabled bullet) t)
                         (incf bullet-disabled-count))))))

               ;; Input logic
               (when (and (or (is-key-pressed +key-right+) (is-key-pressed +key-d+)) (< bullet-rows 359)) (incf bullet-rows))
               (when (and (or (is-key-pressed +key-left+) (is-key-pressed +key-a+)) (> bullet-rows 1)) (decf bullet-rows))
               (when (or (is-key-pressed +key-up+) (is-key-pressed +key-w+)) (incf bullet-speed 0.25))
               (when (and (or (is-key-pressed +key-down+) (is-key-pressed +key-s+)) (> bullet-speed 0.50)) (decf bullet-speed 0.25))
               (when (and (is-key-pressed +key-z+) (> spawn-cooldown 1)) (decf spawn-cooldown))
               (when (is-key-pressed +key-x+) (incf spawn-cooldown))
               (when (is-key-pressed +key-enter+) (setf draw-in-performance-mode (not draw-in-performance-mode)))

               (when (is-key-down +key-space+)
                 (incf angle-increment 1)
                 (setf angle-increment (rem angle-increment 360)))

               (when (is-key-pressed +key-c+)
                 (setf bullet-count 0
                       bullet-disabled-count 0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)
               (clear-background +raywhite+)

               ;; Draw magic circle
               (incf magic-circle-rotation)
               (draw-rectangle-pro (make-rectangle :x (/ (float screen-width) 2) :y (/ (float screen-height) 2) :width 120.0 :height 120.0)
                                   (vec2 60.0 60.0) magic-circle-rotation +purple+)
               (draw-rectangle-pro (make-rectangle :x (/ (float screen-width) 2) :y (/ (float screen-height) 2) :width 120.0 :height 120.0)
                                   (vec2 60.0 60.0) (+ magic-circle-rotation 45) +purple+)
               (draw-circle-lines (truncate screen-width 2) (truncate screen-height 2) 70.0 +black+)
               (draw-circle-lines (truncate screen-width 2) (truncate screen-height 2) 50.0 +black+)
               (draw-circle-lines (truncate screen-width 2) (truncate screen-height 2) 30.0 +black+)

               ;; Draw bullets
               (if draw-in-performance-mode
                   ;; Draw bullets using pre-rendered texture containing circle
                   (let ((texture (render-texture-texture bullet-texture)))
                     (dotimes (i bullet-count)
                       (let ((bullet (aref bullets i)))
                         ;; Do not draw disabled bullets (out of screen)
                         (unless (bullet-disabled bullet)
                           (draw-texture texture
                                         (truncate (- (vx (bullet-position bullet)) (* (texture-width texture) 0.5)))
                                         (truncate (- (vy (bullet-position bullet)) (* (texture-height texture) 0.5)))
                                         (bullet-color bullet))))))
                   ;; Draw bullets using DrawCircle(), less performant
                   (dotimes (i bullet-count)
                     (let ((bullet (aref bullets i)))
                       ;; Do not draw disabled bullets (out of screen)
                       (unless (bullet-disabled bullet)
                         (draw-circle-v (bullet-position bullet) (float bullet-radius) (bullet-color bullet))
                         (draw-circle-lines-v (bullet-position bullet) (float bullet-radius) +black+)))))

               ;; Draw UI
               (draw-rectangle 10 10 280 150 (list 0 0 0 200))
               (draw-text "Controls:" 20 20 10 +lightgray+)
               (draw-text "- Right/Left or A/D: Change rows number" 40 40 10 +lightgray+)
               (draw-text "- Up/Down or W/S: Change bullet speed" 40 60 10 +lightgray+)
               (draw-text "- Z or X: Change spawn cooldown" 40 80 10 +lightgray+)
               (draw-text "- Space (Hold): Change the angle increment" 40 100 10 +lightgray+)
               (draw-text "- Enter: Switch draw method (Performance)" 40 120 10 +lightgray+)
               (draw-text "- C: Clear bullets" 40 140 10 +lightgray+)

               (draw-rectangle 610 10 170 30 (list 0 0 0 200))
               (if draw-in-performance-mode
                   (draw-text "Draw method: DrawTexture(*)" 620 20 10 +green+)
                   (draw-text "Draw method: DrawCircle(*)" 620 20 10 +red+))

               (draw-rectangle 135 410 530 30 (list 0 0 0 200))
               (draw-text (text-format "[ FPS: %d, Bullets: %d, Rows: %d, Bullet speed: %.2f, Angle increment per frame: %d, Cooldown: %.0f ]"
                                       (get-fps) (- bullet-count bullet-disabled-count) bullet-rows bullet-speed angle-increment spawn-cooldown)
                          155 420 10 +green+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture bullet-texture) ; Unload bullet texture

      (close-window))))                 ; Close window and OpenGL context

(main)

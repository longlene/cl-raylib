;;;; raylib [shaders] example - rounded rectangle
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Anstro Pleuton (@anstropleuton) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Anstro Pleuton (@anstropleuton)
;;;; Common Lisp port of raylib/examples/shaders/shaders_rounded_rectangle.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-rounded-rectangle
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-rounded-rectangle)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Types and Structures Definition
;;------------------------------------------------------------------------------------
;; Rounded rectangle data
(defstruct rounded-rectangle
  (corner-radius (vec4 0.0 0.0 0.0 0.0)) ; Individual corner radius (top-left, top-right, bottom-left, bottom-right)

  ;; Shadow variables
  (shadow-radius 0.0)
  (shadow-offset (vec2 0.0 0.0))
  (shadow-scale 0.0)

  ;; Border variables
  (border-thickness 0.0)                ; Inner-border thickness

  ;; Shader locations
  (rectangle-loc 0)
  (radius-loc 0)
  (color-loc 0)
  (shadow-radius-loc 0)
  (shadow-offset-loc 0)
  (shadow-scale-loc 0)
  (shadow-color-loc 0)
  (border-thickness-loc 0)
  (border-color-loc 0))

;;------------------------------------------------------------------------------------
;; Module Functions Definitions
;;------------------------------------------------------------------------------------
;; Update rounded rectangle uniforms
(defun update-rounded-rectangle (rec shader)
  (let ((r (rounded-rectangle-corner-radius rec))
        (o (rounded-rectangle-shadow-offset rec)))
    (set-shader-value shader (rounded-rectangle-radius-loc rec) (list (vx r) (vy r) (vz r) (vw r)) +shader-uniform-vec4+)
    (set-shader-value shader (rounded-rectangle-shadow-radius-loc rec) (rounded-rectangle-shadow-radius rec) +shader-uniform-float+)
    (set-shader-value shader (rounded-rectangle-shadow-offset-loc rec) (list (vx o) (vy o)) +shader-uniform-vec2+)
    (set-shader-value shader (rounded-rectangle-shadow-scale-loc rec) (rounded-rectangle-shadow-scale rec) +shader-uniform-float+)
    (set-shader-value shader (rounded-rectangle-border-thickness-loc rec) (rounded-rectangle-border-thickness rec) +shader-uniform-float+)))

;; Create a rounded rectangle and set uniform locations
(defun create-rounded-rectangle (corner-radius shadow-radius shadow-offset shadow-scale border-thickness shader)
  (let ((rec (make-rounded-rectangle :corner-radius corner-radius
                                     :shadow-radius shadow-radius
                                     :shadow-offset shadow-offset
                                     :shadow-scale shadow-scale
                                     :border-thickness border-thickness)))

    ;; Get shader uniform locations
    (setf (rounded-rectangle-rectangle-loc rec) (get-shader-location shader "rectangle")
          (rounded-rectangle-radius-loc rec) (get-shader-location shader "radius")
          (rounded-rectangle-color-loc rec) (get-shader-location shader "color")
          (rounded-rectangle-shadow-radius-loc rec) (get-shader-location shader "shadowRadius")
          (rounded-rectangle-shadow-offset-loc rec) (get-shader-location shader "shadowOffset")
          (rounded-rectangle-shadow-scale-loc rec) (get-shader-location shader "shadowScale")
          (rounded-rectangle-shadow-color-loc rec) (get-shader-location shader "shadowColor")
          (rounded-rectangle-border-thickness-loc rec) (get-shader-location shader "borderThickness")
          (rounded-rectangle-border-color-loc rec) (get-shader-location shader "borderColor"))

    (update-rounded-rectangle rec shader)

    rec))

(defun normalized-color (color)
  (list (/ (first color) 255.0) (/ (second color) 255.0) (/ (third color) 255.0) (/ (fourth color) 255.0)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - rounded rectangle")

    (let* (;; Load the shader
           (shader (load-shader (text-format "resources/shaders/glsl%i/base.vs" +glsl-version+)
                                (text-format "resources/shaders/glsl%i/rounded_rectangle.fs" +glsl-version+)))

           ;; Create a rounded rectangle
           (rounded-rectangle (create-rounded-rectangle
                               (vec4 5.0 10.0 15.0 20.0) ; Corner radius
                               20.0                      ; Shadow radius
                               (vec2 0.0 -5.0)           ; Shadow offset
                               0.95                      ; Shadow scale
                               5.0                       ; Border thickness
                               shader))                  ; Shader

           (rectangle-color +blue+)
           (shadow-color +darkblue+)
           (border-color +skyblue+)
           (none '(0.0 0.0 0.0 0.0)))

      ;; Update shader uniforms
      (update-rounded-rectangle rounded-rectangle shader)

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      (flet ((draw-rounded (x y width height frame title color shadow border)
               (let ((rec (make-rectangle :x x :y y :width width :height height)))
                 (draw-rectangle-lines (- (truncate (rectangle-x rec)) frame) (- (truncate (rectangle-y rec)) frame)
                                       (+ (truncate (rectangle-width rec)) (* 2 frame)) (+ (truncate (rectangle-height rec)) (* 2 frame)) +darkgray+)
                 (draw-text title (- (truncate (rectangle-x rec)) frame) (- (truncate (rectangle-y rec)) frame 15) 10 +darkgray+)

                 ;; Flip Y axis to match shader coordinate system
                 (setf (rectangle-y rec) (- screen-height (rectangle-y rec) (rectangle-height rec)))
                 (set-shader-value shader (rounded-rectangle-rectangle-loc rounded-rectangle)
                                   (list (rectangle-x rec) (rectangle-y rec) (rectangle-width rec) (rectangle-height rec)) +shader-uniform-vec4+)

                 (set-shader-value shader (rounded-rectangle-color-loc rounded-rectangle) color +shader-uniform-vec4+)
                 (set-shader-value shader (rounded-rectangle-shadow-color-loc rounded-rectangle) shadow +shader-uniform-vec4+)
                 (set-shader-value shader (rounded-rectangle-border-color-loc rounded-rectangle) border +shader-uniform-vec4+)

                 (begin-shader-mode shader)
                 (draw-rectangle 0 0 screen-width screen-height +white+)
                 (end-shader-mode))))

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 ;; Draw rectangle box with rounded corners using shader (only rectangle color)
                 (draw-rounded 50.0 70.0 110.0 60.0 20 "Rounded rectangle"
                               (normalized-color rectangle-color) none none)

                 ;; Draw rectangle shadow using shader (only shadow color)
                 (draw-rounded 50.0 200.0 110.0 60.0 20 "Rounded rectangle shadow"
                               none (normalized-color shadow-color) none)

                 ;; Draw rectangle's border using shader (only border color)
                 (draw-rounded 50.0 330.0 110.0 60.0 20 "Rounded rectangle border"
                               none none (normalized-color border-color))

                 ;; Draw one more rectangle with all three colors
                 (draw-rounded 240.0 80.0 500.0 300.0 30 "Rectangle with all three combined"
                               (normalized-color rectangle-color) (normalized-color shadow-color) (normalized-color border-color))

                 (draw-text "(c) Rounded rectangle SDF by Iñigo Quilez. MIT License." (- screen-width 300) (- screen-height 20) 10 +black+)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader

      (close-window))))                 ; Close window and OpenGL context

(main)

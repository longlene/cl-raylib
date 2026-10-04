;;;; raylib [shapes] example - outlines testbed
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Common Lisp port of raylib/examples/shapes/shapes_outlines_testbed.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-outlines-testbed
  (:use #:cl #:raylib #:raygui))        ; raygui: Required for GUI controls
(in-package #:raylib-examples/shapes-outlines-testbed)

(defparameter *color-filled* +darkblue+)
(defparameter *color-outline* +yellow+)

(defconstant +shape-spacing+ 14.0)
(defconstant +shape-size+ 46.0)
(defconstant +box-spacing+ 10.0)

(defconstant +mouse-camera-zoom-speed+ 0.3)
(defconstant +keyboard-camera-move-speed+ 10.0)
(defconstant +keyboard-camera-zoom-speed+ 0.1)

(defconstant +camera-zoom-min+ 0.1)
(defconstant +camera-zoom-max+ 1000.0)

;; The shapes are ordered according to their order in this enum, left to right
(defconstant +order-rectangle+ 0)
(defconstant +order-rectangle-rounded+ 1)
(defconstant +order-circle+ 2)
(defconstant +order-ellipse+ 3)
(defconstant +order-circle-sector+ 4)
(defconstant +order-ring+ 5)
(defconstant +order-triangle+ 6)
(defconstant +order-polygon+ 7)
(defconstant +count-shapes+ 8)

;; The line style groups are ordered according to their order in this enum, top to bottom
(defconstant +order-lines+ 0)
(defconstant +order-lines-ex-world+ 1)
(defconstant +order-lines-ex-screen+ 2)

(defconstant +box-width+ (+ (* +shape-size+ +count-shapes+) (* +shape-spacing+ (- +count-shapes+ 1)) (* +box-spacing+ 2.0)))
(defconstant +box-height+ (+ +shape-size+ (* +box-spacing+ 2.0)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - outlines testbed")

    (let* ((camera (make-camera2d :zoom 1.0))

           ;; User configurable options while running the program.
           (line-opacity 130.0)
           (line-thickness 4.0)
           (rectangle-roundness 0.4)
           (rectangle-segments 9.0)
           (ellipse-radius-y 0.5)
           (circle-start-angle 20.0)
           (circle-end-angle 270.0)
           (circle-segments 36.0)
           (ring-inner-radius-scale 0.3)
           (polygon-sides 6.0)

           (disable-mouse-control nil)

           (options-background (make-rectangle :x 510.0 :y 0.0 :width 290.0 :height 450.0))

           (zoom-point (vec2 (/ (- (float screen-width) (rectangle-width options-background)) 2.0) (/ (float screen-height) 2.0))))

      (gui-set-style +label+ +text-color-normal+ (color-to-int +white+))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      (flet ((zoom-camera (zoom)
               ;; Change the zoom keeping the world point under ZOOM-POINT fixed
               (let ((prev-world-zoom-point (get-screen-to-world-2d zoom-point camera)))
                 (setf (camera2d-zoom camera) zoom)
                 (let ((world-zoom-point (get-screen-to-world-2d zoom-point camera)))
                   (setf (camera2d-target camera) (vector2-add (camera2d-target camera) (vector2-subtract prev-world-zoom-point world-zoom-point)))))))

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (let ((mouse-pos-screen (get-mouse-position))
                       (mouse-wheel-vec (get-mouse-wheel-move-v)))

                   (when (and (is-mouse-button-pressed +mouse-button-left+) (check-collision-point-rec mouse-pos-screen options-background))
                     (setf disable-mouse-control t))

                   (when (is-mouse-button-released +mouse-button-left+) (setf disable-mouse-control nil))

                   (unless disable-mouse-control
                     (when (/= (vy mouse-wheel-vec) 0.0)
                       (zoom-camera (clamp (* (camera2d-zoom camera) (expt 2.0 (* +mouse-camera-zoom-speed+ (vy mouse-wheel-vec)))) ; Constant zoom rate
                                           +camera-zoom-min+ +camera-zoom-max+)))

                     (when (is-mouse-button-down +mouse-button-left+)
                       (let ((mouse-delta (get-mouse-delta)))
                         (decf (vx (camera2d-target camera)) (/ (vx mouse-delta) (camera2d-zoom camera)))
                         (decf (vy (camera2d-target camera)) (/ (vy mouse-delta) (camera2d-zoom camera))))

                       (let* ((mouse-max-x (truncate (rectangle-x options-background)))
                              (mouse-max-y screen-height)
                              (new-x (truncate (vx mouse-pos-screen)))
                              (new-y (truncate (vy mouse-pos-screen))))

                         ;; In C, the '%' operator computes the remainder, we want the modulus
                         (setf new-x (rem (+ (rem new-x mouse-max-x) mouse-max-x) mouse-max-x)
                               new-y (rem (+ (rem new-y mouse-max-y) mouse-max-y) mouse-max-y))

                         (when (or (/= new-x (truncate (vx mouse-pos-screen))) (/= new-y (truncate (vy mouse-pos-screen))))
                           (set-mouse-position new-x new-y))))))

                 (when (is-key-down +key-a+) (decf (vx (camera2d-target camera)) (/ +keyboard-camera-move-speed+ (camera2d-zoom camera))))
                 (when (is-key-down +key-d+) (incf (vx (camera2d-target camera)) (/ +keyboard-camera-move-speed+ (camera2d-zoom camera))))
                 (when (is-key-down +key-w+) (decf (vy (camera2d-target camera)) (/ +keyboard-camera-move-speed+ (camera2d-zoom camera))))
                 (when (is-key-down +key-s+) (incf (vy (camera2d-target camera)) (/ +keyboard-camera-move-speed+ (camera2d-zoom camera))))

                 (when (is-key-down +key-up+)
                   (zoom-camera (clamp (* (camera2d-zoom camera) (expt 2.0 +keyboard-camera-zoom-speed+)) +camera-zoom-min+ +camera-zoom-max+)))
                 (when (is-key-down +key-down+)
                   (zoom-camera (clamp (* (camera2d-zoom camera) (expt 2.0 (- +keyboard-camera-zoom-speed+))) +camera-zoom-min+ +camera-zoom-max+)))

                 (when (is-key-pressed +key-z+) (zoom-camera 1.0))

                 (when (is-key-pressed +key-c+) (setf (camera2d-target camera) (vec2 0.0 0.0)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background '(50 50 55 255))

                 (let ((color-outline (list (first *color-outline*) (second *color-outline*) (third *color-outline*)
                                            (truncate line-opacity))))

                   (begin-mode-2d camera)

                   (let* ((shape-offset (* +box-spacing+ 2.0))
                          (shape-padding-x (+ +shape-size+ +shape-spacing+))
                          (shape-padding-y (+ +shape-size+ +shape-spacing+ (* +box-spacing+ 2.0)))

                          (radius-x (/ +shape-size+ 2.0))
                          (radius-y (* radius-x ellipse-radius-y))

                          (ring-outer-radius (/ +shape-size+ 2.0))
                          (ring-inner-radius (* ring-outer-radius ring-inner-radius-scale))

                          (triangle-vertex0-offset (vec2 0.0 (* +shape-size+ 0.8)))
                          (triangle-vertex1-offset (vec2 (* +shape-size+ 0.6) +shape-size+))
                          (triangle-vertex2-offset (vec2 +shape-size+ 0.0))
                          (pos-x 0.0)
                          (pos-y 0.0))
                     (labels ((shape-x (order) (setf pos-x (+ shape-offset (* shape-padding-x order))))
                              (rec () (make-rectangle :x pos-x :y pos-y :width +shape-size+ :height +shape-size+))
                              (center () (vec2 (+ pos-x (/ +shape-size+ 2.0)) (+ pos-y (/ +shape-size+ 2.0))))
                              (tri (offset) (vec2 (+ pos-x (vx offset)) (+ pos-y (vy offset))))
                              (group-box (text)
                                ;; Group the shapes
                                (gui-group-box (make-rectangle :x (- shape-offset +box-spacing+) :y (- pos-y +box-spacing+) :width +box-width+ :height +box-height+) text))
                              (shapes-lines-ex (thickness)
                                ;; Rectangle
                                (shape-x +order-rectangle+)
                                (draw-rectangle-rec (rec) *color-filled*)
                                (draw-rectangle-lines-ex (rec) thickness color-outline)

                                ;; Rectangle Rounded
                                (shape-x +order-rectangle-rounded+)
                                (draw-rectangle-rounded (rec) rectangle-roundness (truncate rectangle-segments) *color-filled*)
                                (draw-rectangle-rounded-lines-ex (rec) rectangle-roundness (truncate rectangle-segments) thickness color-outline)

                                ;; Circle
                                (shape-x +order-circle+)
                                (draw-circle-v (center) radius-x *color-filled*)
                                (draw-circle-lines-ex (center) radius-x thickness color-outline)

                                ;; Ellipse
                                (shape-x +order-ellipse+)
                                (draw-ellipse-v (center) radius-x radius-y *color-filled*)
                                (draw-ellipse-lines-ex (center) radius-x radius-y thickness color-outline)

                                ;; Circle Sector
                                (shape-x +order-circle-sector+)
                                (draw-circle-sector (center) radius-x circle-start-angle circle-end-angle (truncate circle-segments) *color-filled*)
                                (draw-circle-sector-lines-ex (center) radius-x circle-start-angle circle-end-angle (truncate circle-segments) thickness color-outline)

                                ;; Ring
                                (shape-x +order-ring+)
                                (draw-ring (center) ring-inner-radius ring-outer-radius circle-start-angle circle-end-angle (truncate circle-segments) *color-filled*)
                                (draw-ring-lines-ex (center) ring-inner-radius ring-outer-radius circle-start-angle circle-end-angle (truncate circle-segments) thickness color-outline)

                                ;; Triangle
                                (shape-x +order-triangle+)
                                (draw-triangle (tri triangle-vertex0-offset) (tri triangle-vertex1-offset) (tri triangle-vertex2-offset) *color-filled*)
                                (draw-triangle-lines-ex (tri triangle-vertex0-offset) (tri triangle-vertex1-offset) (tri triangle-vertex2-offset) thickness color-outline)

                                ;; Polygon
                                (shape-x +order-polygon+)
                                (draw-poly (center) (truncate polygon-sides) radius-x 0.0 *color-filled*)
                                (draw-poly-lines-ex (center) (truncate polygon-sides) radius-x 0.0 thickness color-outline)))

                       ;; ----------------------------------------
                       ;; Draw*Lines()
                       (setf pos-y (+ shape-offset (* shape-padding-y +order-lines+)))

                       ;; Rectangle
                       (shape-x +order-rectangle+)
                       (draw-rectangle-rec (rec) *color-filled*)
                       (draw-rectangle-lines (truncate pos-x) (truncate pos-y) (truncate +shape-size+) (truncate +shape-size+) color-outline)

                       ;; Rectangle Rounded
                       (shape-x +order-rectangle-rounded+)
                       (draw-rectangle-rounded (rec) rectangle-roundness (truncate rectangle-segments) *color-filled*)
                       (draw-rectangle-rounded-lines (rec) rectangle-roundness (truncate rectangle-segments) color-outline)

                       ;; Circle
                       (shape-x +order-circle+)
                       (draw-circle-v (center) radius-x *color-filled*)
                       (draw-circle-lines-v (center) radius-x color-outline)

                       ;; Ellipse
                       (shape-x +order-ellipse+)
                       (draw-ellipse-v (center) radius-x radius-y *color-filled*)
                       (draw-ellipse-lines-v (center) radius-x radius-y color-outline)

                       ;; Circle Sector
                       (shape-x +order-circle-sector+)
                       (draw-circle-sector (center) radius-x circle-start-angle circle-end-angle (truncate circle-segments) *color-filled*)
                       (draw-circle-sector-lines (center) radius-x circle-start-angle circle-end-angle (truncate circle-segments) color-outline)

                       ;; Ring
                       (shape-x +order-ring+)
                       (draw-ring (center) ring-inner-radius ring-outer-radius circle-start-angle circle-end-angle (truncate circle-segments) *color-filled*)
                       (draw-ring-lines (center) ring-inner-radius ring-outer-radius circle-start-angle circle-end-angle (truncate circle-segments) color-outline)

                       ;; Triangle
                       (shape-x +order-triangle+)
                       (draw-triangle (tri triangle-vertex0-offset) (tri triangle-vertex1-offset) (tri triangle-vertex2-offset) *color-filled*)
                       (draw-triangle-lines (tri triangle-vertex0-offset) (tri triangle-vertex1-offset) (tri triangle-vertex2-offset) color-outline)

                       ;; Polygon
                       (shape-x +order-polygon+)
                       (draw-poly (center) (truncate polygon-sides) radius-x 0.0 *color-filled*)
                       (draw-poly-lines (center) (truncate polygon-sides) radius-x 0.0 color-outline)

                       (group-box "Draw*Lines()")
                       ;; ----------------------------------------

                       ;; ----------------------------------------
                       ;; Draw*LinesEx() with world space pixel thickness
                       (setf pos-y (+ shape-offset (* shape-padding-y +order-lines-ex-world+)))
                       (shapes-lines-ex line-thickness)
                       (group-box "Draw*LinesEx() with *world space* pixel thickness")
                       ;; ----------------------------------------

                       ;; ----------------------------------------
                       ;; Draw*LinesEx() with screen space pixel thickness
                       (setf pos-y (+ shape-offset (* shape-padding-y +order-lines-ex-screen+)))
                       (shapes-lines-ex (/ line-thickness (camera2d-zoom camera))) ; constantThickness
                       (group-box "Draw*LinesEx() with *screen space* pixel thickness")))
                   ;; ----------------------------------------

                   (end-mode-2d))

                 (draw-rectangle-rec options-background (fade +darkgray+ 0.75))

                 (draw-rectangle-rec (make-rectangle :x (rectangle-x options-background) :y 360.0 :width (rectangle-width options-background) :height 90.0) (fade +black+ 0.4))
                 (draw-line-ex (vec2 (rectangle-x options-background) 360.0) (vec2 (+ (rectangle-x options-background) (rectangle-width options-background)) 360.0) 1.0 +black+)

                 (draw-text "Move with the mouse or WASD keys" (+ (truncate (rectangle-x options-background)) 10) 370 10 +orange+)
                 (draw-text "Zoom with the mouse or UP and DOWN keys" (+ (truncate (rectangle-x options-background)) 10) 390 10 +orange+)
                 (draw-text "Press C to reset position" (+ (truncate (rectangle-x options-background)) 10) 410 10 +orange+)
                 (draw-text "Press Z to reset zoom" (+ (truncate (rectangle-x options-background)) 10) 430 10 +orange+)

                 (draw-line-ex (vec2 (rectangle-x options-background) (rectangle-y options-background))
                               (vec2 (rectangle-x options-background) (+ (rectangle-y options-background) (rectangle-height options-background))) 1.0 +black+)

                 (macrolet ((slider (y text fmt value-text place min max)
                              `(setf ,place (nth-value 1 (gui-slider-bar (make-rectangle :x 605.0 :y ,y :width 150.0 :height 25.0) ,text (text-format ,fmt ,value-text) ,place ,min ,max)))))
                   (slider 10.0 "Line Opacity" "%d" (truncate line-opacity) line-opacity 0 255)
                   (slider 45.0 "Thickness" "%.2f" line-thickness line-thickness -20.0 40.0)
                   (slider 80.0 "Rect Roundness" "%.2f" rectangle-roundness rectangle-roundness -1.0 2.0)
                   (slider 115.0 "Rect Segments" "%d" (truncate rectangle-segments) rectangle-segments -1 100)
                   (slider 150.0 "Ellipse Radius Y" "%.2f" ellipse-radius-y ellipse-radius-y 0.0 1.25)
                   (slider 185.0 "Start Angle" "%.2f" circle-start-angle circle-start-angle -360.0 360.0)
                   (slider 220.0 "End Angle" "%.2f" circle-end-angle circle-end-angle -360.0 360.0)
                   (slider 255.0 "Circle Segments" "%d" (truncate circle-segments) circle-segments -1 100)
                   (slider 290.0 "Ring Radius" "%.2f" ring-inner-radius-scale ring-inner-radius-scale -1.0 2.0)
                   (slider 325.0 "Poly Sides" "%d" (truncate polygon-sides) polygon-sides -1 100))

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)

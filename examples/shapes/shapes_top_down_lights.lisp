;;;; raylib [shapes] example - top down lights
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 4.2, last time updated with raylib 4.2
;;;;
;;;; Example contributed by Jeffery Myers (@JeffM2501) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2022-2025 Jeffery Myers (@JeffM2501)
;;;; Common Lisp port of raylib/examples/shapes/shapes_top_down_lights.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-top-down-lights
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-top-down-lights)

;; Custom Blend Modes
(defconstant +rlgl-src-alpha+ #x0302)
(defconstant +rlgl-min+ #x8007)
(defconstant +rlgl-max+ #x8008)

(defconstant +max-boxes+ 20)
(defconstant +max-shadows+ (* +max-boxes+ 3)) ; MAX_BOXES*3 - Each box can cast up to two shadow volumes for the edges it is away from, and one for the box itself
(defconstant +max-lights+ 16)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Shadow geometry type: vector of 4 Vector2 vertices

;; Light info type
(defstruct light-info
  (active nil)                          ; Is this light slot active?
  (dirty nil)                           ; Does this light need to be updated?
  (valid nil)                           ; Is this light in a valid position?

  (position (vec2 0.0 0.0))             ; Light position
  (mask nil)                            ; Alpha mask for the light
  (outer-radius 0.0)                    ; The distance the light touches
  (bounds (make-rectangle))             ; A cached rectangle of the light bounds to help with culling

  (shadows (let ((a (make-array +max-shadows+)))
             (dotimes (i +max-shadows+ a) (setf (aref a i) (vector (vec2 0.0 0.0) (vec2 0.0 0.0) (vec2 0.0 0.0) (vec2 0.0 0.0))))))
  (shadow-count 0))

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
(defparameter *lights* (let ((a (make-array +max-lights+)))
                         (dotimes (i +max-lights+ a) (setf (aref a i) (make-light-info)))))

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Move a light and mark it as dirty so that we update it's mask next frame
(defun move-light (slot x y)
  (let ((light (aref *lights* slot)))
    (setf (light-info-dirty light) t)
    (setf (vx (light-info-position light)) x
          (vy (light-info-position light)) y)

    ;; update the cached bounds
    (setf (rectangle-x (light-info-bounds light)) (- x (light-info-outer-radius light))
          (rectangle-y (light-info-bounds light)) (- y (light-info-outer-radius light)))))

;; Compute a shadow volume for the edge
;; It takes the edge and projects it back by the light radius and turns it into a quad
(defun compute-shadow-volume-for-edge (slot sp ep)
  (let ((light (aref *lights* slot)))
    (when (>= (light-info-shadow-count light) +max-shadows+) (return-from compute-shadow-volume-for-edge))

    (let* ((extension (* (light-info-outer-radius light) 2))

           (sp-vector (vector2-normalize (vector2-subtract sp (light-info-position light))))
           (sp-projection (vector2-add sp (vector2-scale sp-vector extension)))

           (ep-vector (vector2-normalize (vector2-subtract ep (light-info-position light))))
           (ep-projection (vector2-add ep (vector2-scale ep-vector extension)))

           (vertices (aref (light-info-shadows light) (light-info-shadow-count light))))

      (setf (aref vertices 0) (vcopy sp)
            (aref vertices 1) (vcopy ep)
            (aref vertices 2) ep-projection
            (aref vertices 3) sp-projection)

      (incf (light-info-shadow-count light)))))

;; Draw the light and shadows to the mask for a light
(defun draw-light-mask (slot)
  (let ((light (aref *lights* slot)))
    ;; Use the light mask
    (begin-texture-mode (light-info-mask light))

    (clear-background +white+)

    ;; Force the blend mode to only set the alpha of the destination
    (rl-set-blend-factors +rlgl-src-alpha+ +rlgl-src-alpha+ +rlgl-min+)
    (rl-set-blend-mode +blend-custom+)

    ;; If we are valid, then draw the light radius to the alpha mask
    (when (light-info-valid light)
      (draw-circle-gradient (light-info-position light) (light-info-outer-radius light) (color-alpha +white+ 0.0) +white+))

    (rl-draw-render-batch-active)

    ;; Cut out the shadows from the light radius by forcing the alpha to maximum
    (rl-set-blend-mode +blend-alpha+)
    (rl-set-blend-factors +rlgl-src-alpha+ +rlgl-src-alpha+ +rlgl-max+)
    (rl-set-blend-mode +blend-custom+)

    ;; Draw the shadows to the alpha mask
    (dotimes (i (light-info-shadow-count light))
      (draw-triangle-fan (aref (light-info-shadows light) i) 4 +white+))

    (rl-draw-render-batch-active)

    ;; Go back to normal blend mode
    (rl-set-blend-mode +blend-alpha+)

    (end-texture-mode)))

;; Setup a light
(defun setup-light (slot x y radius)
  (let ((light (aref *lights* slot)))
    (setf (light-info-active light) t
          (light-info-valid light) nil  ; The light must prove it is valid
          (light-info-mask light) (load-render-texture (get-screen-width) (get-screen-height))
          (light-info-outer-radius light) radius)

    (setf (rectangle-width (light-info-bounds light)) (* radius 2)
          (rectangle-height (light-info-bounds light)) (* radius 2))

    (move-light slot x y)

    ;; Force the render texture to have something in it
    (draw-light-mask slot)))

;; See if a light needs to update it's mask
(defun update-light (slot boxes count)
  (let ((light (aref *lights* slot)))
    (when (or (not (light-info-active light)) (not (light-info-dirty light))) (return-from update-light nil))

    (setf (light-info-dirty light) nil
          (light-info-shadow-count light) 0
          (light-info-valid light) nil)

    (dotimes (i count)
      (let ((box (aref boxes i))
            (position (light-info-position light)))
        ;; Are we in a box? if so we are not valid
        (when (check-collision-point-rec position box) (return-from update-light nil))

        ;; If this box is outside our bounds, we can skip it
        (when (check-collision-recs (light-info-bounds light) box)
          ;; Check the edges that are on the same side we are, and cast shadow volumes out from them

          ;; Top
          (let ((sp (vec2 (rectangle-x box) (rectangle-y box)))
                (ep (vec2 (+ (rectangle-x box) (rectangle-width box)) (rectangle-y box))))

            (when (> (vy position) (vy ep)) (compute-shadow-volume-for-edge slot sp ep))

            ;; Right
            (setf sp (vcopy ep))
            (incf (vy ep) (rectangle-height box))
            (when (< (vx position) (vx ep)) (compute-shadow-volume-for-edge slot sp ep))

            ;; Bottom
            (setf sp (vcopy ep))
            (decf (vx ep) (rectangle-width box))
            (when (< (vy position) (vy ep)) (compute-shadow-volume-for-edge slot sp ep))

            ;; Left
            (setf sp (vcopy ep))
            (decf (vy ep) (rectangle-height box))
            (when (> (vx position) (vx ep)) (compute-shadow-volume-for-edge slot sp ep))

            ;; The box itself
            ;; NOTE: C writes past the shadows array when it is already full, the box is skipped then
            (when (< (light-info-shadow-count light) +max-shadows+)
              (let ((vertices (aref (light-info-shadows light) (light-info-shadow-count light))))
                (setf (aref vertices 0) (vec2 (rectangle-x box) (rectangle-y box))
                      (aref vertices 1) (vec2 (rectangle-x box) (+ (rectangle-y box) (rectangle-height box)))
                      (aref vertices 2) (vec2 (+ (rectangle-x box) (rectangle-width box)) (+ (rectangle-y box) (rectangle-height box)))
                      (aref vertices 3) (vec2 (+ (rectangle-x box) (rectangle-width box)) (rectangle-y box))))
              (incf (light-info-shadow-count light)))))))

    (setf (light-info-valid light) t)

    (draw-light-mask slot)

    t))

;; Set up some boxes, returns the boxes count
(defun setup-boxes (boxes)
  (setf (aref boxes 0) (make-rectangle :x 150.0 :y 80.0 :width 40.0 :height 40.0)
        (aref boxes 1) (make-rectangle :x 1200.0 :y 700.0 :width 40.0 :height 40.0)
        (aref boxes 2) (make-rectangle :x 200.0 :y 600.0 :width 40.0 :height 40.0)
        (aref boxes 3) (make-rectangle :x 1000.0 :y 50.0 :width 40.0 :height 40.0)
        (aref boxes 4) (make-rectangle :x 500.0 :y 350.0 :width 40.0 :height 40.0))

  (loop for i from 5 below +max-boxes+
        do (setf (aref boxes i) (make-rectangle :x (float (get-random-value 0 (get-screen-width)))
                                                :y (float (get-random-value 0 (get-screen-height)))
                                                :width (float (get-random-value 10 100))
                                                :height (float (get-random-value 10 100)))))

  +max-boxes+)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - top down lights")

    ;; Initialize our 'world' of boxes
    (let* ((boxes (make-array +max-boxes+ :initial-element nil))
           (box-count (setup-boxes boxes))

           ;; Create a checkerboard ground texture
           (img (gen-image-checked 64 64 32 32 +darkbrown+ +darkgray+))
           (background-texture (load-texture-from-image img))

           ;; Create a global light mask to hold all the blended lights
           (light-mask (progn (unload-image img)
                              (load-render-texture (get-screen-width) (get-screen-height))))

           (next-light 1)
           (show-lines nil))

      ;; Setup initial light
      (setup-light 0 600.0 400.0 300.0)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Drag light 0
               (when (is-mouse-button-down +mouse-button-left+) (move-light 0 (vx (get-mouse-position)) (vy (get-mouse-position))))

               ;; Make a new light
               (when (and (is-mouse-button-pressed +mouse-button-right+) (< next-light +max-lights+))
                 (setup-light next-light (vx (get-mouse-position)) (vy (get-mouse-position)) 200.0)
                 (incf next-light))

               ;; Toggle debug info
               (when (is-key-pressed +key-f1+) (setf show-lines (not show-lines)))

               ;; Update the lights and keep track if any were dirty so we know if we need to update the master light mask
               (let ((dirty-lights nil))
                 (dotimes (i +max-lights+)
                   (when (update-light i boxes box-count) (setf dirty-lights t)))

                 ;; Update the light mask
                 (when dirty-lights
                   ;; Build up the light mask
                   (begin-texture-mode light-mask)

                   (clear-background +black+)

                   ;; Force the blend mode to only set the alpha of the destination
                   (rl-set-blend-factors +rlgl-src-alpha+ +rlgl-src-alpha+ +rlgl-min+)
                   (rl-set-blend-mode +blend-custom+)

                   ;; Merge in all the light masks
                   (dotimes (i +max-lights+)
                     (when (light-info-active (aref *lights* i))
                       (draw-texture-rec (render-texture-texture (light-info-mask (aref *lights* i)))
                                         (make-rectangle :x 0.0 :y 0.0 :width (float (get-screen-width)) :height (- (float (get-screen-height))))
                                         (vector2-zero) +white+)))

                   (rl-draw-render-batch-active)

                   ;; Go back to normal blend
                   (rl-set-blend-mode +blend-alpha+)
                   (end-texture-mode)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +black+)

               ;; Draw the tile background
               (draw-texture-rec background-texture (make-rectangle :x 0.0 :y 0.0 :width (float (get-screen-width)) :height (float (get-screen-height)))
                                 (vector2-zero) +white+)

               ;; Overlay the shadows from all the lights
               (draw-texture-rec (render-texture-texture light-mask)
                                 (make-rectangle :x 0.0 :y 0.0 :width (float (get-screen-width)) :height (- (float (get-screen-height))))
                                 (vector2-zero) (color-alpha +white+ (if show-lines 0.75 1.0)))

               ;; Draw the lights
               (dotimes (i +max-lights+)
                 (let ((light (aref *lights* i)))
                   (when (light-info-active light)
                     (draw-circle (truncate (vx (light-info-position light))) (truncate (vy (light-info-position light))) 10.0
                                  (if (= i 0) +yellow+ +white+)))))

               (if show-lines
                   (let ((light (aref *lights* 0)))
                     (dotimes (s (light-info-shadow-count light))
                       (draw-triangle-fan (aref (light-info-shadows light) s) 4 +darkpurple+))

                     (dotimes (b box-count)
                       (let ((box (aref boxes b)))
                         (when (check-collision-recs box (light-info-bounds light)) (draw-rectangle-rec box +purple+))

                         (draw-rectangle-lines (truncate (rectangle-x box)) (truncate (rectangle-y box))
                                               (truncate (rectangle-width box)) (truncate (rectangle-height box)) +darkblue+)))

                     (draw-text "(F1) Hide Shadow Volumes" 10 50 10 +green+))
                   (draw-text "(F1) Show Shadow Volumes" 10 50 10 +green+))

               (draw-fps (- screen-width 80) 10)
               (draw-text "Drag to move light #1" 10 10 10 +darkgreen+)
               (draw-text "Right click to add new light" 10 30 10 +darkgreen+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture background-texture)
      (unload-render-texture light-mask)

      (dotimes (i +max-lights+)
        (when (light-info-active (aref *lights* i)) (unload-render-texture (light-info-mask (aref *lights* i)))))

      (close-window))))                 ; Close window and OpenGL context

(main)

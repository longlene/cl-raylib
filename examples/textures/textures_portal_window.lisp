;;;; raylib [textures] example - portal window
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by PanicTitan (@PanicTitan) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 PanicTitan (@PanicTitan)
;;;; Common Lisp port of raylib/examples/textures/textures_portal_window.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-portal-window
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-portal-window)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Starts a 3D mode using an off-axis ("oblique frustum") projection, built directly from
;; an eye point and 3 corners of a rectangular window, instead of a fovy centered straight
;; ahead of the camera. This is the standard technique for rendering a scene as seen through
;; a window that isn't necessarily faced head-on (also used for multi-monitor and VR
;; rendering) - the key difference from a normal Camera3D is that the frustum is allowed
;; to be asymmetric, so perspective lines through the window line up correctly from any
;; eye position, instead of behaving like a flat image pasted onto the window
(defun begin-portal-mode-3d (eye bottom-left bottom-right top-left near-plane far-plane)
  (let* ((right (vector3-normalize (vector3-subtract bottom-right bottom-left)))
         (up (vector3-normalize (vector3-subtract top-left bottom-left)))
         (normal (vector3-normalize (vector3-cross-product right up)))

         ;; Vectors from the eye to 3 corners of the window, used to project the window onto
         ;; the near plane and read off how far it extends left/right/bottom/top of the eye
         (to-bl (vector3-subtract bottom-left eye))
         (to-br (vector3-subtract bottom-right eye))
         (to-tl (vector3-subtract top-left eye))

         (dist (- (vector3-dot-product to-bl normal))))
    (when (< dist 0.01) (setf dist 0.01)) ; Keep the eye from crossing the window plane

    (let ((scale (/ near-plane dist)))

      (rl-draw-render-batch-active)

      (rl-matrix-mode +rl-projection+)
      (rl-push-matrix)
      (rl-set-matrix-projection (matrix-frustum (* (vector3-dot-product right to-bl) scale) (* (vector3-dot-product right to-br) scale)
                                                (* (vector3-dot-product up to-bl) scale) (* (vector3-dot-product up to-tl) scale)
                                                near-plane far-plane))

      ;; View orientation is fixed to the window's own plane (looking straight through it
      ;; along its normal), NOT aimed at any target - that's what the asymmetric frustum
      ;; above is for, and is what lets the eye move off to one side without distorting
      (rl-matrix-mode +rl-modelview+)
      (rl-load-identity)
      (rl-mult-matrixf (matrix-to-float-v (matrix-look-at eye (vector3-subtract eye normal) up)))

      (rl-enable-depth-test))))

;; End portal 3D mode and returns to default 2D orthographic mode
;; NOTE: Similar implementation to EndMode3D()
(defun end-portal-mode-3d ()
  (rl-draw-render-batch-active)         ; Update and draw internal render batch

  (rl-matrix-mode +rl-projection+)      ; Switch to projection matrix
  (rl-pop-matrix)                       ; Restore previous matrix (projection) from matrix stack

  (rl-matrix-mode +rl-modelview+)       ; Switch back to modelview matrix
  (rl-load-identity)                    ; Reset current matrix (modelview)

  (rl-disable-depth-test))              ; Disable DEPTH_TEST for 2D

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - portal window")

    (let* (;; Camera to navigate the "real world" (Dimension A)
           (camera (make-camera3d :position (vec3 0.0 2.5 7.0)
                                  :target (vec3 0.0 1.8 0.0)
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 45.0
                                  :projection +camera-perspective+))

           ;; The archway opening, in Dimension A world space (used both to draw the frame and
           ;; as the window rectangle the oblique projection is built from)
           (arch-bottom-left (vec3 -1.5 0.0 0.0))
           (arch-bottom-right (vec3 1.5 0.0 0.0))
           (arch-top-left (vec3 -1.5 4.0 0.0))

           ;; The archway sits at portalA and looks out onto portalB, far away in world space.
           ;; Every frame, Dimension B gets rendered to a texture using the same relative eye
           ;; and window position, shifted by the offset between the two portals
           (portal-a (vec3 0.0 0.0 0.0))
           (portal-b (vec3 0.0 0.0 -60.0))

           (portal-view (load-render-texture 480 640)))

      (disable-cursor)                  ; Lock cursor for first-person free camera controls

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-free+)

               (let* ((time (float (get-time) 1.0))

                      ;; Eye and window corners, shifted into Dimension B so they match the player's
                      ;; actual position and viewing angle relative to the archway
                      (offset (vector3-subtract portal-b portal-a))
                      (eye-in-b (vector3-add (camera3d-position camera) offset))
                      (bl-in-b (vector3-add arch-bottom-left offset))
                      (br-in-b (vector3-add arch-bottom-right offset))
                      (tl-in-b (vector3-add arch-top-left offset)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 ;; Render Dimension B into an offscreen texture, using an oblique projection so
                 ;; its perspective lines up with the archway exactly as the real camera sees it
                 (begin-texture-mode portal-view)

                 (clear-background (list 10 5 20 255))

                 (begin-portal-mode-3d eye-in-b bl-in-b br-in-b tl-in-b 0.05 100.0)

                 (draw-grid 30 0.8)

                 ;; Floating pulsing core sphere
                 (let ((core-pos (vector3-add portal-b (vec3 0.0 (+ 2.0 (* (sin (* time 2.5)) 0.4)) -4.0))))
                   (draw-sphere core-pos 1.2 +purple+)
                   (draw-sphere-wires core-pos 1.25 16 16 +magenta+))

                 ;; Orbiting cubes
                 (dotimes (i 4)
                   (let* ((angle (+ (* time 1.5) (* i (/ +pi+ 2.0))))
                          (pos (vector3-add portal-b (vec3 (* (sin angle) 2.5)
                                                           (+ 2.0 (* (cos (+ (* time 3.0) i)) 0.5))
                                                           (+ -4.0 (* (cos angle) 2.5))))))
                     (draw-cube pos 0.5 0.5 0.5 +lime+)
                     (draw-cube-wires pos 0.52 0.52 0.52 +darkgreen+)))

                 (end-portal-mode-3d)

                 (end-texture-mode))

               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (draw-grid 20 1.0)

               ;; Side pillars
               (draw-cube (vec3 -3.5 2.0 0.0) 0.8 4.0 0.8 +darkgray+)
               (draw-cube-wires (vec3 -3.5 2.0 0.0) 0.8 4.0 0.8 +orange+)
               (draw-cube (vec3 3.5 2.0 0.0) 0.8 4.0 0.8 +darkgray+)
               (draw-cube-wires (vec3 3.5 2.0 0.0) 0.8 4.0 0.8 +orange+)

               ;; Golden archway frame and solid base
               (draw-cube-wires (vec3 0.0 2.0 0.0) 3.2 4.2 0.2 +gold+)
               (draw-cube (vec3 0.0 0.05 0.0) 3.4 0.1 0.6 +maroon+)

               ;; Solid backing wall, only ever seen if looking at the archway from behind
               (draw-cube (vec3 0.0 2.0 -0.05) 3.1 4.1 0.05 +maroon+)

               ;; The portal opening itself: a plain quad textured with the Dimension B
               ;; render, filling the archway exactly, so nothing "leaks" outside its shape
               (rl-set-texture (texture-id (render-texture-texture portal-view)))
               (rl-begin +rl-quads+)
               (rl-color4ub 255 255 255 255)
               (rl-normal3f 0.0 0.0 1.0)
               (rl-tex-coord2f 0.0 0.0) (rl-vertex3f (vx arch-bottom-left) (vy arch-bottom-left) (vz arch-bottom-left))
               (rl-tex-coord2f 1.0 0.0) (rl-vertex3f (vx arch-bottom-right) (vy arch-bottom-right) (vz arch-bottom-right))
               (rl-tex-coord2f 1.0 1.0) (rl-vertex3f (vx arch-bottom-right) 4.0 (vz arch-bottom-right))
               (rl-tex-coord2f 0.0 1.0) (rl-vertex3f (vx arch-top-left) (vy arch-top-left) (vz arch-top-left))
               (rl-end)
               (rl-set-texture 0)

               (end-mode-3d)

               ;; HUD overlay
               (draw-rectangle 15 15 520 100 (fade +black+ 0.75))
               (draw-rectangle-lines 15 15 520 100 +gold+)
               (draw-text "PORTAL WINDOW" 28 25 20 +gold+)
               (draw-text "Look through the golden arch into Dimension B" 28 52 20 +raywhite+)
               (draw-text "Controls: Mouse to look | WASD to move" 28 80 20 +gray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture portal-view)

      (close-window))))                 ; Close window and OpenGL context

(main)

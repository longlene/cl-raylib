;;;; raylib [others] example - scissor shapes
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 6.0
;;;;
;;;; Example contributed by David Buzatto (@davidbuzatto) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 David Buzatto (@davidbuzatto)
;;;; Common Lisp port of raylib/examples/others/gl_scissor_shapes.c

(require :cl-raylib)

(defpackage #:raylib-examples/gl-scissor-shapes
  (:use #:cl #:raylib))
(in-package #:raylib-examples/gl-scissor-shapes)

;; BeginScissorMode() only clips to an axis-aligned rectangle, since it maps
;; directly to glScissor. Clipping to an arbitrary shape (the same idea as
;; Graphics2D.setClip(Shape) in Java2D) needs the stencil buffer instead:
;; the clip shape is rasterized into the stencil buffer first, then further
;; draws are only let through where the stencil was written.
;;
;; rlgl has no stencil wrapper (see raysan5/raylib discussion #2964), so this
;; example calls the handful of OpenGL 1.0 stencil functions directly,
;; their entry points are queried with rl-get-proc-address()
(defconstant +gl-stencil-buffer-bit+ #x00000400)
(defconstant +gl-stencil-test+ #x0B90)
(defconstant +gl-always+ #x0207)
(defconstant +gl-equal+ #x0202)
(defconstant +gl-keep+ #x1E00)
(defconstant +gl-replace+ #x1E01)

(defmacro define-gl-function (name c-name &rest args)
  "Define NAME calling the OpenGL function C-NAME, args are (name type) pairs"
  (let ((pointer (gensym "POINTER")))
    `(let ((,pointer nil))
       (defun ,name ,(mapcar #'first args)
         (unless ,pointer (setf ,pointer (rl-get-proc-address ,c-name)))
         (cffi:foreign-funcall-pointer ,pointer () ,@(loop for (arg type) in args collect type collect arg) :void)))))

(define-gl-function gl-clear "glClear" (mask :uint))
(define-gl-function gl-enable "glEnable" (cap :uint))
(define-gl-function gl-disable "glDisable" (cap :uint))
(define-gl-function gl-stencil-mask "glStencilMask" (mask :uint))
(define-gl-function gl-stencil-func "glStencilFunc" (func :uint) (ref :int) (mask :uint))
(define-gl-function gl-stencil-op "glStencilOp" (sfail :uint) (dpfail :uint) (dppass :uint))

;; libm sinf(), as used by the C example
(defun sinf (x) (cffi:foreign-funcall "sinf" :float x :float))

;; mask is a callback instead of a fixed shape, so the clip region can be any
;; combination of draw-* calls (circle, polygon, text, several shapes together)
(defun begin-scissor-mode-shape (mask user-data)
  (rl-draw-render-batch-active)         ; Flush before touching stencil state
  (gl-clear +gl-stencil-buffer-bit+)
  (gl-enable +gl-stencil-test+)
  (gl-stencil-mask #xFF)

  ;; Pass 1: rasterize the mask into the stencil buffer only, no color write
  (rl-color-mask nil nil nil nil)
  (gl-stencil-func +gl-always+ 1 #xFF)
  (gl-stencil-op +gl-replace+ +gl-replace+ +gl-replace+)
  (funcall mask user-data)
  (rl-draw-render-batch-active)         ; Flush the mask draw

  ;; Pass 2: subsequent draws only survive where the stencil was written
  (rl-color-mask t t t t)
  (gl-stencil-func +gl-equal+ 1 #xFF)
  (gl-stencil-op +gl-keep+ +gl-keep+ +gl-keep+))

(defun end-scissor-mode-shape ()
  (rl-draw-render-batch-active)
  (gl-disable +gl-stencil-test+))

;; Two example mask shapes, drawn in WHITE (the color is irrelevant here,
;; only the stencil write from pass 1 matters)
(defstruct circle-mask center radius)
(defun draw-circle-mask (m)
  (draw-circle-v (circle-mask-center m) (circle-mask-radius m) +white+))

(defstruct poly-mask center radius sides)
(defun draw-poly-mask (m)
  (draw-poly (poly-mask-center m) (poly-mask-sides m) (poly-mask-radius m) 0.0 +white+))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [others] example - scissor shapes")

    (let ((circle-center (vec2 (- (/ screen-width 2.0) 120) (/ screen-height 2.0)))
          (hex-center (vec2 (+ (/ screen-width 2.0) 120) (/ screen-height 2.0)))
          (mask-radius 100.0))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf (vy circle-center) (+ (/ screen-height 2.0) (* (sinf (float (get-time) 1.0)) 40.0)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Circle-shaped clip: the full-screen rectangle only shows inside the circle
               (let ((circle-mask (make-circle-mask :center circle-center :radius mask-radius)))
                 (begin-scissor-mode-shape #'draw-circle-mask circle-mask)
                 (draw-rectangle 0 0 screen-width screen-height +blue+)
                 (end-scissor-mode-shape))

               ;; Hexagon-shaped clip: same mechanism, a different mask shape
               (let ((hex-mask (make-poly-mask :center hex-center :radius mask-radius :sides 6)))
                 (begin-scissor-mode-shape #'draw-poly-mask hex-mask)
                 (draw-rectangle 0 0 screen-width screen-height +orange+)
                 (end-scissor-mode-shape))

               (draw-text "BeginScissorMode() only clips to a rectangle (glScissor)" 10 10 10 +darkgray+)
               (draw-text "This clips to arbitrary shapes instead, using the stencil buffer" 10 25 10 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)

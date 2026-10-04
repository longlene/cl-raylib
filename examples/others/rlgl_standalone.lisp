;;;; raylib [others] example - standalone
;;;;
;;;; rlgl library is an abstraction layer for multiple OpenGL versions (1.1, 2.1, 3.3 Core, ES 2.0)
;;;; that provides a pseudo-OpenGL 1.1 immediate-mode style API (rlVertex, rlTranslate, rlRotate...)
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 1.6, last time updated with raylib 4.0
;;;;
;;;; WARNING: This example is intended only for PLATFORM_DESKTOP and OpenGL 3.3 Core profile
;;;;     It could work on other platforms if redesigned for those platforms (out-of-scope)
;;;;
;;;; DEPENDENCIES:
;;;;     glfw3     - Windows and context initialization library
;;;;     rlgl.h    - OpenGL abstraction layer to OpenGL 1.1, 3.3 or ES2
;;;;     glad.h    - OpenGL extensions initialization library (required by rlgl)
;;;;     raymath.h - 3D math library
;;;;
;;;; WINDOWS COMPILATION:
;;;;     gcc -o rlgl_standalone.exe rlgl_standalone.c -s -Iexternal\include -I..\..\src  \
;;;;         -L. -Lexternal\lib -lglfw3 -lopengl32 -lgdi32 -Wall -std=c99 -DGRAPHICS_API_OPENGL_33
;;;;
;;;; APPLE COMPILATION:
;;;;     gcc -o rlgl_standalone rlgl_standalone.c -I../../src -Iexternal/include -Lexternal/lib \
;;;;         -lglfw3 -framework CoreVideo -framework OpenGL -framework IOKit -framework Cocoa -framework QuartzCore
;;;;         -Wno-deprecated-declarations -std=c99 -DGRAPHICS_API_OPENGL_33
;;;;
;;;;
;;;; LICENSE: zlib/libpng
;;;;
;;;; This example is licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software:
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;;
;;;; This software is provided "as-is", without any express or implied warranty. In no event
;;;; will the authors be held liable for any damages arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose, including commercial
;;;; applications, and to alter it and redistribute it freely, subject to the following restrictions:
;;;;
;;;;   1. The origin of this software must not be misrepresented; you must not claim that you
;;;;   wrote the original software. If you use this software in a product, an acknowledgment
;;;;   in the product documentation would be appreciated but is not required.
;;;;
;;;;   2. Altered source versions must be plainly marked as such, and must not be misrepresented
;;;;   as being the original software.
;;;;
;;;;   3. This notice may not be removed or altered from any source distribution
;;;; Common Lisp port of raylib/examples/others/rlgl_standalone.c

(require :cl-raylib)                   ; Provides rlgl (gl.lisp), raymath (math.lisp) and the %glfw bindings

(defpackage #:raylib-examples/rlgl-standalone
  (:use #:cl #:raylib)
  (:local-nicknames (#:%glfw #:org.shirakumo.fraf.glfw.cffi)) ; GLFW3 C API bindings (glfw system)
  ;; This example defines its own Color values, Camera type and drawing functions on top of rlgl
  (:shadow #:+red+ #:+raywhite+ #:+darkgray+
           #:camera #:make-camera #:copy-camera #:camera-p #:camera-position #:camera-target
           #:camera-up #:camera-fovy #:camera-projection
           #:draw-rectangle-v #:draw-grid #:draw-cube #:draw-cube-wires))
(in-package #:raylib-examples/rlgl-standalone)

;; NOTE: rlgl can be configured just re-defining the following values:
;;+rl-default-batch-buffer-elements+   8192    ; Default internal render batch elements limits
;;+rl-default-batch-buffers+              1    ; Default number of batch buffers (multi-buffering)
;;+rl-default-batch-drawcalls+          256    ; Default number of batch draw calls (by state changes: mode, texture)
;;+rl-default-batch-max-texture-units+    4    ; Maximum number of textures units that can be activated on batch drawing (SetShaderValueTexture())
;;+rl-max-matrix-stack-size+             32    ; Maximum size of internal Matrix stack
;;+rl-max-shader-locations+              32    ; Maximum number of shader locations supported
;;+rl-cull-distance-near+              0.01    ; Default projection matrix near cull distance
;;+rl-cull-distance-far+             1000.0    ; Default projection matrix far cull distance

;; GLFW3 constants (GLFW/glfw3.h)
(defconstant +glfw-samples+ #x0002100D)
(defconstant +glfw-depth-bits+ #x00021005)
(defconstant +glfw-context-version-major+ #x00022002)
(defconstant +glfw-context-version-minor+ #x00022003)
(defconstant +glfw-opengl-profile+ #x00022008)
(defconstant +glfw-opengl-core-profile+ #x00032001)
(defconstant +glfw-opengl-forward-compat+ #x00022006)
(defconstant +glfw-true+ 1)
(defconstant +glfw-key-escape+ 256)
(defconstant +glfw-press+ 1)

(defparameter +red+ (list 230 41 55 255))       ; Red
(defparameter +raywhite+ (list 245 245 245 255)) ; My own White (raylib logo)
(defparameter +darkgray+ (list 80 80 80 255))    ; Dark Gray

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Camera type, defines a camera position/orientation in 3d space
(defstruct camera
  (position (vec3 0.0 0.0 0.0))         ; Camera position
  (target (vec3 0.0 0.0 0.0))           ; Camera target it looks-at
  (up (vec3 0.0 0.0 0.0))               ; Camera up vector (rotation over its axis)
  (fovy 0.0)                            ; Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
  (projection 0))                       ; Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC

;;----------------------------------------------------------------------------------
;; Module Functions Definitions
;;----------------------------------------------------------------------------------

;; GLFW3: Error callback
(cffi:defcallback error-callback :void ((error :int) (description :string))
  (declare (ignore error))
  (format *error-output* "~a" description))

;; GLFW3: Keyboard callback
(cffi:defcallback key-callback :void ((window :pointer) (key :int) (scancode :int) (action :int) (mods :int))
  (declare (ignore scancode mods))
  (when (and (= key +glfw-key-escape+) (= action +glfw-press+))
    (%glfw:set-window-should-close window t)))

;; Draw rectangle using rlgl OpenGL 1.1 style coding (translated to OpenGL 3.3 internally)
(defun draw-rectangle-v (position size color)
  (rl-begin +rl-triangles+)
  (destructuring-bind (r g b a) color
    (rl-color4ub r g b a))

  (rl-vertex2f (vx position) (vy position))
  (rl-vertex2f (vx position) (+ (vy position) (vy size)))
  (rl-vertex2f (+ (vx position) (vx size)) (+ (vy position) (vy size)))

  (rl-vertex2f (vx position) (vy position))
  (rl-vertex2f (+ (vx position) (vx size)) (+ (vy position) (vy size)))
  (rl-vertex2f (+ (vx position) (vx size)) (vy position))
  (rl-end))

;; Draw a grid centered at (0, 0, 0)
(defun draw-grid (slices spacing)
  (let ((half-slices (truncate slices 2)))

    (rl-begin +rl-lines+)
    (loop for i from (- half-slices) to half-slices
          do (if (= i 0)
                 (progn
                   (rl-color3f 0.5 0.5 0.5)
                   (rl-color3f 0.5 0.5 0.5)
                   (rl-color3f 0.5 0.5 0.5)
                   (rl-color3f 0.5 0.5 0.5))
                 (progn
                   (rl-color3f 0.75 0.75 0.75)
                   (rl-color3f 0.75 0.75 0.75)
                   (rl-color3f 0.75 0.75 0.75)
                   (rl-color3f 0.75 0.75 0.75)))

             (rl-vertex3f (* (float i) spacing) 0.0 (* (float (- half-slices)) spacing))
             (rl-vertex3f (* (float i) spacing) 0.0 (* (float half-slices) spacing))

             (rl-vertex3f (* (float (- half-slices)) spacing) 0.0 (* (float i) spacing))
             (rl-vertex3f (* (float half-slices) spacing) 0.0 (* (float i) spacing)))
    (rl-end)))

;; Draw cube
;; NOTE: Cube position is the center position
(defun draw-cube (position width height length color)
  (let ((x 0.0)
        (y 0.0)
        (z 0.0))

    (rl-push-matrix)

    ;; NOTE: Be careful! Function order matters (rotate -> scale -> translate)
    (rl-translatef (vx position) (vy position) (vz position))
    ;;(rl-scalef 2.0 2.0 2.0)
    ;;(rl-rotatef 45 0 1 0)

    (rl-begin +rl-triangles+)
    (destructuring-bind (r g b a) color
      (rl-color4ub r g b a))

    ;; Front Face -----------------------------------------------------
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Left
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Right
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left

    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Right
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Right

    ;; Back Face ------------------------------------------------------
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Left
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Left
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Right

    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Right
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Right
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Left

    ;; Top Face -------------------------------------------------------
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Left
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Bottom Left
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Bottom Right

    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Right
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Left
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Bottom Right

    ;; Bottom Face ----------------------------------------------------
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Top Left
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Right
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Left

    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Top Right
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Right
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Top Left

    ;; Right face -----------------------------------------------------
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Right
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Right
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left

    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Left
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Right
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left

    ;; Left Face ------------------------------------------------------
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Right
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Right

    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Left
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Right
    (rl-end)
    (rl-pop-matrix)))

;; Draw cube wires
(defun draw-cube-wires (position width height length color)
  (let ((x 0.0)
        (y 0.0)
        (z 0.0))

    (rl-push-matrix)

    (rl-translatef (vx position) (vy position) (vz position))
    ;;(rl-rotatef 45 0 1 0)

    (rl-begin +rl-lines+)
    (destructuring-bind (r g b a) color
      (rl-color4ub r g b a))

    ;; Front Face -----------------------------------------------------
    ;; Bottom Line
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Left
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Right

    ;; Left Line
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Right
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Right

    ;; Top Line
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Right
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left

    ;; Right Line
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Bottom Left

    ;; Back Face ------------------------------------------------------
    ;; Bottom Line
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Left
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Right

    ;; Left Line
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Right
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Right

    ;; Top Line
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Right
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Left

    ;; Right Line
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Left
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Bottom Left

    ;; Top Face -------------------------------------------------------
    ;; Left Line
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Left Front
    (rl-vertex3f (- x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Left Back

    ;; Right Line
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (+ z (/ length 2))) ; Top Right Front
    (rl-vertex3f (+ x (/ width 2)) (+ y (/ height 2)) (- z (/ length 2))) ; Top Right Back

    ;; Bottom Face  ---------------------------------------------------
    ;; Left Line
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Top Left Front
    (rl-vertex3f (- x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Top Left Back

    ;; Right Line
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (+ z (/ length 2))) ; Top Right Front
    (rl-vertex3f (+ x (/ width 2)) (- y (/ height 2)) (- z (/ length 2))) ; Top Right Back
    (rl-end)
    (rl-pop-matrix)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    ;; GLFW3 Initialization + OpenGL 3.3 Context + Extensions
    ;;--------------------------------------------------------
    ;; NOTE: GLFW and the OpenGL driver may raise floating point exceptions, masked as in C
    (float-features:with-float-traps-masked t
      (%glfw:set-error-callback (cffi:callback error-callback))

      (if (not (%glfw:init))
          (progn
            (format t "GLFW3: Can not initialize GLFW~%")
            (return-from main 1))
          (format t "GLFW3: GLFW initialized successfully~%"))

      (%glfw:window-hint +glfw-samples+ 4)
      (%glfw:window-hint +glfw-depth-bits+ 16)

      ;; WARNING: OpenGL 3.3 Core profile only
      (%glfw:window-hint +glfw-context-version-major+ 3)
      (%glfw:window-hint +glfw-context-version-minor+ 3)
      (%glfw:window-hint +glfw-opengl-profile+ +glfw-opengl-core-profile+)
      ;;(%glfw:window-hint +glfw-opengl-debug-context+ +glfw-true+)
      #+darwin (%glfw:window-hint +glfw-opengl-forward-compat+ +glfw-true+)

      (let ((window (%glfw:create-window screen-width screen-height "raylib [others] example - rlgl standalone"
                                         (cffi:null-pointer) (cffi:null-pointer))))

        (if (cffi:null-pointer-p window)
            (progn
              (%glfw:terminate)
              (return-from main 2))
            (format t "GLFW3: Window created successfully~%"))

        (%glfw:set-window-pos window 200 200)

        (%glfw:set-key-callback window (cffi:callback key-callback))

        (%glfw:make-context-current window)
        (%glfw:swap-interval 0)

        ;; Load OpenGL 3.3 supported extensions
        (rl-load-extensions (cffi:foreign-symbol-pointer "glfwGetProcAddress"))
        ;;--------------------------------------------------------

        ;; Initialize OpenGL context (states and resources)
        (rlgl-init screen-width screen-height)

        ;; Initialize viewport and internal projection/modelview matrices
        (rl-viewport 0 0 screen-width screen-height)
        (rl-matrix-mode +rl-projection+)                   ; Switch to PROJECTION matrix
        (rl-load-identity)                                 ; Reset current matrix (PROJECTION)
        (rl-ortho 0 screen-width screen-height 0 0.0 1.0)  ; Orthographic projection with top-left corner at (0,0)
        (rl-matrix-mode +rl-modelview+)                    ; Switch back to MODELVIEW matrix
        (rl-load-identity)                                 ; Reset current matrix (MODELVIEW)

        (rl-clear-color 245 245 245 255)                   ; Define clear color
        (rl-enable-depth-test)                             ; Enable DEPTH_TEST for 3D

        (let ((camera (make-camera))
              (cube-position (vec3 0.0 0.0 0.0)))       ; Cube default position (center)
          (setf (camera-position camera) (vec3 5.0 5.0 5.0)) ; Camera position
          (setf (camera-target camera) (vec3 0.0 0.0 0.0))   ; Camera looking at point
          (setf (camera-up camera) (vec3 0.0 1.0 0.0))       ; Camera up vector (rotation towards target)
          (setf (camera-fovy camera) 45.0)                   ; Camera field-of-view Y
          ;;--------------------------------------------------------------------------------------

          ;; Main game loop
          (loop until (%glfw:window-should-close window)
                do ;; Update
                   ;;----------------------------------------------------------------------------------
                   ;;(incf (vx (camera-position camera)) 0.01)
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (rl-clear-screen-buffers)    ; Clear current framebuffer

                   ;; Draw '3D' elements in the scene
                   ;;-----------------------------------------------
                   ;; Calculate projection matrix (from perspective) and view matrix from camera look at
                   (let ((mat-proj (matrix-perspective (float (* (camera-fovy camera) +deg2rad+) 1d0)
                                                       (/ (float screen-width 1d0) (float screen-height 1d0)) 0.01d0 1000d0))
                         (mat-view (matrix-look-at (camera-position camera) (camera-target camera) (camera-up camera))))

                     (rl-set-matrix-modelview mat-view) ; Set internal modelview matrix (default shader)
                     (rl-set-matrix-projection mat-proj) ; Set internal projection matrix (default shader)

                     (draw-cube cube-position 2.0 2.0 2.0 +red+)
                     (draw-cube-wires cube-position 2.0 2.0 2.0 +raywhite+)
                     (draw-grid 10 1.0)

                     ;; Draw internal render batch buffers (3D data)
                     (rl-draw-render-batch-active)
                     ;;-----------------------------------------------

                     ;; Draw '2D' elements in the scene (GUI)
                     ;;-----------------------------------------------
                     ;; NOTE: C defines RLGL_SET_MATRIX_MANUALLY, the alternative is letting rlgl
                     ;; generate and multiply the matrix internally (rl-matrix-mode, rl-load-identity, rl-ortho)
                     (setf mat-proj (matrix-ortho 0d0 screen-width screen-height 0d0 0d0 1d0))
                     (setf mat-view (matrix-identity))

                     (rl-set-matrix-modelview mat-view) ; Set internal modelview matrix (default shader)
                     (rl-set-matrix-projection mat-proj)) ; Set internal projection matrix (default shader)

                   (draw-rectangle-v (vec2 10.0 10.0) (vec2 780.0 20.0) +darkgray+)

                   ;; Draw internal render batch buffers (2D data)
                   (rl-draw-render-batch-active)
                   ;;-----------------------------------------------

                   (%glfw:swap-buffers window)
                   (%glfw:poll-events))
          ;;----------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (rlgl-close)                  ; Unload rlgl internal buffers and default shader/texture

          (%glfw:destroy-window window) ; Close window
          (%glfw:terminate)))))         ; Free GLFW3 resources
  ;;--------------------------------------------------------------------------------------

  0)

(main)

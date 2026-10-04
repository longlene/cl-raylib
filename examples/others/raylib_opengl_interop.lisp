;;;; raylib [others] example - OpenGL interoperatibility
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 3.8, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Stephan Soller (@arkanis) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2021-2025 Stephan Soller (@arkanis) and Ramon Santamaria (@raysan5)
;;;;
;;;; *******************************************************************************************
;;;;
;;;; Mixes raylib and plain OpenGL code to draw a GL_POINTS based particle system. The
;;;; primary point is to demonstrate raylib and OpenGL interop
;;;;
;;;; rlgl batched draw operations internally so we have to flush the current batch before
;;;; doing our own OpenGL work (rlDrawRenderBatchActive())
;;;;
;;;; The example also demonstrates how to get the current model view projection matrix of
;;;; raylib. That way raylib cameras and so on work as expected
;;;; Common Lisp port of raylib/examples/others/raylib_opengl_interop.c

(require :cl-raylib)

(defpackage #:raylib-examples/raylib-opengl-interop
  (:use #:cl #:raylib))
(in-package #:raylib-examples/raylib-opengl-interop)

(defconstant +glsl-version+ 330)

(defconstant +max-particles+ 1000)

;; Plain OpenGL functionality: the entry points are queried with rl-get-proc-address()
(defconstant +gl-array-buffer+ #x8892)
(defconstant +gl-static-draw+ #x88E4)
(defconstant +gl-float+ #x1406)
(defconstant +gl-false+ 0)
(defconstant +gl-points+ #x0000)
(defconstant +gl-program-point-size+ #x8642)

(defmacro define-gl-function (name c-name result-type &rest args)
  "Define NAME calling the OpenGL function C-NAME, args are (name type) pairs"
  (let ((pointer (gensym "POINTER")))
    `(let ((,pointer nil))
       (defun ,name ,(mapcar #'first args)
         (unless ,pointer (setf ,pointer (rl-get-proc-address ,c-name)))
         (cffi:foreign-funcall-pointer ,pointer () ,@(loop for (arg type) in args collect type collect arg) ,result-type)))))

(define-gl-function gl-gen-vertex-arrays "glGenVertexArrays" :void (n :int) (arrays :pointer))
(define-gl-function gl-bind-vertex-array "glBindVertexArray" :void (array :uint))
(define-gl-function gl-delete-vertex-arrays "glDeleteVertexArrays" :void (n :int) (arrays :pointer))
(define-gl-function gl-gen-buffers "glGenBuffers" :void (n :int) (buffers :pointer))
(define-gl-function gl-bind-buffer "glBindBuffer" :void (target :uint) (buffer :uint))
(define-gl-function gl-buffer-data "glBufferData" :void (target :uint) (size :long) (data :pointer) (usage :uint))
(define-gl-function gl-delete-buffers "glDeleteBuffers" :void (n :int) (buffers :pointer))
(define-gl-function gl-vertex-attrib-pointer "glVertexAttribPointer" :void
  (index :uint) (size :int) (type :uint) (normalized :uchar) (stride :int) (pointer :pointer))
(define-gl-function gl-enable-vertex-attrib-array "glEnableVertexAttribArray" :void (index :uint))
(define-gl-function gl-enable "glEnable" :void (cap :uint))
(define-gl-function gl-use-program "glUseProgram" :void (program :uint))
(define-gl-function gl-uniform1f "glUniform1f" :void (location :int) (v0 :float))
(define-gl-function gl-uniform4fv "glUniform4fv" :void (location :int) (count :int) (value :pointer))
(define-gl-function gl-uniform-matrix4fv "glUniformMatrix4fv" :void
  (location :int) (count :int) (transpose :uchar) (value :pointer))
(define-gl-function gl-draw-arrays "glDrawArrays" :void (mode :uint) (first :int) (count :int))

;;------------------------------------------------------------------------------------
;; Types and Structures Definition
;;------------------------------------------------------------------------------------
;; Particle type
;; NOTE: Particles are stored packed as x, y, period floats in a foreign array, the vertex buffer data
(defconstant +particle-floats+ 3)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [others] example - OpenGL interoperatibility")

    (let* ((shader (load-shader (text-format "resources/shaders/glsl%i/point_particle.vs" +glsl-version+)
                                (text-format "resources/shaders/glsl%i/point_particle.fs" +glsl-version+)))

           (current-time-loc (get-shader-location shader "currentTime"))
           (color-loc (get-shader-location shader "color")))

      ;; Initialize the vertex buffer for the particles and assign each particle random values
      (cffi:with-foreign-objects ((particles :float (* +max-particles+ +particle-floats+))
                                  (vao :uint)
                                  (vbo :uint)
                                  (color-v :float 4))
        (dotimes (i +max-particles+)
          (let ((p (* i +particle-floats+)))
            (setf (cffi:mem-aref particles :float p) (float (get-random-value 20 (- screen-width 20))))
            (setf (cffi:mem-aref particles :float (+ p 1)) (float (get-random-value 50 (- screen-height 20))))

            ;; Give each particle a slightly different period. But don't spread it to much
            ;; This way the particles line up every so often and you get a glimps of what is going on
            (setf (cffi:mem-aref particles :float (+ p 2)) (/ (float (get-random-value 10 30)) 10.0))))

        ;; Create a plain OpenGL vertex buffer with the data and an vertex array object
        ;; that feeds the data from the buffer into the vertexPosition shader attribute
        (setf (cffi:mem-ref vao :uint) 0
              (cffi:mem-ref vbo :uint) 0)
        (gl-gen-vertex-arrays 1 vao)
        (gl-bind-vertex-array (cffi:mem-ref vao :uint))
        (gl-gen-buffers 1 vbo)
        (gl-bind-buffer +gl-array-buffer+ (cffi:mem-ref vbo :uint))
        (gl-buffer-data +gl-array-buffer+ (* +max-particles+ +particle-floats+ 4) particles +gl-static-draw+)
        ;; Note: load-shader automatically fetches the attribute index of "vertexPosition" and saves it in shader.locs[SHADER_LOC_VERTEX_POSITION]
        (gl-vertex-attrib-pointer (aref (shader-locs shader) +shader-loc-vertex-position+) 3 +gl-float+ +gl-false+ 0 (cffi:null-pointer))
        (gl-enable-vertex-attrib-array 0)
        (gl-bind-buffer +gl-array-buffer+ 0)
        (gl-bind-vertex-array 0)

        ;; Allows the vertex shader to set the point size of each particle individually
        (gl-enable +gl-program-point-size+)

        (set-target-fps 60)
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)
                 (clear-background +white+)

                 (draw-rectangle 10 10 210 30 +maroon+)
                 (draw-text (text-format "%zu particles in one vertex buffer" +max-particles+) 20 20 10 +raywhite+)

                 (rl-draw-render-batch-active) ; Draw iternal buffers data (previous draw calls)

                 ;; Switch to plain OpenGL
                 ;;------------------------------------------------------------------------------
                 (gl-use-program (shader-id shader))

                 (gl-uniform1f current-time-loc (float (get-time) 1.0))

                 (let ((color (color-normalize (list 255 0 0 128))))
                   (setf (cffi:mem-aref color-v :float 0) (vx color)
                         (cffi:mem-aref color-v :float 1) (vy color)
                         (cffi:mem-aref color-v :float 2) (vz color)
                         (cffi:mem-aref color-v :float 3) (vw color))
                   (gl-uniform4fv color-loc 1 color-v))

                 ;; Get the current modelview and projection matrix so the particle system is displayed and transformed
                 (let ((model-view-projection (matrix-multiply (rl-get-matrix-modelview) (rl-get-matrix-projection))))
                   (cffi:with-foreign-object (mvp :float 16)
                     (loop for v across (matrix-to-float-v model-view-projection)
                           for i from 0
                           do (setf (cffi:mem-aref mvp :float i) v))
                     (gl-uniform-matrix4fv (aref (shader-locs shader) +shader-loc-matrix-mvp+) 1 +gl-false+ mvp)))

                 (gl-bind-vertex-array (cffi:mem-ref vao :uint))
                 (gl-draw-arrays +gl-points+ 0 +max-particles+)
                 (gl-bind-vertex-array 0)

                 (gl-use-program 0)
                 ;;------------------------------------------------------------------------------

                 (draw-fps (- screen-width 100) 10)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (gl-delete-buffers 1 vbo)
        (gl-delete-vertex-arrays 1 vao))

      (unload-shader shader)            ; Unload shader

      (close-window))))                 ; Close window and OpenGL context

(main)

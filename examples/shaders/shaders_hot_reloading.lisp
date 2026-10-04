;;;; raylib [shaders] example - hot reloading
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 for shaders support and only #version 330
;;;;       is currently supported. OpenGL ES 2.0 platforms are not supported at the moment
;;;;
;;;; Example originally created with raylib 3.0, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2020-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_hot_reloading.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-hot-reloading
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-hot-reloading)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;; C asctime(localtime(&t)): "Www Mmm dd hh:mm:ss yyyy\n" for a Unix time in the local time zone
(defun asctime-localtime (unix-time)
  (multiple-value-bind (second minute hour day month year day-of-week)
      (decode-universal-time (+ unix-time (encode-universal-time 0 0 0 1 1 1970 0)))
    (format nil "~a ~a~3d ~2,'0d:~2,'0d:~2,'0d ~d~%"
            (aref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week)
            (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (1- month))
            day hour minute second year)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - hot reloading")

    (let* ((frag-shader-file-name "resources/shaders/glsl%i/reload.fs")
           (frag-shader-file-mod-time (get-file-mod-time (text-format frag-shader-file-name +glsl-version+)))

           ;; Load raymarching shader
           ;; NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
           (shader (load-shader nil (text-format frag-shader-file-name +glsl-version+)))

           ;; Get shader locations for required uniforms
           (resolution-loc (get-shader-location shader "resolution"))
           (mouse-loc (get-shader-location shader "mouse"))
           (time-loc (get-shader-location shader "time"))

           (resolution (list (float screen-width) (float screen-height)))

           (total-time 0.0)
           (shader-auto-reloading nil))

      (set-shader-value shader resolution-loc resolution +shader-uniform-vec2+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf total-time (get-frame-time))
               (let* ((mouse (get-mouse-position))
                      (mouse-pos (list (vx mouse) (vy mouse))))

                 ;; Set shader required uniform values
                 (set-shader-value shader time-loc total-time +shader-uniform-float+)
                 (set-shader-value shader mouse-loc mouse-pos +shader-uniform-vec2+))

               ;; Hot shader reloading
               (when (or shader-auto-reloading (is-mouse-button-pressed +mouse-button-left+))
                 (let ((current-frag-shader-mod-time (get-file-mod-time (text-format frag-shader-file-name +glsl-version+))))

                   ;; Check if shader file has been modified
                   (when (/= current-frag-shader-mod-time frag-shader-file-mod-time)
                     ;; Try reloading updated shader
                     (let ((updated-shader (load-shader nil (text-format frag-shader-file-name +glsl-version+))))

                       (when (/= (shader-id updated-shader) (rl-get-shader-id-default)) ; It was correctly loaded
                         (unload-shader shader)
                         (setf shader updated-shader)

                         ;; Get shader locations for required uniforms
                         (setf resolution-loc (get-shader-location shader "resolution")
                               mouse-loc (get-shader-location shader "mouse")
                               time-loc (get-shader-location shader "time"))

                         ;; Reset required uniforms
                         (set-shader-value shader resolution-loc resolution +shader-uniform-vec2+)))

                     (setf frag-shader-file-mod-time current-frag-shader-mod-time))))

               (when (is-key-pressed +key-a+) (setf shader-auto-reloading (not shader-auto-reloading)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; We only draw a white full-screen rectangle, frame is generated in shader
               (begin-shader-mode shader)
               (draw-rectangle 0 0 screen-width screen-height +white+)
               (end-shader-mode)

               (draw-text (text-format "PRESS [A] to TOGGLE SHADER AUTOLOADING: %s"
                                       (if shader-auto-reloading "AUTO" "MANUAL")) 10 10 10 (if shader-auto-reloading +red+ +black+))
               (unless shader-auto-reloading (draw-text "MOUSE CLICK to SHADER RE-LOADING" 10 30 10 +black+))

               (draw-text (text-format "Shader last modification: %s" (asctime-localtime frag-shader-file-mod-time)) 10 430 10 +black+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader

      (close-window))))                 ; Close window and OpenGL context

(main)

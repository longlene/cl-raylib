;;;; raylib [textures] example - image processing
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: Images are loaded in CPU memory (RAM); textures are loaded in GPU memory (VRAM)
;;;;
;;;; Example originally created with raylib 1.4, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2016-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_image_processing.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-image-processing
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-image-processing)

(defconstant +num-processes+ 9)

;; ImageProcess
(defconstant +none+ 0)
(defconstant +color-grayscale+ 1)
(defconstant +color-tint+ 2)
(defconstant +color-invert+ 3)
(defconstant +color-contrast+ 4)
(defconstant +color-brightness+ 5)
(defconstant +gaussian-blur+ 6)
(defconstant +flip-vertical+ 7)
(defconstant +flip-horizontal+ 8)

(defparameter *process-text*
  (vector "NO PROCESSING"
          "COLOR GRAYSCALE"
          "COLOR TINT"
          "COLOR INVERT"
          "COLOR CONTRAST"
          "COLOR BRIGHTNESS"
          "GAUSSIAN BLUR"
          "FLIP VERTICAL"
          "FLIP HORIZONTAL"))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - image processing")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let ((im-origin (load-image "resources/parrots.png"))) ; Loaded in CPU memory (RAM)
      (image-format im-origin +pixelformat-uncompressed-r8g8b8a8+) ; Format image to RGBA 32bit (required for texture update) <-- ISSUE

      (let ((texture (load-texture-from-image im-origin)) ; Image converted to texture, GPU memory (VRAM)
            (im-copy (image-copy im-origin))
            (current-process +none+)
            (texture-reload nil)
            (toggle-recs (make-array +num-processes+))
            (mouse-hover-rec -1))

        (dotimes (i +num-processes+)
          (setf (aref toggle-recs i) (make-rectangle :x 40.0 :y (float (+ 50 (* 32 i))) :width 150.0 :height 30.0)))

        (set-target-fps 60)
        ;;---------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 ;; Mouse toggle group logic
                 (dotimes (i +num-processes+)
                   (if (check-collision-point-rec (get-mouse-position) (aref toggle-recs i))
                       (progn
                         (setf mouse-hover-rec i)

                         (when (is-mouse-button-released +mouse-button-left+)
                           (setf current-process i
                                 texture-reload t))
                         (return))
                       (setf mouse-hover-rec -1)))

                 ;; Keyboard toggle group logic
                 (cond ((is-key-pressed +key-down+)
                        (incf current-process)
                        (when (> current-process (1- +num-processes+)) (setf current-process 0))
                        (setf texture-reload t))
                       ((is-key-pressed +key-up+)
                        (decf current-process)
                        (when (< current-process 0) (setf current-process 7))
                        (setf texture-reload t)))

                 ;; Reload texture when required
                 (when texture-reload
                   (unload-image im-copy)       ; Unload image-copy data
                   (setf im-copy (image-copy im-origin)) ; Restore image-copy from image-origin

                   ;; NOTE: Image processing is a costly CPU process to be done every frame,
                   ;; If image processing is required in a frame-basis, it should be done
                   ;; with a texture and by shaders
                   (case current-process
                     (#.+color-grayscale+ (image-color-grayscale im-copy))
                     (#.+color-tint+ (image-color-tint im-copy +green+))
                     (#.+color-invert+ (image-color-invert im-copy))
                     (#.+color-contrast+ (image-color-contrast im-copy -40))
                     (#.+color-brightness+ (image-color-brightness im-copy -80))
                     (#.+gaussian-blur+ (image-blur-gaussian im-copy 10))
                     (#.+flip-vertical+ (image-flip-vertical im-copy))
                     (#.+flip-horizontal+ (image-flip-horizontal im-copy)))

                   (let ((pixels (load-image-colors im-copy))) ; Load pixel data from image (RGBA 32bit)
                     (update-texture texture pixels) ; Update texture with new image data
                     (unload-image-colors pixels)) ; Unload pixels data from RAM

                   (setf texture-reload nil))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (draw-text "IMAGE PROCESSING:" 40 30 10 +darkgray+)

                 ;; Draw rectangles
                 (dotimes (i +num-processes+)
                   (let ((rec (aref toggle-recs i))
                         (active (or (= i current-process) (= i mouse-hover-rec))))
                     (draw-rectangle-rec rec (if active +skyblue+ +lightgray+))
                     (draw-rectangle-lines (truncate (rectangle-x rec)) (truncate (rectangle-y rec)) (truncate (rectangle-width rec)) (truncate (rectangle-height rec))
                                           (if active +blue+ +gray+))
                     (draw-text (aref *process-text* i)
                                (truncate (- (+ (rectangle-x rec) (/ (rectangle-width rec) 2)) (/ (float (measure-text (aref *process-text* i) 10)) 2)))
                                (+ (truncate (rectangle-y rec)) 11) 10 (if active +darkblue+ +darkgray+))))

                 (draw-texture texture (- screen-width (texture-width texture) 60) (- (truncate screen-height 2) (truncate (texture-height texture) 2)) +white+)
                 (draw-rectangle-lines (- screen-width (texture-width texture) 60) (- (truncate screen-height 2) (truncate (texture-height texture) 2))
                                       (texture-width texture) (texture-height texture) +black+)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-texture texture)        ; Unload texture from VRAM
        (unload-image im-origin)        ; Unload image-origin from RAM
        (unload-image im-copy)          ; Unload image-copy from RAM

        (close-window)))))              ; Close window and OpenGL context

(main)

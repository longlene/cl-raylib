;;;; raylib [core] example - viewport scaling
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Agnis Aldiņš (@nezvers) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2025 Agnis Aldiņš (@nezvers)
;;;; Common Lisp port of raylib/examples/core/core_viewport_scaling.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-viewport-scaling
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-viewport-scaling)

(defconstant +resolution-count+ 4)      ; For iteration purposes and teaching example

;; ViewportType
;; Only upscale, useful for pixel art
(defconstant +keep-aspect-integer+ 0)
(defconstant +keep-height-integer+ 1)
(defconstant +keep-width-integer+ 2)
;; Can also downscale
(defconstant +keep-aspect+ 3)
(defconstant +keep-height+ 4)
(defconstant +keep-width+ 5)
;; For itteration purposes and as a teaching example
(defconstant +viewport-type-count+ 6)

;; For displaying on GUI
(defparameter *viewport-type-names*
  (vector "KEEP_ASPECT_INTEGER"
          "KEEP_HEIGHT_INTEGER"
          "KEEP_WIDTH_INTEGER"
          "KEEP_ASPECT"
          "KEEP_HEIGHT"
          "KEEP_WIDTH"))

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
(defun %int (x) (float (truncate x)))   ; (float)(int)x

(defun keep-aspect-centered-integer (screen-width screen-height game-width game-height source-rect dest-rect)
  (setf (rectangle-x source-rect) 0.0
        (rectangle-y source-rect) (float game-height)
        (rectangle-width source-rect) (float game-width)
        (rectangle-height source-rect) (float (- game-height)))

  (let* ((ratio-x (truncate screen-width game-width))
         (ratio-y (truncate screen-height game-height))
         (resize-ratio (float (if (< ratio-x ratio-y) ratio-x ratio-y))))
    (setf (rectangle-x dest-rect) (%int (* (- screen-width (* game-width resize-ratio)) 0.5))
          (rectangle-y dest-rect) (%int (* (- screen-height (* game-height resize-ratio)) 0.5))
          (rectangle-width dest-rect) (%int (* game-width resize-ratio))
          (rectangle-height dest-rect) (%int (* game-height resize-ratio)))))

(defun keep-height-centered-integer (screen-width screen-height game-width game-height source-rect dest-rect)
  (let ((resize-ratio (/ (float screen-height) game-height)))
    (setf (rectangle-x source-rect) 0.0
          (rectangle-y source-rect) 0.0
          (rectangle-width source-rect) (%int (/ screen-width resize-ratio))
          (rectangle-height source-rect) (float (- game-height)))

    (setf (rectangle-x dest-rect) (%int (* (- screen-width (* (rectangle-width source-rect) resize-ratio)) 0.5))
          (rectangle-y dest-rect) (%int (* (- screen-height (* game-height resize-ratio)) 0.5))
          (rectangle-width dest-rect) (%int (* (rectangle-width source-rect) resize-ratio))
          (rectangle-height dest-rect) (%int (* game-height resize-ratio)))))

(defun keep-width-centered-integer (screen-width screen-height game-width game-height source-rect dest-rect)
  (let ((resize-ratio (/ (float screen-width) game-width)))
    (setf (rectangle-x source-rect) 0.0
          (rectangle-y source-rect) 0.0
          (rectangle-width source-rect) (float game-width)
          (rectangle-height source-rect) (%int (/ screen-height resize-ratio)))

    (setf (rectangle-x dest-rect) (%int (* (- screen-width (* game-width resize-ratio)) 0.5))
          (rectangle-y dest-rect) (%int (* (- screen-height (* (rectangle-height source-rect) resize-ratio)) 0.5))
          (rectangle-width dest-rect) (%int (* game-width resize-ratio))
          (rectangle-height dest-rect) (%int (* (rectangle-height source-rect) resize-ratio)))

    (setf (rectangle-height source-rect) (* (rectangle-height source-rect) -1.0))))

(defun keep-aspect-centered (screen-width screen-height game-width game-height source-rect dest-rect)
  (setf (rectangle-x source-rect) 0.0
        (rectangle-y source-rect) (float game-height)
        (rectangle-width source-rect) (float game-width)
        (rectangle-height source-rect) (float (- game-height)))

  (let* ((ratio-x (/ (float screen-width) (float game-width)))
         (ratio-y (/ (float screen-height) (float game-height)))
         (resize-ratio (if (< ratio-x ratio-y) ratio-x ratio-y)))
    (setf (rectangle-x dest-rect) (%int (* (- screen-width (* game-width resize-ratio)) 0.5))
          (rectangle-y dest-rect) (%int (* (- screen-height (* game-height resize-ratio)) 0.5))
          (rectangle-width dest-rect) (%int (* game-width resize-ratio))
          (rectangle-height dest-rect) (%int (* game-height resize-ratio)))))

(defun keep-height-centered (screen-width screen-height game-width game-height source-rect dest-rect)
  (declare (ignore game-width))
  (let ((resize-ratio (/ (float screen-height) (float game-height))))
    (setf (rectangle-x source-rect) 0.0
          (rectangle-y source-rect) 0.0
          (rectangle-width source-rect) (%int (/ (float screen-width) resize-ratio))
          (rectangle-height source-rect) (float (- game-height)))

    (setf (rectangle-x dest-rect) (%int (* (- screen-width (* (rectangle-width source-rect) resize-ratio)) 0.5))
          (rectangle-y dest-rect) (%int (* (- screen-height (* game-height resize-ratio)) 0.5))
          (rectangle-width dest-rect) (%int (* (rectangle-width source-rect) resize-ratio))
          (rectangle-height dest-rect) (%int (* game-height resize-ratio)))))

(defun keep-width-centered (screen-width screen-height game-width game-height source-rect dest-rect)
  (declare (ignore game-height))
  (let ((resize-ratio (/ (float screen-width) (float game-width))))
    (setf (rectangle-x source-rect) 0.0
          (rectangle-y source-rect) 0.0
          (rectangle-width source-rect) (float game-width)
          (rectangle-height source-rect) (%int (/ (float screen-height) resize-ratio)))

    (setf (rectangle-x dest-rect) (%int (* (- screen-width (* game-width resize-ratio)) 0.5))
          (rectangle-y dest-rect) (%int (* (- screen-height (* (rectangle-height source-rect) resize-ratio)) 0.5))
          (rectangle-width dest-rect) (%int (* game-width resize-ratio))
          (rectangle-height dest-rect) (%int (* (rectangle-height source-rect) resize-ratio)))

    (setf (rectangle-height source-rect) (* (rectangle-height source-rect) -1.0))))

(defun resize-render-size (viewport-type game-width game-height source-rect dest-rect target)
  "Returns the new screen width, screen height and render texture"
  (let ((screen-width (get-screen-width))
        (screen-height (get-screen-height)))

    (funcall (ecase viewport-type
               (#.+keep-aspect-integer+ #'keep-aspect-centered-integer)
               (#.+keep-height-integer+ #'keep-height-centered-integer)
               (#.+keep-width-integer+ #'keep-width-centered-integer)
               (#.+keep-aspect+ #'keep-aspect-centered)
               (#.+keep-height+ #'keep-height-centered)
               (#.+keep-width+ #'keep-width-centered))
             screen-width screen-height game-width game-height source-rect dest-rect)

    (when target (unload-render-texture target))
    (values screen-width screen-height
            (load-render-texture (truncate (rectangle-width source-rect)) (- (truncate (rectangle-height source-rect)))))))

;; Example how to calculate position on RenderTexture
(defun screen2-render-texture-position (point texture-rect scaled-rect)
  (let ((relative-position (vec2 (- (vx point) (rectangle-x scaled-rect)) (- (vy point) (rectangle-y scaled-rect))))
        (ratio (vec2 (/ (rectangle-width texture-rect) (rectangle-width scaled-rect))
                     (/ (- (rectangle-height texture-rect)) (rectangle-height scaled-rect)))))
    (vec2 (* (vx relative-position) (vx ratio)) (* (vy relative-position) (vx ratio)))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-window-resizable+)
    (init-window screen-width screen-height "raylib [core] example - viewport scaling")

    (let* (;; Preset resolutions that could be created by subdividing screen resolution
           (resolution-list (vector (vec2 64.0 64.0)
                                    (vec2 256.0 240.0)
                                    (vec2 320.0 180.0)
                                    ;; 4K doesn't work with integer scaling but included for example purposes with non-integer scaling
                                    (vec2 3840.0 2160.0)))
           (resolution-index 0)
           (game-width 64)
           (game-height 64)
           (target nil)
           (source-rect (make-rectangle))
           (dest-rect (make-rectangle))
           (viewport-type +keep-aspect-integer+)
           ;; Button rectangles
           (decrease-resolution-button (make-rectangle :x 200.0 :y 30.0 :width 10.0 :height 10.0))
           (increase-resolution-button (make-rectangle :x 215.0 :y 30.0 :width 10.0 :height 10.0))
           (decrease-type-button (make-rectangle :x 200.0 :y 45.0 :width 10.0 :height 10.0))
           (increase-type-button (make-rectangle :x 215.0 :y 45.0 :width 10.0 :height 10.0)))

      (flet ((resize ()
               (multiple-value-setq (screen-width screen-height target)
                 (resize-render-size viewport-type game-width game-height source-rect dest-rect target))))
        (resize)

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (when (is-window-resized) (resize))

                 (let ((mouse-position (get-mouse-position))
                       (mouse-pressed (is-mouse-button-pressed +mouse-button-left+)))

                   ;; Check buttons and rescale
                   (when (and (check-collision-point-rec mouse-position decrease-resolution-button) mouse-pressed)
                     (setf resolution-index (mod (+ resolution-index +resolution-count+ -1) +resolution-count+)
                           game-width (truncate (vx (aref resolution-list resolution-index)))
                           game-height (truncate (vy (aref resolution-list resolution-index))))
                     (resize))

                   (when (and (check-collision-point-rec mouse-position increase-resolution-button) mouse-pressed)
                     (setf resolution-index (mod (1+ resolution-index) +resolution-count+)
                           game-width (truncate (vx (aref resolution-list resolution-index)))
                           game-height (truncate (vy (aref resolution-list resolution-index))))
                     (resize))

                   (when (and (check-collision-point-rec mouse-position decrease-type-button) mouse-pressed)
                     (setf viewport-type (mod (+ viewport-type +viewport-type-count+ -1) +viewport-type-count+))
                     (resize))

                   (when (and (check-collision-point-rec mouse-position increase-type-button) mouse-pressed)
                     (setf viewport-type (mod (1+ viewport-type) +viewport-type-count+))
                     (resize))

                   (let ((texture-mouse-position (screen2-render-texture-position mouse-position source-rect dest-rect)))
                     ;;----------------------------------------------------------------------------------

                     ;; Draw
                     ;;----------------------------------------------------------------------------------
                     ;; Draw our scene to the render texture
                     (begin-texture-mode target)
                     (clear-background +white+)
                     (draw-circle-v texture-mouse-position 20.0 +lime+)
                     (end-texture-mode)))

                 ;; Draw render texture to main framebuffer
                 (begin-drawing)
                 (clear-background +black+)

                 ;; Draw our render texture with rotation applied
                 (draw-texture-pro (render-texture-texture target) source-rect dest-rect (vec2 0.0 0.0) 0.0 +white+)

                 ;; Draw Native resolution (GUI or anything)
                 ;; Draw info box
                 (let ((info-rect (make-rectangle :x 5.0 :y 5.0 :width 330.0 :height 105.0)))
                   (draw-rectangle-rec info-rect (fade +lightgray+ 0.7))
                   (draw-rectangle-lines-ex info-rect 1.0 +blue+))

                 (draw-text (text-format "Window Resolution: %d x %d" screen-width screen-height) 15 15 10 +black+)
                 (draw-text (text-format "Game Resolution: %d x %d" game-width game-height) 15 30 10 +black+)

                 (draw-text (text-format "Type: %s" (aref *viewport-type-names* viewport-type)) 15 45 10 +black+)
                 (let ((scale-ratio (vec2 (/ (rectangle-width dest-rect) (rectangle-width source-rect))
                                          (/ (- (rectangle-height dest-rect)) (rectangle-height source-rect)))))
                   (if (or (< (vx scale-ratio) 0.001) (< (vy scale-ratio) 0.001))
                       (draw-text (text-format "Scale ratio: INVALID") 15 60 10 +black+)
                       (draw-text (text-format "Scale ratio: %.2f x %.2f" (vx scale-ratio) (vy scale-ratio)) 15 60 10 +black+)))

                 (draw-text (text-format "Source size: %.2f x %.2f" (rectangle-width source-rect) (- (rectangle-height source-rect))) 15 75 10 +black+)
                 (draw-text (text-format "Destination size: %.2f x %.2f" (rectangle-width dest-rect) (rectangle-height dest-rect)) 15 90 10 +black+)

                 ;; Draw buttons
                 (draw-rectangle-rec decrease-type-button +skyblue+)
                 (draw-rectangle-rec increase-type-button +skyblue+)
                 (draw-rectangle-rec decrease-resolution-button +skyblue+)
                 (draw-rectangle-rec increase-resolution-button +skyblue+)
                 (draw-text "<" (+ (truncate (rectangle-x decrease-type-button)) 3) (+ (truncate (rectangle-y decrease-type-button)) 1) 10 +black+)
                 (draw-text ">" (+ (truncate (rectangle-x increase-type-button)) 3) (+ (truncate (rectangle-y increase-type-button)) 1) 10 +black+)
                 (draw-text "<" (+ (truncate (rectangle-x decrease-resolution-button)) 3) (+ (truncate (rectangle-y decrease-resolution-button)) 1) 10 +black+)
                 (draw-text ">" (+ (truncate (rectangle-x increase-resolution-button)) 3) (+ (truncate (rectangle-y increase-resolution-button)) 1) 10 +black+)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (close-window)))))              ; Close window and OpenGL context

(main)

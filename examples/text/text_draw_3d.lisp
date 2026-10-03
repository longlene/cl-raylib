;;;; text_draw_3d.lisp - Draw 3D text example
;;;; Translated from raylib/examples/text/text_draw_3d.c

(require :cl-raylib)

(defpackage :text-draw-3d
  (:use :cl :cl-raylib))

(in-package :text-draw-3d)

(defconstant +letter-boundary-size+ 0.25)
(defconstant +text-max-layers+ 32)

(defparameter *show-letter-boundary* nil)
(defparameter *show-text-boundary* nil)

;; Configuration structure for waving the text
(defstruct wave-text-config
  wave-range
  wave-speed
  wave-offset)

(defun draw-text-codepoint-3d (font codepoint position font-size backface tint)
  "Draw codepoint at specified position in 3D space"
  (let ((index (get-glyph-index font codepoint))
        (scale (/ font-size (font-base-size font))))
    
    ;; Character destination rectangle on screen
    (let ((char-pos (vec3 (+ (vx position) (* (- (glyph-offset-x (aref (font-glyphs font) index))
                                                 (font-glyph-padding font)) scale))
                          (vy position)
                          (+ (vz position) (* (- (glyph-offset-y (aref (font-glyphs font) index))
                                                 (font-glyph-padding font)) scale)))))
      
      ;; Character source rectangle from font texture atlas
      (let ((src-rec (aref (font-recs font) index))
            (width (* (+ (rectangle-width src-rec) (* 2.0 (font-glyph-padding font))) scale))
            (height (* (+ (rectangle-height src-rec) (* 2.0 (font-glyph-padding font))) scale)))
        
        (when (> (texture-id (font-texture font)) 0)
          (when *show-letter-boundary*
            (draw-cube-wires-v (vec3 (+ (vx char-pos) (/ width 2.0))
                                    (vy char-pos)
                                    (+ (vz char-pos) (/ height 2.0)))
                              (vec3 width +letter-boundary-size+ height)
                              +violet+))
          
          ;; Draw the character quad with texture
          (rl-push-matrix)
          (rl-translatef (vx char-pos) (vy char-pos) (vz char-pos))
          
          ;; Simplified 3D text rendering using draw-texture-pro
          (draw-texture-pro (font-texture font)
                           src-rec
                           (make-rectangle :x 0 :y 0 :width width :height height)
                           (vec2 0.0 0.0)
                           0.0
                           tint)
          
          (rl-pop-matrix))))))

(defun draw-text-3d (font text position font-size font-spacing line-spacing backface tint)
  "Draw a 2D text in 3D space"
  (let ((length (text-length text))
        (text-offset-y 0.0)
        (text-offset-x 0.0)
        (scale (/ font-size (font-base-size font))))
    
    (loop for i from 0 below length do
      (let ((codepoint (get-codepoint text i)))
        (cond
          ((= codepoint 10) ; newline
           (incf text-offset-y (+ font-size line-spacing))
           (setf text-offset-x 0.0))
          ((and (/= codepoint 32) (/= codepoint 9)) ; not space or tab
           (draw-text-codepoint-3d font codepoint
                                  (vec3 (+ (vx position) text-offset-x)
                                        (vy position)
                                        (+ (vz position) text-offset-y))
                                  font-size backface tint)
           (let ((index (get-glyph-index font codepoint)))
             (if (= (glyph-advance-x (aref (font-glyphs font) index)) 0)
                 (incf text-offset-x (+ (* (rectangle-width (aref (font-recs font) index)) scale) font-spacing))
                 (incf text-offset-x (+ (* (glyph-advance-x (aref (font-glyphs font) index)) scale) font-spacing))))))))))

(defun generate-random-color (s v)
  "Generate a nice color with a random hue"
  (let ((h (random 360.0)))
    (color-from-hsv h s v)))

(defun main ()
  "Main function - draw 3D text example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (set-config-flags (logior +flag-msaa-4x-hint+ +flag-vsync-hint+))
    (init-window screen-width screen-height "raylib [text] example - draw 2D text in 3D")

    (let ((spin t)
          (multicolor nil)
          (camera (make-camera-3d :position (vec3 -10.0 15.0 -10.0)
                                 :target (vec3 0.0 0.0 0.0)
                                 :up (vec3 0.0 1.0 0.0)
                                 :fovy 45.0
                                 :projection +camera-perspective+))
          (camera-mode +camera-orbital+)
          (cube-position (vec3 0.0 1.0 0.0))
          (cube-size (vec3 2.0 2.0 2.0))
          (font (get-font-default))
          (font-size 0.8)
          (font-spacing 0.05)
          (line-spacing -0.1)
          (text "Hello World in 3D!")
          (layers 1)
          (layer-distance 0.01)
          (light +maroon+)
          (dark +red+)
          (time 0.0))

      (disable-cursor)
      (set-target-fps 60)

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        (update-camera camera camera-mode)
        (incf time (get-frame-time))

        ;; Handle events
        (when (is-key-pressed +key-f1+)
          (setf *show-letter-boundary* (not *show-letter-boundary*)))
        (when (is-key-pressed +key-f2+)
          (setf *show-text-boundary* (not *show-text-boundary*)))
        (when (is-key-pressed +key-f3+)
          (setf spin (not spin))
          (setf camera (make-camera-3d :target (vec3 0.0 0.0 0.0)
                                      :up (vec3 0.0 1.0 0.0)
                                      :fovy 45.0
                                      :projection +camera-perspective+))
          (if spin
              (progn
                (setf (camera-3d-position camera) (vec3 -10.0 15.0 -10.0))
                (setf camera-mode +camera-orbital+))
              (progn
                (setf (camera-3d-position camera) (vec3 10.0 10.0 -10.0))
                (setf camera-mode +camera-free+))))

        ;; Handle text changes
        (when (is-key-pressed +key-left+) (decf font-size 0.1))
        (when (is-key-pressed +key-right+) (incf font-size 0.1))
        (when (is-key-pressed +key-up+) (decf font-spacing 0.01))
        (when (is-key-pressed +key-down+) (incf font-spacing 0.01))
        (when (is-key-pressed +key-page-up+) (decf line-spacing 0.1))
        (when (is-key-pressed +key-page-down+) (incf line-spacing 0.1))
        (when (is-key-down +key-insert+) (decf layer-distance 0.001))
        (when (is-key-down +key-delete+) (incf layer-distance 0.001))

        ;; Handle layers
        (when (is-key-pressed +key-home+) (when (> layers 1) (decf layers)))
        (when (is-key-pressed +key-end+) (when (< layers +text-max-layers+) (incf layers)))

        ;; Handle clicking the cube
        (when (is-mouse-button-pressed +mouse-button-left+)
          (let ((ray (get-screen-to-world-ray (get-mouse-position) camera)))
            (let ((collision (get-ray-collision-box ray
                                                   (make-bounding-box 
                                                    :min (vec3 (- (vx cube-position) (/ (vx cube-size) 2.0))
                                                              (- (vy cube-position) (/ (vy cube-size) 2.0))
                                                              (- (vz cube-position) (/ (vz cube-size) 2.0)))
                                                    :max (vec3 (+ (vx cube-position) (/ (vx cube-size) 2.0))
                                                              (+ (vy cube-position) (/ (vy cube-size) 2.0))
                                                              (+ (vz cube-position) (/ (vz cube-size) 2.0)))))))
              (when (ray-collision-hit collision)
                (setf light (generate-random-color 0.5 0.78))
                (setf dark (generate-random-color 0.4 0.58))))))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          (begin-mode-3d camera)
            (draw-cube-v cube-position cube-size dark)
            (draw-cube-wires (vx cube-position) (vy cube-position) (vz cube-position)
                            2.1 2.1 2.1 light)

            (draw-grid 10 2.0)

            ;; Draw the 3D text above the cube
            (rl-push-matrix)
              (rl-rotatef 90.0 1.0 0.0 0.0)
              (rl-rotatef 90.0 0.0 0.0 -1.0)

              ;; Draw multiple layers
              (dotimes (i layers)
                (draw-text-3d font text 
                             (vec3 -2.0 (* layer-distance i) -4.5)
                             font-size font-spacing line-spacing t light))

            (rl-pop-matrix)
          (end-mode-3d)

          ;; Draw 2D info
          (draw-text "Press [F1] to toggle letter boundary" 10 10 20 +black+)
          (draw-text "Press [F2] to toggle text boundary" 10 35 20 +black+)
          (draw-text "Press [F3] to toggle camera mode" 10 60 20 +black+)
          (draw-text "Use arrow keys to adjust font size and spacing" 10 85 20 +black+)
          (draw-text "Use [Home]/[End] to add/remove layers" 10 110 20 +black+)

          (draw-fps 10 (- screen-height 30))

        (end-drawing))

      ;; De-Initialization
      (unload-font font))

    ;; Close window
    (close-window)))

;; Run the example
(main)
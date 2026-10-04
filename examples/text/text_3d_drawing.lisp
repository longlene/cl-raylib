;;;; raylib [text] example - 3d drawing
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; NOTE: Draw a 2D text in 3D space, each letter is drawn in a quad (or 2 quads if backface is set)
;;;; where the texture coodinates of each quad map to the texture coordinates of the glyphs
;;;; inside the font texture
;;;;
;;;; A more efficient approach, i believe, would be to render the text in a render texture and
;;;; map that texture to a plane and render that, or maybe a shader but my method allows more
;;;; flexibility...for example to change position of each letter individually to make somethink
;;;; like a wavy text effect
;;;;
;;;; Special thanks to:
;;;;      @Nighten for the DrawTextStyle() code https://github.com/NightenDushi/Raylib_DrawTextStyle
;;;;      Chris Camacho (codifies - http://bedroomcoders.co.uk/) for the alpha discard shader
;;;;
;;;; Example originally created with raylib 3.5, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Vlad Adrian (@demizdor) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2021-2025 Vlad Adrian (@demizdor)
;;;; Common Lisp port of raylib/examples/text/text_3d_drawing.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-3d-drawing
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-3d-drawing)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;--------------------------------------------------------------------------------------
;; Global variables
;;--------------------------------------------------------------------------------------
(defconstant +letter-boundry-size+ 0.25)
(defconstant +text-max-layers+ 32)
(defparameter *letter-boundry-color* +violet+)

(defvar *show-letter-boundry* nil)
(defvar *show-text-boundry* nil)

;;--------------------------------------------------------------------------------------
;; Types and Structures Definition
;;--------------------------------------------------------------------------------------
;; Configuration structure for waving the text
(defstruct wave-text-config
  (wave-range (vec3 0.0 0.0 0.0))
  (wave-speed (vec3 0.0 0.0 0.0))
  (wave-offset (vec3 0.0 0.0 0.0)))

;;--------------------------------------------------------------------------------------
;; Module Functions Definitions
;;--------------------------------------------------------------------------------------
;; NOTE: The texts drawn here are ASCII, string indices are used as the C byte indices

;; Draw codepoint at specified position in 3D space
(defun draw-text-codepoint-3d (font codepoint position font-size backface tint)
  ;; Character index position in sprite font
  ;; NOTE: In case a codepoint is not available in the font, index returned points to '?'
  (let* ((index (get-glyph-index font codepoint))
         (scale (/ font-size (float (font-base-size font))))
         (glyph (aref (font-glyphs font) index))
         (rec (aref (font-recs font) index))
         (padding (font-glyph-padding font))
         (position (vcopy position)))

    ;; Character destination rectangle on screen
    ;; NOTE: We consider charsPadding on drawing
    (incf (vx position) (* (float (- (glyph-info-offset-x glyph) padding)) scale))
    (incf (vz position) (* (float (- (glyph-info-offset-y glyph) padding)) scale))

    ;; Character source rectangle from font texture atlas
    ;; NOTE: We consider chars padding when drawing, it could be required for outline/glow shader effects
    (let ((src-rec (make-rectangle :x (- (rectangle-x rec) (float padding)) :y (- (rectangle-y rec) (float padding))
                                   :width (+ (rectangle-width rec) (* 2.0 padding)) :height (+ (rectangle-height rec) (* 2.0 padding))))
          (width (* (+ (rectangle-width rec) (* 2.0 padding)) scale))
          (height (* (+ (rectangle-height rec) (* 2.0 padding)) scale))
          (texture (font-texture font)))

      (when (> (texture-id texture) 0)
        (let* ((x 0.0)
               (y 0.0)
               (z 0.0)

               ;; normalized texture coordinates of the glyph inside the font texture (0.0f -> 1.0f)
               (tx (/ (rectangle-x src-rec) (texture-width texture)))
               (ty (/ (rectangle-y src-rec) (texture-height texture)))
               (tw (/ (+ (rectangle-x src-rec) (rectangle-width src-rec)) (texture-width texture)))
               (th (/ (+ (rectangle-y src-rec) (rectangle-height src-rec)) (texture-height texture))))

          (when *show-letter-boundry*
            (draw-cube-wires-v (vec3 (+ (vx position) (/ width 2)) (vy position) (+ (vz position) (/ height 2)))
                               (vec3 width +letter-boundry-size+ height) *letter-boundry-color*))

          (rl-check-render-batch-limit (+ 4 (* 4 (if backface 1 0))))
          (rl-set-texture (texture-id texture))

          (rl-push-matrix)
          (rl-translatef (vx position) (vy position) (vz position))

          (rl-begin +rl-quads+)
          (rl-color4ub (first tint) (second tint) (third tint) (fourth tint))

          ;; Front Face
          (rl-normal3f 0.0 1.0 0.0)                                           ; Normal Pointing Up
          (rl-tex-coord2f tx ty) (rl-vertex3f x y z)                          ; Top Left Of The Texture and Quad
          (rl-tex-coord2f tx th) (rl-vertex3f x y (+ z height))               ; Bottom Left Of The Texture and Quad
          (rl-tex-coord2f tw th) (rl-vertex3f (+ x width) y (+ z height))     ; Bottom Right Of The Texture and Quad
          (rl-tex-coord2f tw ty) (rl-vertex3f (+ x width) y z)                ; Top Right Of The Texture and Quad

          (when backface
            ;; Back Face
            (rl-normal3f 0.0 -1.0 0.0)                                        ; Normal Pointing Down
            (rl-tex-coord2f tx ty) (rl-vertex3f x y z)                        ; Top Right Of The Texture and Quad
            (rl-tex-coord2f tw ty) (rl-vertex3f (+ x width) y z)              ; Top Left Of The Texture and Quad
            (rl-tex-coord2f tw th) (rl-vertex3f (+ x width) y (+ z height))   ; Bottom Left Of The Texture and Quad
            (rl-tex-coord2f tx th) (rl-vertex3f x y (+ z height)))            ; Bottom Right Of The Texture and Quad
          (rl-end)
          (rl-pop-matrix)

          (rl-set-texture 0))))))

;; Draw a 2D text in 3D space
(defun draw-text-3d (font text position font-size font-spacing line-spacing backface tint)
  (let ((length (length text))          ; Total length of the text, scanned by codepoints in loop

        (text-offset-y 0.0)             ; Offset between lines (on line break '\n')
        (text-offset-x 0.0)             ; Offset X to next character to draw

        (scale (/ font-size (float (font-base-size font)))))

    (dotimes (i length)
      ;; Get next codepoint from string and glyph index in font
      (let* ((codepoint (char-code (char text i)))
             (index (get-glyph-index font codepoint)))

        (if (= codepoint (char-code #\Newline))
            (progn
              ;; NOTE: Fixed line spacing of 1.5 line-height
              ;; TODO: Support custom line spacing defined by user
              (incf text-offset-y (+ font-size line-spacing))
              (setf text-offset-x 0.0))
            (progn
              (when (and (/= codepoint (char-code #\Space)) (/= codepoint (char-code #\Tab)))
                (draw-text-codepoint-3d font codepoint (vec3 (+ (vx position) text-offset-x) (vy position) (+ (vz position) text-offset-y)) font-size backface tint))

              (if (= (glyph-info-advance-x (aref (font-glyphs font) index)) 0)
                  (incf text-offset-x (+ (* (float (rectangle-width (aref (font-recs font) index))) scale) font-spacing))
                  (incf text-offset-x (+ (* (float (glyph-info-advance-x (aref (font-glyphs font) index))) scale) font-spacing)))))))))

;; Draw a 2D text in 3D space and wave the parts that start with `~~` and end with `~~`
;; This is a modified version of the original code by @Nighten found here https://github.com/NightenDushi/Raylib_DrawTextStyle
(defun draw-text-wave-3d (font text position font-size font-spacing line-spacing backface config time tint)
  (let ((length (length text))          ; Total length of the text, scanned by codepoints in loop

        (text-offset-y 0.0)             ; Offset between lines (on line break '\n')
        (text-offset-x 0.0)             ; Offset X to next character to draw

        (scale (/ font-size (float (font-base-size font))))

        (wave nil)
        (i 0)
        (k 0))

    (loop while (< i length)
          do (let* ((codepoint (char-code (char text i)))
                    (codepoint-byte-count 1)
                    (index (get-glyph-index font codepoint)))

               (cond ((= codepoint (char-code #\Newline))
                      ;; NOTE: Fixed line spacing of 1.5 line-height
                      ;; TODO: Support custom line spacing defined by user
                      (incf text-offset-y (+ font-size line-spacing))
                      (setf text-offset-x 0.0
                            k 0))
                     ((= codepoint (char-code #\~))
                      (when (and (< (1+ i) length) (char= (char text (1+ i)) #\~))
                        (incf codepoint-byte-count 1)
                        (setf wave (not wave))))
                     (t
                      (when (and (/= codepoint (char-code #\Space)) (/= codepoint (char-code #\Tab)))
                        (let ((pos (vcopy position)))
                          (when wave        ; Apply the wave effect
                            (let ((range (wave-text-config-wave-range config))
                                  (speed (wave-text-config-wave-speed config))
                                  (offset (wave-text-config-wave-offset config)))
                              (incf (vx pos) (* (sin (- (* time (vx speed)) (* k (vx offset)))) (vx range)))
                              (incf (vy pos) (* (sin (- (* time (vy speed)) (* k (vy offset)))) (vy range)))
                              (incf (vz pos) (* (sin (- (* time (vz speed)) (* k (vz offset)))) (vz range)))))

                          (draw-text-codepoint-3d font codepoint (vec3 (+ (vx pos) text-offset-x) (vy pos) (+ (vz pos) text-offset-y)) font-size backface tint)))

                      (if (= (glyph-info-advance-x (aref (font-glyphs font) index)) 0)
                          (incf text-offset-x (+ (* (float (rectangle-width (aref (font-recs font) index))) scale) font-spacing))
                          (incf text-offset-x (+ (* (float (glyph-info-advance-x (aref (font-glyphs font) index))) scale) font-spacing)))))

               (incf i codepoint-byte-count) ; Move text counter to next codepoint
               (incf k)))))

;; Measure a text in 3D ignoring the `~~` chars
(defun measure-text-wave-3d (font text font-size font-spacing line-spacing)
  (let* ((len (length text))
         (temp-len 0)                   ; Used to count longer text line num chars
         (len-counter 0)

         (temp-text-width 0.0)          ; Used to count longer text line width

         (scale (/ font-size (float (font-base-size font))))
         (text-height scale)
         (text-width 0.0)
         (i 0))

    (loop while (< i len)
          do (let* ((letter (char-code (char text i))) ; Current character
                    (index (get-glyph-index font letter))) ; Index position in sprite font

               (if (/= letter (char-code #\Newline))
                   (if (and (= letter (char-code #\~)) (< (1+ i) len) (char= (char text (1+ i)) #\~))
                       (incf i)
                       (let ((glyph (aref (font-glyphs font) index)))
                         (incf len-counter)
                         (if (/= (glyph-info-advance-x glyph) 0)
                             (incf text-width (* (glyph-info-advance-x glyph) scale))
                             (incf text-width (* (+ (rectangle-width (aref (font-recs font) index)) (glyph-info-offset-x glyph)) scale)))))
                   (progn
                     (when (< temp-text-width text-width) (setf temp-text-width text-width))
                     (setf len-counter 0
                           text-width 0.0)
                     (incf text-height (+ font-size line-spacing))))

               (when (< temp-len len-counter) (setf temp-len len-counter)))
             (incf i))

    (when (< temp-text-width text-width) (setf temp-text-width text-width))

    (vec3 (+ temp-text-width (float (* (1- temp-len) font-spacing))) ; Adds chars spacing to measure
          0.25
          text-height)))

;; Generates a nice color with a random hue
(defun generate-random-color (s v)
  (let* ((phi 0.618033988749895)        ; Golden ratio conjugate
         (h (float (get-random-value 0 360))))
    (setf h (rem (+ h (* h phi)) 360.0))
    (color-from-hsv h s v)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))
    (declare (ignorable screen-height))

    (set-config-flags (logior +flag-msaa-4x-hint+ +flag-vsync-hint+))
    (init-window screen-width screen-height "raylib [text] example - 3d drawing")

    (let* ((spin t)                     ; Spin the camera?
           (multicolor nil)             ; Multicolor mode

           ;; Define the camera to look into our 3d world
           (camera (make-camera3d :position (vec3 -10.0 15.0 -10.0) ; Camera position
                                  :target (vec3 0.0 0.0 0.0)        ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)            ; Camera up vector (rotation towards target)
                                  :fovy 45.0                        ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           (camera-mode +camera-orbital+)

           (cube-position (vec3 0.0 1.0 0.0))
           (cube-size (vec3 2.0 2.0 2.0))

           ;; Use the default font
           (font (get-font-default))
           (font-size 0.8)
           (font-spacing 0.05)
           (line-spacing -0.1)

           ;; Set the text (using markdown!)
           (text (make-array 63 :element-type 'character :fill-pointer 0))
           (tbox (vec3 0.0 0.0 0.0))
           (layers 1)
           (quads 0)
           (layer-distance 0.01)

           (wcfg (make-wave-text-config :wave-speed (vec3 3.0 3.0 0.5)
                                        :wave-offset (vec3 0.35 0.35 0.35)
                                        :wave-range (vec3 0.45 0.45 0.45)))

           (time 0.0)

           ;; Setup a light and dark color
           (light +maroon+)
           (dark +red+)

           ;; Load the alpha discard shader
           (alpha-discard (load-shader nil (text-format "resources/shaders/glsl%i/alpha_discard.fs" +glsl-version+)))

           ;; Array filled with multiple random colors (when multicolor mode is set)
           (multi (make-array +text-max-layers+ :initial-element +blank+)))

      (loop for ch across "Hello ~~World~~ in 3D!" do (vector-push ch text))

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera camera-mode)

               ;; Handle font files dropped
               (when (is-file-dropped)
                 (let ((dropped-files (load-dropped-files)))

                   ;; NOTE: We only support first ttf file dropped
                   (cond ((is-file-extension (aref (file-path-list-paths dropped-files) 0) ".ttf")
                          (unload-font font)
                          (setf font (load-font-ex (aref (file-path-list-paths dropped-files) 0) (truncate font-size) nil 0)))
                         ((is-file-extension (aref (file-path-list-paths dropped-files) 0) ".fnt")
                          (unload-font font)
                          (setf font (load-font (aref (file-path-list-paths dropped-files) 0)))
                          (setf font-size (float (font-base-size font)))))

                   (unload-dropped-files dropped-files))) ; Unload filepaths from memory

               ;; Handle Events
               (when (is-key-pressed +key-f1+) (setf *show-letter-boundry* (not *show-letter-boundry*)))
               (when (is-key-pressed +key-f2+) (setf *show-text-boundry* (not *show-text-boundry*)))
               (when (is-key-pressed +key-f3+)
                 ;; Handle camera change
                 (setf spin (not spin))
                 ;; we need to reset the camera when changing modes
                 (setf camera (make-camera3d :position (vec3 0.0 0.0 0.0)
                                             :target (vec3 0.0 0.0 0.0)       ; Camera looking at point
                                             :up (vec3 0.0 1.0 0.0)           ; Camera up vector (rotation towards target)
                                             :fovy 45.0                       ; Camera field-of-view Y
                                             :projection +camera-perspective+)) ; Camera mode type

                 (if spin
                     (setf (camera3d-position camera) (vec3 -10.0 15.0 -10.0) ; Camera position
                           camera-mode +camera-orbital+)
                     (setf (camera3d-position camera) (vec3 10.0 10.0 -10.0) ; Camera position
                           camera-mode +camera-free+)))

               ;; Handle clicking the cube
               (when (is-mouse-button-pressed +mouse-button-left+)
                 (let* ((ray (get-screen-to-world-ray (get-mouse-position) camera))

                        ;; Check collision between ray and box
                        (collision (get-ray-collision-box ray
                                                          (make-bounding-box :min (vec3 (- (vx cube-position) (/ (vx cube-size) 2)) (- (vy cube-position) (/ (vy cube-size) 2)) (- (vz cube-position) (/ (vz cube-size) 2)))
                                                                             :max (vec3 (+ (vx cube-position) (/ (vx cube-size) 2)) (+ (vy cube-position) (/ (vy cube-size) 2)) (+ (vz cube-position) (/ (vz cube-size) 2)))))))
                   (when (ray-collision-hit collision)
                     ;; Generate new random colors
                     (setf light (generate-random-color 0.5 0.78)
                           dark (generate-random-color 0.4 0.58)))))

               ;; Handle text layers changes
               (cond ((is-key-pressed +key-home+) (when (> layers 1) (decf layers)))
                     ((is-key-pressed +key-end+) (when (< layers +text-max-layers+) (incf layers))))

               ;; Handle text changes
               (cond ((is-key-pressed +key-left+) (decf font-size 0.5))
                     ((is-key-pressed +key-right+) (incf font-size 0.5))
                     ((is-key-pressed +key-up+) (decf font-spacing 0.1))
                     ((is-key-pressed +key-down+) (incf font-spacing 0.1))
                     ((is-key-pressed +key-page-up+) (decf line-spacing 0.1))
                     ((is-key-pressed +key-page-down+) (incf line-spacing 0.1))
                     ((is-key-down +key-insert+) (decf layer-distance 0.001))
                     ((is-key-down +key-delete+) (incf layer-distance 0.001))
                     ((is-key-pressed +key-tab+)
                      (setf multicolor (not multicolor)) ; Enable /disable multicolor mode

                      (when multicolor
                        ;; Fill color array with random colors
                        (dotimes (i +text-max-layers+)
                          (setf (aref multi i) (generate-random-color 0.5 0.8))
                          (setf (aref multi i) (list (first (aref multi i)) (second (aref multi i)) (third (aref multi i)) (get-random-value 0 255)))))))

               ;; Handle text input
               (let ((ch (get-char-pressed)))
                 (cond ((is-key-pressed +key-backspace+)
                        ;; Remove last char
                        (when (> (length text) 0) (decf (fill-pointer text))))
                       ((is-key-pressed +key-enter+)
                        ;; handle newline
                        (when (< (length text) 63) (vector-push #\Newline text)))
                       (t
                        ;; append only printable chars
                        ;; NOTE: C stores ch (0 when no char was pressed) and the NUL terminator
                        (when (and (< (length text) 63) (> ch 0)) (vector-push (code-char ch) text)))))

               ;; Measure 3D text so we can center it
               (setf tbox (measure-text-wave-3d font text font-size font-spacing line-spacing))

               (setf quads 0)                   ; Reset quad counter
               (incf time (get-frame-time))     ; Update timer needed by `DrawTextWave3D()`
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)
               (draw-cube-v cube-position cube-size dark)
               (draw-cube-wires cube-position 2.1 2.1 2.1 light)

               (draw-grid 10 2.0)

               ;; Use a shader to handle the depth buffer issue with transparent textures
               ;; NOTE: more info at https://bedroomcoders.co.uk/posts/198
               (begin-shader-mode alpha-discard)

               ;; Draw the 3D text above the red cube
               (rl-push-matrix)
               (rl-rotatef 90.0 1.0 0.0 0.0)
               (rl-rotatef 90.0 0.0 0.0 -1.0)

               (dotimes (i layers)
                 (let ((clr (if multicolor (aref multi i) light)))
                   (draw-text-wave-3d font text (vec3 (/ (- (vx tbox)) 2.0) (* layer-distance i) -4.5) font-size font-spacing line-spacing t wcfg time clr)))

               ;; Draw the text boundry if set
               (when *show-text-boundry* (draw-cube-wires-v (vec3 0.0 0.0 (+ -4.5 (/ (vz tbox) 2))) tbox dark))
               (rl-pop-matrix)

               ;; Don't draw the letter boundries for the 3D text below
               (let ((slb *show-letter-boundry*)
                     (opt nil)
                     (m nil)
                     (pos nil))
                 (setf *show-letter-boundry* nil)

                 ;; Draw 3D options (use default font)
                 ;;-------------------------------------------------------------------------
                 (rl-push-matrix)
                 (rl-rotatef 180.0 0.0 1.0 0.0)
                 (setf opt (text-format "< SIZE: %2.1f >" font-size))
                 (incf quads (length opt))
                 (setf m (measure-text-ex (get-font-default) opt 0.8 0.1))
                 (setf pos (vec3 (/ (- (vx m)) 2.0) 0.01 2.0))
                 (draw-text-3d (get-font-default) opt pos 0.8 0.1 0.0 nil +blue+)
                 (incf (vz pos) (+ 0.5 (vy m)))

                 (flet ((option (text color)
                          (setf opt text)
                          (incf quads (length opt))
                          (setf m (measure-text-ex (get-font-default) opt 0.8 0.1))
                          (setf (vx pos) (/ (- (vx m)) 2.0))
                          (draw-text-3d (get-font-default) opt pos 0.8 0.1 0.0 nil color)
                          (incf (vz pos) (+ 0.5 (vy m)))))
                   (option (text-format "< SPACING: %2.1f >" font-spacing) +blue+)
                   (option (text-format "< LINE: %2.1f >" line-spacing) +blue+)
                   (option (text-format "< LBOX: %3s >" (if slb "ON" "OFF")) +red+)
                   (option (text-format "< TBOX: %3s >" (if *show-text-boundry* "ON" "OFF")) +red+)
                   (option (text-format "< LAYER DISTANCE: %.3f >" layer-distance) +darkpurple+))
                 (rl-pop-matrix)
                 ;;-------------------------------------------------------------------------

                 ;; Draw 3D info text (use default font)
                 ;;-------------------------------------------------------------------------
                 (setf opt "All the text displayed here is in 3D")
                 (incf quads 36)
                 (setf m (measure-text-ex (get-font-default) opt 1.0 0.05))
                 (setf pos (vec3 (/ (- (vx m)) 2.0) 0.01 2.0))
                 (draw-text-3d (get-font-default) opt pos 1.0 0.05 0.0 nil +darkblue+)
                 (incf (vz pos) (+ 1.5 (vy m)))

                 (flet ((info (text count)
                          (setf opt text)
                          (incf quads count)
                          (setf m (measure-text-ex (get-font-default) opt 0.6 0.05))
                          (setf (vx pos) (/ (- (vx m)) 2.0))
                          (draw-text-3d (get-font-default) opt pos 0.6 0.05 0.0 nil +darkblue+)
                          (incf (vz pos) (+ 0.5 (vy m)))))
                   (info "press [Left]/[Right] to change the font size" 44)
                   (info "press [Up]/[Down] to change the font spacing" 44)
                   (info "press [PgUp]/[PgDown] to change the line spacing" 48)
                   (info "press [F1] to toggle the letter boundry" 39)
                   (info "press [F2] to toggle the text boundry" 37))
                 ;;-------------------------------------------------------------------------

                 (setf *show-letter-boundry* slb))
               (end-shader-mode)

               (end-mode-3d)

               ;; Draw 2D info text & stats
               ;;-------------------------------------------------------------------------
               (draw-text (format nil "Drag & drop a font file to change the font!~%Type something, see what happens!~%~%Press [F3] to toggle the camera") 10 35 10 +black+)

               (incf quads (* (length text) 2 layers))
               (let* ((tmp (text-format "%2i layer(s) | %s camera | %4i quads (%4i verts)" layers (if spin "ORBITAL" "FREE") quads (* quads 4)))
                      (width (measure-text tmp 10)))
                 (draw-text tmp (- screen-width 20 width) 10 10 +darkgreen+))

               (loop for tmp in '("[Home]/[End] to add/remove 3D text layers"
                                  "[Insert]/[Delete] to increase/decrease distance between layers"
                                  "click the [CUBE] for a random color"
                                  "[Tab] to toggle multicolor mode")
                     for y from 25 by 15
                     do (draw-text tmp (- screen-width 20 (measure-text tmp 10)) y 10 +darkgray+))
               ;;-------------------------------------------------------------------------

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader alpha-discard)
      (unload-font font)
      (close-window))))                 ; Close window and OpenGL context

(main)

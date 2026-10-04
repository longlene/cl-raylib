;;;; raylib [text] example - font filters
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; NOTE: After font loading, font texture atlas filter could be configured for a softer
;;;; display of the font when scaling it to different sizes, that way, it's not required
;;;; to generate multiple fonts at multiple sizes (as long as the scaling is not very different)
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 4.2
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_font_filters.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-font-filters
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-font-filters)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - font filters")

    (let* ((msg "Loaded Font")

           ;; NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)

           ;; TTF Font loading with custom generation parameters
           (font (load-font-ex "resources/KAISG.ttf" 96 nil 0))

           (font-size 0.0)
           (font-position (vec2 40.0 (- (/ screen-height 2.0) 80.0)))
           (text-size (vec2 0.0 0.0))
           (current-font-filter 0))       ; TEXTURE_FILTER_POINT

      ;; Generate mipmap levels to use trilinear filtering
      ;; NOTE: On 2D drawing it won't be noticeable, it looks like FILTER_BILINEAR
      (gen-texture-mipmaps (font-texture font))

      (setf font-size (float (font-base-size font)))

      ;; Setup texture scaling filter
      (set-texture-filter (font-texture font) +texture-filter-point+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf font-size (* (get-mouse-wheel-move) 4.0))

               ;; Choose font texture filter method
               (cond ((is-key-pressed +key-one+)
                      (set-texture-filter (font-texture font) +texture-filter-point+)
                      (setf current-font-filter 0))
                     ((is-key-pressed +key-two+)
                      (set-texture-filter (font-texture font) +texture-filter-bilinear+)
                      (setf current-font-filter 1))
                     ((is-key-pressed +key-three+)
                      ;; NOTE: Trilinear filter won't be noticed on 2D drawing
                      (set-texture-filter (font-texture font) +texture-filter-trilinear+)
                      (setf current-font-filter 2)))

               (setf text-size (measure-text-ex font msg font-size 0))

               (cond ((is-key-down +key-left+) (decf (vx font-position) 10))
                     ((is-key-down +key-right+) (incf (vx font-position) 10)))

               ;; Load a dropped TTF file dynamically (at current fontSize)
               (when (is-file-dropped)
                 (let ((dropped-files (load-dropped-files)))

                   ;; NOTE: We only support first ttf file dropped
                   (when (is-file-extension (aref (file-path-list-paths dropped-files) 0) ".ttf")
                     (unload-font font)
                     (setf font (load-font-ex (aref (file-path-list-paths dropped-files) 0) (truncate font-size) nil 0)))

                   (unload-dropped-files dropped-files))) ; Unload filepaths from memory
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "Use mouse wheel to change font size" 20 20 10 +gray+)
               (draw-text "Use KEY_RIGHT and KEY_LEFT to move text" 20 40 10 +gray+)
               (draw-text "Use 1, 2, 3 to change texture filter" 20 60 10 +gray+)
               (draw-text "Drop a new TTF font for dynamic loading" 20 80 10 +darkgray+)

               (draw-text-ex font msg font-position font-size 0 +black+)

               ;; TODO: It seems texSize measurement is not accurate due to chars offsets...
               ;;(draw-rectangle-lines (vx font-position) (vy font-position) (vx text-size) (vy text-size) +red+)

               (draw-rectangle 0 (- screen-height 80) screen-width 80 +lightgray+)
               (draw-text (text-format "Font size: %02.02f" font-size) 20 (- screen-height 50) 10 +darkgray+)
               (draw-text (text-format "Text size: [%02.02f, %02.02f]" (vx text-size) (vy text-size)) 20 (- screen-height 30) 10 +darkgray+)
               (draw-text "CURRENT TEXTURE FILTER:" 250 400 20 +gray+)

               (case current-font-filter
                 (0 (draw-text "POINT" 570 400 20 +black+))
                 (1 (draw-text "BILINEAR" 570 400 20 +black+))
                 (2 (draw-text "TRILINEAR" 570 400 20 +black+)))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-font font)                ; Font unloading

      (close-window))))                 ; Close window and OpenGL context

(main)

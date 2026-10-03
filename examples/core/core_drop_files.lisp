;;;; raylib [core] example - drop files
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; NOTE: This example only works on platforms that support drag & drop (Windows, Linux, OSX, Html5?)
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 4.2
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_drop_files.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-drop-files
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-drop-files)

(defconstant +max-filepath-recorded+ 4096)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - drop files")

    (let ((file-path-counter 0)
          ;; We will register a maximum of filepaths
          (file-paths (make-array +max-filepath-recorded+ :initial-element "")))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-file-dropped)
                 (let ((dropped-files (load-dropped-files)))

                   (loop for i from 0 below (file-path-list-count dropped-files)
                         with offset = file-path-counter
                         do (when (< file-path-counter (1- +max-filepath-recorded+))
                              (setf (aref file-paths (+ offset i)) (elt (file-path-list-paths dropped-files) i))
                              (incf file-path-counter)))

                   (unload-dropped-files dropped-files))) ; Unload filepaths from memory
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (if (= file-path-counter 0)
                   (draw-text "Drop your files to this window!" 100 40 20 +darkgray+)
                   (progn
                     (draw-text "Dropped files:" 100 40 20 +darkgray+)

                     (dotimes (i file-path-counter)
                       (if (= (mod i 2) 0)
                           (draw-rectangle 0 (+ 85 (* 40 i)) screen-width 40 (fade +lightgray+ 0.5))
                           (draw-rectangle 0 (+ 85 (* 40 i)) screen-width 40 (fade +lightgray+ 0.3)))

                       (draw-text (aref file-paths i) 120 (+ 100 (* 40 i)) 10 +gray+))

                     (draw-text "Drop new files..." 100 (+ 110 (* 40 file-path-counter)) 20 +darkgray+)))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)

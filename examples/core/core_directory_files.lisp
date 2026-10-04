;;;; raylib [core] example - directory files
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Hugo ARNAL (@hugoarnal) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Hugo ARNAL (@hugoarnal)
;;;; Common Lisp port of raylib/examples/core/core_directory_files.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-directory-files
  (:use #:cl #:raylib #:raygui))        ; raygui: Required for GUI controls
(in-package #:raylib-examples/core-directory-files)

(defparameter *file-filter* "DIRS*;.png;.c")

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - directory files")

    (let* ((directory (get-working-directory))

           ;; Load file-paths on current working directory
           ;; NOTE: LoadDirectoryFiles() loads files and directories by default,
           ;; use LoadDirectoryFilesEx() for custom filters and recursive directories loading
           ;;(files (load-directory-files directory))
           (files (load-directory-files-ex directory *file-filter* nil))

           (btn-back-pressed 0)

           (list-scroll-index 0)
           (list-item-active -1)
           (list-item-focused -1))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (/= btn-back-pressed 0)
                 (setf directory (get-prev-directory-path directory))
                 (unload-directory-files files)
                 (setf files (load-directory-files-ex directory *file-filter* nil))

                 (setf list-scroll-index 0
                       list-item-active -1
                       list-item-focused -1))

               (when (and (>= list-item-active 0) (< list-item-active (file-path-list-count files))
                          (directory-exists (aref (file-path-list-paths files) list-item-active)))
                 (setf directory (aref (file-path-list-paths files) list-item-active))
                 (unload-directory-files files)
                 (setf files (load-directory-files-ex directory *file-filter* nil))

                 (setf list-scroll-index 0
                       list-item-active -1
                       list-item-focused -1))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)
               (clear-background +raywhite+)

               (setf btn-back-pressed (gui-button (make-rectangle :x 40.0 :y 10.0 :width 48.0 :height 28.0) "<"))

               (gui-set-style +default+ +text-size+ (* (font-base-size (gui-get-font)) 2))
               (gui-label (make-rectangle :x (+ 40.0 48 10) :y 10.0 :width 700.0 :height 28.0) directory)
               (gui-set-style +default+ +text-size+ (font-base-size (gui-get-font)))

               (gui-set-style +listview+ +text-alignment+ +text-align-left+)
               (gui-set-style +listview+ +text-padding+ 40)
               (multiple-value-bind (result scroll-index active focused)
                   (gui-list-view-ex (make-rectangle :x 0.0 :y 50.0 :width (float (get-screen-width)) :height (- (float (get-screen-height)) 50))
                                     (file-path-list-paths files) (file-path-list-count files)
                                     list-scroll-index list-item-active list-item-focused)
                 (declare (ignore result))
                 (setf list-scroll-index scroll-index
                       list-item-active active
                       list-item-focused focused))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-directory-files files)

      (close-window))))                 ; Close window and OpenGL context

(main)

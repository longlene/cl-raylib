;;;; raylib [core] example - text file loading
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Aanjishnu Bhattacharyya (@NimComPoo-04) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 0 Aanjishnu Bhattacharyya (@NimComPoo-04)
;;;; Common Lisp port of raylib/examples/core/core_text_file_loading.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-text-file-loading
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-text-file-loading)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - text file loading")

    ;; Setting up the camera
    (let* ((cam (make-camera2d :offset (vec2 0.0 0.0) :target (vec2 0.0 0.0) :rotation 0.0 :zoom 1.0))
           ;; Loading text file from resources/text_file.txt
           (file-name "resources/text_file.txt")
           (text (load-file-text file-name)))

      ;; Loading all the text lines
      (multiple-value-bind (lines line-count) (load-text-lines text)
        (let* (;; Stylistic choises
               (font-size 20)
               (text-top (+ 25 font-size)) ; Top of the screen from where the text is rendered
               (wrap-width (- screen-width 20)))

          ;; Wrap the lines as needed
          (dotimes (i line-count)
            (let ((line (aref lines i))
                  (j 0)
                  (last-space 0)        ; Keeping track of last valid space to insert '\n'
                  (last-wrap-start 0))  ; Keeping track of the start of this wrapped line.

              (loop while (<= j (length line))
                    do (when (or (= j (length line)) (char= (char line j) #\Space))
                         ;; Measure the text up to this position (C inserts a '\0' to use MeasureText)
                         ;; Checking if the text has crossed the wrapWidth, then going back and inserting a newline
                         (when (> (measure-text (subseq line last-wrap-start j) font-size) wrap-width)
                           (setf (char line last-space) #\Newline)

                           ;; Since we added a newline the place of wrap changed so we update our lastWrapStart
                           (setf last-wrap-start (1+ last-space)))

                         (setf last-space j)) ; Since we encountered a new space we update our last encountered space location
                       (incf j))))

          ;; Calculating the total height so that we can show a scrollbar
          (let ((text-height 0))

            (dotimes (i line-count)
              (let ((size (measure-text-ex (get-font-default) (aref lines i) (float font-size) 2.0)))
                (incf text-height (+ (truncate (vy size)) 10))))

            ;; A simple scrollbar on the side to show how far we have read into the file
            (let ((scroll-bar (make-rectangle :x (- (float screen-width) 5) :y 0.0 :width 5.0
                                              :height (/ (* screen-height 100.0) (- text-height screen-height))))) ; Scrollbar height is just a percentage

              (set-target-fps 60)
              ;;--------------------------------------------------------------------------------------

              ;; Main game loop
              (loop until (window-should-close) ; Detect window close button or ESC key
                    do ;; Update
                       ;;----------------------------------------------------------------------------------
                       (let ((scroll (get-mouse-wheel-move)))
                         (decf (vy (camera2d-target cam)) (* scroll font-size 1.5))) ; Choosing an arbitrary speed for scroll

                       (when (< (vy (camera2d-target cam)) 0) (setf (vy (camera2d-target cam)) 0.0)) ; Snapping to 0 if we go too far back

                       ;; Ensuring that the camera does not scroll past all text
                       (when (> (vy (camera2d-target cam)) (+ (- text-height screen-height) text-top))
                         (setf (vy (camera2d-target cam)) (float (+ (- text-height screen-height) text-top))))

                       ;; Computing the position of the scrollBar depending on the percentage of text covered
                       (setf (rectangle-y scroll-bar) (lerp (float text-top) (- (float screen-height) (rectangle-height scroll-bar))
                                                            (/ (- (vy (camera2d-target cam)) text-top) (- text-height screen-height))))
                       ;;----------------------------------------------------------------------------------

                       ;; Draw
                       ;;----------------------------------------------------------------------------------
                       (begin-drawing)

                       (clear-background +raywhite+)

                       (begin-mode-2d cam)

                       ;; Going through all the read lines
                       (let ((tt text-top))
                         (dotimes (i line-count)
                           ;; Each time we go through and calculate the height of the text to move the cursor appropriately
                           (let ((size (if (string/= (aref lines i) "")
                                           (measure-text-ex (get-font-default) (aref lines i) (float font-size) 2.0)
                                           ;; Fix for empty line in the text file
                                           (measure-text-ex (get-font-default) " " (float font-size) 2.0))))

                             (draw-text (aref lines i) 10 tt font-size +red+)

                             ;; Inserting extra space for real newlines,
                             ;; wrapped lines are rendered closer together
                             (incf tt (+ (truncate (vy size)) 10)))))

                       (end-mode-2d)

                       ;; Header displaying which file is being read currently
                       (draw-rectangle 0 0 screen-width (- text-top 10) +beige+)
                       (draw-text (text-format "File: %s" file-name) 10 10 font-size +maroon+)

                       (draw-rectangle-rec scroll-bar +maroon+)

                       (end-drawing))
              ;;----------------------------------------------------------------------------------

              ;; De-Initialization
              ;;--------------------------------------------------------------------------------------
              (unload-text-lines lines line-count)
              (unload-file-text text)

              (close-window))))))))     ; Close window and OpenGL context

(main)

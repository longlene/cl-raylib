;;;; text_input_box.lisp - Input Box example
;;;; Translated from raylib/examples/text/text_input_box.c

(require :cl-raylib)

(defpackage :text-input-box
  (:use :cl :cl-raylib))

(in-package :text-input-box)

(defconstant +max-input-chars+ 9)

(defun main ()
  "Main function - input box example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [text] example - input box")

    ;; NOTE: One extra space required for null terminator char
    (let ((name "")
          (letter-count 0)
          (text-box (make-rectangle :x (- (/ screen-width 2.0) 100.0) 
                                   :y 180.0 :width 225.0 :height 50.0))
          (mouse-on-text nil)
          (frames-counter 0))

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        (setf mouse-on-text (check-collision-point-rec (get-mouse-position) text-box))

        (if mouse-on-text
            (progn
              ;; Set the window's cursor to the I-Beam
              (set-mouse-cursor +mouse-cursor-ibeam+)

              ;; Get char pressed (unicode character) on the queue
              (let ((key (get-char-pressed)))
                ;; Check if more characters have been pressed on the same frame
                (loop while (> key 0) do
                  ;; NOTE: Only allow keys in range [32..125]
                  (when (and (>= key 32) (<= key 125) (< letter-count +max-input-chars+))
                    (setf name (concatenate 'string name (string (code-char key))))
                    (incf letter-count))

                  (setf key (get-char-pressed))))

              (when (is-key-pressed +key-backspace+)
                (decf letter-count)
                (when (< letter-count 0) (setf letter-count 0))
                (setf name (subseq name 0 letter-count))))
            
            (set-mouse-cursor +mouse-cursor-default+))

        (if mouse-on-text
            (incf frames-counter)
            (setf frames-counter 0))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          (draw-text "PLACE MOUSE OVER INPUT BOX!" 240 140 20 +gray+)

          (draw-rectangle-rec text-box +lightgray+)
          (if mouse-on-text
              (draw-rectangle-lines (truncate (rectangle-x text-box)) 
                                   (truncate (rectangle-y text-box)) 
                                   (truncate (rectangle-width text-box)) 
                                   (truncate (rectangle-height text-box)) +red+)
              (draw-rectangle-lines (truncate (rectangle-x text-box)) 
                                   (truncate (rectangle-y text-box)) 
                                   (truncate (rectangle-width text-box)) 
                                   (truncate (rectangle-height text-box)) +darkgray+))

          (draw-text name 
                    (+ (truncate (rectangle-x text-box)) 5) 
                    (+ (truncate (rectangle-y text-box)) 8) 
                    40 +maroon+)

          (draw-text (format nil "INPUT CHARS: ~d/~d" letter-count +max-input-chars+) 
                    315 250 20 +darkgray+)

          (when mouse-on-text
            (if (< letter-count +max-input-chars+)
                ;; Draw blinking underscore char
                (when (evenp (truncate frames-counter 20))
                  (draw-text "_" 
                            (+ (truncate (rectangle-x text-box)) 8 (measure-text name 40)) 
                            (+ (truncate (rectangle-y text-box)) 12) 
                            40 +maroon+))
                (draw-text "Press BACKSPACE to delete chars..." 230 300 20 +gray+)))

        (end-drawing)))

    ;; Close window
    (close-window)))

;; Run the example
(main)
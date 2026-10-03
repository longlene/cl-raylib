;;;; text_rectangle_bounds.lisp - Rectangle bounds example
;;;; Translated from raylib/examples/text/text_rectangle_bounds.c

(require :cl-raylib)

(defpackage :text-rectangle-bounds
  (:use :cl :cl-raylib))

(in-package :text-rectangle-bounds)

(defun draw-text-boxed (font text rec font-size spacing word-wrap tint)
  "Draw text using font inside rectangle limits"
  (let ((text-length (text-length text))
        (text-offset-y 0.0)
        (text-offset-x 0.0)
        (scale-factor (/ font-size (font-base-size font)))
        (line-spacing font-size)
        (word-start 0)
        (word-length 0)
        (word-width 0.0)
        (space-width (vx (measure-text-ex font " " font-size spacing))))
    
    (loop for i from 0 below text-length do
      (let ((codepoint (get-codepoint text i)))
        (let ((index (get-glyph-index font codepoint)))
          (cond
            ;; New line
            ((= codepoint 10)
             (if word-wrap
                 (progn
                   (incf text-offset-y line-spacing)
                   (setf text-offset-x 0.0))
                 (progn
                   (incf text-offset-y line-spacing)
                   (setf text-offset-x 0.0))))
            
            ;; Space
            ((= codepoint 32)
             (if (and word-wrap (> (+ text-offset-x word-width) (rectangle-width rec)))
                 (progn
                   (incf text-offset-y line-spacing)
                   (setf text-offset-x 0.0))
                 (progn
                   (setf text-offset-x (+ text-offset-x space-width))
                   (setf word-start (1+ i))
                   (setf word-length 0)
                   (setf word-width 0.0))))
            
            ;; Regular character
            (t
             (if (= word-length 0)
                 (setf word-start i))
             (incf word-length)
             (let ((char-width (if (= (glyph-info-advance-x (aref (font-glyphs font) index)) 0)
                                  (* (rectangle-width (aref (font-recs font) index)) scale-factor)
                                  (* (glyph-info-advance-x (aref (font-glyphs font) index)) scale-factor))))
               (setf word-width (+ word-width char-width)))
             
             ;; Check if we need to wrap
             (when (and word-wrap (> (+ text-offset-x word-width) (rectangle-width rec)))
               (incf text-offset-y line-spacing)
               (setf text-offset-x 0.0))
             
             ;; Draw character if it's within bounds
             (when (and (< (+ text-offset-y font-size) (rectangle-height rec))
                       (>= text-offset-y 0))
               (draw-text-codepoint font codepoint
                                  (vec2 (+ (rectangle-x rec) text-offset-x)
                                        (+ (rectangle-y rec) text-offset-y))
                                  font-size tint))
             
             ;; Advance position
             (let ((char-width (if (= (glyph-info-advance-x (aref (font-glyphs font) index)) 0)
                                  (* (rectangle-width (aref (font-recs font) index)) scale-factor)
                                  (* (glyph-info-advance-x (aref (font-glyphs font) index)) scale-factor))))
               (setf text-offset-x (+ text-offset-x char-width spacing))))))))))

(defun main ()
  "Main function - rectangle bounds example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [text] example - draw text inside a rectangle")

    (let ((text "Text cannot escape	this container	...word wrap also works when active so here's a long text for testing.

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Nec ullamcorper sit amet risus nullam eget felis eget.")
          (resizing nil)
          (word-wrap t)
          (container (make-rectangle :x 25.0 :y 25.0 :width (- screen-width 50.0) :height (- screen-height 250.0)))
          (min-width 60.0)
          (min-height 60.0)
          (max-width (- screen-width 50.0))
          (max-height (- screen-height 160.0))
          (last-mouse (vec2 0.0 0.0))
          (border-color +maroon+)
          (font (get-font-default)))

      (let ((resizer (make-rectangle :x (+ (rectangle-x container) (rectangle-width container) -17.0)
                                    :y (+ (rectangle-y container) (rectangle-height container) -17.0)
                                    :width 14.0
                                    :height 14.0)))

        (set-target-fps 60)

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          (when (is-key-pressed +key-space+)
            (setf word-wrap (not word-wrap)))

          (let ((mouse (get-mouse-position)))
            ;; Check if the mouse is inside the container and toggle border color
            (if (check-collision-point-rec mouse container)
                (setf border-color (fade +maroon+ 0.4))
                (unless resizing
                  (setf border-color +maroon+)))

            ;; Container resizing logic
            (if resizing
                (progn
                  (when (is-mouse-button-released +mouse-button-left+)
                    (setf resizing nil))

                  (let ((width (+ (rectangle-width container) (- (vx mouse) (vx last-mouse)))))
                    (setf (rectangle-width container) 
                          (cond ((> width min-width) (if (< width max-width) width max-width))
                                (t min-width))))

                  (let ((height (+ (rectangle-height container) (- (vy mouse) (vy last-mouse)))))
                    (setf (rectangle-height container) 
                          (cond ((> height min-height) (if (< height max-height) height max-height))
                                (t min-height))))

                  ;; Update resizer position
                  (setf (rectangle-x resizer) (+ (rectangle-x container) (rectangle-width container) -17.0))
                  (setf (rectangle-y resizer) (+ (rectangle-y container) (rectangle-height container) -17.0)))
                (progn
                  ;; Check resizer
                  (when (and (check-collision-point-rec mouse resizer)
                            (is-mouse-button-pressed +mouse-button-left+))
                    (setf resizing t)
                    (setf border-color +red+))))

            (setf last-mouse mouse))

          ;; Draw
          (begin-drawing)
            (clear-background +raywhite+)

            (draw-rectangle-lines-ex container 3 border-color)

            ;; Draw the text inside the container
            (draw-text-boxed font text container 20 1 word-wrap +gray+)

            ;; Draw the resizer
            (draw-rectangle-rec resizer border-color)

            ;; Draw bottom info
            (draw-rectangle 0.0 (+ (rectangle-y container) (rectangle-height container) 54.0) (float screen-width) 60.0 +gray+)
            (draw-rectangle-lines 0.0 (+ (rectangle-y container) (rectangle-height container) 54.0) (float screen-width) 60.0 +darkgray+)
            (draw-text "Word Wrap: " 218 (+ (rectangle-y container) (rectangle-height container) 63) 20 +black+)
            (draw-text (if word-wrap "ON" "OFF") 313 (+ (rectangle-y container) (rectangle-height container) 63) 20 +red+)
            (draw-text "Press [SPACE] to toggle word wrap" 218 (+ (rectangle-y container) (rectangle-height container) 86) 20 +gray+)
            (draw-text "Click hold & drag the    to resize the container" 155 (+ (rectangle-y container) (rectangle-height container) 110) 20 +gray+)
            (draw-rectangle (float (+ 354 (measure-text "Click hold & drag the " 20))) (+ (rectangle-y container) (rectangle-height container) 108.0) 12.0 12.0 border-color)

          (end-drawing))))

    ;; Close window
    (close-window)))

;; Run the example
(main)

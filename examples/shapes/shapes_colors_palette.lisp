(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defconstant +max-colors-count+ 21 "Number of colors available")

(defun main ()
  "raylib [shapes] example - Colors palette"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [shapes] example - colors palette")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      (let ((colors (list :darkgray :maroon :orange :darkgreen :darkblue :darkpurple :darkbrown
                          :gray :red :gold :lime :blue :violet :brown :lightgray :pink :yellow
                          :green :skyblue :purple :beige))
            (color-names (list "DARKGRAY" "MAROON" "ORANGE" "DARKGREEN" "DARKBLUE" "DARKPURPLE"
                               "DARKBROWN" "GRAY" "RED" "GOLD" "LIME" "BLUE" "VIOLET" "BROWN"
                               "LIGHTGRAY" "PINK" "YELLOW" "GREEN" "SKYBLUE" "PURPLE" "BEIGE"))
            (colors-recs (make-array +max-colors-count+))
            (color-state (make-array +max-colors-count+ :initial-element 0)))

        ;; Fill colorsRecs data (for every rectangle)
        (loop for i from 0 below +max-colors-count+ do
          (setf (aref colors-recs i)
                (make-rectangle :x (+ 20.0 (* 100.0 (mod i 7)) (* 10.0 (mod i 7)))
                                :y (+ 80.0 (* 100.0 (floor i 7)) (* 10.0 (floor i 7)))
                                :width 100.0
                                :height 100.0)))

        (loop
          until (window-should-close) ; Detect window close button or ESC key
          do (progn
               ;; Update
               (let ((mouse-point (get-mouse-position)))
                 (loop for i from 0 below +max-colors-count+ do
                   (if (check-collision-point-rec mouse-point (aref colors-recs i))
                       (setf (aref color-state i) 1)
                       (setf (aref color-state i) 0))))

               ;; Draw
               (with-drawing
                 (clear-background :raywhite)

                 (draw-text "raylib colors palette" 28 42 20 :black)
                 (draw-text "press SPACE to see all colors" 
                            (- (get-screen-width) 180) 
                            (- (get-screen-height) 40) 
                            10 :gray)

                 ;; Draw all rectangles
                 (loop for i from 0 below +max-colors-count+ do
                   (let ((color (nth i colors))
                         (color-name (nth i color-names))
                         (rect (aref colors-recs i))
                         (state (aref color-state i)))
                     
                     (draw-rectangle-rec rect (fade color (if (= state 1) 0.6 1.0)))

                     (when (or (is-key-down :key-space) (= state 1))
                       (draw-rectangle (floor (rectangle-x rect))
                                       (floor (+ (rectangle-y rect) (rectangle-height rect) -26))
                                       (floor (rectangle-width rect))
                                       20 :black)
                       (draw-rectangle-lines-ex rect 6 (fade :black 0.3))
                       (draw-text color-name
                                  (floor (+ (rectangle-x rect) 
                                            (rectangle-width rect) 
                                            (- (measure-text color-name 10)) 
                                            -12))
                                  (floor (+ (rectangle-y rect) 
                                            (rectangle-height rect) 
                                            -20))
                                  10 color)))))))))))

(main)
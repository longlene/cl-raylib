;;;; text_unicode.lisp - Unicode text example
;;;; Translated from raylib/examples/text/text_unicode.c

(require :cl-raylib)

(defpackage :text-unicode
  (:use :cl :cl-raylib))

(in-package :text-unicode)

(defconstant +emoji-per-width+ 8)
(defconstant +emoji-per-height+ 4)

;; Sample emoji codepoints (simplified subset)
(defparameter *emoji-codepoints* 
  (vector #x1F300 #x1F600 #x1F602 #x1F923 #x1F603 #x1F606 #x1F609 #x1F60B
          #x1F60E #x1F60D #x1F618 #x1F617 #x1F619 #x1F61A #x1F642 #x1F917
          #x1F929 #x1F914 #x1F928 #x1F610 #x1F611 #x1F636 #x1F644 #x1F60F
          #x1F623 #x1F625 #x1F62E #x1F910 #x1F62F #x1F62A #x1F62B #x1F634))

;; Sample multilingual messages
(defparameter *messages*
  (vector 
    (cons "Falsches Üben von Xylophonmusik quält jeden größeren Zwerg" "German")
    (cons "Beiß nicht in die Hand, die dich füttert." "German")
    (cons "Я люблю raylib!" "Russian")
    (cons "Îți mulțumesc că ai ales raylib. Și sper să ai o zi bună!" "Romanian")
    (cons "Jeżu klątw, spłódź Finom część gry hańb!" "Polish")
    (cons "こんにちは世界！" "Japanese")
    (cons "你好世界！" "Chinese")
    (cons "안녕하세요 세계!" "Korean")))

(defun main ()
  "Main function - unicode text example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [text] example - Unicode")

    ;; Load font supporting Unicode characters
    (let ((font (load-font-ex "resources/DotGothic16-Regular.ttf" 32 
                             *emoji-codepoints*)))  ; Load with emoji support
      
      (let ((hovered -1)
            (selected -1)
            (scroll-index 0)
            (font-size 20.0)
            (show-languages nil))

        (set-target-fps 60)

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          (let ((mouse-pos (get-mouse-position))
                (wheel-move (get-mouse-wheel-move)))
            
            ;; Font size adjustment
            (when (> wheel-move 0) (incf font-size 2.0))
            (when (< wheel-move 0) (decf font-size 2.0))
            (when (< font-size 10.0) (setf font-size 10.0))
            (when (> font-size 50.0) (setf font-size 50.0))

            ;; Toggle language display
            (when (is-key-pressed +key-space+)
              (setf show-languages (not show-languages)))

            ;; Scroll through messages
            (when (is-key-pressed +key-up+)
              (when (> scroll-index 0) (decf scroll-index)))
            (when (is-key-pressed +key-down+)
              (when (< scroll-index (1- (length *messages*))) (incf scroll-index)))

            ;; Check emoji hover
            (setf hovered -1)
            (when (>= (vy mouse-pos) 50)
              (let ((emoji-x (truncate (/ (vx mouse-pos) (/ screen-width +emoji-per-width+))))
                    (emoji-y (truncate (/ (- (vy mouse-pos) 50) (/ (- screen-height 200) +emoji-per-height+)))))
                (when (and (>= emoji-x 0) (< emoji-x +emoji-per-width+)
                          (>= emoji-y 0) (< emoji-y +emoji-per-height+))
                  (let ((emoji-index (+ (* emoji-y +emoji-per-width+) emoji-x)))
                    (when (< emoji-index (length *emoji-codepoints*))
                      (setf hovered emoji-index))))))

            ;; Select emoji on click
            (when (and (>= hovered 0) (is-mouse-button-pressed +mouse-button-left+))
              (setf selected hovered)))

          ;; Draw
          (begin-drawing)
            (clear-background +raywhite+)

            ;; Draw title
            (draw-text "raylib [text] example - Unicode support" 20 10 20 +darkgray+)

            ;; Draw emojis grid
            (let ((emoji-size (/ screen-width +emoji-per-width+)))
              (dotimes (y +emoji-per-height+)
                (dotimes (x +emoji-per-width+)
                  (let ((emoji-index (+ (* y +emoji-per-width+) x)))
                    (when (< emoji-index (length *emoji-codepoints*))
                      (let ((emoji-x (* x emoji-size))
                            (emoji-y (+ 50 (* y (/ (- screen-height 200) +emoji-per-height+))))
                            (color (cond ((= emoji-index selected) +red+)
                                        ((= emoji-index hovered) +lightgray+)
                                        (t +gray+))))
                        
                        ;; Draw emoji background
                        (draw-rectangle (truncate emoji-x) (truncate emoji-y) 
                                       (truncate emoji-size) (truncate emoji-size) 
                                       color)
                        
                        ;; Draw emoji (simplified - just draw the codepoint as hex)
                        (draw-text (format nil "U+~4,'0X" (aref *emoji-codepoints* emoji-index))
                                  (+ (truncate emoji-x) 5) (+ (truncate emoji-y) 15) 
                                  12 +black+)))))))

            ;; Draw multilingual text samples
            (let ((text-y (+ 50 (/ (- screen-height 200) +emoji-per-height+) (* +emoji-per-height+ (/ (- screen-height 200) +emoji-per-height+)) 20)))
              (draw-text "Multilingual text samples:" 20 (truncate text-y) 16 +darkgray+)
              (incf text-y 25)
              
              (dotimes (i (min 3 (length *messages*)))
                (let ((msg-index (mod (+ scroll-index i) (length *messages*))))
                  (when (< msg-index (length *messages*))
                    (let ((message (car (aref *messages* msg-index)))
                          (language (cdr (aref *messages* msg-index))))
                      (draw-text-ex font message 
                                   (vec2 20.0 text-y) 
                                   font-size 1.0 +black+)
                      (when show-languages
                        (draw-text (format nil "[~a]" language) 
                                  (+ 20 (truncate (vx (measure-text-ex font message font-size 1.0))) 10)
                                  (truncate text-y) 12 +gray+))
                      (incf text-y (+ font-size 5)))))))

            ;; Draw instructions
            (draw-text "Use mouse wheel to adjust font size" 20 (- screen-height 80) 12 +darkgray+)
            (draw-text "Use UP/DOWN arrows to scroll messages" 20 (- screen-height 65) 12 +darkgray+)
            (draw-text "Press SPACE to toggle language labels" 20 (- screen-height 50) 12 +darkgray+)
            (draw-text "Click on emojis to select them" 20 (- screen-height 35) 12 +darkgray+)
            
            ;; Draw selected emoji info
            (when (>= selected 0)
              (draw-text (format nil "Selected emoji: U+~4,'0X" (aref *emoji-codepoints* selected))
                        20 (- screen-height 20) 12 +red+))

            ;; Draw font size info
            (draw-text (format nil "Font size: ~,1f" font-size) 
                      (- screen-width 150) (- screen-height 20) 12 +darkgray+)

          (end-drawing))

        ;; De-Initialization
        (unload-font font)))

    ;; Close window
    (close-window)))

;; Run the example
(main)
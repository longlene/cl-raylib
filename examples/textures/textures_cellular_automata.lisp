;;;; raylib [textures] example - cellular automata
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.6, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Jordi Santonja (@JordSant) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Jordi Santonja (@JordSant)
;;;; Common Lisp port of raylib/examples/textures/textures_cellular_automata.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-cellular-automata
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-cellular-automata)

;; Initialization constants
(defconstant +screen-width+ 800)
(defconstant +screen-height+ 450)
(defconstant +image-width+ 800)
(defconstant +image-height+ (truncate 800 2))

;; Rule button sizes and positions
(defconstant +draw-rule-start-x+ 585)
(defconstant +draw-rule-start-y+ 10)
(defconstant +draw-rule-spacing+ 15)
(defconstant +draw-rule-group-spacing+ 50)
(defconstant +draw-rule-size+ 14)
(defconstant +draw-rule-inner-size+ 10)

;; Preset button sizes
(defconstant +presets-size-x+ 42)
(defconstant +presets-size-y+ 22)

(defconstant +lines-updated-per-frame+ 4)

;; Functions
(defun compute-line (image line rule)
  ;; Compute next line pixels. Boundaries are not computed, always 0
  (loop for i from 1 below (1- +image-width+)
        do (let* (;; Get, from the previous line, the 3 pixels states as a binary value
                  (prev-value (+ (if (< (first (get-image-color image (1- i) (1- line))) 5) 4 0) ; Left pixel
                                 (if (< (first (get-image-color image i (1- line))) 5) 2 0)      ; Center pixel
                                 (if (< (first (get-image-color image (1+ i) (1- line))) 5) 1 0))) ; Right pixel
                  ;; Get next value from rule bitmask
                  (curr-value (logbitp prev-value rule)))
             ;; Update pixel color
             (image-draw-pixel image i line (if curr-value +black+ +raywhite+)))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (init-window +screen-width+ +screen-height+ "raylib [textures] example - cellular automata")

  ;; Image that contains the cellular automaton
  (let ((image (gen-image-color +image-width+ +image-height+ +raywhite+)))
    ;; The top central pixel set as black
    (image-draw-pixel image (truncate +image-width+ 2) 0 +black+)

    (let* ((texture (load-texture-from-image image))
           ;; Some interesting rules
           (preset-values (vector 18 30 60 86 102 124 126 150 182 225))
           (presets-count (length preset-values))
           ;; Variables
           (rule 30)                    ; Starting rule
           (line 1))                    ; Line to compute, starting from line 1. One point in line 0 is already set

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Handle mouse
               (let ((mouse (get-mouse-position))
                     (mouse-in-cell -1))  ; -1: outside any button; 0-7: rule cells; 8+: preset cells

                 ;; Check mouse on rule cells
                 (dotimes (i 8)
                   (let ((cell-x (+ (- +draw-rule-start-x+ (* +draw-rule-group-spacing+ i)) +draw-rule-spacing+))
                         (cell-y (+ +draw-rule-start-y+ +draw-rule-spacing+)))
                     (when (and (>= (vx mouse) cell-x) (<= (vx mouse) (+ cell-x +draw-rule-size+))
                                (>= (vy mouse) cell-y) (<= (vy mouse) (+ cell-y +draw-rule-size+)))
                       (setf mouse-in-cell i) ; 0-7: rule cells
                       (return))))

                 ;; Check mouse on preset cells
                 (when (< mouse-in-cell 0)
                   (dotimes (i presets-count)
                     (let ((cell-x (+ 4 (* (+ +presets-size-x+ 2) (truncate i 2))))
                           (cell-y (+ 2 (* (+ +presets-size-y+ 2) (mod i 2)))))
                       (when (and (>= (vx mouse) cell-x) (<= (vx mouse) (+ cell-x +presets-size-x+))
                                  (>= (vy mouse) cell-y) (<= (vy mouse) (+ cell-y +presets-size-y+)))
                         (setf mouse-in-cell (+ i 8)) ; 8+: preset cells
                         (return)))))

                 (when (and (is-mouse-button-pressed +mouse-button-left+) (>= mouse-in-cell 0))
                   ;; Rule changed both by selecting a preset or toggling a bit
                   (if (< mouse-in-cell 8)
                       (setf rule (logxor rule (ash 1 mouse-in-cell)))
                       (setf rule (aref preset-values (- mouse-in-cell 8))))

                   ;; Reset image
                   (image-clear-background image +raywhite+)
                   (image-draw-pixel image (truncate +image-width+ 2) 0 +black+)
                   (setf line 1))

                 ;; Compute next lines
                 (when (< line +image-height+)
                   (loop for i from 0
                         while (and (< i +lines-updated-per-frame+) (< (+ line i) +image-height+))
                         do (compute-line image (+ line i) rule))
                   (incf line +lines-updated-per-frame+)
                   (update-texture texture (image-data image)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 ;; Draw cellular automaton texture
                 (draw-texture texture 0 (- +screen-height+ +image-height+) +white+)

                 ;; Draw preset values
                 (dotimes (i presets-count)
                   (draw-text (text-format "%i" (aref preset-values i)) (+ 8 (* (+ +presets-size-x+ 2) (truncate i 2))) (+ 4 (* (+ +presets-size-y+ 2) (mod i 2))) 20 +gray+)
                   (draw-rectangle-lines (+ 4 (* (+ +presets-size-x+ 2) (truncate i 2))) (+ 2 (* (+ +presets-size-y+ 2) (mod i 2))) +presets-size-x+ +presets-size-y+ +blue+)

                   ;; If the mouse is on this preset, highlight it
                   (when (= mouse-in-cell (+ i 8))
                     (draw-rectangle-lines-ex (make-rectangle :x (+ 2 (* (+ +presets-size-x+ 2.0) (truncate i 2)))
                                                              :y (* (+ +presets-size-y+ 2.0) (mod i 2))
                                                              :width (+ +presets-size-x+ 4.0) :height (+ +presets-size-y+ 4.0))
                                              3.0 +red+)))

                 ;; Draw rule bits
                 (dotimes (i 8)
                   ;; The three input bits
                   (dotimes (j 3)
                     (draw-rectangle-lines (+ (- +draw-rule-start-x+ (* +draw-rule-group-spacing+ i)) (* +draw-rule-spacing+ j)) +draw-rule-start-y+ +draw-rule-size+ +draw-rule-size+ +gray+)
                     (when (logtest i (ash 4 (- j)))
                       (draw-rectangle (+ (- (+ +draw-rule-start-x+ 2) (* +draw-rule-group-spacing+ i)) (* +draw-rule-spacing+ j)) (+ +draw-rule-start-y+ 2) +draw-rule-inner-size+ +draw-rule-inner-size+ +black+)))

                   ;; The output bit
                   (draw-rectangle-lines (+ (- +draw-rule-start-x+ (* +draw-rule-group-spacing+ i)) +draw-rule-spacing+) (+ +draw-rule-start-y+ +draw-rule-spacing+) +draw-rule-size+ +draw-rule-size+ +blue+)
                   (when (logbitp i rule)
                     (draw-rectangle (+ (- (+ +draw-rule-start-x+ 2) (* +draw-rule-group-spacing+ i)) +draw-rule-spacing+) (+ +draw-rule-start-y+ 2 +draw-rule-spacing+) +draw-rule-inner-size+ +draw-rule-inner-size+ +black+))

                   ;; If the mouse is on this rule bit, highlight it
                   (when (= mouse-in-cell i)
                     (draw-rectangle-lines-ex (make-rectangle :x (- (+ (- +draw-rule-start-x+ (* +draw-rule-group-spacing+ i)) +draw-rule-spacing+) 2.0)
                                                              :y (- (+ +draw-rule-start-y+ +draw-rule-spacing+) 2.0)
                                                              :width (+ +draw-rule-size+ 4.0) :height (+ +draw-rule-size+ 4.0))
                                              3.0 +red+)))

                 (draw-text (text-format "RULE: %i" rule) (+ +draw-rule-start-x+ (* +draw-rule-spacing+ 4)) (+ +draw-rule-start-y+ 1) 30 +gray+)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-image image)
      (unload-texture texture)

      (close-window))))                 ; Close window and OpenGL context

(main)

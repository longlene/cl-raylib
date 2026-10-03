;;;; raylib [core] example - undo redo
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_undo_redo.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-undo-redo
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-undo-redo)

(defconstant +max-undo-states+ 26)      ; Maximum undo states supported for the ring buffer

(defconstant +grid-cell-size+ 24)
(defconstant +max-grid-cells-x+ 30)
(defconstant +max-grid-cells-y+ 13)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Player state struct
;; NOTE: Contains all player data that needs to be affected by undo/redo
;; cell: Point struct, like Vector2 but using int (x y)
(defstruct player-state
  (cell (list 0 0))
  (color (list 0 0 0 0)))

(defun copy-state (state)
  (make-player-state :cell (copy-list (player-state-cell state)) :color (copy-list (player-state-color state))))

(defun state-equal (a b)
  "memcmp() of two PlayerState"
  (and (equal (player-state-cell a) (player-state-cell b)) (equal (player-state-color a) (player-state-color b))))

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Draw undo system visualization logic
;; NOTE: Visualizing the ring buffer array, every square can store a player state
(defun draw-undo-buffer (position first-undo-index last-undo-index current-undo-index slot-size)
  (let ((px (truncate (vx position))) (py (truncate (vy position))))
    (flet ((slot (i fill line)
             (draw-rectangle (+ px (* slot-size i)) py slot-size slot-size fill)
             (draw-rectangle-lines (+ px (* slot-size i)) py slot-size slot-size line)))
      ;; Draw index marks
      (draw-rectangle (+ px 8 (* slot-size current-undo-index)) (- py 10) 8 8 +red+)
      (draw-rectangle-lines (+ px 2 (* slot-size first-undo-index)) (+ py 27) 8 8 +black+)
      (draw-rectangle (+ px 14 (* slot-size last-undo-index)) (+ py 27) 8 8 +black+)

      ;; Draw background gray slots
      (dotimes (i +max-undo-states+) (slot i +lightgray+ +gray+))

      ;; Draw occupied slots: firstUndoIndex --> lastUndoIndex
      (cond ((<= first-undo-index last-undo-index)
             (loop for i from first-undo-index below (1+ last-undo-index) do (slot i +skyblue+ +blue+)))
            ((< last-undo-index first-undo-index)
             (loop for i from first-undo-index below +max-undo-states+ do (slot i +skyblue+ +blue+))
             (loop for i from 0 below (1+ last-undo-index) do (slot i +skyblue+ +blue+))))

      ;; Draw occupied slots: firstUndoIndex --> currentUndoIndex
      (cond ((< first-undo-index current-undo-index)
             (loop for i from first-undo-index below current-undo-index do (slot i +green+ +lime+)))
            ((< current-undo-index first-undo-index)
             (loop for i from first-undo-index below +max-undo-states+ do (slot i +green+ +lime+))
             (loop for i from 0 below current-undo-index do (slot i +green+ +lime+))))

      ;; Draw current selected UNDO slot
      (slot current-undo-index +gold+ +orange+))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    ;; We have multiple options to implement an Undo/Redo system
    ;; Probably the most professional one is using the Command pattern to
    ;; define Actions and store those actions into an array as the events happen,
    ;; raylib internal Automation System actually uses a similar approach,
    ;; but in this example we are using another more simple solution,
    ;; just record PlayerState changes when detected, checking for changes every certain frames
    ;; This approach requires more memory and is more performance costly but it is quite simple to implement

    (init-window screen-width screen-height "raylib [core] example - undo redo")

    (let* (;; Undo/redo system variables
           (current-undo-index 0)
           (first-undo-index 0)
           (last-undo-index 0)
           (undo-frame-counter 0)
           (undo-info-pos (vec2 110.0 400.0))
           ;; Init current player state and undo/redo recorded states array
           (player (make-player-state :cell (list 10 10) :color (copy-list +red+)))
           ;; Init undo buffer to store MAX_UNDO_STATES states
           ;; Init all undo states to current state
           (states (let ((v (make-array +max-undo-states+)))
                     (dotimes (i +max-undo-states+ v) (setf (aref v i) (copy-state player)))))
           ;; Grid variables
           (grid-position (vec2 40.0 60.0)))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Player movement logic
               (let ((cell (player-state-cell player)))
                 (cond ((is-key-pressed +key-right+) (incf (first cell)))
                       ((is-key-pressed +key-left+) (decf (first cell)))
                       ((is-key-pressed +key-up+) (decf (second cell)))
                       ((is-key-pressed +key-down+) (incf (second cell))))

                 ;; Make sure player does not go out of bounds
                 (cond ((< (first cell) 0) (setf (first cell) 0))
                       ((>= (first cell) +max-grid-cells-x+) (setf (first cell) (1- +max-grid-cells-x+))))
                 (cond ((< (second cell) 0) (setf (second cell) 0))
                       ((>= (second cell) +max-grid-cells-y+) (setf (second cell) (1- +max-grid-cells-y+)))))

               ;; Player color change logic
               (when (is-key-pressed +key-space+)
                 (setf (first (player-state-color player)) (logand (get-random-value 20 255) #xff)
                       (second (player-state-color player)) (logand (get-random-value 20 220) #xff)
                       (third (player-state-color player)) (logand (get-random-value 20 240) #xff)))

               ;; Undo state change logic
               (incf undo-frame-counter)

               ;; Waiting a number of frames before checking if we should store a new state snapshot
               (when (>= undo-frame-counter 2) ; Checking every 2 frames
                 (unless (state-equal (aref states current-undo-index) player)
                   ;; Move cursor to next available position of the undo ring buffer to record state
                   (incf current-undo-index)
                   (when (>= current-undo-index +max-undo-states+) (setf current-undo-index 0))
                   (when (= current-undo-index first-undo-index) (incf first-undo-index))
                   (when (>= first-undo-index +max-undo-states+) (setf first-undo-index 0))

                   (setf (aref states current-undo-index) (copy-state player))

                   (setf last-undo-index current-undo-index))

                 (setf undo-frame-counter 0))

               ;; Recover previous state from buffer: CTRL+Z
               (when (and (is-key-down +key-left-control+) (is-key-pressed +key-z+))
                 (when (/= current-undo-index first-undo-index)
                   (decf current-undo-index)
                   (when (< current-undo-index 0) (setf current-undo-index (1- +max-undo-states+)))

                   (unless (state-equal (aref states current-undo-index) player)
                     (setf player (copy-state (aref states current-undo-index))))))

               ;; Recover next state from buffer: CTRL+Y
               (when (and (is-key-down +key-left-control+) (is-key-pressed +key-y+))
                 (when (/= current-undo-index last-undo-index)
                   (let ((next-undo-index (1+ current-undo-index)))
                     (when (>= next-undo-index +max-undo-states+) (setf next-undo-index 0))

                     (when (/= next-undo-index first-undo-index)
                       (setf current-undo-index next-undo-index)

                       (unless (state-equal (aref states current-undo-index) player)
                         (setf player (copy-state (aref states current-undo-index))))))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Draw controls info
               (draw-text "[ARROWS] MOVE PLAYER - [SPACE] CHANGE PLAYER COLOR" 40 20 20 +darkgray+)

               ;; Draw player visited cells recorded by undo
               ;; NOTE: Remember we are using a ring buffer approach so,
               ;; some cells info could start at the end of the array and end at the beginning
               (flet ((cell-rec (i)
                        (let ((cell (player-state-cell (aref states i))))
                          (draw-rectangle-rec (make-rectangle :x (+ (vx grid-position) (* (first cell) +grid-cell-size+))
                                                              :y (+ (vy grid-position) (* (second cell) +grid-cell-size+))
                                                              :width (float +grid-cell-size+) :height (float +grid-cell-size+))
                                              +lightgray+)))
                      (cell-int (i)
                        (let ((cell (player-state-cell (aref states i))))
                          (draw-rectangle (+ (truncate (vx grid-position)) (* (first cell) +grid-cell-size+))
                                          (+ (truncate (vy grid-position)) (* (second cell) +grid-cell-size+))
                                          +grid-cell-size+ +grid-cell-size+ +lightgray+))))
                 (cond ((> last-undo-index first-undo-index)
                        (loop for i from first-undo-index below current-undo-index do (cell-rec i)))
                       ((> first-undo-index last-undo-index)
                        (if (and (< current-undo-index +max-undo-states+) (> current-undo-index last-undo-index))
                            (loop for i from first-undo-index below current-undo-index do (cell-rec i))
                            (progn
                              (loop for i from first-undo-index below +max-undo-states+ do (cell-int i))
                              (loop for i from 0 below current-undo-index do (cell-int i)))))))

               ;; Draw game grid
               (let ((gx (truncate (vx grid-position))) (gy (truncate (vy grid-position))))
                 (loop for y from 0 to +max-grid-cells-y+
                       do (draw-line gx (+ gy (* y +grid-cell-size+))
                                     (+ gx (* +max-grid-cells-x+ +grid-cell-size+)) (+ gy (* y +grid-cell-size+)) +gray+))
                 (loop for x from 0 to +max-grid-cells-x+
                       do (draw-line (+ gx (* x +grid-cell-size+)) gy
                                     (+ gx (* x +grid-cell-size+)) (+ gy (* +max-grid-cells-y+ +grid-cell-size+)) +gray+))

                 ;; Draw player
                 (draw-rectangle (+ gx (* (first (player-state-cell player)) +grid-cell-size+))
                                 (+ gy (* (second (player-state-cell player)) +grid-cell-size+))
                                 (1+ +grid-cell-size+) (1+ +grid-cell-size+) (player-state-color player)))

               ;; Draw undo system buffer info
               (draw-text "UNDO STATES:" (- (truncate (vx undo-info-pos)) 85) (+ (truncate (vy undo-info-pos)) 9) 10 +darkgray+)
               (draw-undo-buffer undo-info-pos first-undo-index last-undo-index current-undo-index 24)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)

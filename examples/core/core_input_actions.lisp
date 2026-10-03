;;;; raylib [core] example - input actions
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Jett (@JettMonstersGoBoom) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2025 Jett (@JettMonstersGoBoom)
;;;; Common Lisp port of raylib/examples/core/core_input_actions.c

;; Simple example for decoding input as actions, allowing remapping of input to different keys or gamepad buttons
;; For example instead of using `IsKeyDown(KEY_LEFT)`, you can use `IsActionDown(ACTION_LEFT)`
;; which can be reassigned to e.g. KEY_A and also assigned to a gamepad button. the action will trigger with either gamepad or keys

(require :cl-raylib)

(defpackage #:raylib-examples/core-input-actions
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-input-actions)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; ActionType
(defconstant +no-action+ 0)
(defconstant +action-up+ 1)
(defconstant +action-down+ 2)
(defconstant +action-left+ 3)
(defconstant +action-right+ 4)
(defconstant +action-fire+ 5)
(defconstant +max-action+ 6)

;; Key and button inputs
(defstruct action-input
  (key 0)
  (button 0))

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
(defparameter *gamepad-index* 0)    ; Gamepad default index
(defparameter *action-inputs* (let ((v (make-array +max-action+)))
                                (dotimes (i +max-action+ v) (setf (aref v i) (make-action-input)))))

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
;; Check action key/button pressed
;; NOTE: Combines key pressed and gamepad button pressed in one action
(defun is-action-pressed (action)
  (and (< action +max-action+)
       (or (is-key-pressed (action-input-key (aref *action-inputs* action)))
           (is-gamepad-button-pressed *gamepad-index* (action-input-button (aref *action-inputs* action))))))

;; Check action key/button released
;; NOTE: Combines key released and gamepad button released in one action
(defun is-action-released (action)
  (and (< action +max-action+)
       (or (is-key-released (action-input-key (aref *action-inputs* action)))
           (is-gamepad-button-released *gamepad-index* (action-input-button (aref *action-inputs* action))))))

;; Check action key/button down
;; NOTE: Combines key down and gamepad button down in one action
(defun is-action-down (action)
  (and (< action +max-action+)
       (or (is-key-down (action-input-key (aref *action-inputs* action)))
           (is-gamepad-button-down *gamepad-index* (action-input-button (aref *action-inputs* action))))))

(defun set-action (action key button)
  (setf (action-input-key (aref *action-inputs* action)) key
        (action-input-button (aref *action-inputs* action)) button))

;; Set the "default" keyset
;; NOTE: Here WASD and gamepad buttons on the left side for movement
(defun set-actions-default ()
  (set-action +action-up+ +key-w+ +gamepad-button-left-face-up+)
  (set-action +action-down+ +key-s+ +gamepad-button-left-face-down+)
  (set-action +action-left+ +key-a+ +gamepad-button-left-face-left+)
  (set-action +action-right+ +key-d+ +gamepad-button-left-face-right+)
  (set-action +action-fire+ +key-space+ +gamepad-button-right-face-down+))

;; Set the "alternate" keyset
;; NOTE: Here cursor keys and gamepad buttons on the right side for movement
(defun set-actions-cursor ()
  (set-action +action-up+ +key-up+ +gamepad-button-right-face-up+)
  (set-action +action-down+ +key-down+ +gamepad-button-right-face-down+)
  (set-action +action-left+ +key-left+ +gamepad-button-right-face-left+)
  (set-action +action-right+ +key-right+ +gamepad-button-right-face-right+)
  (set-action +action-fire+ +key-space+ +gamepad-button-left-face-down+))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - input actions")

    ;; Set default actions
    (let ((action-set 0)
          (release-action nil)
          (position (vec2 400.0 200.0))
          (size (vec2 40.0 40.0)))
      (set-actions-default)

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf *gamepad-index* 0) ; Set gamepad being checked

               (when (is-action-down +action-up+) (decf (vy position) 2))
               (when (is-action-down +action-down+) (incf (vy position) 2))
               (when (is-action-down +action-left+) (decf (vx position) 2))
               (when (is-action-down +action-right+) (incf (vx position) 2))
               (when (is-action-pressed +action-fire+)
                 (setf (vx position) (/ (- screen-width (vx size)) 2)
                       (vy position) (/ (- screen-height (vy size)) 2)))

               ;; Register release action for one frame
               (setf release-action nil)
               (when (is-action-released +action-fire+) (setf release-action t))

               ;; Switch control scheme by pressing TAB
               (when (is-key-pressed +key-tab+)
                 (setf action-set (if (= action-set 0) 1 0))
                 (if (= action-set 0) (set-actions-default) (set-actions-cursor)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +gray+)

               (draw-rectangle-v position size (if release-action +blue+ +red+))

               (draw-text (if (= action-set 0) "Current input set: WASD (default)" "Current input set: Arrow keys") 10 10 20 +white+)
               (draw-text "Use TAB key to toggles Actions keyset" 10 50 20 +green+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)

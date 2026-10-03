;;;; raylib [core] example - keyboard testbed
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; NOTE: raylib defined keys refer to ENG-US Keyboard layout,
;;;; mapping to other layouts is up to the user
;;;;
;;;; Example originally created with raylib 5.6, last time updated with raylib 5.6
;;;;
;;;; Copyright (c) 2026 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_keyboard_testbed.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-keyboard-testbed
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-keyboard-testbed)

(defconstant +key-rec-spacing+ 4)       ; Space in pixels between key rectangles

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Get keyboard keycode as text (US keyboard)
;; NOTE: Mapping for other keyboard layouts can be done here
(defparameter *key-texts*
  (list (cons +key-apostrophe+ "'") (cons +key-comma+ ",") (cons +key-minus+ "-") (cons +key-period+ ".")
        (cons +key-slash+ "/") (cons +key-zero+ "0") (cons +key-one+ "1") (cons +key-two+ "2") (cons +key-three+ "3")
        (cons +key-four+ "4") (cons +key-five+ "5") (cons +key-six+ "6") (cons +key-seven+ "7") (cons +key-eight+ "8")
        (cons +key-nine+ "9") (cons +key-semicolon+ ";") (cons +key-equal+ "=")
        (cons +key-a+ "A") (cons +key-b+ "B") (cons +key-c+ "C") (cons +key-d+ "D") (cons +key-e+ "E") (cons +key-f+ "F")
        (cons +key-g+ "G") (cons +key-h+ "H") (cons +key-i+ "I") (cons +key-j+ "J") (cons +key-k+ "K") (cons +key-l+ "L")
        (cons +key-m+ "M") (cons +key-n+ "N") (cons +key-o+ "O") (cons +key-p+ "P") (cons +key-q+ "Q") (cons +key-r+ "R")
        (cons +key-s+ "S") (cons +key-t+ "T") (cons +key-u+ "U") (cons +key-v+ "V") (cons +key-w+ "W") (cons +key-x+ "X")
        (cons +key-y+ "Y") (cons +key-z+ "Z")
        (cons +key-left-bracket+ "[") (cons +key-backslash+ "\\") (cons +key-right-bracket+ "]") (cons +key-grave+ "`")
        (cons +key-space+ "SPACE") (cons +key-escape+ "ESC") (cons +key-enter+ "ENTER") (cons +key-tab+ "TAB")
        (cons +key-backspace+ "BACK") (cons +key-insert+ "INS") (cons +key-delete+ "DEL") (cons +key-right+ "RIGHT")
        (cons +key-left+ "LEFT") (cons +key-down+ "DOWN") (cons +key-up+ "UP") (cons +key-page-up+ "PGUP")
        (cons +key-page-down+ "PGDOWN") (cons +key-home+ "HOME") (cons +key-end+ "END") (cons +key-caps-lock+ "CAPS")
        (cons +key-scroll-lock+ "LOCK") (cons +key-num-lock+ "NUMLOCK") (cons +key-print-screen+ "PRINTSCR")
        (cons +key-pause+ "PAUSE")
        (cons +key-f1+ "F1") (cons +key-f2+ "F2") (cons +key-f3+ "F3") (cons +key-f4+ "F4") (cons +key-f5+ "F5")
        (cons +key-f6+ "F6") (cons +key-f7+ "F7") (cons +key-f8+ "F8") (cons +key-f9+ "F9") (cons +key-f10+ "F10")
        (cons +key-f11+ "F11") (cons +key-f12+ "F12")
        (cons +key-left-shift+ "LSHIFT") (cons +key-left-control+ "LCTRL") (cons +key-left-alt+ "LALT")
        (cons +key-left-super+ "WIN") (cons +key-right-shift+ "RSHIFT") (cons +key-right-control+ "RCTRL")
        (cons +key-right-alt+ "ALTGR") (cons +key-right-super+ "RSUPER") (cons +key-kb-menu+ "KBMENU")
        (cons +key-kp-0+ "KP0") (cons +key-kp-1+ "KP1") (cons +key-kp-2+ "KP2") (cons +key-kp-3+ "KP3")
        (cons +key-kp-4+ "KP4") (cons +key-kp-5+ "KP5") (cons +key-kp-6+ "KP6") (cons +key-kp-7+ "KP7")
        (cons +key-kp-8+ "KP8") (cons +key-kp-9+ "KP9") (cons +key-kp-decimal+ "KPDEC") (cons +key-kp-divide+ "KPDIV")
        (cons +key-kp-multiply+ "KPMUL") (cons +key-kp-subtract+ "KPSUB") (cons +key-kp-add+ "KPADD")
        (cons +key-kp-enter+ "KPENTER") (cons +key-kp-equal+ "KPEQU")))

(defun get-key-text (key)
  (or (cdr (assoc key *key-texts*)) ""))

;; Draw keyboard key
(defun gui-keyboard-key (bounds key)
  (if (= key +key-null+)
      (draw-rectangle-lines-ex bounds 2.0 +lightgray+)
      (if (is-key-down key)
          (progn
            (draw-rectangle-lines-ex bounds 2.0 +maroon+)
            (draw-text (get-key-text key) (truncate (+ (rectangle-x bounds) 4)) (truncate (+ (rectangle-y bounds) 4)) 10 +maroon+))
          (progn
            (draw-rectangle-lines-ex bounds 2.0 +darkgray+)
            (draw-text (get-key-text key) (truncate (+ (rectangle-x bounds) 4)) (truncate (+ (rectangle-y bounds) 4)) 10 +darkgray+))))

  (when (check-collision-point-rec (get-mouse-position) bounds)
    (draw-rectangle-rec bounds (fade +red+ 0.2))
    (draw-rectangle-lines-ex bounds 3.0 +red+)))

(defun key-widths (count default &rest overrides)
  "Key widths array: COUNT keys of DEFAULT width, OVERRIDES as index width pairs"
  (let ((widths (make-array count :initial-element default)))
    (loop for (i w) on overrides by #'cddr do (setf (aref widths i) w))
    widths))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - keyboard testbed")
    (set-exit-key +key-null+)           ; Avoid exit on KEY_ESCAPE

    (let* (;; Keyboard line 01
           (line01-key-widths (key-widths 15 45 13 62)) ; PRINTSCREEN
           (line01-keys (vector +key-escape+ +key-f1+ +key-f2+ +key-f3+ +key-f4+ +key-f5+
                                +key-f6+ +key-f7+ +key-f8+ +key-f9+ +key-f10+ +key-f11+
                                +key-f12+ +key-print-screen+ +key-pause+))
           ;; Keyboard line 02
           (line02-key-widths (key-widths 15 45 0 25 13 82)) ; GRAVE, BACKSPACE
           (line02-keys (vector +key-grave+ +key-one+ +key-two+ +key-three+ +key-four+
                                +key-five+ +key-six+ +key-seven+ +key-eight+ +key-nine+
                                +key-zero+ +key-minus+ +key-equal+ +key-backspace+ +key-delete+))
           ;; Keyboard line 03
           (line03-key-widths (key-widths 15 45 0 50 13 57)) ; TAB, BACKSLASH
           (line03-keys (vector +key-tab+ +key-q+ +key-w+ +key-e+ +key-r+ +key-t+ +key-y+
                                +key-u+ +key-i+ +key-o+ +key-p+ +key-left-bracket+
                                +key-right-bracket+ +key-backslash+ +key-insert+))
           ;; Keyboard line 04
           (line04-key-widths (key-widths 14 45 0 68 12 88)) ; CAPS, ENTER
           (line04-keys (vector +key-caps-lock+ +key-a+ +key-s+ +key-d+ +key-f+ +key-g+
                                +key-h+ +key-j+ +key-k+ +key-l+ +key-semicolon+
                                +key-apostrophe+ +key-enter+ +key-page-up+))
           ;; Keyboard line 05
           (line05-key-widths (key-widths 14 45 0 80 11 76)) ; LSHIFT, RSHIFT
           (line05-keys (vector +key-left-shift+ +key-z+ +key-x+ +key-c+ +key-v+ +key-b+
                                +key-n+ +key-m+ +key-comma+ +key-period+ ;KEY_MINUS
                                +key-slash+ +key-right-shift+ +key-up+ +key-page-down+))
           ;; Keyboard line 06
           (line06-key-widths (key-widths 11 45 0 80 3 208 7 60)) ; LCTRL, SPACE, RCTRL
           (line06-keys (vector +key-left-control+ +key-left-super+ +key-left-alt+
                                +key-space+ +key-right-alt+ 162 +key-null+
                                +key-right-control+ +key-left+ +key-down+ +key-right+))
           (keyboard-offset (vec2 26.0 80.0))
           (lines (list (list line01-keys line01-key-widths 0 30.0)
                        (list line02-keys line02-key-widths (+ 30 +key-rec-spacing+) 38.0)
                        (list line03-keys line03-key-widths (+ 30 38 (* +key-rec-spacing+ 2)) 38.0)
                        (list line04-keys line04-key-widths (+ 30 (* 38 2) (* +key-rec-spacing+ 3)) 38.0)
                        (list line05-keys line05-key-widths (+ 30 (* 38 3) (* +key-rec-spacing+ 4)) 38.0)
                        (list line06-keys line06-key-widths (+ 30 (* 38 4) (* +key-rec-spacing+ 5)) 38.0))))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((key (get-key-pressed))) ; Get pressed keycode
                 (when (> key 0) (trace-log +log-info+ "KEYBOARD TESTBED: KEY PRESSED:    ~d" key)))

               (let ((ch (get-char-pressed))) ; Get pressed char for text input, using OS mapping
                 (when (> ch 0) (trace-log +log-info+ "KEYBOARD TESTBED: CHAR PRESSED:   ~c (~d)" (code-char ch) ch)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "KEYBOARD LAYOUT: ENG-US" 26 38 20 +lightgray+)

               ;; Keyboard lines 01 to 06
               ;; ESC, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, IMP, CLOSE
               ;; `, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, -, =, BACKSPACE, DEL
               ;; TAB, Q, W, E, R, T, Y, U, I, O, P, [, ], \, INS
               ;; MAYUS, A, S, D, F, G, H, J, K, L, ;, ', ENTER, REPAG
               ;; LSHIFT, Z, X, C, V, B, N, M, ,, ., /, RSHIFT, UP, AVPAG
               ;; LCTRL, WIN, LALT, SPACE, ALTGR, \, FN, RCTRL, LEFT, DOWN, RIGHT
               (loop for (keys widths offset-y height) in lines
                     do (let ((rec-offset-x 0))
                          (dotimes (i (length keys))
                            (gui-keyboard-key (make-rectangle :x (+ (vx keyboard-offset) rec-offset-x)
                                                              :y (+ (vy keyboard-offset) offset-y)
                                                              :width (float (aref widths i)) :height height)
                                              (aref keys i))
                            (incf rec-offset-x (+ (aref widths i) +key-rec-spacing+)))))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)

;;;; raylib [core] example - clipboard text
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Ananth S (@Ananth1839) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Ananth S (@Ananth1839)
;;;; Common Lisp port of raylib/examples/core/core_clipboard_text.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-clipboard-text
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/core-clipboard-text)

(defconstant +max-text-samples+ 5)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - clipboard text")

    ;; Define some sample texts
    (let ((sample-texts #("Hello from raylib!"
                          "The quick brown fox jumps over the lazy dog"
                          "Clipboard operations are useful!"
                          "raylib is a simple and easy-to-use library"
                          "Copy and paste me!"))

          (clipboard-text nil)
          (input-buffer "Hello from raylib!") ; Random initial string

          ;; UI required variables
          (text-box-edit-mode nil)

          (btn-cut-pressed 0)
          (btn-copy-pressed 0)
          (btn-paste-pressed 0)
          (btn-clear-pressed 0)
          (btn-random-pressed 0))

      ;; Set UI style
      (gui-set-style +default+ +text-size+ 20)
      (gui-set-icon-scale 2)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Handle button interactions
               (when (/= btn-cut-pressed 0)
                 (set-clipboard-text input-buffer)
                 (setf clipboard-text (get-clipboard-text))
                 (setf input-buffer ""))     ; Quick solution to clear text

               (when (/= btn-copy-pressed 0)
                 (set-clipboard-text input-buffer) ; Copy text to clipboard
                 (setf clipboard-text (get-clipboard-text))) ; Get text from clipboard

               (when (/= btn-paste-pressed 0)
                 ;; Paste text from clipboard
                 (setf clipboard-text (get-clipboard-text))
                 (when clipboard-text (setf input-buffer clipboard-text)))

               (when (/= btn-clear-pressed 0)
                 (setf input-buffer ""))     ; Quick solution to clear text

               (when (/= btn-random-pressed 0)
                 ;; Get random text from sample list
                 (setf input-buffer (aref sample-texts (get-random-value 0 (1- +max-text-samples+)))))

               ;; Quick cut/copy/paste with keyboard shortcuts
               (when (or (is-key-down +key-left-control+) (is-key-down +key-right-control+))
                 (when (is-key-pressed +key-x+)
                   (set-clipboard-text input-buffer)
                   (setf input-buffer ""))   ; Quick solution to clear text

                 (when (is-key-pressed +key-c+) (set-clipboard-text input-buffer))

                 (when (is-key-pressed +key-v+)
                   (setf clipboard-text (get-clipboard-text))
                   (when clipboard-text (setf input-buffer clipboard-text))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Draw instructions
               (gui-label (make-rectangle :x 50.0 :y 20.0 :width 700.0 :height 36.0) "Use the BUTTONS or KEY SHORTCUTS:")
               (draw-text "[CTRL+X] - CUT | [CTRL+C] COPY | [CTRL+V] | PASTE" 50 60 20 +maroon+)

               ;; Draw text box
               (multiple-value-bind (result text)
                   (gui-text-box (make-rectangle :x 50.0 :y 120.0 :width 652.0 :height 40.0) input-buffer 256 text-box-edit-mode)
                 (setf input-buffer text)
                 (when (/= result 0) (setf text-box-edit-mode (not text-box-edit-mode))))

               ;; Random text button
               (setf btn-random-pressed (gui-button (make-rectangle :x (+ 50.0 652 8) :y 120.0 :width 40.0 :height 40.0) "#77#"))

               ;; Draw buttons
               (setf btn-cut-pressed (gui-button (make-rectangle :x 50.0 :y 180.0 :width 158.0 :height 40.0) "#17#CUT"))
               (setf btn-copy-pressed (gui-button (make-rectangle :x (+ 50.0 165) :y 180.0 :width 158.0 :height 40.0) "#16#COPY"))
               (setf btn-paste-pressed (gui-button (make-rectangle :x (+ 50.0 (* 165 2)) :y 180.0 :width 158.0 :height 40.0) "#18#PASTE"))
               (setf btn-clear-pressed (gui-button (make-rectangle :x (+ 50.0 (* 165 3)) :y 180.0 :width 158.0 :height 40.0) "#143#CLEAR"))

               ;; Draw clipboard status
               (gui-set-state +state-disabled+)
               (gui-label (make-rectangle :x 50.0 :y 260.0 :width 700.0 :height 40.0) "Clipboard current text data:")
               (gui-set-style +textbox+ +text-readonly+ 1)
               (gui-text-box (make-rectangle :x 50.0 :y 300.0 :width 700.0 :height 40.0) clipboard-text 256 nil)
               (gui-set-style +textbox+ +text-readonly+ 0)
               (gui-label (make-rectangle :x 50.0 :y 360.0 :width 700.0 :height 40.0) "Try copying text from other applications and pasting here!")
               (gui-set-state +state-normal+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)

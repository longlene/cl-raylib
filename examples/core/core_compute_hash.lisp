;;;; raylib [core] example - compute hash
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_compute_hash.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-compute-hash
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/core-compute-hash)

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
(defun get-data-as-hex-text (data data-size)
  (if (and data (> data-size 0) (< data-size (- (/ 128 8) 1)))
      (with-output-to-string (s)
        (dotimes (i data-size) (write-string (text-format "%08X" (aref data i)) s)))
      "00000000"))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - compute hash")

    (let (;; UI controls variables
          (text-input "The quick brown fox jumps over the lazy dog.")
          (text-box-edit-mode nil)
          (btn-compute-hashes 0)

          ;; Data hash values
          (hash-crc32 (vector 0))
          (hash-md5 nil)
          (hash-sha1 nil)
          (hash-sha256 nil)

          ;; Base64 encoded data
          (base64-text nil))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (/= btn-compute-hashes 0)
                 (let* ((data (babel:string-to-octets text-input :encoding :utf-8))
                        (text-input-len (length data)))

                   ;; Encode data to Base64 string
                   (setf base64-text (encode-data-base64 data text-input-len))

                   (setf (aref hash-crc32 0) (compute-crc32 data text-input-len) ; Compute CRC32 hash code (4 bytes)
                         hash-md5 (compute-md5 data text-input-len)           ; Compute MD5 hash code, returns int[4] (16 bytes)
                         hash-sha1 (compute-sha1 data text-input-len)         ; Compute SHA1 hash code, returns int[5] (20 bytes)
                         hash-sha256 (compute-sha256 data text-input-len))))  ; Compute SHA256 hash code, returns int[8] (32 bytes)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (gui-set-style +default+ +text-size+ 20)
               (gui-set-style +default+ +text-spacing+ 2)
               (gui-label (make-rectangle :x 40.0 :y 26.0 :width 720.0 :height 32.0) "INPUT DATA (TEXT):")
               (gui-set-style +default+ +text-spacing+ 1)
               (gui-set-style +default+ +text-size+ 10)

               (multiple-value-bind (result text)
                   (gui-text-box (make-rectangle :x 40.0 :y 64.0 :width 720.0 :height 32.0) text-input 95 text-box-edit-mode)
                 (setf text-input text)
                 (when (/= result 0) (setf text-box-edit-mode (not text-box-edit-mode))))

               (setf btn-compute-hashes (gui-button (make-rectangle :x 40.0 :y (+ 64.0 40) :width 720.0 :height 32.0) "COMPUTE INPUT DATA HASHES"))

               (gui-set-style +default+ +text-size+ 20)
               (gui-set-style +default+ +text-spacing+ 2)
               (gui-label (make-rectangle :x 40.0 :y 160.0 :width 720.0 :height 32.0) "INPUT DATA HASH VALUES:")
               (gui-set-style +default+ +text-spacing+ 1)
               (gui-set-style +default+ +text-size+ 10)

               (gui-set-style +textbox+ +text-readonly+ 1)
               (gui-label (make-rectangle :x 40.0 :y 200.0 :width 120.0 :height 32.0) "CRC32 [32 bit]:")
               (gui-text-box (make-rectangle :x (+ 40.0 120) :y 200.0 :width (- 720.0 120) :height 32.0) (get-data-as-hex-text hash-crc32 1) 120 nil)
               (gui-label (make-rectangle :x 40.0 :y (+ 200.0 36) :width 120.0 :height 32.0) "MD5 [128 bit]:")
               (gui-text-box (make-rectangle :x (+ 40.0 120) :y (+ 200.0 36) :width (- 720.0 120) :height 32.0) (get-data-as-hex-text hash-md5 4) 120 nil)
               (gui-label (make-rectangle :x 40.0 :y (+ 200.0 (* 36 2)) :width 120.0 :height 32.0) "SHA1 [160 bit]:")
               (gui-text-box (make-rectangle :x (+ 40.0 120) :y (+ 200.0 (* 36 2)) :width (- 720.0 120) :height 32.0) (get-data-as-hex-text hash-sha1 5) 120 nil)
               (gui-label (make-rectangle :x 40.0 :y (+ 200.0 (* 36 3)) :width 120.0 :height 32.0) "SHA256 [256 bit]:")
               (gui-text-box (make-rectangle :x (+ 40.0 120) :y (+ 200.0 (* 36 3)) :width (- 720.0 120) :height 32.0) (get-data-as-hex-text hash-sha256 8) 120 nil)

               (gui-set-state +state-focused+)
               (gui-label (make-rectangle :x 40.0 :y (- (+ 200.0 (* 36 5)) 30) :width 320.0 :height 32.0) "BONUS - BAS64 ENCODED STRING:")
               (gui-set-state +state-normal+)
               (gui-label (make-rectangle :x 40.0 :y (+ 200.0 (* 36 5)) :width 120.0 :height 32.0) "BASE64 ENCODING:")
               (gui-text-box (make-rectangle :x (+ 40.0 120) :y (+ 200.0 (* 36 5)) :width (- 720.0 120) :height 32.0) base64-text 120 nil)
               (gui-set-style +textbox+ +text-readonly+ 0)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)

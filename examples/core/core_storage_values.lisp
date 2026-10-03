;;;; raylib [core] example - storage values
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.4, last time updated with raylib 4.2
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_storage_values.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-storage-values
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-storage-values)

(defparameter +storage-data-file+ "storage.data") ; Storage file

;; NOTE: Storage positions must start with 0, directly related to file memory layout
(defconstant +storage-position-score+ 0)
(defconstant +storage-position-hiscore+ 1)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; int values stored as 4 bytes little endian, like the C int array in memory
(defun %put-int (data position value)
  (dotimes (k 4) (setf (aref data (+ (* position 4) k)) (ldb (byte 8 (* 8 k)) value))))

(defun %get-int (data position)
  (let ((u (loop for k below 4 sum (ash (aref data (+ (* position 4) k)) (* 8 k)))))
    (if (>= u #x80000000) (- u #x100000000) u)))

;; Save integer value to storage file (to defined position)
;; NOTE: Storage positions is directly related to file memory layout (4 bytes each integer)
(defun save-storage-value (position value)
  (let ((success nil))
    (multiple-value-bind (file-data data-size) (load-file-data +storage-data-file+)
      (if file-data
          (let ((new-file-data nil) (new-data-size 0))
            (if (<= data-size (* position 4))
                (progn
                  ;; Increase data size up to position and store value
                  (setf new-data-size (* (1+ position) 4)
                        new-file-data (replace (make-array new-data-size :element-type '(unsigned-byte 8) :initial-element 0) file-data))
                  (%put-int new-file-data position value))
                (progn
                  ;; Store the old size of the file
                  (setf new-file-data file-data
                        new-data-size data-size)

                  ;; Replace value on selected position
                  (%put-int new-file-data position value)))

            (setf success (save-file-data +storage-data-file+ new-file-data new-data-size))

            (trace-log +log-info+ "FILEIO: [~a] Saved storage value: ~d" +storage-data-file+ value))
          (progn
            (trace-log +log-info+ "FILEIO: [~a] File created successfully" +storage-data-file+)

            (let* ((data-size (* (1+ position) 4))
                   (file-data (make-array data-size :element-type '(unsigned-byte 8) :initial-element 0)))
              (%put-int file-data position value)

              (setf success (save-file-data +storage-data-file+ file-data data-size)))

            (trace-log +log-info+ "FILEIO: [~a] Saved storage value: ~d" +storage-data-file+ value))))
    success))

;; Load integer value from storage file (from defined position)
;; NOTE: If requested position could not be found, value 0 is returned
(defun load-storage-value (position)
  (let ((value 0))
    (multiple-value-bind (file-data data-size) (load-file-data +storage-data-file+)
      (when file-data
        (if (< data-size (* position 4))
            (trace-log +log-warning+ "FILEIO: [~a] Failed to find storage position: ~d" +storage-data-file+ position)
            (setf value (%get-int file-data position)))

        (trace-log +log-info+ "FILEIO: [~a] Loaded storage value: ~d" +storage-data-file+ value)))
    value))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - storage values")

    (let ((score 0)
          (hiscore 0)
          (frames-counter 0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-r+)
                 (setf score (get-random-value 1000 2000)
                       hiscore (get-random-value 2000 4000)))

               (cond ((is-key-pressed +key-enter+)
                      (save-storage-value +storage-position-score+ score)
                      (save-storage-value +storage-position-hiscore+ hiscore))
                     ((is-key-pressed +key-space+)
                      ;; NOTE: If requested position could not be found, value 0 is returned
                      (setf score (load-storage-value +storage-position-score+)
                            hiscore (load-storage-value +storage-position-hiscore+))))

               (incf frames-counter)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text (text-format "SCORE: %i" score) 280 130 40 +maroon+)
               (draw-text (text-format "HI-SCORE: %i" hiscore) 210 200 50 +black+)

               (draw-text (text-format "frames: %i" frames-counter) 10 10 20 +lime+)

               (draw-text "Press R to generate random numbers" 220 40 20 +lightgray+)
               (draw-text "Press ENTER to SAVE values" 250 310 20 +lightgray+)
               (draw-text "Press SPACE to LOAD values" 252 350 20 +lightgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)

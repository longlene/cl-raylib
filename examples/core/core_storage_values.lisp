(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defconstant +storage-data-file+ "storage.data" "Storage file")

;; Storage positions
(defconstant +storage-position-score+ 0)
(defconstant +storage-position-hiscore+ 1)

(defun save-storage-value (position value)
  "Save integer value to storage file (to defined position)"
  (let ((success nil)
        (file-data (load-file-data +storage-data-file+))
        (data-size (if file-data (length file-data) 0)))
    
    (if file-data
        (progn
          ;; File exists, update or extend it
          (let* ((required-size (* (1+ position) 4)) ; 4 bytes per integer
                 (new-data (if (< data-size required-size)
                               ;; Need to extend file
                               (let ((extended-data (make-array required-size 
                                                                :element-type '(unsigned-byte 8) 
                                                                :initial-element 0)))
                                 ;; Copy existing data
                                 (loop for i from 0 below data-size do
                                   (setf (aref extended-data i) (aref file-data i)))
                                 extended-data)
                               ;; File is large enough
                               file-data)))
            
            ;; Store the value at the specified position
            (let ((byte-position (* position 4)))
              ;; Convert integer to bytes (little-endian)
              (setf (aref new-data byte-position) (logand value #xFF))
              (setf (aref new-data (+ byte-position 1)) (logand (ash value -8) #xFF))
              (setf (aref new-data (+ byte-position 2)) (logand (ash value -16) #xFF))
              (setf (aref new-data (+ byte-position 3)) (logand (ash value -24) #xFF)))
            
            (setf success (save-file-data +storage-data-file+ new-data (length new-data)))
            (trace-log-info "FILEIO: [~a] Saved storage value: ~d" +storage-data-file+ value)))
        
        (progn
          ;; File doesn't exist, create new one
          (let* ((data-size (* (1+ position) 4))
                 (new-data (make-array data-size :element-type '(unsigned-byte 8) :initial-element 0))
                 (byte-position (* position 4)))
            
            ;; Store the value
            (setf (aref new-data byte-position) (logand value #xFF))
            (setf (aref new-data (+ byte-position 1)) (logand (ash value -8) #xFF))
            (setf (aref new-data (+ byte-position 2)) (logand (ash value -16) #xFF))
            (setf (aref new-data (+ byte-position 3)) (logand (ash value -24) #xFF))
            
            (setf success (save-file-data +storage-data-file+ new-data data-size))
            (trace-log-info "FILEIO: [~a] File created successfully" +storage-data-file+)
            (trace-log-info "FILEIO: [~a] Saved storage value: ~d" +storage-data-file+ value))))
    
    success))

(defun load-storage-value (position)
  "Load integer value from storage file (from defined position)"
  (let ((value 0)
        (file-data (load-file-data +storage-data-file+)))
    
    (when file-data
      (let ((data-size (length file-data))
            (required-size (* (1+ position) 4)))
        
        (if (< data-size required-size)
            (trace-log-warning "FILEIO: [~a] Failed to find storage position: ~d" +storage-data-file+ position)
            (let ((byte-position (* position 4)))
              ;; Read integer from bytes (little-endian)
              (setf value (+ (aref file-data byte-position)
                             (ash (aref file-data (+ byte-position 1)) 8)
                             (ash (aref file-data (+ byte-position 2)) 16)
                             (ash (aref file-data (+ byte-position 3)) 24)))))
        
        (trace-log-info "FILEIO: [~a] Loaded storage value: ~d" +storage-data-file+ value)))
    
    value))

(defun main ()
  "raylib [core] example - Storage save/load values"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [core] example - storage save/load values")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      (let ((score 0)
            (hiscore 0)
            (frames-counter 0))

        (loop
          until (window-should-close) ; Detect window close button or ESC key
          do (progn
               ;; Update
               (when (is-key-pressed :key-r)
                 (setf score (get-random-value 1000 2000))
                 (setf hiscore (get-random-value 2000 4000)))

               (when (is-key-pressed :key-enter)
                 (save-storage-value +storage-position-score+ score)
                 (save-storage-value +storage-position-hiscore+ hiscore))

               (when (is-key-pressed :key-space)
                 ;; NOTE: If requested position could not be found, value 0 is returned
                 (setf score (load-storage-value +storage-position-score+))
                 (setf hiscore (load-storage-value +storage-position-hiscore+)))

               (incf frames-counter)

               ;; Draw
               (with-drawing
                 (clear-background :raywhite)

                 (draw-text (text-format "SCORE: ~d" score) 280 130 40 :maroon)
                 (draw-text (text-format "HI-SCORE: ~d" hiscore) 210 200 50 :black)
                 (draw-text (text-format "frames: ~d" frames-counter) 10 10 20 :lime)

                 (draw-text "Press R to generate random numbers" 220 40 20 :lightgray)
                 (draw-text "Press ENTER to SAVE values" 250 310 20 :lightgray)
                 (draw-text "Press SPACE to LOAD values" 252 350 20 :lightgray))))))))

(main)
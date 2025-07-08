(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun main ()
  "raylib [models] example - Drawing billboards"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [models] example - drawing billboards")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      ;; Define the camera to look into our 3d world
      (let ((camera (make-camera3d :position (vec 5.0 4.0 5.0)
                                   :target (vec 0.0 2.0 0.0)
                                   :up (vec 0.0 1.0 0.0)
                                   :fovy 45.0
                                   :projection :camera-perspective))
            (bill nil)
            (bill-position-static (vec 0.0 2.0 0.0))
            (bill-position-rotating (vec 1.0 2.0 1.0))
            (bill-up (vec 0.0 1.0 0.0))
            (rotation 0.0))

        ;; Try to load billboard texture
        (handler-case
            (setf bill (load-texture "resources/billboard.png"))
          (error ()
            ;; If texture loading fails, we'll just skip billboard drawing
            (setf bill nil)))

        (let* ((source (if bill 
                           (make-rectangle :x 0.0 :y 0.0 
                                          :width (texture-width bill) 
                                          :height (texture-height bill))
                           (make-rectangle :x 0.0 :y 0.0 :width 64.0 :height 64.0)))
               ;; Set the height of the rotating billboard to 1.0 with the aspect ratio fixed
               (size (vec2 (/ (rectangle-width source) (rectangle-height source)) 1.0))
               ;; Rotate around origin - choose to rotate around the image center
               (origin (v* size 0.5)))

          (loop
            until (window-should-close) ; Detect window close button or ESC key
            do (progn
                 ;; Update
                 (update-camera camera :camera-orbital)

                 (incf rotation 0.4)
                 (let ((distance-static (vlength (v- (camera3d-position camera) bill-position-static)))
                       (distance-rotating (vlength (v- (camera3d-position camera) bill-position-rotating))))

                   ;; Draw
                   (with-drawing
                     (clear-background :raywhite)

                     (with-mode-3d (camera)
                       (draw-grid 10 1.0)

                       ;; Draw order matters!
                       (if bill
                           (if (> distance-static distance-rotating)
                               (progn
                                 (draw-billboard camera bill bill-position-static 2.0 :white)
                                 (draw-billboard-pro camera bill source bill-position-rotating 
                                                    bill-up size origin rotation :white))
                               (progn
                                 (draw-billboard-pro camera bill source bill-position-rotating 
                                                    bill-up size origin rotation :white)
                                 (draw-billboard camera bill bill-position-static 2.0 :white)))
                           ;; If no texture, draw simple cubes as placeholders
                           (progn
                             (draw-cube bill-position-static 0.5 0.5 0.5 :red)
                             (draw-cube bill-position-rotating 0.5 0.5 0.5 :blue))))

                     (when (not bill)
                       (draw-text "Billboard texture not found - showing cubes instead" 10 30 20 :red))

                     (draw-fps 10 10)))))

          ;; Cleanup
          (when bill (unload-texture bill)))))))

(main)
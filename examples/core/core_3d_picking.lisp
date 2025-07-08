(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors :3d-matrices))

(in-package :raylib-user)

(defun main ()
  "raylib [core] example - Picking in 3d mode"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [core] example - 3d picking")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      ;; Define the camera to look into our 3d world
      (let ((camera (make-camera3d :position (vec 10.0 10.0 10.0)
                                   :target (vec 0.0 0.0 0.0)
                                   :up (vec 0.0 1.0 0.0)
                                   :fovy 45.0
                                   :projection :camera-perspective))
            (cube-position (vec 0.0 1.0 0.0))
            (cube-size (vec 2.0 2.0 2.0))
            (ray nil)
            (collision nil))

        (loop
          until (window-should-close) ; Detect window close button or ESC key
          do (progn
               ;; Update
               (when (is-cursor-hidden)
                 (update-camera camera :camera-first-person))

               ;; Toggle camera controls
               (when (is-mouse-button-pressed :mouse-button-right)
                 (if (is-cursor-hidden)
                     (enable-cursor)
                     (disable-cursor)))

               (when (is-mouse-button-pressed :mouse-button-left)
                 (if (not (and collision (ray-collision-hit collision)))
                     (progn
                       (setf ray (get-screen-to-world-ray (get-mouse-position) camera))
                       ;; Check collision between ray and box
                       (let ((min-pos (v- cube-position (v* cube-size 0.5)))
                             (max-pos (v+ cube-position (v* cube-size 0.5))))
                         (setf collision (get-ray-collision-box ray 
                                                                (make-bounding-box :min min-pos
                                                                                   :max max-pos)))))
                     (setf collision (make-ray-collision :hit nil))))

               ;; Draw
               (with-drawing
                 (clear-background :raywhite)

                 (with-mode-3d (camera)
                   (if (and collision (ray-collision-hit collision))
                       (progn
                         (draw-cube cube-position (vx cube-size) (vy cube-size) (vz cube-size) :red)
                         (draw-cube-wires cube-position (vx cube-size) (vy cube-size) (vz cube-size) :maroon)
                         (draw-cube-wires cube-position 
                                          (+ (vx cube-size) 0.2) 
                                          (+ (vy cube-size) 0.2) 
                                          (+ (vz cube-size) 0.2) 
                                          :green))
                       (progn
                         (draw-cube cube-position (vx cube-size) (vy cube-size) (vz cube-size) :gray)
                         (draw-cube-wires cube-position (vx cube-size) (vy cube-size) (vz cube-size) :darkgray)))

                   (when ray (draw-ray ray :maroon))
                   (draw-grid 10 1.0))

                 (draw-text "Try clicking on the box with your mouse!" 240 10 20 :darkgray)

                 (when (and collision (ray-collision-hit collision))
                   (let ((text "BOX SELECTED"))
                     (draw-text text 
                                (/ (- screen-width (measure-text text 30)) 2)
                                (floor (* screen-height 0.1))
                                30 :green)))

                 (draw-text "Right click mouse to toggle camera controls" 10 430 10 :gray)
                 (draw-fps 10 10))))))))

(main)
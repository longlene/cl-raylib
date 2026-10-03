(require :cl-raylib)

(defpackage :test-camera-debug
  (:use :cl :cl-raylib :3d-vectors))

(in-package :test-camera-debug)

(with-window (800 450 "Camera Debug Test")
  (let ((camera (make-camera3d :position (vec3 10.0 10.0 10.0)
                               :target (vec3 0.0 0.0 0.0)
                               :up (vec3 0.0 1.0 0.0)
                               :fovy 45.0
                               :projection :camera-perspective)))
    (set-target-fps 60)
    (loop until (window-should-close) do
      (let* ((frame-time (get-frame-time))
             (w-down (is-key-down :key-w))
             (s-down (is-key-down :key-s))
             (a-down (is-key-down :key-a))
             (d-down (is-key-down :key-d))
             (wheel (get-mouse-wheel-move))
             (old-pos (vcopy (camera3d-position camera))))

        ;; Update camera
        (update-camera camera :camera-free)

        (let* ((new-pos (camera3d-position camera))
               (pos-delta (v- new-pos old-pos))
               (moved (> (vlength pos-delta) 0.0001)))

          (with-drawing
            (clear-background :raywhite)

            (with-mode-3d (camera)
              (draw-cube (vec3 0.0 0.0 0.0) 2.0 2.0 2.0 :red)
              (draw-grid 10 1.0))

            (draw-text (format nil "Frame time: ~,4f" frame-time) 20 20 15 :black)
            (draw-text (format nil "W: ~a S: ~a A: ~a D: ~a" w-down s-down a-down d-down) 20 40 15
                       (if (or w-down s-down a-down d-down) :green :gray))
            (draw-text (format nil "Wheel: ~,2f" wheel) 20 60 15 (if (/= wheel 0.0) :green :gray))
            (draw-text (format nil "Pos: (~,2f ~,2f ~,2f)"
                              (vx new-pos) (vy new-pos) (vz new-pos)) 20 80 15 :blue)
            (draw-text (format nil "Camera moved: ~a" moved) 20 100 15
                       (if moved :green :red))
            (when moved
              (draw-text (format nil "Delta: (~,3f ~,3f ~,3f)"
                                (vx pos-delta) (vy pos-delta) (vz pos-delta))
                        20 120 15 :darkgreen))
            (draw-text "Try moving with WASD and mouse wheel" 20 380 12 :darkgray)))))))

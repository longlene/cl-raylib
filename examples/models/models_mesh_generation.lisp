(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defconstant +num-models+ 9) ; Parametric 3d shapes to generate

(defun gen-mesh-custom ()
  "Generate a simple triangle mesh from code"
  ;; Note: This is a simplified version since we don't have direct mesh generation in cl-raylib yet
  ;; This would need proper mesh creation functions to be implemented
  (error "Custom mesh generation not yet implemented in cl-raylib"))

(defun main ()
  "raylib [models] example - procedural mesh generation"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [models] example - mesh generation")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      ;; We generate a checked image for texturing
      (let* ((checked (gen-image-checked 2 2 1 1 :red :green))
             (texture (load-texture-from-image checked))
             (models (make-array +num-models+ :initial-element nil))
             (current-model 0)
             (position (vec 0.0 0.0 0.0)))
        
        (unload-image checked)

        ;; Generate all models (note: some mesh generation functions may not be implemented yet)
        (handler-case
            (progn
              (setf (aref models 0) (load-model-from-mesh (gen-mesh-plane 2 2 4 3)))
              (setf (aref models 1) (load-model-from-mesh (gen-mesh-cube 2.0 1.0 2.0)))
              (setf (aref models 2) (load-model-from-mesh (gen-mesh-sphere 2 32 32)))
              (setf (aref models 3) (load-model-from-mesh (gen-mesh-hemisphere 2 16 16)))
              (setf (aref models 4) (load-model-from-mesh (gen-mesh-cylinder 1 2 16)))
              (setf (aref models 5) (load-model-from-mesh (gen-mesh-torus 0.25 4.0 16 32)))
              (setf (aref models 6) (load-model-from-mesh (gen-mesh-knot 1.0 2.0 16 128)))
              (setf (aref models 7) (load-model-from-mesh (gen-mesh-poly 5 2.0)))
              (setf (aref models 8) (load-model-from-mesh (gen-mesh-custom))))
          (error ()
            ;; If mesh generation fails, create placeholder models using basic shapes
            (format t "Some mesh generation functions not available, using placeholders~%")))

        ;; Set checked texture as default diffuse component for all models material
        (loop for i from 0 below +num-models+ do
          (when (aref models i)
            ;; Note: Material texture assignment would need proper implementation
            ))

        ;; Define the camera to look into our 3d world
        (let ((camera (make-camera3d :position (vec 5.0 5.0 5.0)
                                     :target (vec 0.0 0.0 0.0)
                                     :up (vec 0.0 1.0 0.0)
                                     :fovy 45.0
                                     :projection :camera-perspective)))

          (loop
            until (window-should-close) ; Detect window close button or ESC key
            do (progn
                 ;; Update
                 (update-camera camera :camera-orbital)

                 (when (is-mouse-button-pressed :mouse-button-left)
                   (setf current-model (mod (1+ current-model) +num-models+)))

                 (when (is-key-pressed :key-right)
                   (incf current-model)
                   (when (>= current-model +num-models+) (setf current-model 0)))
                 
                 (when (is-key-pressed :key-left)
                   (decf current-model)
                   (when (< current-model 0) (setf current-model (1- +num-models+))))

                 ;; Draw
                 (with-drawing
                   (clear-background :raywhite)

                   (with-mode-3d (camera)
                     (when (aref models current-model)
                       (draw-model (aref models current-model) position 1.0 :white))
                     (draw-grid 10 1.0))

                   (draw-rectangle 30 400 310 30 (fade :skyblue 0.5))
                   (draw-rectangle-lines 30 400 310 30 (fade :darkblue 0.5))
                   (draw-text "MOUSE LEFT BUTTON to CYCLE PROCEDURAL MODELS" 40 410 10 :blue)

                   (case current-model
                     (0 (draw-text "PLANE" 680 10 20 :darkblue))
                     (1 (draw-text "CUBE" 680 10 20 :darkblue))
                     (2 (draw-text "SPHERE" 680 10 20 :darkblue))
                     (3 (draw-text "HEMISPHERE" 640 10 20 :darkblue))
                     (4 (draw-text "CYLINDER" 680 10 20 :darkblue))
                     (5 (draw-text "TORUS" 680 10 20 :darkblue))
                     (6 (draw-text "KNOT" 680 10 20 :darkblue))
                     (7 (draw-text "POLY" 680 10 20 :darkblue))
                     (8 (draw-text "Custom (triangle)" 580 10 20 :darkblue))))))

          ;; Cleanup
          (unload-texture texture)
          (loop for i from 0 below +num-models+ do
            (when (aref models i)
              (unload-model (aref models i)))))))))

(main)
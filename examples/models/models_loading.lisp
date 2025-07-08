(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors :3d-matrices))

(in-package :raylib-user)

(defun main ()
  "raylib [models] example - Models loading"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [models] example - models loading")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      ;; Define the camera to look into our 3d world
      (let ((camera (make-camera3d :position (vec 50.0 50.0 50.0)
                                   :target (vec 0.0 10.0 0.0)
                                   :up (vec 0.0 1.0 0.0)
                                   :fovy 45.0
                                   :projection :camera-perspective))
            (model nil))

        ;; Try to load a model (this will likely fail without the resource file)
        (handler-case
            (progn
              (setf model (load-model "resources/models/obj/castle.obj"))
              ;; Try to load texture and set it to the model's material
              (handler-case
                  (let ((texture (load-texture "resources/models/obj/castle_diffuse.png")))
                    (when (and model (model-materials model) (> (model-material-count model) 0))
                      (set-material-texture (first (model-materials model)) 
                                          +material-map-diffuse+ texture)))
                (error ()
                  ;; Texture loading failed, continue without texture
                  )))
          (error ()
            ;; If model loading fails, we'll just draw a primitive cube
            (setf model nil)))

        (loop
          until (window-should-close) ; Detect window close button or ESC key
          do (progn
               ;; Update camera
               (update-camera camera :camera-orbital)

               ;; Draw
               (with-drawing
                 (clear-background :raywhite)

                 (with-mode-3d (camera)
                   (if model
                       ;; Draw the loaded model
                       (draw-model model (vec 0.0 0.0 0.0) 1.0 :white)
                       ;; Draw a simple cube if no model loaded
                       (draw-cube (vec 0.0 0.0 0.0) 2.0 2.0 2.0 :red))

                   (draw-grid 10 1.0))

                 (if model
                     (draw-text "Model loaded successfully!" 190 200 20 :darkgreen)
                     (draw-text "Model loading failed - showing cube instead" 140 200 20 :red))

                 (draw-text "(c) Castle 3D model by Alberto Cano" (- screen-width 200) (- screen-height 20) 10 :gray)
                 (draw-fps 10 10))))

        ;; Cleanup
        (when model (unload-model model))))))

(main)
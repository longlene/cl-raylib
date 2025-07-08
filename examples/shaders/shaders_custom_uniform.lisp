(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defparameter +glsl-version+ 330) ; Desktop version

(defun main ()
  "raylib [shaders] example - Postprocessing with custom uniform variable"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Enable Multi Sampling Anti Aliasing 4x (if available)
    (set-config-flags '(:flag-msaa-4x-hint))
    
    (with-window (screen-width screen-height "raylib [shaders] example - custom uniform variable")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      ;; Define the camera to look into our 3d world
      (let ((camera (make-camera3d :position (vec 8.0 8.0 8.0)
                                   :target (vec 0.0 1.5 0.0)
                                   :up (vec 0.0 1.0 0.0)
                                   :fovy 45.0
                                   :projection :camera-perspective))
            (model nil)
            (texture nil)
            (shader nil)
            (target nil)
            (swirl-center-loc -1)
            (position (vec 0.0 0.0 0.0)))

        ;; Try to load model and texture
        (handler-case
            (progn
              (setf model (load-model "resources/models/barracks.obj"))
              (setf texture (load-texture "resources/models/barracks_diffuse.png"))
              ;; Set model diffuse texture (this would need proper material API)
              )
          (error ()
            ;; If model/texture loading fails, we'll use placeholders
            (setf model nil)
            (setf texture nil)))

        ;; Load postprocessing shader
        (handler-case
            (progn
              (setf shader (load-shader nil (format nil "resources/shaders/glsl~d/swirl.fs" +glsl-version+)))
              ;; Get variable (uniform) location on the shader
              (setf swirl-center-loc (get-shader-location shader "center")))
          (error ()
            ;; If shader loading fails, disable shader effects
            (setf shader nil)))

        ;; Create a RenderTexture2D to be used for render to texture
        (setf target (load-render-texture screen-width screen-height))

        (let ((swirl-center (list (/ screen-width 2.0) (/ screen-height 2.0))))

          (loop
            until (window-should-close) ; Detect window close button or ESC key
            do (progn
                 ;; Update
                 (update-camera camera :camera-orbital)

                 (let ((mouse-position (get-mouse-position)))
                   (setf (first swirl-center) (vx mouse-position))
                   (setf (second swirl-center) (- screen-height (vy mouse-position)))

                   ;; Send new value to the shader to be used on drawing
                   (when (and shader (>= swirl-center-loc 0))
                     (set-shader-value shader swirl-center-loc swirl-center :shader-uniform-vec2)))

                 ;; Draw to render texture first
                 (with-texture-mode (target)
                   (clear-background :raywhite)

                   (with-mode-3d (camera)
                     (if model
                         (draw-model model position 0.5 :white)
                         ;; Draw a simple cube if no model loaded
                         (draw-cube position 2.0 2.0 2.0 :red))
                     (draw-grid 10 1.0))

                   (draw-text "TEXT DRAWN IN RENDER TEXTURE" 200 10 30 :red))

                 ;; Draw to screen
                 (with-drawing
                   (clear-background :raywhite)

                   ;; Apply shader if available
                   (if shader
                       (with-shader-mode (shader)
                         ;; NOTE: Render texture must be y-flipped due to default OpenGL coordinates
                         (let ((source-rect (make-rectangle :x 0 :y 0 
                                                           :width (texture-width (render-texture-texture target))
                                                           :height (- (texture-height (render-texture-texture target))))))
                           (draw-texture-rec (render-texture-texture target) source-rect (vec2 0.0 0.0) :white)))
                       ;; If no shader, draw texture normally
                       (let ((source-rect (make-rectangle :x 0 :y 0 
                                                         :width (texture-width (render-texture-texture target))
                                                         :height (- (texture-height (render-texture-texture target))))))
                         (draw-texture-rec (render-texture-texture target) source-rect (vec2 0.0 0.0) :white)))

                   ;; Draw some 2d text over drawn texture
                   (if model
                       (draw-text "(c) Barracks 3D model by Alberto Cano" (- screen-width 220) (- screen-height 20) 10 :gray)
                       (draw-text "Model/shader files not found - showing placeholder" 10 30 20 :red))
                   (draw-fps 10 10))))

          ;; Cleanup
          (when shader (unload-shader shader))
          (when texture (unload-texture texture))
          (when model (unload-model model))
          (when target (unload-render-texture target)))))))

(main)
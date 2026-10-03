;;;; models_skybox.lisp - Skybox loading and drawing
;;;; Translated from raylib/examples/models/models_skybox.c

(require :cl-raylib)

(defpackage :models-skybox
  (:use :cl :cl-raylib))

(in-package :models-skybox)

(defconstant +glsl-version+ 330)  ; Desktop OpenGL version

(defun main ()
  "Main function - skybox example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [models] example - skybox loading and drawing")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera-3d :position (vec3 1.0 1.0 1.0)
                                 :target (vec3 4.0 1.0 4.0)
                                 :up (vec3 0.0 1.0 0.0)
                                 :fovy 45.0
                                 :projection +camera-perspective+)))

      ;; Load skybox model
      (let ((cube (gen-mesh-cube 1.0 1.0 1.0))
            (use-hdr nil))  ; Set to true for HDR support
        
        (let ((skybox (load-model-from-mesh cube)))

          ;; Load skybox shader
          (let ((skybox-shader (load-shader (format nil "resources/shaders/glsl~a/skybox.vs" +glsl-version+)
                                           (format nil "resources/shaders/glsl~a/skybox.fs" +glsl-version+))))

            ;; Set shader values
            (set-shader-value skybox-shader 
                             (get-shader-location skybox-shader "environmentMap") 
                             (vector +material-map-cubemap+) 
                             +shader-uniform-int+)
            (set-shader-value skybox-shader 
                             (get-shader-location skybox-shader "doGamma") 
                             (vector (if use-hdr 1 0)) 
                             +shader-uniform-int+)
            (set-shader-value skybox-shader 
                             (get-shader-location skybox-shader "vflipped") 
                             (vector (if use-hdr 1 0)) 
                             +shader-uniform-int+)

            ;; Set skybox material shader
            (setf (material-shader (aref (model-materials skybox) 0)) skybox-shader)

            ;; Load cubemap texture
            (let ((skybox-texture
                   (if use-hdr
                       ;; HDR version - would need cubemap generation from HDR panorama
                       (progn
                         (format t "HDR skybox not implemented yet~%")
                         (load-texture "resources/skybox.png"))
                       ;; Regular version
                       (let ((img (load-image "resources/skybox.png")))
                         (prog1 (load-texture-cubemap img +cubemap-layout-auto-detect+)
                           (unload-image img))))))

              ;; Set skybox texture to material
              (setf (material-map-texture (aref (material-maps (aref (model-materials skybox) 0)) 
                                               +material-map-cubemap+))
                    skybox-texture)

              (disable-cursor)  ; Limit cursor to relative movement inside the window
              (set-target-fps 60)  ; Set our game to run at 60 frames-per-second

              ;; Main game loop
              (loop until (window-should-close) do
                ;; Update
                (update-camera camera +camera-first-person+)

                ;; Handle file drop for new skybox textures
                (when (is-file-dropped)
                  (let ((dropped-files (load-dropped-files)))
                    (when (= (file-path-list-count dropped-files) 1)
                      (let ((file-path (aref (file-path-list-paths dropped-files) 0)))
                        (when (is-file-extension file-path ".png;.jpg;.hdr;.bmp;.tga")
                          ;; Unload current texture
                          (unload-texture skybox-texture)
                          
                          ;; Load new texture
                          (let ((img (load-image file-path)))
                            (setf skybox-texture (load-texture-cubemap img +cubemap-layout-auto-detect+))
                            (unload-image img))
                          
                          ;; Update material texture
                          (setf (material-map-texture (aref (material-maps (aref (model-materials skybox) 0)) 
                                                           +material-map-cubemap+))
                                skybox-texture))))
                    (unload-dropped-files dropped-files)))

                ;; Draw
                (begin-drawing)
                  (clear-background +raywhite+)

                  (begin-mode-3d camera)
                    ;; We are inside the cube, disable backface culling and depth mask
                    (rl-disable-backface-culling)
                    (rl-disable-depth-mask)
                    (draw-model skybox (vec3 0.0 0.0 0.0) 1.0 +white+)
                    (rl-enable-backface-culling)
                    (rl-enable-depth-mask)

                    (draw-grid 10 1.0)
                  (end-mode-3d)

                  (draw-text "Skybox example - drag and drop skybox image files" 
                            10 (- (get-screen-height) 20) 10 +black+)
                  (draw-fps 10 10)

                (end-drawing))

              ;; De-Initialization
              (unload-shader skybox-shader)
              (unload-texture skybox-texture)
              (unload-model skybox)))))

    ;; Close window
    (close-window)))

;; Run the example
(main)
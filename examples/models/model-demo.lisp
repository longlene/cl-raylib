;;;; 3D Model Demo for cl-raylib
;;;; This demonstrates comprehensive 3D model loading, manipulation, and rendering

(require :cl-raylib)

(defpackage :cl-raylib-model-demo
  (:use :cl :cl-raylib))

(in-package :cl-raylib-model-demo)

(defun model-demo ()
  "Comprehensive 3D model demo"
  (let ((screen-width 800)
        (screen-height 600))
    
    ;; Set window flags for better 3D performance
    (set-window-state (logior +flag-window-resizable+ +flag-vsync-hint+ +flag-msaa-4x-hint+))
    
    (with-window (screen-width screen-height "cl-raylib [models] - 3D Model Loading & Rendering Demo")
      (set-target-fps 60)
      
      ;; Initialize systems
      (init-texture-system)
      (init-model-rendering)
      
      ;; Create 3D camera
      (let* ((camera (camera3d-default))
             
             ;; Create various models
             (cube-model (load-model-cube 2.0 2.0 2.0))
             (sphere-model (load-model-sphere 1.5 16 32))
             (plane-model (load-model-plane 20.0 20.0 10 10))
             
             ;; Model positions
             (cube-position (vec3 -3.0 1.0 0.0))
             (sphere-position (vec3 0.0 1.5 0.0))
             (plane-position (vec3 0.0 0.0 0.0))
             
             ;; Animation variables
             (rotation 0.0)
             (wave-time 0.0)
             (model-scale 1.0)
             
             ;; Rendering mode flags
             (wireframe-mode nil)
             (show-bounding-boxes nil)
             (show-model-info nil)
             
             ;; Demo models array for easy iteration
             (demo-models nil))
        
        ;; Setup camera
        (camera3d-set-position camera (vec3 8.0 8.0 8.0))
        (camera3d-set-target camera (vec3 0.0 1.0 0.0))
        (set-camera-mode camera +camera-orbital+)
        
        ;; Create additional generated models
        (let ((pyramid-mesh (gen-mesh-cube 1.5 2.0 1.5))  ; Simple pyramid approximation
              (custom-mesh (create-custom-mesh)))
          
          ;; Transform pyramid mesh to look more like pyramid
          (mesh-transform pyramid-mesh (matrix4-scale 1.0 0.5 1.0))
          (mesh-calculate-normals pyramid-mesh)
          (upload-mesh pyramid-mesh)
          
          ;; Upload custom mesh
          (upload-mesh custom-mesh)
          
          ;; Create models from meshes
          (let ((pyramid-model (load-model-from-mesh pyramid-mesh))
                (custom-model (load-model-from-mesh custom-mesh)))
            
            (setf demo-models (list
                               (list :model cube-model :position (vec3 -4.0 1.0 0.0) :name "Cube")
                               (list :model sphere-model :position (vec3 -1.0 1.5 0.0) :name "Sphere")
                               (list :model pyramid-model :position (vec3 2.0 1.0 0.0) :name "Pyramid")
                               (list :model custom-model :position (vec3 5.0 1.0 0.0) :name "Custom")))))
        
        (loop until (window-should-close) do
          ;; Update animation
          (incf rotation 1.0)
          (incf wave-time 0.05)
          (setf model-scale (+ 0.8 (* 0.3 (sin wave-time))))
          
          ;; Update camera
          (update-camera camera)
          
          ;; Handle input
          (when (is-key-pressed +key-w+)
            (setf wireframe-mode (not wireframe-mode))
            (set-wireframe-mode wireframe-mode))
          (when (is-key-pressed +key-b+)
            (setf show-bounding-boxes (not show-bounding-boxes)))
          (when (is-key-pressed +key-i+)
            (setf show-model-info (not show-model-info)))
          (when (is-key-pressed +key-r+)
            (setf rotation 0.0)
            (setf wave-time 0.0))
          
          ;; Camera controls
          (when (is-key-pressed +key-one+)
            (set-camera-mode camera +camera-orbital+))
          (when (is-key-pressed +key-two+)
            (set-camera-mode camera +camera-free+))
          (when (is-key-pressed +key-three+)
            (set-camera-mode camera +camera-first-person+))
          
          ;; Drawing
          (with-drawing
            (clear-background +raywhite+)
            
            ;; 3D drawing
            (with-mode-3d camera
              ;; Draw ground plane
              (draw-model plane-model plane-position 1.0 +lightgray+)
              
              ;; Draw grid
              (draw-grid 20 1.0)
              
              ;; Draw all demo models
              (loop for model-data in demo-models
                    for i from 0 do
                (let* ((model (getf model-data :model))
                       (position (getf model-data :position))
                       (animated-position (vec3 (first position)
                                                (+ (second position) (* 0.5 (sin (+ wave-time (* i 0.5)))))
                                                (third position)))
                       (color (case i
                                (0 +red+)
                                (1 +blue+)
                                (2 +green+)
                                (3 +purple+)
                                (t +yellow+))))
                  
                  ;; Draw model with animation
                  (if wireframe-mode
                      (draw-model-wires-ex model animated-position 
                                          (vec3 0.0 1.0 0.0) rotation model-scale color)
                      (draw-model-ex model animated-position 
                                    (vec3 0.0 1.0 0.0) rotation model-scale color))
                  
                  ;; Draw bounding boxes if enabled
                  (when show-bounding-boxes
                    (let ((bbox (get-model-bounding-box model)))
                      (when bbox
                        ;; Transform bounding box to world space
                        (let ((transformed-min (vector3-add animated-position 
                                                           (vector3-scale (bounding-box-min bbox) model-scale)))
                              (transformed-max (vector3-add animated-position 
                                                           (vector3-scale (bounding-box-max bbox) model-scale))))
                          (draw-bounding-box (make-bounding-box :min transformed-min :max transformed-max) +yellow+)))))))
              
              ;; Draw some additional elements
              (draw-cube-v (vec3 0.0 4.0 0.0) (vec3 0.5 0.5 0.5) +orange+)
              (draw-sphere (vec3 0.0 6.0 0.0) 0.3 +magenta+)
              
              ;; Draw coordinate axes
              (draw-line-3d (vec3 0.0 0.1 0.0) (vec3 2.0 0.1 0.0) +red+)    ; X axis
              (draw-line-3d (vec3 0.0 0.1 0.0) (vec3 0.0 2.1 0.0) +green+)  ; Y axis
              (draw-line-3d (vec3 0.0 0.1 0.0) (vec3 0.0 0.1 2.0) +blue+))  ; Z axis
            
            ;; 2D UI overlay
            (draw-text "3D Model Demo" 10 10 20 +darkgray+)
            (draw-text "Controls:" 10 40 16 +darkgray+)
            (draw-text "- Mouse: Rotate camera" 10 60 12 +gray+)
            (draw-text "- W: Toggle wireframe" 10 75 12 +gray+)
            (draw-text "- B: Toggle bounding boxes" 10 90 12 +gray+)
            (draw-text "- I: Toggle model info" 10 105 12 +gray+)
            (draw-text "- R: Reset animation" 10 120 12 +gray+)
            (draw-text "- 1/2/3: Camera modes" 10 135 12 +gray+)
            (draw-text "- ESC: Exit" 10 150 12 +gray+)
            
            ;; Display rendering info
            (let ((info-y 180))
              (draw-text (format nil "Wireframe: ~a" wireframe-mode) 10 info-y 12 +blue+)
              (draw-text (format nil "Bounding Boxes: ~a" show-bounding-boxes) 10 (+ info-y 15) 12 +blue+)
              (draw-text (format nil "Camera Mode: ~a" 
                                (case *camera-mode*
                                  (+camera-orbital+ "Orbital")
                                  (+camera-free+ "Free") 
                                  (+camera-first-person+ "First Person")
                                  (t "Custom"))) 
                        10 (+ info-y 30) 12 +blue+)
              (draw-text (format nil "Rotation: ~,1f°" rotation) 10 (+ info-y 45) 12 +blue+)
              (draw-text (format nil "Scale: ~,2f" model-scale) 10 (+ info-y 60) 12 +blue+))
            
            ;; Display model information if enabled
            (when show-model-info
              (let ((info-x 400)
                    (info-y 60))
                (draw-text "Model Information:" info-x info-y 16 +darkblue+)
                (loop for model-data in demo-models
                      for i from 0 do
                  (let* ((model (getf model-data :model))
                         (name (getf model-data :name))
                         (line-y (+ info-y 30 (* i 50))))
                    (draw-text (format nil "~a:" name) info-x line-y 14 +darkgreen+)
                    (draw-text (format nil "  Meshes: ~d" (model-mesh-count model)) 
                              info-x (+ line-y 15) 10 +gray+)
                    (when (> (model-mesh-count model) 0)
                      (let ((mesh (first (model-meshes model))))
                        (draw-text (format nil "  Vertices: ~d" (mesh-vertex-count mesh)) 
                                  info-x (+ line-y 25) 10 +gray+)
                        (draw-text (format nil "  Triangles: ~d" (mesh-triangle-count mesh)) 
                                  info-x (+ line-y 35) 10 +gray+)))))))
            
            ;; Draw FPS and performance info
            (draw-text (format nil "FPS: ~d" 60) ; Placeholder FPS
                      (- screen-width 80) 10 16 +lime+)
            (draw-text (format nil "Models: ~d" (length demo-models))
                      (- screen-width 100) 30 12 +lime+)
            
            ;; Draw mini coordinate system
            (let ((axes-size 40)
                  (axes-x (- screen-width 60))
                  (axes-y (- screen-height 60)))
              (draw-line axes-x axes-y (+ axes-x axes-size) axes-y +red+)     ; X axis
              (draw-line axes-x axes-y axes-x (- axes-y axes-size) +green+)   ; Y axis
              (draw-text "X" (+ axes-x axes-size 5) (- axes-y 5) 10 +red+)
              (draw-text "Y" (- axes-x 15) (- axes-y axes-size 5) 10 +green+)
              (draw-text "Z" (- axes-x 15) (+ axes-y 15) 10 +blue+)))
        
        ;; Cleanup
        (loop for model-data in demo-models do
          (unload-model (getf model-data :model)))
        (unload-model plane-model)
        (cleanup-models)
        (cleanup-texture-system)))))

(defun create-custom-mesh ()
  "Create a custom mesh (simple tetrahedron)"
  (let ((vertices (list
                   ;; Base triangle
                   (create-vertex (vec3 -1.0 0.0 -1.0) (vec3 0.0 -1.0 0.0) (vec2 0.0 0.0) +white+)
                   (create-vertex (vec3  1.0 0.0 -1.0) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0) +white+)
                   (create-vertex (vec3  0.0 0.0  1.0) (vec3 0.0 -1.0 0.0) (vec2 0.5 1.0) +white+)
                   
                   ;; Top point for three side faces
                   (create-vertex (vec3  0.0 2.0  0.0) (vec3 0.0 1.0 0.0) (vec2 0.5 0.5) +white+)))
        
        (indices (list
                  ;; Base triangle
                  0 1 2
                  ;; Side faces
                  0 3 1   ; Front face
                  1 3 2   ; Right face  
                  2 3 0))) ; Left face
    
    (let ((mesh (create-mesh vertices indices)))
      (mesh-calculate-normals mesh)
      mesh)))

;; Run the demo
(model-demo)
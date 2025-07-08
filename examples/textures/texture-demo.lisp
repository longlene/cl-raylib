;;;; GPU Texture Demo for cl-raylib
;;;; This demonstrates comprehensive texture loading, manipulation, and drawing

(require :cl-raylib)

(defpackage :cl-raylib-texture-demo
  (:use :cl :cl-raylib))

(in-package :cl-raylib-texture-demo)

(defun texture-demo ()
  "Comprehensive texture and rendering demo"
  (let ((screen-width 800)
        (screen-height 600))
    
    ;; Set window flags for better visuals
    (set-window-state (logior +flag-window-resizable+ +flag-vsync-hint+ +flag-msaa-4x-hint+))
    
    (with-window (screen-width screen-height "cl-raylib [texture] - GPU Texture System Demo")
      (set-target-fps 60)
      
      ;; Initialize texture system
      (init-texture-system)
      
      ;; Create various textures for demonstration
      (let* (;; Create test images
             (red-image (gen-image-color 64 64 +red+))
             (green-image (gen-image-color 64 64 +green+))
             (blue-image (gen-image-color 32 32 +blue+))
             (gradient-image (gen-image-gradient-radial 128 128 0.0 +white+ +black+))
             (checker-image (gen-image-checked 64 64 8 8 +gray+ +white+))
             
             ;; Load textures from images
             (red-texture (load-texture-from-image red-image))
             (green-texture (load-texture-from-image green-image))
             (blue-texture (load-texture-from-image blue-image))
             (gradient-texture (load-texture-from-image gradient-image))
             (checker-texture (load-texture-from-image checker-image))
             
             ;; Create render texture for off-screen rendering
             (render-target (load-render-texture 200 200))
             
             ;; Demo variables
             (rotation 0.0)
             (scale 1.0)
             (texture-filter +texture-filter-bilinear+))
        
        ;; Set texture filtering
        (set-texture-filter gradient-texture texture-filter)
        (set-texture-wrap checker-texture +texture-wrap-repeat+)
        
        (loop until (window-should-close) do
          ;; Update demo variables
          (incf rotation 1.0)
          (setf scale (+ 0.8 (* 0.4 (sin (* rotation 0.05)))))
          
          ;; Handle input for texture filtering
          (when (is-key-pressed +key-one+)
            (setf texture-filter +texture-filter-point+)
            (set-texture-filter gradient-texture texture-filter))
          (when (is-key-pressed +key-two+)
            (setf texture-filter +texture-filter-bilinear+)
            (set-texture-filter gradient-texture texture-filter))
          (when (is-key-pressed +key-three+)
            (setf texture-filter +texture-filter-trilinear+)
            (set-texture-filter gradient-texture texture-filter))
          
          ;; Render to texture first
          (with-texture-mode render-target
            (clear-background +raywhite+)
            
            ;; Draw some stuff to render texture
            (draw-texture blue-texture 10 10 +white+)
            (draw-texture-ex red-texture (vec2 50 50) (* rotation 2) 0.5 +white+)
            (draw-circle 100 100 30 +green+))
          
          ;; Main drawing
          (with-drawing
            (clear-background +darkgray+)
            
            ;; Draw regular textures
            (draw-texture red-texture 50 50 +white+)
            (draw-texture-v green-texture (vec2 150 50) +white+)
            
            ;; Draw texture with transformations
            (draw-texture-ex blue-texture (vec2 250 50) rotation scale +white+)
            
            ;; Draw gradient texture with filtering demo
            (draw-texture-pro gradient-texture
                              (make-rectangle :x 0 :y 0 :width 128 :height 128)
                              (make-rectangle :x 350 :y 50 :width 100 :height 100)
                              (vec2 50 50) rotation +white+)
            
            ;; Draw checker texture (wrapped)
            (draw-texture-pro checker-texture
                              (make-rectangle :x 0 :y 0 :width 128 :height 128)
                              (make-rectangle :x 500 :y 50 :width 150 :height 100)
                              (vec2 0 0) 0.0 +white+)
            
            ;; Draw render texture result
            (draw-texture (render-texture-texture render-target) 50 200 +white+)
            (draw-text "Render Texture" 50 410 12 +white+)
            
            ;; Draw texture portions
            (draw-texture-rec gradient-texture 
                              (make-rectangle :x 32 :y 32 :width 64 :height 64)
                              (vec2 300 200) +white+)
            (draw-text "Texture Portion" 300 275 12 +white+)
            
            ;; Draw scaled textures
            (let* ((mouse-pos (get-mouse-position))
                   (dest-rect (make-rectangle :x (first mouse-pos) :y (second mouse-pos)
                                            :width (* 64 scale) :height (* 64 scale))))
              (draw-texture-pro red-texture
                                (make-rectangle :x 0 :y 0 :width 64 :height 64)
                                dest-rect
                                (vec2 (* 32 scale) (* 32 scale))
                                rotation
                                (color-alpha +yellow+ 0.8)))
            
            ;; Instructions
            (draw-text "GPU Texture System Demo" 10 10 20 +lightgray+)
            (draw-text "Controls:" 10 40 16 +lightgray+)
            (draw-text "- 1/2/3: Change texture filtering (Point/Bilinear/Trilinear)" 10 60 12 +gray+)
            (draw-text "- Mouse: Move yellow texture" 10 75 12 +gray+)
            (draw-text "- ESC: Exit" 10 90 12 +gray+)
            
            ;; Display texture info
            (let ((info-y 120))
              (draw-text (format nil "Texture Filter: ~a" 
                                (case texture-filter
                                  (+texture-filter-point+ "Point")
                                  (+texture-filter-bilinear+ "Bilinear") 
                                  (+texture-filter-trilinear+ "Trilinear")
                                  (t "Unknown"))) 
                        10 info-y 12 +lime+)
              (draw-text (format nil "Rotation: ~,1f°" rotation) 10 (+ info-y 15) 12 +lime+)
              (draw-text (format nil "Scale: ~,2f" scale) 10 (+ info-y 30) 12 +lime+)
              (draw-text (format nil "Mouse: ~a" (get-mouse-position)) 10 (+ info-y 45) 12 +lime+))
            
            ;; Draw FPS
            (draw-text (format nil "FPS: ~d" 60) ; Placeholder FPS
                      (- (get-screen-width) 80) 10 16 +green+)))
        
        ;; Cleanup
        (unload-texture red-texture)
        (unload-texture green-texture)
        (unload-texture blue-texture)
        (unload-texture gradient-texture)
        (unload-texture checker-texture)
        (unload-render-texture render-target)
        (unload-image red-image)
        (unload-image green-image)
        (unload-image blue-image)
        (unload-image gradient-image)
        (unload-image checker-image)
        (cleanup-texture-system)))))

;; Run the demo
(texture-demo)
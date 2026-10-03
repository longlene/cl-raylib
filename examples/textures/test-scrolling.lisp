(ql:quickload :cl-raylib :silent t)
(in-package :cl-raylib)

(init-window 800 450 "Test Scrolling")

(let ((background (load-texture-from-image 
                    (gen-image-gradient-linear 512 192 0 +darkblue+ +blue+)))
      (scrolling-back 0.0))
  
  (set-target-fps 60)
  
  (dotimes (i 300) ; Run for 5 seconds
    (when (window-should-close) (return))
    
    ;; Update scrolling
    (decf scrolling-back 1.0)
    (when (<= scrolling-back (- (* (texture-width background) 2)))
      (setf scrolling-back 0.0))
    
    ;; Draw
    (begin-drawing)
      (clear-background +black+)
      
      ;; Draw background layer twice for seamless scrolling
      (draw-texture-ex background 
                       (vec2 scrolling-back 100) 
                       0.0 2.0 +white+)
      (draw-texture-ex background 
                       (vec2 (+ (* (texture-width background) 2) scrolling-back) 100) 
                       0.0 2.0 +white+)
      
      (draw-text "SCROLLING TEST" 10 10 20 +red+)
      (draw-text (format nil "Scrolling position: ~,1f" scrolling-back) 10 40 20 +white+)
      
    (end-drawing))
  
  (unload-texture background))

(close-window)
(format t "Scrolling test completed!~%")
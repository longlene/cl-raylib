;;;; Collision Detection Demo for cl-raylib
;;;; This demonstrates the comprehensive collision detection system

(require :cl-raylib)

(defpackage :cl-raylib-collision-demo
  (:use :cl :cl-raylib))

(in-package :cl-raylib-collision-demo)

(defun collision-demo ()
  "Demonstrate collision detection features"
  (let ((screen-width 1200)
        (screen-height 800))
    
    ;; Initialize logging system
    (set-trace-log-level +log-info+)
    (trace-log-info "Starting Collision Detection Demo")
    
    ;; Set window flags
    (set-window-state (logior +flag-window-resizable+ +flag-vsync-hint+))
    
    (with-window (screen-width screen-height "cl-raylib [collision] - Collision Detection Demo")
      (set-target-fps 60)
      
      ;; Initialize systems
      (init-texture-system)
      (init-text-system)
      (init-collision-system)
      
      (let* (;; Demo state
             (demo-mode 0) ; 0=2D basic, 1=2D advanced, 2=3D, 3=physics
             (mode-names '("2D Basic Shapes" "2D Advanced" "3D Collision" "Physics Response"))
             
             ;; 2D Objects
             (player-circle (make-circle-at 100 100 20))
             (static-boxes (list (make-aabb-from-center-size 300 150 80 60)
                                (make-aabb-from-center-size 500 300 100 40)
                                (make-aabb-from-center-size 200 400 60 80)))
             (moving-circle (make-circle-at 400 200 15))
             (test-line (make-line-segment :start (vec2 50 500) :end (vec2 150 550)))
             
             ;; 3D Objects
             (player-sphere (make-sphere-at 0 0 0 1.0))
             (static-boxes-3d (list (make-aabb3d-from-center-size 3 0 0 2 2 2)
                                   (make-aabb3d-from-center-size -3 0 0 1 3 1)))
             
             ;; Animation and movement
             (time-counter 0.0)
             (mouse-pos (vec2 0 0))
             (collision-results nil)
             (show-collision-info t)
             
             ;; Physics simulation
             (physics-objects nil)
             (gravity (vec2 0 200)) ; Pixels per second squared
             (damping 0.98))
        
        ;; Initialize physics objects
        (setf physics-objects
              (list (list :type :circle
                         :shape (make-circle-at 150 100 12)
                         :velocity (vec2 50 0)
                         :mass 1.0
                         :restitution 0.8)
                    (list :type :circle
                         :shape (make-circle-at 250 150 8)
                         :velocity (vec2 -30 20)
                         :mass 0.5
                         :restitution 0.9)
                    (list :type :rectangle
                         :shape (make-aabb-from-center-size 350 200 30 30)
                         :velocity (vec2 0 0)
                         :mass 2.0
                         :restitution 0.6)))
        
        (trace-log-info "Collision demo initialized")
        
        (loop until (window-should-close) do
          ;; Update
          (incf time-counter 0.016)
          (setf mouse-pos (get-mouse-position))
          
          ;; Handle input
          (when (is-key-pressed +key-tab+)
            (setf demo-mode (mod (1+ demo-mode) (length mode-names))))
          
          (when (is-key-pressed +key-space+)
            (setf show-collision-info (not show-collision-info)))
          
          (when (is-key-pressed +key-r+)
            (reset-demo-objects physics-objects))
          
          ;; Update objects based on demo mode
          (case demo-mode
            (0 (update-2d-basic mouse-pos player-circle moving-circle time-counter))
            (1 (update-2d-advanced mouse-pos test-line time-counter))
            (2 (update-3d-objects mouse-pos player-sphere time-counter))
            (3 (update-physics-simulation physics-objects gravity damping 0.016)))
          
          ;; Perform collision detection
          (setf collision-results (perform-collision-detection demo-mode player-circle static-boxes
                                                              moving-circle test-line player-sphere 
                                                              static-boxes-3d physics-objects))
          
          ;; Drawing
          (with-drawing
            (clear-background +raywhite+)
            
            ;; Draw title
            (draw-text "PURE-RAYLIB COLLISION DETECTION DEMO" 20 20 24 +darkblue+)
            (draw-text (format nil "Mode: ~a (TAB to switch)" (nth demo-mode mode-names)) 20 50 16 +darkgray+)
            
            ;; Draw mode-specific content
            (case demo-mode
              (0 (draw-2d-basic-demo player-circle static-boxes moving-circle collision-results))
              (1 (draw-2d-advanced-demo test-line static-boxes collision-results))
              (2 (draw-3d-demo player-sphere static-boxes-3d collision-results))
              (3 (draw-physics-demo physics-objects collision-results)))
            
            ;; Draw common UI
            (draw-collision-info collision-results show-collision-info)
            (draw-common-collision-controls demo-mode)
            
            ;; Draw performance info
            (let ((info-x (- screen-width 300))
                  (info-y 20))
              (draw-text "Performance:" info-x info-y 16 +darkgreen+)
              (draw-text (format nil "FPS: ~d" (get-fps)) info-x (+ info-y 25) 14 +green+)
              (draw-text (format nil "Collision Checks: ~d" (length collision-results)) info-x (+ info-y 45) 14 +green+)
              (draw-text (format nil "Mouse: ~,0f, ~,0f" (vx mouse-pos) (vy mouse-pos)) info-x (+ info-y 65) 12 +gray+)))
        
        ;; Cleanup
        (trace-log-info "Collision demo completed")
        (cleanup-collision-system)
        (cleanup-text-system)
        (cleanup-texture-system))))))

(defun update-2d-basic (mouse-pos player-circle moving-circle time-counter)
  "Update 2D basic demo objects"
  ;; Move player circle to mouse position
  (setf (circle-center player-circle) mouse-pos)
  
  ;; Move the moving circle in a circular pattern
  (let ((center-x 400)
        (center-y 300)
        (radius 50))
    (setf (circle-center moving-circle) 
          (vec2 (+ center-x (* radius (cos time-counter)))
                (+ center-y (* radius (sin time-counter)))))))

(defun update-2d-advanced (mouse-pos test-line time-counter)
  "Update 2D advanced demo objects"
  ;; Update line end point to follow mouse
  (setf (line-segment-end test-line) mouse-pos)
  
  ;; Animate line start point
  (setf (line-segment-start test-line)
        (vec2 (+ 100 (* 50 (sin time-counter)))
              (+ 500 (* 30 (cos (* time-counter 1.5)))))))

(defun update-3d-objects (mouse-pos player-sphere time-counter)
  "Update 3D demo objects"
  ;; Map 2D mouse to 3D position
  (let ((x (/ (- (vx mouse-pos) 600) 100))  ; Center around screen middle
        (y (/ (- 400 (vy mouse-pos)) 100))  ; Invert Y axis
        (z (* 2 (sin time-counter))))            ; Animate Z
    (setf (sphere-center player-sphere) (vec3 x y z))))

(defun update-physics-simulation (physics-objects gravity damping dt)
  "Update physics simulation"
  (loop for obj in physics-objects do
    (let ((shape (getf obj :shape))
          (velocity (getf obj :velocity))
          (mass (getf obj :mass)))
      
      ;; Apply gravity
      (setf velocity (vector2-add velocity (vector2-scale gravity dt)))
      
      ;; Apply damping
      (setf velocity (vector2-scale velocity damping))
      
      ;; Update position
      (case (getf obj :type)
        (:circle
         (let ((center (circle-center shape)))
           (setf (circle-center shape) (vector2-add center (vector2-scale velocity dt)))))
        (:rectangle
         (let ((center (get-aabb-center shape))
               (width (get-aabb-width shape))
               (height (get-aabb-height shape)))
           (let ((new-center (vector2-add center (vector2-scale velocity dt))))
             (setf (getf obj :shape) (make-aabb-from-center-size (first new-center) (second new-center) width height))))))
      
      ;; Boundary collision
      (handle-boundary-collision obj 1200 800)
      
      ;; Update velocity in object
      (setf (getf obj :velocity) velocity))))

(defun handle-boundary-collision (obj screen-width screen-height)
  "Handle collision with screen boundaries"
  (let ((shape (getf obj :shape))
        (velocity (getf obj :velocity))
        (restitution (getf obj :restitution)))
    
    (case (getf obj :type)
      (:circle
       (let ((center (circle-center shape))
             (radius (circle-radius shape)))
         (when (or (<= (vx center) radius) (>= (vx center) (- screen-width radius)))
           (setf (first velocity) (* (first velocity) (- restitution))))
         (when (or (<= (vy center) radius) (>= (vy center) (- screen-height radius)))
           (setf (second velocity) (* (second velocity) (- restitution))))
         
         ;; Clamp position to boundaries
         (setf (circle-center shape) 
               (vec2 (clamp (vx center) radius (- screen-width radius))
                     (clamp (vy center) radius (- screen-height radius))))))
      
      (:rectangle
       (let* ((center (get-aabb-center shape))
              (half-width (/ (get-aabb-width shape) 2))
              (half-height (/ (get-aabb-height shape) 2)))
         (when (or (<= (vx center) half-width) (>= (vx center) (- screen-width half-width)))
           (setf (first velocity) (* (first velocity) (- restitution))))
         (when (or (<= (vy center) half-height) (>= (vy center) (- screen-height half-height)))
           (setf (second velocity) (* (second velocity) (- restitution)))))))
    
    (setf (getf obj :velocity) velocity)))

(defun perform-collision-detection (demo-mode player-circle static-boxes moving-circle test-line 
                                   player-sphere static-boxes-3d physics-objects)
  "Perform collision detection based on current demo mode"
  (let ((results nil))
    (case demo-mode
      (0 ; 2D Basic
       ;; Check player circle against static boxes
       (loop for box in static-boxes do
         (when (check-collision-circle-rectangle player-circle box)
           (push (list :type "Circle-Rectangle" :object1 player-circle :object2 box :colliding t) results)))
       
       ;; Check player circle against moving circle
       (when (check-collision-circles player-circle moving-circle)
         (push (list :type "Circle-Circle" :object1 player-circle :object2 moving-circle :colliding t) results)))
      
      (1 ; 2D Advanced
       ;; Check line against static boxes
       (loop for box in static-boxes do
         (when (check-collision-line-rectangle test-line box)
           (push (list :type "Line-Rectangle" :object1 test-line :object2 box :colliding t) results))))
      
      (2 ; 3D
       ;; Check player sphere against 3D boxes
       (loop for box in static-boxes-3d do
         (when (check-collision-sphere-aabb3d player-sphere box)
           (push (list :type "Sphere-AABB3D" :object1 player-sphere :object2 box :colliding t) results))))
      
      (3 ; Physics
       ;; Check physics objects against each other
       (loop for i from 0 below (length physics-objects) do
         (loop for j from (1+ i) below (length physics-objects) do
           (let ((obj1 (nth i physics-objects))
                 (obj2 (nth j physics-objects)))
             (when (check-physics-collision obj1 obj2)
               (push (list :type "Physics-Object" :object1 obj1 :object2 obj2 :colliding t) results)))))))
    
    results))

(defun check-physics-collision (obj1 obj2)
  "Check collision between two physics objects"
  (let ((shape1 (getf obj1 :shape))
        (shape2 (getf obj2 :shape))
        (type1 (getf obj1 :type))
        (type2 (getf obj2 :type)))
    
    (cond
      ((and (eq type1 :circle) (eq type2 :circle))
       (check-collision-circles shape1 shape2))
      ((and (eq type1 :rectangle) (eq type2 :rectangle))
       (check-collision-rectangle-rectangle shape1 shape2))
      ((or (and (eq type1 :circle) (eq type2 :rectangle))
           (and (eq type1 :rectangle) (eq type2 :circle)))
       (let ((circle (if (eq type1 :circle) shape1 shape2))
             (rect (if (eq type1 :rectangle) shape1 shape2)))
         (check-collision-circle-rectangle circle rect)))
      (t nil))))

(defun draw-2d-basic-demo (player-circle static-boxes moving-circle collision-results)
  "Draw 2D basic collision demo"
  (let ((y-offset 100))
    (draw-text "2D BASIC COLLISION DETECTION" 20 y-offset 20 +darkblue+)
    (draw-text "Move mouse to control blue circle" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Draw static boxes
    (loop for box in static-boxes do
      (let ((colliding (find-if (lambda (result) 
                                 (and (string= (getf result :type) "Circle-Rectangle")
                                      (eq (getf result :object2) box))) 
                               collision-results)))
        (draw-rectangle (vx (aabb-min box)) (vy (aabb-min box))
                       (get-aabb-width box) (get-aabb-height box)
                       (if colliding +red+ +lightgray+))
        (draw-rectangle-lines (vx (aabb-min box)) (vy (aabb-min box))
                             (get-aabb-width box) (get-aabb-height box) +black+)))
    
    ;; Draw moving circle
    (let ((colliding (find-if (lambda (result) 
                               (string= (getf result :type) "Circle-Circle")) 
                             collision-results)))
      (draw-circle (round (vx (circle-center moving-circle))) 
                  (round (vy (circle-center moving-circle)))
                  (circle-radius moving-circle)
                  (if colliding +orange+ +green+))
      (draw-circle-lines (round (vx (circle-center moving-circle))) 
                        (round (vy (circle-center moving-circle)))
                        (circle-radius moving-circle) +darkgreen+))
    
    ;; Draw player circle
    (draw-circle (round (vx (circle-center player-circle))) 
                (round (vy (circle-center player-circle)))
                (circle-radius player-circle) +blue+)
    (draw-circle-lines (round (vx (circle-center player-circle))) 
                      (round (vy (circle-center player-circle)))
                      (circle-radius player-circle) +darkblue+)
    
    ;; Draw legend
    (draw-text "Legend:" 20 (+ y-offset 400) 16 +darkgreen+)
    (draw-text "Blue Circle: Player (mouse controlled)" 40 (+ y-offset 430) 12 +blue+)
    (draw-text "Green Circle: Moving object" 40 (+ y-offset 450) 12 +green+)
    (draw-text "Gray Rectangles: Static obstacles" 40 (+ y-offset 470) 12 +gray+)
    (draw-text "Red: Collision detected" 40 (+ y-offset 490) 12 +red+)))

(defun draw-2d-advanced-demo (test-line static-boxes collision-results)
  "Draw 2D advanced collision demo"
  (let ((y-offset 100))
    (draw-text "2D ADVANCED COLLISION DETECTION" 20 y-offset 20 +darkblue+)
    (draw-text "Move mouse to control line end point" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Draw static boxes
    (loop for box in static-boxes do
      (let ((colliding (find-if (lambda (result) 
                                 (and (string= (getf result :type) "Line-Rectangle")
                                      (eq (getf result :object2) box))) 
                               collision-results)))
        (draw-rectangle (vx (aabb-min box)) (vy (aabb-min box))
                       (get-aabb-width box) (get-aabb-height box)
                       (if colliding +red+ +lightgray+))
        (draw-rectangle-lines (vx (aabb-min box)) (vy (aabb-min box))
                             (get-aabb-width box) (get-aabb-height box) +black+)))
    
    ;; Draw line
    (let ((colliding (some (lambda (result) 
                            (string= (getf result :type) "Line-Rectangle")) 
                          collision-results)))
      (draw-line-v (line-segment-start test-line) (line-segment-end test-line)
                  (if colliding +red+ +purple+))
      
      ;; Draw line endpoints
      (draw-circle (round (first (line-segment-start test-line))) 
                  (round (second (line-segment-start test-line))) 4 +darkblue+)
      (draw-circle (round (first (line-segment-end test-line))) 
                  (round (second (line-segment-end test-line))) 4 +blue+))
    
    ;; Draw algorithm info
    (draw-text "Line-Rectangle Collision Algorithm:" 20 (+ y-offset 350) 16 +darkgreen+)
    (draw-text "1. Check if line endpoints are inside rectangle" 40 (+ y-offset 380) 12 +black+)
    (draw-text "2. Use parametric line equation for intersection" 40 (+ y-offset 400) 12 +black+)
    (draw-text "3. Test against each rectangle edge" 40 (+ y-offset 420) 12 +black+)
    (draw-text "4. Return true if any intersection found" 40 (+ y-offset 440) 12 +black+)))

(defun draw-3d-demo (player-sphere static-boxes-3d collision-results)
  "Draw 3D collision demo"
  (let ((y-offset 100))
    (draw-text "3D COLLISION DETECTION" 20 y-offset 20 +darkblue+)
    (draw-text "Move mouse to control sphere position" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Draw 3D scene (projected to 2D)
    (let ((projection-center-x 400)
          (projection-center-y 300)
          (scale 50))
      
      ;; Draw static 3D boxes (as wireframes)
      (loop for box in static-boxes-3d do
        (let ((colliding (find-if (lambda (result) 
                                   (and (string= (getf result :type) "Sphere-AABB3D")
                                        (eq (getf result :object2) box))) 
                                 collision-results)))
          (draw-3d-aabb-wireframe box projection-center-x projection-center-y scale 
                                 (if colliding +red+ +gray+))))
      
      ;; Draw player sphere
      (let* ((sphere-center (sphere-center player-sphere))
             (projected-x (+ projection-center-x (* (first sphere-center) scale)))
             (projected-y (+ projection-center-y (* (- (second sphere-center)) scale))) ; Invert Y
             (projected-radius (* (sphere-radius player-sphere) scale)))
        (draw-circle (round projected-x) (round projected-y) (round projected-radius) +blue+)
        (draw-circle-lines (round projected-x) (round projected-y) (round projected-radius) +darkblue+))
      
      ;; Draw coordinate system
      (draw-line projection-center-x projection-center-y 
                (+ projection-center-x (* scale 2)) projection-center-y +red+) ; X axis
      (draw-line projection-center-x projection-center-y 
                projection-center-x (- projection-center-y (* scale 2)) +green+) ; Y axis
      (draw-text "X" (+ projection-center-x (* scale 2) 10) (- projection-center-y 5) 12 +red+)
      (draw-text "Y" (- projection-center-x 10) (- projection-center-y (* scale 2) 10) 12 +green+))
    
    ;; Draw 3D info
    (draw-text "3D Collision Features:" 20 (+ y-offset 400) 16 +darkgreen+)
    (draw-text "• Sphere-AABB collision detection" 40 (+ y-offset 430) 12 +black+)
    (draw-text "• 3D coordinate system mapping" 40 (+ y-offset 450) 12 +black+)
    (draw-text "• Z-axis animation with sine wave" 40 (+ y-offset 470) 12 +black+)
    (draw-text "• Wireframe 3D box visualization" 40 (+ y-offset 490) 12 +black+)))

(defun draw-3d-aabb-wireframe (aabb3d center-x center-y scale color)
  "Draw 3D AABB as wireframe"
  (let* ((min-point (aabb3d-min aabb3d))
         (max-point (aabb3d-max aabb3d))
         (x1 (+ center-x (* (first min-point) scale)))
         (y1 (+ center-y (* (- (second min-point)) scale)))
         (x2 (+ center-x (* (first max-point) scale)))
         (y2 (+ center-y (* (- (second max-point)) scale))))
    
    ;; Draw front face
    (draw-rectangle-lines (round x1) (round y1) (round (- x2 x1)) (round (- y2 y1)) color)
    
    ;; Draw back face offset (simple 3D effect)
    (let ((offset 10))
      (draw-rectangle-lines (round (- x1 offset)) (round (- y1 offset)) 
                           (round (- x2 x1)) (round (- y2 y1)) color)
      
      ;; Connect corners
      (draw-line (round x1) (round y1) (round (- x1 offset)) (round (- y1 offset)) color)
      (draw-line (round x2) (round y1) (round (- x2 offset)) (round (- y1 offset)) color)
      (draw-line (round x1) (round y2) (round (- x1 offset)) (round (- y2 offset)) color)
      (draw-line (round x2) (round y2) (round (- x2 offset)) (round (- y2 offset)) color))))

(defun draw-physics-demo (physics-objects collision-results)
  "Draw physics simulation demo"
  (let ((y-offset 100))
    (draw-text "PHYSICS COLLISION RESPONSE" 20 y-offset 20 +darkblue+)
    (draw-text "Press R to reset objects" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Draw physics objects
    (loop for obj in physics-objects do
      (let ((shape (getf obj :shape))
            (type (getf obj :type))
            (velocity (getf obj :velocity))
            (colliding (find-if (lambda (result) 
                                 (or (eq (getf result :object1) obj)
                                     (eq (getf result :object2) obj))) 
                               collision-results)))
        
        (case type
          (:circle
           (draw-circle (round (vx (circle-center shape))) 
                       (round (vy (circle-center shape)))
                       (circle-radius shape)
                       (if colliding +orange+ +cyan+))
           (draw-circle-lines (round (vx (circle-center shape))) 
                             (round (vy (circle-center shape)))
                             (circle-radius shape) +black+))
          (:rectangle
           (draw-rectangle (vx (aabb-min shape)) (vy (aabb-min shape))
                          (get-aabb-width shape) (get-aabb-height shape)
                          (if colliding +orange+ +magenta+))
           (draw-rectangle-lines (vx (aabb-min shape)) (vy (aabb-min shape))
                                (get-aabb-width shape) (get-aabb-height shape) +black+)))
        
        ;; Draw velocity vector
        (let ((center (case type
                       (:circle (circle-center shape))
                       (:rectangle (get-aabb-center shape)))))
          (when (> (vector2-length velocity) 1.0)
            (let ((vel-end (vector2-add center (vector2-scale (vector2-normalize velocity) 30))))
              (draw-line-v center vel-end +darkblue+)
              (draw-circle (round (first vel-end)) (round (second vel-end)) 3 +darkblue+))))))
    
    ;; Draw physics info
    (draw-text "Physics Properties:" 20 (+ y-offset 500) 16 +darkgreen+)
    (draw-text "• Gravity simulation" 40 (+ y-offset 530) 12 +black+)
    (draw-text "• Velocity damping" 40 (+ y-offset 550) 12 +black+)
    (draw-text "• Boundary collision response" 40 (+ y-offset 570) 12 +black+)
    (draw-text "• Different object masses and restitution" 40 (+ y-offset 590) 12 +black+)))

(defun draw-collision-info (collision-results show-info)
  "Draw collision information panel"
  (when show-info
    (let ((info-x 20)
          (info-y 650))
      (draw-text "Collision Information:" info-x info-y 14 +darkblue+)
      (if collision-results
        (progn
          (draw-text (format nil "Active Collisions: ~d" (length collision-results)) info-x (+ info-y 20) 12 +black+)
          (loop for result in (subseq collision-results 0 (min 3 (length collision-results)))
                for i from 0 do
            (draw-text (format nil "~d. ~a" (1+ i) (getf result :type)) 
                      (+ info-x 20) (+ info-y 40 (* i 15)) 10 +darkgreen+)))
        (draw-text "No collisions detected" info-x (+ info-y 20) 12 +gray+)))))

(defun draw-common-collision-controls (demo-mode)
  "Draw common control instructions"
  (let ((controls-y 750))
    (draw-text "Controls:" 20 controls-y 14 +darkblue+)
    (let ((controls (case demo-mode
                     (3 "TAB: Switch modes  SPACE: Toggle info  R: Reset physics")
                     (t "TAB: Switch modes  SPACE: Toggle collision info"))))
      (draw-text controls 20 (+ controls-y 20) 12 +gray+))))

(defun reset-demo-objects (physics-objects)
  "Reset physics objects to initial positions"
  (setf (getf (first physics-objects) :shape) (make-circle-at 150 100 12))
  (setf (getf (first physics-objects) :velocity) (vec2 50 0))
  (setf (getf (second physics-objects) :shape) (make-circle-at 250 150 8))
  (setf (getf (second physics-objects) :velocity) (vec2 -30 20))
  (setf (getf (third physics-objects) :shape) (make-aabb-from-center-size 350 200 30 30))
  (setf (getf (third physics-objects) :velocity) (vec2 0 0))
  (trace-log-info "Physics objects reset"))

;; Run the demo
(collision-demo)

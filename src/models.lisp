(in-package #:cl-raylib)

;;;===================================================================================
;;; rmodels - Basic functions to draw 3d shapes and load and draw 3d models
;;; Port of raylib/src/rmodels.c (GRAPHICS_API_OPENGL_33 path)
;;;
;;; CONFIGURATION (raylib config.h defaults):
;;;   SUPPORT_FILEFORMAT_OBJ/MTL/IQM/GLTF/VOX/M3D, SUPPORT_MESH_GENERATION enabled,
;;;   SUPPORT_GPU_SKINNING disabled
;;;
;;; NOTE: Vector3/Vector2 arguments accept 3d-vectors vecs or lists, colors accept
;;; color lists or color keywords, Matrix values are 3d-matrices mat4
;;; NOTE: Mesh vertex data arrays are specialized vectors: single-float for float
;;; attributes, (unsigned-byte 8) for colors/bone indices, (unsigned-byte 16) for indices
;;;===================================================================================

;;;----------------------------------------------------------------------------------
;;; Defines and Macros
;;;----------------------------------------------------------------------------------
(defconstant +max-mesh-vertex-buffers+ 7 "Maximum vertex buffers (VBO) per mesh")
(defconstant +max-filepath-length+ 4096 "Maximum length for filepaths (Linux PATH_MAX default value)")

;;;----------------------------------------------------------------------------------
;;; Module Internal Functions
;;;----------------------------------------------------------------------------------
(declaim (inline %z %vertex3))

(defun %z (v) (float (if (consp v) (third v) (vz v)) 1.0))

(defun %v3 (v)
  "Vector3 argument as a vec3"
  (if (consp v) (vec3 (float (first v) 1.0) (float (second v) 1.0) (float (third v) 1.0)) v))

(defun %vertex3 (x y z)
  (rl-vertex3f (float x 1.0) (float y 1.0) (float z 1.0)))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition
;;;----------------------------------------------------------------------------------

;; Draw a line in 3D world space
(defun draw-line-3d (start-pos end-pos color)
  "Draw a line in 3D world space"
  (rl-begin +rl-lines+)
  (%color color)
  (%vertex3 (%x start-pos) (%y start-pos) (%z start-pos))
  (%vertex3 (%x end-pos) (%y end-pos) (%z end-pos))
  (rl-end))

;; Draw a point in 3D space, actually a small line
;; WARNING: OpenGL ES 2.0 does not support point mode drawing
(defun draw-point-3d (position color)
  "Draw a point in 3D space, actually a small line"
  (rl-push-matrix)
  (rl-translatef (%x position) (%y position) (%z position))
  (rl-begin +rl-lines+)
  (%color color)
  (rl-vertex3f 0.0 0.0 0.0)
  (rl-vertex3f 0.0 0.0 0.1)
  (rl-end)
  (rl-pop-matrix))

;; Draw a circle in 3D world space
(defun draw-circle-3d (center radius rotation-axis rotation-angle color)
  "Draw a circle in 3D world space"
  (let ((radius (float radius 1.0)))
    (rl-push-matrix)
    (rl-translatef (%x center) (%y center) (%z center))
    (rl-rotatef rotation-angle (%x rotation-axis) (%y rotation-axis) (%z rotation-axis))
    (rl-begin +rl-lines+)
    (loop for i from 0 below 360 by 10
          do (%color color)
             (rl-vertex3f (* (%sinf (* +deg2rad+ i)) radius) (* (%cosf (* +deg2rad+ i)) radius) 0.0)
             (rl-vertex3f (* (%sinf (* +deg2rad+ (+ i 10))) radius) (* (%cosf (* +deg2rad+ (+ i 10))) radius) 0.0))
    (rl-end)
    (rl-pop-matrix)))

;; Draw a color-filled triangle (vertex in counter-clockwise order!)
(defun draw-triangle-3d (v1 v2 v3 color)
  "Draw a color-filled triangle (vertex in counter-clockwise order!)"
  (rl-begin +rl-triangles+)
  (%color color)
  (%vertex3 (%x v1) (%y v1) (%z v1))
  (%vertex3 (%x v2) (%y v2) (%z v2))
  (%vertex3 (%x v3) (%y v3) (%z v3))
  (rl-end))

;; Draw a triangle strip defined by points
(defun draw-triangle-strip-3d (points point-count color)
  "Draw a triangle strip defined by points"
  (when (< point-count 3) (return-from draw-triangle-strip-3d)) ; Security check
  (let ((points (%points points)))
    (flet ((v (i) (let ((p (svref points i))) (%vertex3 (%x p) (%y p) (%z p)))))
      (rl-begin +rl-triangles+)
      (%color color)
      (loop for i from 2 below point-count
            do (if (= (mod i 2) 0)
                   (progn (v i) (v (- i 2)) (v (- i 1)))
                   (progn (v i) (v (- i 1)) (v (- i 2)))))
      (rl-end))))

;; Draw cube
;; NOTE: Cube position is the center position
(defun draw-cube (position width height length color)
  "Draw cube"
  (let ((x 0.0) (y 0.0) (z 0.0)
        (width (float width 1.0)) (height (float height 1.0)) (length (float length 1.0)))
    (rl-push-matrix)
    ;; NOTE: Transformation is applied in inverse order (scale -> rotate -> translate)
    (rl-translatef (%x position) (%y position) (%z position))
    ;;rlRotatef(45, 0, 1, 0);
    ;;rlScalef(1.0f, 1.0f, 1.0f);   // NOTE: Vertices are directly scaled on definition
    (rl-begin +rl-triangles+)
    (%color color)
    (let ((x- (- x (/ width 2))) (x+ (+ x (/ width 2)))
          (y- (- y (/ height 2))) (y+ (+ y (/ height 2)))
          (z- (- z (/ length 2))) (z+ (+ z (/ length 2))))
      ;; Front face
      (rl-normal3f 0.0 0.0 1.0)
      (rl-vertex3f x- y- z+)            ; Bottom Left
      (rl-vertex3f x+ y- z+)            ; Bottom Right
      (rl-vertex3f x- y+ z+)            ; Top Left

      (rl-vertex3f x+ y+ z+)            ; Top Right
      (rl-vertex3f x- y+ z+)            ; Top Left
      (rl-vertex3f x+ y- z+)            ; Bottom Right

      ;; Back face
      (rl-normal3f 0.0 0.0 -1.0)
      (rl-vertex3f x- y- z-)            ; Bottom Left
      (rl-vertex3f x- y+ z-)            ; Top Left
      (rl-vertex3f x+ y- z-)            ; Bottom Right

      (rl-vertex3f x+ y+ z-)            ; Top Right
      (rl-vertex3f x+ y- z-)            ; Bottom Right
      (rl-vertex3f x- y+ z-)            ; Top Left

      ;; Top face
      (rl-normal3f 0.0 1.0 0.0)
      (rl-vertex3f x- y+ z-)            ; Top Left
      (rl-vertex3f x- y+ z+)            ; Bottom Left
      (rl-vertex3f x+ y+ z+)            ; Bottom Right

      (rl-vertex3f x+ y+ z-)            ; Top Right
      (rl-vertex3f x- y+ z-)            ; Top Left
      (rl-vertex3f x+ y+ z+)            ; Bottom Right

      ;; Bottom face
      (rl-normal3f 0.0 -1.0 0.0)
      (rl-vertex3f x- y- z-)            ; Top Left
      (rl-vertex3f x+ y- z+)            ; Bottom Right
      (rl-vertex3f x- y- z+)            ; Bottom Left

      (rl-vertex3f x+ y- z-)            ; Top Right
      (rl-vertex3f x+ y- z+)            ; Bottom Right
      (rl-vertex3f x- y- z-)            ; Top Left

      ;; Right face
      (rl-normal3f 1.0 0.0 0.0)
      (rl-vertex3f x+ y- z-)            ; Bottom Right
      (rl-vertex3f x+ y+ z-)            ; Top Right
      (rl-vertex3f x+ y+ z+)            ; Top Left

      (rl-vertex3f x+ y- z+)            ; Bottom Left
      (rl-vertex3f x+ y- z-)            ; Bottom Right
      (rl-vertex3f x+ y+ z+)            ; Top Left

      ;; Left face
      (rl-normal3f -1.0 0.0 0.0)
      (rl-vertex3f x- y- z-)            ; Bottom Right
      (rl-vertex3f x- y+ z+)            ; Top Left
      (rl-vertex3f x- y+ z-)            ; Top Right

      (rl-vertex3f x- y- z+)            ; Bottom Left
      (rl-vertex3f x- y+ z+)            ; Top Left
      (rl-vertex3f x- y- z-))           ; Bottom Right
    (rl-end)
    (rl-pop-matrix)))

;; Draw cube (Vector version)
(defun draw-cube-v (position size color)
  "Draw cube (Vector version)"
  (draw-cube position (%x size) (%y size) (%z size) color))

;; Draw cube wires
(defun draw-cube-wires (position width height length color)
  "Draw cube wires"
  (let ((x 0.0) (y 0.0) (z 0.0)
        (width (float width 1.0)) (height (float height 1.0)) (length (float length 1.0)))
    (rl-push-matrix)
    (rl-translatef (%x position) (%y position) (%z position))
    (rl-begin +rl-lines+)
    (%color color)
    (let ((x- (- x (/ width 2))) (x+ (+ x (/ width 2)))
          (y- (- y (/ height 2))) (y+ (+ y (/ height 2)))
          (z- (- z (/ length 2))) (z+ (+ z (/ length 2))))
      ;; Front face
      ;;------------------------------------------------------------------
      ;; Bottom line
      (rl-vertex3f x- y- z+)            ; Bottom left
      (rl-vertex3f x+ y- z+)            ; Bottom right
      ;; Left line
      (rl-vertex3f x+ y- z+)            ; Bottom right
      (rl-vertex3f x+ y+ z+)            ; Top right
      ;; Top line
      (rl-vertex3f x+ y+ z+)            ; Top right
      (rl-vertex3f x- y+ z+)            ; Top left
      ;; Right line
      (rl-vertex3f x- y+ z+)            ; Top left
      (rl-vertex3f x- y- z+)            ; Bottom left

      ;; Back face
      ;;------------------------------------------------------------------
      ;; Bottom line
      (rl-vertex3f x- y- z-)            ; Bottom left
      (rl-vertex3f x+ y- z-)            ; Bottom right
      ;; Left line
      (rl-vertex3f x+ y- z-)            ; Bottom right
      (rl-vertex3f x+ y+ z-)            ; Top right
      ;; Top line
      (rl-vertex3f x+ y+ z-)            ; Top right
      (rl-vertex3f x- y+ z-)            ; Top left
      ;; Right line
      (rl-vertex3f x- y+ z-)            ; Top left
      (rl-vertex3f x- y- z-)            ; Bottom left

      ;; Top face
      ;;------------------------------------------------------------------
      ;; Left line
      (rl-vertex3f x- y+ z+)            ; Top left front
      (rl-vertex3f x- y+ z-)            ; Top left back
      ;; Right line
      (rl-vertex3f x+ y+ z+)            ; Top right front
      (rl-vertex3f x+ y+ z-)            ; Top right back

      ;; Bottom face
      ;;------------------------------------------------------------------
      ;; Left line
      (rl-vertex3f x- y- z+)            ; Top left front
      (rl-vertex3f x- y- z-)            ; Top left back
      ;; Right line
      (rl-vertex3f x+ y- z+)            ; Top right front
      (rl-vertex3f x+ y- z-))           ; Top right back
    (rl-end)
    (rl-pop-matrix)))

;; Draw cube wires (vector version)
(defun draw-cube-wires-v (position size color)
  "Draw cube wires (Vector version)"
  (draw-cube-wires position (%x size) (%y size) (%z size) color))

;; Draw sphere
(defun draw-sphere (center-pos radius color)
  "Draw sphere"
  (draw-sphere-ex center-pos radius 16 16 color))

(defmacro %do-sphere-faces ((v0 v1 v2 v3 rings slices) &body body)
  "Iterate DrawSphereEx()/DrawSphereWires() faces, V0..V3 bound to (x y z) lists"
  (let ((ringangle (gensym)) (sliceangle (gensym)) (cosring (gensym)) (sinring (gensym))
        (cosslice (gensym)) (sinslice (gensym)) (i (gensym)) (j (gensym)))
    `(let* ((,ringangle (* +deg2rad+ (/ 180.0 ,rings))) ; Angle between latitudinal parallels
            (,sliceangle (* +deg2rad+ (/ 360.0 ,slices))) ; Angle between longitudinal meridians
            (,cosring (%cosf ,ringangle))
            (,sinring (%sinf ,ringangle))
            (,cosslice (%cosf ,sliceangle))
            (,sinslice (%sinf ,sliceangle))
            ;; Required to store face vertices
            (,v0 (list 0.0 0.0 0.0))
            (,v1 (list 0.0 0.0 0.0))
            (,v2 (list 0.0 1.0 0.0))
            (,v3 (list ,sinring ,cosring 0.0)))
       (flet ((rotate-y (v) (destructuring-bind (x y z) v
                              (list (- (* ,cosslice x) (* ,sinslice z)) y (+ (* ,sinslice x) (* ,cosslice z)))))
              (rotate-z (v) (destructuring-bind (x y z) v
                              (list (+ (* ,cosring x) (* ,sinring y)) (+ (* (- ,sinring) x) (* ,cosring y)) z))))
         (dotimes (,i ,rings)
           (dotimes (,j ,slices)
             (setf ,v0 ,v2              ; Rotate around y axis to set up vertices for next face
                   ,v1 ,v3
                   ,v2 (rotate-y ,v2)   ; Rotation matrix around y axis
                   ,v3 (rotate-y ,v3))
             ,@body)
           (setf ,v2 ,v3                ; Rotate around z axis to set up  starting vertices for next ring
                 ,v3 (rotate-z ,v3))))))) ; Rotation matrix around z axis

;; Draw sphere with defined rings and slices
(defun draw-sphere-ex (center-pos radius rings slices color)
  "Draw sphere with extended parameters"
  (rl-push-matrix)
  ;; NOTE: Transformation is applied in inverse order (scale -> translate)
  (rl-translatef (%x center-pos) (%y center-pos) (%z center-pos))
  (rl-scalef radius radius radius)
  (rl-begin +rl-triangles+)
  (%color color)
  (flet ((nv (v) (apply #'rl-normal3f v) (apply #'rl-vertex3f v)))
    (%do-sphere-faces (v0 v1 v2 v3 rings slices)
      (nv v0) (nv v3) (nv v1)
      (nv v0) (nv v2) (nv v3)))
  (rl-end)
  (rl-pop-matrix))

;; Draw sphere wires
(defun draw-sphere-wires (center-pos radius rings slices color)
  "Draw sphere wires"
  (rl-push-matrix)
  ;; NOTE: Transformation is applied in inverse order (scale -> translate)
  (rl-translatef (%x center-pos) (%y center-pos) (%z center-pos))
  (rl-scalef radius radius radius)
  (rl-begin +rl-lines+)
  (%color color)
  (flet ((v (v) (apply #'rl-vertex3f v)))
    (%do-sphere-faces (v0 v1 v2 v3 rings slices)
      ;; Longitude Lines
      (v v0) (v v1)
      ;; Latitude Lines
      (v v0) (v v2)
      ;; Diagonal Lines
      (v v0) (v v3)))
  (rl-end)
  (rl-pop-matrix))

;; Draw a cylinder
;; NOTE: It could be also used for pyramid and cone
(defun draw-cylinder (position radius-top radius-bottom height sides color)
  "Draw a cylinder/cone"
  (when (< sides 3) (setf sides 3))
  (let ((angle-step (/ 360.0 sides))
        (radius-top (float radius-top 1.0))
        (radius-bottom (float radius-bottom 1.0))
        (height (float height 1.0)))
    (flet ((v (i radius y)
             (rl-vertex3f (* (%sinf (* +deg2rad+ i angle-step)) radius) y (* (%cosf (* +deg2rad+ i angle-step)) radius))))
      (rl-push-matrix)
      (rl-translatef (%x position) (%y position) (%z position))
      (rl-begin +rl-triangles+)
      (%color color)
      (if (> radius-top 0)
          (progn
            ;; Draw Body -------------------------------------------------------------------------------------
            (dotimes (i sides)
              (v i radius-bottom 0.0)        ; Bottom Left
              (v (1+ i) radius-bottom 0.0)   ; Bottom Right
              (v (1+ i) radius-top height)   ; Top Right

              (v i radius-top height)        ; Top Left
              (v i radius-bottom 0.0)        ; Bottom Left
              (v (1+ i) radius-top height))  ; Top Right
            ;; Draw Cap --------------------------------------------------------------------------------------
            (dotimes (i sides)
              (rl-vertex3f 0.0 height 0.0)
              (v i radius-top height)
              (v (1+ i) radius-top height)))
          ;; Draw Cone -------------------------------------------------------------------------------------
          (dotimes (i sides)
            (rl-vertex3f 0.0 height 0.0)
            (v i radius-bottom 0.0)
            (v (1+ i) radius-bottom 0.0)))
      ;; Draw Base -----------------------------------------------------------------------------------------
      (dotimes (i sides)
        (rl-vertex3f 0.0 0.0 0.0)
        (v (1+ i) radius-bottom 0.0)
        (v i radius-bottom 0.0))
      (rl-end)
      (rl-pop-matrix))))

(defun %cylinder-ex-vertices (start-pos end-pos start-radius end-radius sides)
  "Basis and per-side vertices (w1 w2 w3 w4) of DrawCylinderEx()/DrawCylinderWiresEx(), NIL for a null direction"
  (let ((direction (vec3 (- (%x end-pos) (%x start-pos)) (- (%y end-pos) (%y start-pos)) (- (%z end-pos) (%z start-pos)))))
    (unless (and (= (vx3 direction) 0) (= (vy3 direction) 0) (= (vz3 direction) 0)) ; Security check
      ;; Construct a basis of the base and the top face:
      (let* ((b1 (vector3-normalize (vector3-perpendicular direction)))
             (b2 (vector3-normalize (vector3-cross-product b1 direction)))
             (base-angle (/ (* 2.0 +pi+) sides)))
        (flet ((w (pos s c)
                 (vec3 (+ (%x pos) (* s (vx3 b1)) (* c (vx3 b2)))
                       (+ (%y pos) (* s (vy3 b1)) (* c (vy3 b2)))
                       (+ (%z pos) (* s (vz3 b1)) (* c (vz3 b2))))))
          (loop for i below sides
                collect (let ((s1 (* (%sinf (* base-angle (+ i 0))) start-radius))
                              (c1 (* (%cosf (* base-angle (+ i 0))) start-radius))
                              (s2 (* (%sinf (* base-angle (+ i 1))) start-radius))
                              (c2 (* (%cosf (* base-angle (+ i 1))) start-radius))
                              (s3 (* (%sinf (* base-angle (+ i 0))) end-radius))
                              (c3 (* (%cosf (* base-angle (+ i 0))) end-radius))
                              (s4 (* (%sinf (* base-angle (+ i 1))) end-radius))
                              (c4 (* (%cosf (* base-angle (+ i 1))) end-radius)))
                          ;; Compute the four vertices
                          (list (w start-pos s1 c1) (w start-pos s2 c2) (w end-pos s3 c3) (w end-pos s4 c4)))))))))

(declaim (inline %vertex-v3))
(defun %vertex-v3 (v)
  (rl-vertex3f (%x v) (%y v) (%z v)))

;; Draw a cylinder with base at startPos and top at endPos
;; NOTE: It could be also used for pyramid and cone
(defun draw-cylinder-ex (start-pos end-pos start-radius end-radius sides color)
  "Draw a cylinder with base at startPos and top at endPos"
  (when (< sides 3) (setf sides 3))
  (let* ((start-radius (float start-radius 1.0))
         (end-radius (float end-radius 1.0))
         (faces (%cylinder-ex-vertices start-pos end-pos start-radius end-radius sides)))
    (unless faces (return-from draw-cylinder-ex)) ; Security check
    (rl-begin +rl-triangles+)
    (%color color)
    (loop for (w1 w2 w3 w4) in faces
          do (when (> start-radius 0)
               (%vertex-v3 start-pos)   ; |
               (%vertex-v3 w2)          ; T0
               (%vertex-v3 w1))         ; |
                                        ;          w2 x.-----------x startPos
             (%vertex-v3 w1)            ; |           |\'.  T0    /
             (%vertex-v3 w2)            ; T1          | \ '.     /
             (%vertex-v3 w3)            ; |           |T \  '.  /
                                        ;             | 2 \ T 'x w1
             (%vertex-v3 w2)            ; |        w4 x.---\-1-|---x endPos
             (%vertex-v3 w4)            ; T2            '.  \  |T3/
             (%vertex-v3 w3)            ; |               '. \ | /
                                        ;                   '.\|/
             (when (> end-radius 0)     ;                     'x w3
               (%vertex-v3 end-pos)     ; |
               (%vertex-v3 w3)          ; T3
               (%vertex-v3 w4)))        ; |
    (rl-end)))

;; Draw a wired cylinder
;; NOTE: It could be also used for pyramid and cone
(defun draw-cylinder-wires (position radius-top radius-bottom height sides color)
  "Draw a cylinder/cone wires"
  (when (< sides 3) (setf sides 3))
  (let ((angle-step (/ 360.0 sides))
        (radius-top (float radius-top 1.0))
        (radius-bottom (float radius-bottom 1.0))
        (height (float height 1.0)))
    (flet ((v (i radius y)
             (rl-vertex3f (* (%sinf (* +deg2rad+ i angle-step)) radius) y (* (%cosf (* +deg2rad+ i angle-step)) radius))))
      (rl-push-matrix)
      (rl-translatef (%x position) (%y position) (%z position))
      (rl-begin +rl-lines+)
      (%color color)
      (dotimes (i sides)
        (v i radius-bottom 0.0)
        (v (1+ i) radius-bottom 0.0)

        (v (1+ i) radius-bottom 0.0)
        (v (1+ i) radius-top height)

        (v (1+ i) radius-top height)
        (v i radius-top height)

        (v i radius-top height)
        (v i radius-bottom 0.0))
      (rl-end)
      (rl-pop-matrix))))

;; Draw a wired cylinder with base at startPos and top at endPos
;; NOTE: It could be also used for pyramid and cone
(defun draw-cylinder-wires-ex (start-pos end-pos start-radius end-radius slices color)
  "Draw a cylinder wires with base at startPos and top at endPos"
  (when (< slices 3) (setf slices 3))
  (let ((faces (%cylinder-ex-vertices start-pos end-pos (float start-radius 1.0) (float end-radius 1.0) slices)))
    (unless faces (return-from draw-cylinder-wires-ex)) ; Security check
    (rl-begin +rl-lines+)
    (%color color)
    (loop for (w1 w2 w3 w4) in faces
          do (%vertex-v3 w1) (%vertex-v3 w2)
             (%vertex-v3 w1) (%vertex-v3 w3)
             (%vertex-v3 w3) (%vertex-v3 w4))
    (rl-end)))

(defun %capsule-faces (start-pos end-pos radius rings slices)
  "DrawCapsule()/DrawCapsuleWires() geometry: returns (values cap-faces middle-faces sphere-case)
CAP-FACES is a list of (c w1 w2 w3 w4), MIDDLE-FACES a list of (w1 w2 w3 w4)"
  (let* ((radius (float radius 1.0))
         (direction (vec3 (- (%x end-pos) (%x start-pos)) (- (%y end-pos) (%y start-pos)) (- (%z end-pos) (%z start-pos))))
         ;; draw a sphere if start and end points are the same
         (sphere-case (and (= (vx3 direction) 0) (= (vy3 direction) 0) (= (vz3 direction) 0))))
    (when sphere-case (setf direction (vec3 0.0 1.0 0.0)))
    ;; Construct a basis of the base and the caps:
    (let* ((b0 (vector3-normalize direction))
           (b1 (vector3-normalize (vector3-perpendicular direction)))
           (b2 (vector3-normalize (vector3-cross-product b1 direction)))
           (cap-center (%v3 end-pos))
           (base-slice-angle (/ (* 2.0 +pi+) slices))
           (base-ring-angle (/ (* +pi+ 0.5) rings))
           (caps nil)
           (middle nil))
      ;; render both caps
      (dotimes (c 2)
        (dotimes (i rings)
          (dotimes (j slices)
            ;; Building up the rings from capCenter in the direction of the 'direction' vector computed earlier
            ;; Compute the four vertices
            (flet ((w (jj ii)
                     (let ((ring-sin (* (%sinf (* base-slice-angle jj)) (%cosf (* base-ring-angle ii))))
                           (ring-cos (* (%cosf (* base-slice-angle jj)) (%cosf (* base-ring-angle ii))))
                           (s (%sinf (* base-ring-angle ii))))
                       (vec3 (+ (vx3 cap-center) (* (+ (* s (vx3 b0)) (* ring-sin (vx3 b1)) (* ring-cos (vx3 b2))) radius))
                             (+ (vy3 cap-center) (* (+ (* s (vy3 b0)) (* ring-sin (vy3 b1)) (* ring-cos (vy3 b2))) radius))
                             (+ (vz3 cap-center) (* (+ (* s (vz3 b0)) (* ring-sin (vz3 b1)) (* ring-cos (vz3 b2))) radius))))))
              (push (list c (w (+ j 0) (+ i 0)) (w (+ j 1) (+ i 0)) (w (+ j 0) (+ i 1)) (w (+ j 1) (+ i 1))) caps))))
        (setf cap-center (%v3 start-pos)
              b0 (vector3-scale b0 -1.0)))
      ;; render middle
      (unless sphere-case
        (dotimes (j slices)
          ;; compute the four vertices
          (flet ((w (pos jj)
                   (let ((ring-sin (* (%sinf (* base-slice-angle jj)) radius))
                         (ring-cos (* (%cosf (* base-slice-angle jj)) radius)))
                     (vec3 (+ (%x pos) (* ring-sin (vx3 b1)) (* ring-cos (vx3 b2)))
                           (+ (%y pos) (* ring-sin (vy3 b1)) (* ring-cos (vy3 b2)))
                           (+ (%z pos) (* ring-sin (vz3 b1)) (* ring-cos (vz3 b2)))))))
            (push (list (w start-pos (+ j 0)) (w start-pos (+ j 1)) (w end-pos (+ j 0)) (w end-pos (+ j 1))) middle))))
      (values (nreverse caps) (nreverse middle) sphere-case))))

;; Draw a capsule with the center of its sphere caps at startPos and endPos
(defun draw-capsule (start-pos end-pos radius rings slices color)
  "Draw a capsule with the center of its sphere caps at startPos and endPos"
  (when (< slices 3) (setf slices 3))
  (when (< rings 1) (setf rings 1))
  (multiple-value-bind (caps middle) (%capsule-faces start-pos end-pos radius rings slices)
    (rl-begin +rl-triangles+)
    (%color color)
    (loop for (c w1 w2 w3 w4) in caps
          ;; Make sure cap triangle normals are facing outwards
          do (if (= c 0)
                 (progn (%vertex-v3 w1) (%vertex-v3 w2) (%vertex-v3 w3)
                        (%vertex-v3 w2) (%vertex-v3 w4) (%vertex-v3 w3))
                 (progn (%vertex-v3 w1) (%vertex-v3 w3) (%vertex-v3 w2)
                        (%vertex-v3 w2) (%vertex-v3 w3) (%vertex-v3 w4))))
    (loop for (w1 w2 w3 w4) in middle
          do (%vertex-v3 w1) (%vertex-v3 w2) (%vertex-v3 w3)
             (%vertex-v3 w2) (%vertex-v3 w4) (%vertex-v3 w3))
    (rl-end)))

;; Draw capsule wires with the center of its sphere caps at startPos and endPos
(defun draw-capsule-wires (start-pos end-pos radius rings slices color)
  "Draw capsule wireframe with the center of its sphere caps at startPos and endPos"
  (when (< slices 3) (setf slices 3))
  (when (< rings 1) (setf rings 1))
  (multiple-value-bind (caps middle) (%capsule-faces start-pos end-pos radius rings slices)
    (rl-begin +rl-lines+)
    (%color color)
    (loop for (nil w1 w2 w3 w4) in caps
          do (%vertex-v3 w1) (%vertex-v3 w2)
             (%vertex-v3 w2) (%vertex-v3 w3)
             (%vertex-v3 w1) (%vertex-v3 w3)
             (%vertex-v3 w2) (%vertex-v3 w4)
             (%vertex-v3 w3) (%vertex-v3 w4))
    (loop for (w1 w2 w3 w4) in middle
          do (%vertex-v3 w1) (%vertex-v3 w3)
             (%vertex-v3 w2) (%vertex-v3 w4)
             (%vertex-v3 w2) (%vertex-v3 w3))
    (rl-end)))

;; Draw a plane
(defun draw-plane (center-pos size color)
  "Draw a plane XZ"
  ;; NOTE: Plane is always created on XZ ground
  (rl-push-matrix)
  (rl-translatef (%x center-pos) (%y center-pos) (%z center-pos))
  (rl-scalef (%x size) 1.0 (%y size))
  (rl-begin +rl-quads+)
  (%color color)
  (rl-normal3f 0.0 1.0 0.0)
  (rl-vertex3f -0.5 0.0 -0.5)
  (rl-vertex3f -0.5 0.0 0.5)
  (rl-vertex3f 0.5 0.0 0.5)
  (rl-vertex3f 0.5 0.0 -0.5)
  (rl-end)
  (rl-pop-matrix))

;; Draw a ray line
(defun draw-ray (ray color)
  "Draw a ray line"
  (let ((scale 10000.0)
        (p (ray-position ray))
        (d (ray-direction ray)))
    (rl-begin +rl-lines+)
    (%color color)
    (%color color)
    (rl-vertex3f (%x p) (%y p) (%z p))
    (rl-vertex3f (+ (%x p) (* (%x d) scale)) (+ (%y p) (* (%y d) scale)) (+ (%z p) (* (%z d) scale)))
    (rl-end)))

;; Draw a grid centered at (0, 0, 0)
(defun draw-grid (slices spacing)
  "Draw a grid (centered at (0, 0, 0))"
  (let ((half-slices (truncate slices 2))
        (spacing (float spacing 1.0)))
    (rl-begin +rl-lines+)
    (loop for i from (- half-slices) to half-slices
          do (if (= i 0)
                 (rl-color3f 0.5 0.5 0.5)
                 (rl-color3f 0.75 0.75 0.75))
             (rl-vertex3f (* (float i 1.0) spacing) 0.0 (* (float (- half-slices) 1.0) spacing))
             (rl-vertex3f (* (float i 1.0) spacing) 0.0 (* (float half-slices 1.0) spacing))

             (rl-vertex3f (* (float (- half-slices) 1.0) spacing) 0.0 (* (float i 1.0) spacing))
             (rl-vertex3f (* (float half-slices 1.0) spacing) 0.0 (* (float i 1.0) spacing)))
    (rl-end)))

(defun %zero-texture ()
  (make-texture :id 0 :width 0 :height 0 :mipmaps 0 :format 0))

(defun %floats (n)
  (make-array n :element-type 'single-float :initial-element 0.0))

(defun %c-format-float (x precision)
  "C printf() %.<precision>f of a float"
  (%sprintf (format nil "%.~df" precision) x))

;; Load model from files (mesh and material)
(defun load-model (file-name)
  "Load model from files (meshes and materials)"
  (let ((model (make-model)))
    (when (is-file-extension file-name ".obj") (setf model (%load-obj file-name)))
    (when (is-file-extension file-name ".iqm") (setf model (%load-iqm file-name)))
    (when (or (is-file-extension file-name ".gltf") (is-file-extension file-name ".glb")) (setf model (%load-gltf file-name)))
    (when (is-file-extension file-name ".vox") (setf model (%load-vox file-name)))
    (when (is-file-extension file-name ".m3d") (setf model (%load-m3d file-name)))

    ;; Make sure model transform is set to identity matrix!
    (setf (model-transform model) (matrix-identity))

    (if (and (/= (model-mesh-count model) 0) (model-meshes model))
        ;; Upload vertex data to GPU (static meshes)
        (dotimes (i (model-mesh-count model)) (upload-mesh (aref (model-meshes model) i) nil))
        (trace-log +log-warning+ "MESH: [~a] Failed to load model mesh(es) data" file-name))

    (when (= (model-material-count model) 0)
      (trace-log +log-warning+ "MATERIAL: [~a] Failed to load model material data, default to white material" file-name)
      (setf (model-material-count model) 1
            (model-materials model) (vector (load-material-default)))
      (unless (model-mesh-material model)
        (setf (model-mesh-material model) (make-array (model-mesh-count model) :initial-element 0))))
    model))

;; Load model from generated mesh
;; WARNING: A shallow copy of mesh is generated, passed by value,
;; as long as struct contains pointers to data and some values, get a copy
;; of mesh pointing to same data as original version... be careful!
(defun load-model-from-mesh (mesh)
  "Load model from generated mesh (default material)"
  (make-model :transform (matrix-identity)
              :mesh-count 1
              :meshes (vector mesh)
              :material-count 1
              :materials (vector (load-material-default))
              :mesh-material (make-array 1 :initial-element 0))) ; First material index

;; Check if model is valid (loaded in GPU, VAO/VBOs)
(defun is-model-valid (model)
  "Check if a model is valid (loaded in GPU, VAO/VBOs)"
  (let ((result (and (model-meshes model)          ; Validate model contains some mesh
                     (model-materials model)       ; Validate model contains some material (at least default one)
                     (model-mesh-material model)   ; Validate mesh-material linkage
                     (> (model-mesh-count model) 0) ; Validate mesh count
                     (> (model-material-count model) 0) ; Validate material count
                     t)))
    ;; NOTE: Many elements could be validated from a model, including every model mesh VAO/VBOs
    ;; but some VBOs could not be used, it depends on Mesh vertex data
    (dotimes (i (model-mesh-count model))
      (let* ((mesh (aref (model-meshes model) i))
             (vbo (or (mesh-vbo-id mesh) #(0 0 0 0 0 0 0))))
        (when (or (and (mesh-vertices mesh) (= (aref vbo 0) 0))    ; Vertex position buffer not uploaded to GPU
                  (and (mesh-texcoords mesh) (= (aref vbo 1) 0))   ; Vertex textcoords buffer not uploaded to GPU
                  (and (mesh-normals mesh) (= (aref vbo 2) 0))     ; Vertex normals buffer not uploaded to GPU
                  (and (mesh-colors mesh) (= (aref vbo 3) 0))      ; Vertex colors buffer not uploaded to GPU
                  (and (mesh-tangents mesh) (= (aref vbo 4) 0))    ; Vertex tangents buffer not uploaded to GPU
                  (and (mesh-texcoords2 mesh) (= (aref vbo 5) 0))  ; Vertex texcoords2 buffer not uploaded to GPU
                  (and (mesh-indices mesh) (= (aref vbo 6) 0)))    ; Vertex indices buffer not uploaded to GPU
          (setf result nil)
          (return))))
    result))

;; Unload model (meshes/materials) from memory (RAM and/or VRAM)
;; NOTE: This function takes care of all model elements, for a detailed control
;; over them, use UnloadMesh() and UnloadMaterial()
(defun unload-model (model)
  "Unload model (including meshes) from memory (RAM and/or VRAM)"
  ;; Unload meshes
  (dotimes (i (model-mesh-count model)) (unload-mesh (aref (model-meshes model) i)))

  ;; Unload materials maps
  ;; NOTE: As the user could be sharing shaders and textures between models,
  ;; don't unload the material but free its maps,
  ;; the user is responsible for freeing models shaders and textures
  (dotimes (i (model-material-count model)) (setf (material-maps (aref (model-materials model) i)) nil))

  ;; Unload arrays
  (setf (model-meshes model) nil
        (model-materials model) nil
        (model-mesh-material model) nil)

  ;; Unload animation data
  (setf (model-skeleton-bones (model-skeleton model)) nil
        (model-skeleton-bind-pose (model-skeleton model)) nil
        (model-current-pose model) nil
        (model-bone-matrices model) nil)

  (trace-log +log-info+ "MODEL: Unloaded model (and meshes) from RAM and VRAM"))

;; Compute model bounding box limits (considers all meshes)
(defun get-model-bounding-box (model)
  "Compute model bounding box limits (considers all meshes)"
  (let ((bounds (make-bounding-box)))
    (when (> (model-mesh-count model) 0)
      (setf bounds (get-mesh-bounding-box (aref (model-meshes model) 0)))
      (loop for i from 1 below (model-mesh-count model)
            do (let ((temp-bounds (get-mesh-bounding-box (aref (model-meshes model) i)))
                     (bmin (bounding-box-min bounds))
                     (bmax (bounding-box-max bounds)))
                 (setf (bounding-box-min bounds)
                       (vec3 (if (< (vx3 bmin) (vx3 (bounding-box-min temp-bounds))) (vx3 bmin) (vx3 (bounding-box-min temp-bounds)))
                             (if (< (vy3 bmin) (vy3 (bounding-box-min temp-bounds))) (vy3 bmin) (vy3 (bounding-box-min temp-bounds)))
                             (if (< (vz3 bmin) (vz3 (bounding-box-min temp-bounds))) (vz3 bmin) (vz3 (bounding-box-min temp-bounds))))
                       (bounding-box-max bounds)
                       (vec3 (if (> (vx3 bmax) (vx3 (bounding-box-max temp-bounds))) (vx3 bmax) (vx3 (bounding-box-max temp-bounds)))
                             (if (> (vy3 bmax) (vy3 (bounding-box-max temp-bounds))) (vy3 bmax) (vy3 (bounding-box-max temp-bounds)))
                             (if (> (vz3 bmax) (vz3 (bounding-box-max temp-bounds))) (vz3 bmax) (vz3 (bounding-box-max temp-bounds))))))))
    ;; Apply model.transform to bounding box
    ;; WARNING: Current BoundingBox structure design does not support rotation transformations,
    ;; in those cases is up to the user to calculate the proper box bounds (8 vertices transformed)
    (make-bounding-box :min (vector3-transform (bounding-box-min bounds) (model-transform model))
                       :max (vector3-transform (bounding-box-max bounds) (model-transform model)))))

;; Upload vertex data into a VAO (if supported) and VBO
(defun upload-mesh (mesh dynamic)
  "Upload mesh vertex data in GPU and provide VAO/VBO ids"
  (when (> (mesh-vao-id mesh) 0)
    ;; Check if mesh has already been loaded in GPU
    (trace-log +log-warning+ "VAO: [ID ~d] Trying to re-load an already loaded mesh" (mesh-vao-id mesh))
    (return-from upload-mesh))

  (let ((vbo (make-array +max-mesh-vertex-buffers+ :initial-element 0))
        (vc (mesh-vertex-count mesh)))
    (setf (mesh-vbo-id mesh) vbo
          (mesh-vao-id mesh) 0)         ; Vertex Array Object

    (setf (mesh-vao-id mesh) (rl-load-vertex-array))
    (rl-enable-vertex-array (mesh-vao-id mesh))

    ;; NOTE: Vertex attributes must be uploaded considering default locations points and available vertex data

    ;; Enable vertex attributes: position (shader-location = 0)
    (let ((vertices (or (mesh-anim-vertices mesh) (mesh-vertices mesh))))
      (setf (aref vbo +rl-default-shader-attrib-location-position+) (rl-load-vertex-buffer vertices (* vc 3 4) dynamic))
      (rl-set-vertex-attribute +rl-default-shader-attrib-location-position+ 3 +rl-float+ 0 0 0)
      (rl-enable-vertex-attribute +rl-default-shader-attrib-location-position+))

    ;; Enable vertex attributes: texcoords (shader-location = 1)
    (if (mesh-texcoords mesh)
        (progn
          (setf (aref vbo +rl-default-shader-attrib-location-texcoord+) (rl-load-vertex-buffer (mesh-texcoords mesh) (* vc 2 4) dynamic))
          (rl-set-vertex-attribute +rl-default-shader-attrib-location-texcoord+ 2 +rl-float+ 0 0 0)
          (rl-enable-vertex-attribute +rl-default-shader-attrib-location-texcoord+))
        (progn
          (rl-set-vertex-attribute-default +rl-default-shader-attrib-location-texcoord+ '(0.0 0.0) +shader-attrib-vec2+ 2)
          (rl-disable-vertex-attribute +rl-default-shader-attrib-location-texcoord+)))
    ;; WARNING: When setting default vertex attribute values, the values for each generic vertex attribute
    ;; is part of current state, and it is maintained even if a different program object is used

    (if (mesh-normals mesh)
        ;; Enable vertex attributes: normals (shader-location = 2)
        (let ((normals (or (mesh-anim-normals mesh) (mesh-normals mesh))))
          (setf (aref vbo +rl-default-shader-attrib-location-normal+) (rl-load-vertex-buffer normals (* vc 3 4) dynamic))
          (rl-set-vertex-attribute +rl-default-shader-attrib-location-normal+ 3 +rl-float+ 0 0 0)
          (rl-enable-vertex-attribute +rl-default-shader-attrib-location-normal+))
        (progn
          ;; Default vertex attribute: normal
          ;; WARNING: Default value provided to shader if location available
          (rl-set-vertex-attribute-default +rl-default-shader-attrib-location-normal+ '(0.0 0.0 1.0) +shader-attrib-vec3+ 3)
          (rl-disable-vertex-attribute +rl-default-shader-attrib-location-normal+)))

    (if (mesh-colors mesh)
        (progn
          ;; Enable vertex attribute: color (shader-location = 3)
          (setf (aref vbo +rl-default-shader-attrib-location-color+) (rl-load-vertex-buffer (mesh-colors mesh) (* vc 4) dynamic))
          (rl-set-vertex-attribute +rl-default-shader-attrib-location-color+ 4 +rl-unsigned-byte+ 1 0 0)
          (rl-enable-vertex-attribute +rl-default-shader-attrib-location-color+))
        (progn
          ;; Default vertex attribute: color
          ;; WARNING: Default value provided to shader if location available
          (rl-set-vertex-attribute-default +rl-default-shader-attrib-location-color+ '(1.0 1.0 1.0 1.0) +shader-attrib-vec4+ 4) ; WHITE
          (rl-disable-vertex-attribute +rl-default-shader-attrib-location-color+)))

    (if (mesh-tangents mesh)
        (progn
          ;; Enable vertex attribute: tangent (shader-location = 4)
          (setf (aref vbo +rl-default-shader-attrib-location-tangent+) (rl-load-vertex-buffer (mesh-tangents mesh) (* vc 4 4) dynamic))
          (rl-set-vertex-attribute +rl-default-shader-attrib-location-tangent+ 4 +rl-float+ 0 0 0)
          (rl-enable-vertex-attribute +rl-default-shader-attrib-location-tangent+))
        (progn
          ;; Default vertex attribute: tangent
          ;; WARNING: Default value provided to shader if location available
          (rl-set-vertex-attribute-default +rl-default-shader-attrib-location-tangent+ '(1.0 0.0 0.0 1.0) +shader-attrib-vec4+ 4)
          (rl-disable-vertex-attribute +rl-default-shader-attrib-location-tangent+)))

    (if (mesh-texcoords2 mesh)
        (progn
          ;; Enable vertex attribute: texcoord2 (shader-location = 5)
          (setf (aref vbo +rl-default-shader-attrib-location-texcoord2+) (rl-load-vertex-buffer (mesh-texcoords2 mesh) (* vc 2 4) dynamic))
          (rl-set-vertex-attribute +rl-default-shader-attrib-location-texcoord2+ 2 +rl-float+ 0 0 0)
          (rl-enable-vertex-attribute +rl-default-shader-attrib-location-texcoord2+))
        (progn
          ;; Default vertex attribute: texcoord2
          ;; WARNING: Default value provided to shader if location available
          (rl-set-vertex-attribute-default +rl-default-shader-attrib-location-texcoord2+ '(0.0 0.0) +shader-attrib-vec2+ 2)
          (rl-disable-vertex-attribute +rl-default-shader-attrib-location-texcoord2+)))

    (when (mesh-indices mesh)
      (setf (aref vbo +rl-default-shader-attrib-location-indices+)
            (rl-load-vertex-buffer-element (mesh-indices mesh) (* (mesh-triangle-count mesh) 3 2) dynamic)))

    (if (> (mesh-vao-id mesh) 0)
        (trace-log +log-info+ "VAO: [ID ~d] Mesh uploaded successfully to VRAM (GPU)" (mesh-vao-id mesh))
        (trace-log +log-info+ "VBO: Mesh uploaded successfully to VRAM (GPU)"))

    (rl-disable-vertex-array)))

;; Update mesh vertex data in GPU for a specific buffer index
(defun update-mesh-buffer (mesh index data data-size offset)
  "Update mesh vertex data in GPU for a specific buffer index"
  (rl-update-vertex-buffer (aref (mesh-vbo-id mesh) index) data data-size offset))

(defun %color-normalized-values (color)
  "Color as 4 floats (c/255.0f)"
  (destructuring-bind (r g b a) (%col color)
    (list (/ (float r 1.0) 255.0) (/ (float g 1.0) 255.0) (/ (float b 1.0) 255.0) (/ (float a 1.0) 255.0))))

(defun %material-map (material index)
  (svref (material-maps material) index))

(defun %draw-mesh-setup (mesh material)
  "DrawMesh()/DrawMeshInstanced() common steps: material colors upload"
  (declare (ignore mesh))
  (let ((locs (shader-locs (material-shader material))))
    ;; Upload to shader material.colDiffuse
    (when (/= (aref locs +shader-loc-color-diffuse+) -1)
      (rl-set-uniform (aref locs +shader-loc-color-diffuse+)
                      (%color-normalized-values (material-map-color (%material-map material +material-map-diffuse+)))
                      +shader-uniform-vec4+ 1))
    ;; Upload to shader material.colSpecular (if location available)
    (when (/= (aref locs +shader-loc-color-specular+) -1)
      (rl-set-uniform (aref locs +shader-loc-color-specular+)
                      (%color-normalized-values (material-map-color (%material-map material +material-map-specular+)))
                      +shader-uniform-vec4+ 1))))

(defun %bind-material-textures (material)
  "Bind active texture maps (if available)"
  (let ((locs (shader-locs (material-shader material))))
    (dotimes (i +max-material-maps+)
      (let ((texture (material-map-texture (%material-map material i))))
        (when (> (texture-id texture) 0)
          ;; Select current shader texture slot
          (rl-active-texture-slot i)
          ;; Enable texture for active slot
          (if (or (= i +material-map-irradiance+) (= i +material-map-prefilter+) (= i +material-map-cubemap+))
              (rl-enable-texture-cubemap (texture-id texture))
              (rl-enable-texture (texture-id texture)))
          (rl-set-uniform (aref locs (+ +shader-loc-map-diffuse+ i)) i +shader-uniform-int+ 1))))))

(defun %unbind-material-textures (material)
  "Unbind all bound texture maps"
  (dotimes (i +max-material-maps+)
    (when (> (texture-id (material-map-texture (%material-map material i))) 0)
      ;; Select current shader texture slot
      (rl-active-texture-slot i)
      ;; Disable texture for active slot
      (if (or (= i +material-map-irradiance+) (= i +material-map-prefilter+) (= i +material-map-cubemap+))
          (rl-disable-texture-cubemap)
          (rl-disable-texture)))))

(defun %bind-mesh-vbos (mesh material)
  "Bind mesh VBOs when VAO is not available"
  (let ((locs (shader-locs (material-shader material)))
        (vbo (mesh-vbo-id mesh)))
    ;; Bind mesh VBO data: vertex position (shader-location = 0)
    (rl-enable-vertex-buffer (aref vbo +rl-default-shader-attrib-location-position+))
    (rl-set-vertex-attribute (aref locs +shader-loc-vertex-position+) 3 +rl-float+ 0 0 0)
    (rl-enable-vertex-attribute (aref locs +shader-loc-vertex-position+))

    ;; Bind mesh VBO data: vertex texcoords (shader-location = 1)
    (rl-enable-vertex-buffer (aref vbo +rl-default-shader-attrib-location-texcoord+))
    (rl-set-vertex-attribute (aref locs +shader-loc-vertex-texcoord01+) 2 +rl-float+ 0 0 0)
    (rl-enable-vertex-attribute (aref locs +shader-loc-vertex-texcoord01+))

    (when (/= (aref locs +shader-loc-vertex-normal+) -1)
      ;; Bind mesh VBO data: vertex normals (shader-location = 2)
      (rl-enable-vertex-buffer (aref vbo +rl-default-shader-attrib-location-normal+))
      (rl-set-vertex-attribute (aref locs +shader-loc-vertex-normal+) 3 +rl-float+ 0 0 0)
      (rl-enable-vertex-attribute (aref locs +shader-loc-vertex-normal+)))

    ;; Bind mesh VBO data: vertex colors (shader-location = 3, if available)
    (when (/= (aref locs +shader-loc-vertex-color+) -1)
      (if (/= (aref vbo +rl-default-shader-attrib-location-color+) 0)
          (progn
            (rl-enable-vertex-buffer (aref vbo +rl-default-shader-attrib-location-color+))
            (rl-set-vertex-attribute (aref locs +shader-loc-vertex-color+) 4 +rl-unsigned-byte+ 1 0 0)
            (rl-enable-vertex-attribute (aref locs +shader-loc-vertex-color+)))
          (progn
            ;; Set default value for defined vertex attribute in shader but not provided by mesh
            ;; WARNING: It could result in GPU undefined behaviour
            (rl-set-vertex-attribute-default (aref locs +shader-loc-vertex-color+) '(1.0 1.0 1.0 1.0) +shader-attrib-vec4+ 4)
            (rl-disable-vertex-attribute (aref locs +shader-loc-vertex-color+)))))

    ;; Bind mesh VBO data: vertex tangents (shader-location = 4, if available)
    (when (/= (aref locs +shader-loc-vertex-tangent+) -1)
      (rl-enable-vertex-buffer (aref vbo +rl-default-shader-attrib-location-tangent+))
      (rl-set-vertex-attribute (aref locs +shader-loc-vertex-tangent+) 4 +rl-float+ 0 0 0)
      (rl-enable-vertex-attribute (aref locs +shader-loc-vertex-tangent+)))

    ;; Bind mesh VBO data: vertex texcoords2 (shader-location = 5, if available)
    (when (/= (aref locs +shader-loc-vertex-texcoord02+) -1)
      (rl-enable-vertex-buffer (aref vbo +rl-default-shader-attrib-location-texcoord2+))
      (rl-set-vertex-attribute (aref locs +shader-loc-vertex-texcoord02+) 2 +rl-float+ 0 0 0)
      (rl-enable-vertex-attribute (aref locs +shader-loc-vertex-texcoord02+)))

    (when (mesh-indices mesh) (rl-enable-vertex-buffer-element (aref vbo +rl-default-shader-attrib-location-indices+)))))

(defun %mesh-eye-mvp (mat-model-view mat-projection eye eye-count)
  "Calculate model-view-projection matrix (MVP) for an eye"
  (if (= eye-count 1)
      (matrix-multiply mat-model-view mat-projection)
      (progn
        ;; Setup current eye viewport (half screen width)
        (rl-viewport (truncate (* eye (rl-get-framebuffer-width)) 2) 0
                     (truncate (rl-get-framebuffer-width) 2) (rl-get-framebuffer-height))
        (matrix-multiply (matrix-multiply mat-model-view (rl-get-matrix-view-offset-stereo eye))
                         (rl-get-matrix-projection-stereo eye)))))

;; Draw a 3d mesh with material and transform
(defun draw-mesh (mesh material transform)
  "Draw a 3d mesh with material and transform"
  ;; Bind shader program
  (rl-enable-shader (shader-id (material-shader material)))

  (unless (shader-locs (material-shader material)) (return-from draw-mesh))

  ;; Send required data to shader (matrices, values)
  ;;-----------------------------------------------------
  (%draw-mesh-setup mesh material)

  (let* ((locs (shader-locs (material-shader material)))
         ;; Get a copy of current matrices to work with,
         ;; in case stereo render is required, and they need to be modified
         ;; NOTE: At this point the modelview matrix contains the view matrix (camera)
         ;; That's because BeginMode3D() sets it and there is no model-drawing function
         ;; that modifies it, all use rlPushMatrix() and rlPopMatrix()
         (mat-view (rl-get-matrix-modelview))
         (mat-projection (rl-get-matrix-projection))
         ;; Accumulate several model transformations:
         ;;    transform: model transformation provided (includes DrawModel() params combined with model.transform)
         ;;    rlGetMatrixTransform(): rlgl internal transform matrix due to push/pop matrix stack
         (mat-model (matrix-multiply transform (rl-get-matrix-transform)))
         (mat-model-view nil))

    ;; Upload view and projection matrices (if locations available)
    (when (/= (aref locs +shader-loc-matrix-view+) -1) (rl-set-uniform-matrix (aref locs +shader-loc-matrix-view+) mat-view))
    (when (/= (aref locs +shader-loc-matrix-projection+) -1) (rl-set-uniform-matrix (aref locs +shader-loc-matrix-projection+) mat-projection))

    ;; Model transformation matrix is sent to shader uniform location: SHADER_LOC_MATRIX_MODEL
    (when (/= (aref locs +shader-loc-matrix-model+) -1) (rl-set-uniform-matrix (aref locs +shader-loc-matrix-model+) mat-model))

    ;; Get model-view matrix
    (setf mat-model-view (matrix-multiply mat-model mat-view))

    ;; Upload model normal matrix (if locations available)
    (when (/= (aref locs +shader-loc-matrix-normal+) -1)
      (rl-set-uniform-matrix (aref locs +shader-loc-matrix-normal+) (matrix-transpose (matrix-invert mat-model))))
    ;;-----------------------------------------------------

    ;; Bind active texture maps (if available)
    (%bind-material-textures material)

    ;; Try binding vertex array objects (VAO) or use VBOs if not possible
    ;; WARNING: UploadMesh() enables all vertex attributes available in mesh and sets default attribute values
    ;; for shader expected vertex attributes that are not provided by the mesh (i.e. colors)
    ;; This could be a dangerous approach because different meshes with different shaders can enable/disable some attributes
    (unless (rl-enable-vertex-array (mesh-vao-id mesh))
      (%bind-mesh-vbos mesh material))

    (let ((eye-count (if (rl-is-stereo-render-enabled) 2 1)))
      (dotimes (eye eye-count)
        ;; Send combined model-view-projection matrix to shader
        (rl-set-uniform-matrix (aref locs +shader-loc-matrix-mvp+) (%mesh-eye-mvp mat-model-view mat-projection eye eye-count))
        ;; Draw mesh
        (if (mesh-indices mesh)
            (rl-draw-vertex-array-elements 0 (* (mesh-triangle-count mesh) 3) nil)
            (rl-draw-vertex-array 0 (mesh-vertex-count mesh)))))

    ;; Unbind all bound texture maps
    (%unbind-material-textures material)

    ;; Disable all possible vertex array objects (or VBOs)
    (rl-disable-vertex-array)
    (rl-disable-vertex-buffer)
    (rl-disable-vertex-buffer-element)

    ;; Disable shader program
    (rl-disable-shader)

    ;; Restore rlgl internal modelview and projection matrices
    (rl-set-matrix-modelview mat-view)
    (rl-set-matrix-projection mat-projection)))

;; Draw multiple mesh instances with material and different transforms
(defun draw-mesh-instanced (mesh material transforms instances)
  "Draw multiple mesh instances with material and different transforms"
  ;; Bind shader program
  (rl-enable-shader (shader-id (material-shader material)))

  ;; Send required data to shader (matrices, values)
  ;;-----------------------------------------------------
  (%draw-mesh-setup mesh material)

  (let* ((locs (shader-locs (material-shader material)))
         ;; Get a copy of current matrices to work with,
         ;; in case stereo render is required, and they need to be modified
         ;; NOTE: At this point the modelview matrix contains the view matrix (camera)
         ;; That's because BeginMode3D() sets it and there is no model-drawing function
         ;; that modifies it, all use rlPushMatrix() and rlPopMatrix()
         (mat-model (matrix-identity))
         (mat-view (rl-get-matrix-modelview))
         (mat-model-view nil)
         (mat-projection (rl-get-matrix-projection))
         ;; Create instances buffer
         (instance-transform (%floats (* instances 16)))
         (instances-vbo-id 0))

    ;; Upload view and projection matrices (if locations available)
    (when (/= (aref locs +shader-loc-matrix-view+) -1) (rl-set-uniform-matrix (aref locs +shader-loc-matrix-view+) mat-view))
    (when (/= (aref locs +shader-loc-matrix-projection+) -1) (rl-set-uniform-matrix (aref locs +shader-loc-matrix-projection+) mat-projection))

    ;; Fill buffer with instances transformations as float16 arrays
    (let ((transforms (coerce transforms 'simple-vector)))
      (dotimes (i instances)
        (replace instance-transform (matrix-to-float-v (svref transforms i)) :start1 (* i 16))))

    ;; Enable mesh VAO to attach new buffer
    (rl-enable-vertex-array (mesh-vao-id mesh))

    ;; This could alternatively use a static VBO and either glMapBuffer() or glBufferSubData()
    ;; It isn't clear which would be reliably faster in all cases and on all platforms,
    ;; anecdotally glMapBuffer() seems quite slow (syncs) while glBufferSubData() seems
    ;; no faster, since all the transform matrices are transferred anyway
    (setf instances-vbo-id (rl-load-vertex-buffer instance-transform (* instances 64) nil))

    ;; Instances transformation matrices are sent to shader attribute location: SHADER_LOC_VERTEX_INSTANCETRANSFORM
    (when (/= (aref locs +shader-loc-vertex-instancetransform+) -1)
      (dotimes (i 4)
        (rl-enable-vertex-attribute (+ (aref locs +shader-loc-vertex-instancetransform+) i))
        (rl-set-vertex-attribute (+ (aref locs +shader-loc-vertex-instancetransform+) i) 4 +rl-float+ 0 64 (* i 16))
        (rl-set-vertex-attribute-divisor (+ (aref locs +shader-loc-vertex-instancetransform+) i) 1)))

    (rl-disable-vertex-buffer)
    (rl-disable-vertex-array)

    ;; Accumulate internal matrix transform (push/pop) and view matrix
    ;; NOTE: In this case, model instance transformation must be computed in the shader
    (setf mat-model-view (matrix-multiply (rl-get-matrix-transform) mat-view))

    ;; Upload model normal matrix (if locations available)
    (when (/= (aref locs +shader-loc-matrix-normal+) -1)
      (rl-set-uniform-matrix (aref locs +shader-loc-matrix-normal+) (matrix-transpose (matrix-invert mat-model))))
    ;;-----------------------------------------------------

    ;; Bind active texture maps (if available)
    (%bind-material-textures material)

    ;; Try binding vertex array objects (VAO)
    ;; or use VBOs if not possible
    (unless (rl-enable-vertex-array (mesh-vao-id mesh))
      (%bind-mesh-vbos mesh material))

    (let ((eye-count (if (rl-is-stereo-render-enabled) 2 1)))
      (dotimes (eye eye-count)
        ;; Send combined model-view-projection matrix to shader
        (rl-set-uniform-matrix (aref locs +shader-loc-matrix-mvp+) (%mesh-eye-mvp mat-model-view mat-projection eye eye-count))
        ;; Draw mesh instanced
        (if (mesh-indices mesh)
            (rl-draw-vertex-array-elements-instanced 0 (* (mesh-triangle-count mesh) 3) nil instances)
            (rl-draw-vertex-array-instanced 0 (mesh-vertex-count mesh) instances))))

    ;; Unbind all bound texture maps
    (%unbind-material-textures material)

    ;; Disable all possible vertex array objects (or VBOs)
    (rl-disable-vertex-array)
    (rl-disable-vertex-buffer)
    (rl-disable-vertex-buffer-element)

    ;; Disable shader program
    (rl-disable-shader)

    ;; Remove instance transforms buffer
    (rl-unload-vertex-buffer instances-vbo-id)))

;; Unload mesh from memory (RAM and VRAM)
(defun unload-mesh (mesh)
  "Unload mesh data from CPU and GPU"
  ;; Unload rlgl mesh vboId data
  (rl-unload-vertex-array (mesh-vao-id mesh))

  (when (mesh-vbo-id mesh)
    (dotimes (i +max-mesh-vertex-buffers+) (rl-unload-vertex-buffer (aref (mesh-vbo-id mesh) i))))
  (setf (mesh-vbo-id mesh) nil)

  ;; Unload mesh vertex buffers
  (setf (mesh-vertices mesh) nil
        (mesh-texcoords mesh) nil
        (mesh-normals mesh) nil
        (mesh-colors mesh) nil
        (mesh-tangents mesh) nil
        (mesh-texcoords2 mesh) nil
        (mesh-indices mesh) nil)

  ;; Unload mesh skin animation data
  (setf (mesh-bone-weights mesh) nil
        (mesh-bone-indices mesh) nil)

  ;; Unload mesh runtime CPU skinning data
  (setf (mesh-anim-vertices mesh) nil
        (mesh-anim-normals mesh) nil))

;; Export mesh data to file
(defun export-mesh (mesh file-name)
  "Export mesh data to file, returns true on success"
  (let ((result nil))
    (cond
      ((is-file-extension file-name ".obj")
       (let ((out (make-string-output-stream))
             (vertices (mesh-vertices mesh))
             (texcoords (mesh-texcoords mesh))
             (normals (mesh-normals mesh)))
         (format out "# //////////////////////////////////////////////////////////////////////////////////~%")
         (format out "# //                                                                              //~%")
         (format out "# // rMeshOBJ exporter v1.0 - Mesh exported as triangle faces and not optimized   //~%")
         (format out "# //                                                                              //~%")
         (format out "# // more info and bugs-report:  github.com/raysan5/raylib                        //~%")
         (format out "# // feedback and support:       ray[at]raylib.com                                //~%")
         (format out "# //                                                                              //~%")
         (format out "# // Copyright (c) 2018-2026 Ramon Santamaria (@raysan5)                          //~%")
         (format out "# //                                                                              //~%")
         (format out "# //////////////////////////////////////////////////////////////////////////////////~%~%")
         (format out "# Vertex Count:     ~d~%" (mesh-vertex-count mesh))
         (format out "# Triangle Count:   ~d~%~%" (mesh-triangle-count mesh))
         (format out "g mesh~%")
         (loop for v from 0 below (* 3 (mesh-vertex-count mesh)) by 3
               do (format out "v ~a ~a ~a~%" (%c-format-float (aref vertices v) 6)
                          (%c-format-float (aref vertices (+ v 1)) 6) (%c-format-float (aref vertices (+ v 2)) 6)))
         (loop for v from 0 below (* 2 (mesh-vertex-count mesh)) by 2
               do (format out "vt ~a ~a~%" (%c-format-float (aref texcoords v) 6) (%c-format-float (aref texcoords (+ v 1)) 6)))
         (loop for v from 0 below (* 3 (mesh-vertex-count mesh)) by 3
               do (format out "vn ~a ~a ~a~%" (%c-format-float (aref normals v) 4)
                          (%c-format-float (aref normals (+ v 1)) 4) (%c-format-float (aref normals (+ v 2)) 4)))
         (if (mesh-indices mesh)
             (loop for v from 0 below (* 3 (mesh-triangle-count mesh)) by 3
                   do (let ((a (1+ (aref (mesh-indices mesh) v)))
                            (b (1+ (aref (mesh-indices mesh) (+ v 1))))
                            (c (1+ (aref (mesh-indices mesh) (+ v 2)))))
                        (format out "f ~d/~d/~d ~d/~d/~d ~d/~d/~d~%" a a a b b b c c c)))
             (loop for i below (mesh-triangle-count mesh)
                   for v from 1 by 3
                   do (format out "f ~d/~d/~d ~d/~d/~d ~d/~d/~d~%" v v v (+ v 1) (+ v 1) (+ v 1) (+ v 2) (+ v 2) (+ v 2))))
         ;; NOTE: Text data length exported is determined by '\0' (NULL) character
         (setf result (save-file-text file-name (get-output-stream-string out)))))
      ((is-file-extension file-name ".gltf")) ; Or .glb
      ;; TODO: Implement gltf/glb support
      ((is-file-extension file-name ".raw")))
      ;; TODO: Support additional file formats to export mesh vertex data
    result))

;; Export mesh as code file (.h) defining multiple arrays of vertex attributes
(defun export-mesh-as-code (mesh file-name)
  "Export mesh as code file (.h) defining multiple arrays of vertex attributes"
  (let ((out (make-string-output-stream))
        (text-bytes-per-line 20)
        ;; Get file name from path and convert variable name to uppercase
        (var-file-name (string-upcase (get-file-name-without-ext file-name))))
    (format out "////////////////////////////////////////////////////////////////////////////////////////~%")
    (format out "//                                                                                    //~%")
    (format out "// MeshAsCode exporter v1.0 - Mesh vertex data exported as arrays                     //~%")
    (format out "//                                                                                    //~%")
    (format out "// more info and bugs-report:  github.com/raysan5/raylib                              //~%")
    (format out "// feedback and support:       ray[at]raylib.com                                      //~%")
    (format out "//                                                                                    //~%")
    (format out "// Copyright (c) 2023 Ramon Santamaria (@raysan5)                                     //~%")
    (format out "//                                                                                    //~%")
    (format out "////////////////////////////////////////////////////////////////////////////////////////~%~%")

    ;; Add image information
    (format out "// Mesh basic information~%")
    (format out "#define ~a_VERTEX_COUNT    ~d~%" var-file-name (mesh-vertex-count mesh))
    (format out "#define ~a_TRIANGLE_COUNT   ~d~%~%" var-file-name (mesh-triangle-count mesh))

    ;; Define vertex attributes data as separate arrays
    ;;-----------------------------------------------------------------------------------------
    (flet ((float-array (name type data count)
             (format out "static ~a ~a_~a[~d] = { " type var-file-name name count)
             (dotimes (i (1- count))
               (format out (if (= (mod i text-bytes-per-line) 0) "~af,~%" "~af, ") (%c-format-float (aref data i) 3)))
             (format out "~af };~%~%" (%c-format-float (aref data (1- count)) 3))))
      (when (mesh-vertices mesh)       ; Vertex position (XYZ - 3 components per vertex - float)
        (float-array "VERTEX_DATA" "float" (mesh-vertices mesh) (* (mesh-vertex-count mesh) 3)))
      (when (mesh-texcoords mesh)      ; Vertex texture coordinates (UV - 2 components per vertex - float)
        (float-array "TEXCOORD_DATA" "float" (mesh-texcoords mesh) (* (mesh-vertex-count mesh) 2)))
      (when (mesh-texcoords2 mesh)     ; Vertex texture coordinates (UV - 2 components per vertex - float)
        (float-array "TEXCOORD2_DATA" "float" (mesh-texcoords2 mesh) (* (mesh-vertex-count mesh) 2)))
      (when (mesh-normals mesh)        ; Vertex normals (XYZ - 3 components per vertex - float)
        (float-array "NORMAL_DATA" "float" (mesh-normals mesh) (* (mesh-vertex-count mesh) 3)))
      (when (mesh-tangents mesh)       ; Vertex tangents (XYZW - 4 components per vertex - float)
        (float-array "TANGENT_DATA" "float" (mesh-tangents mesh) (* (mesh-vertex-count mesh) 4))))

    (when (mesh-colors mesh)           ; Vertex colors (RGBA - 4 components per vertex - unsigned char)
      (let ((count (* (mesh-vertex-count mesh) 4)) (data (mesh-colors mesh)))
        (format out "static unsigned char ~a_COLOR_DATA[~d] = { " var-file-name count)
        (dotimes (i (1- count))
          (format out (if (= (mod i text-bytes-per-line) 0) "0x~(~x~),~%" "0x~(~x~), ") (aref data i)))
        (format out "0x~(~x~) };~%~%" (aref data (1- count)))))

    (when (mesh-indices mesh)          ; Vertex indices (3 index per triangle - unsigned short)
      (let ((count (* (mesh-triangle-count mesh) 3)) (data (mesh-indices mesh)))
        (format out "static unsigned short ~a_INDEX_DATA[~d] = { " var-file-name count)
        (dotimes (i (1- count))
          (format out (if (= (mod i text-bytes-per-line) 0) "~d,~%" "~d, ") (aref data i)))
        (format out "~d };~%" (aref data (1- count)))))
    ;;-----------------------------------------------------------------------------------------

    ;; NOTE: Text data size exported is determined by '\0' (NULL) character
    (save-file-text file-name (get-output-stream-string out))))

;; Process obj materials
(defun %process-materials-obj (materials mats material-count)
  ;; Init model mats
  (dotimes (m material-count)
    ;; Init material to default
    ;; NOTE: Uses default shader, which only supports MATERIAL_MAP_DIFFUSE
    (setf (aref materials m) (load-material-default))
    (when mats
      (let ((material (aref materials m))
            (mat (aref mats m)))
        (flet ((mmap (i) (%material-map material i))
               (color (rgb) (list (%u8 (* (aref rgb 0) 255.0)) (%u8 (* (aref rgb 1) 255.0)) (%u8 (* (aref rgb 2) 255.0)) 255)))
          ;; Get default texture, in case no texture is defined
          ;; NOTE: rlgl default texture is a 1x1 pixel UNCOMPRESSED_R8G8B8A8
          (setf (material-map-texture (mmap +material-map-diffuse+))
                (make-texture :id (rl-get-texture-id-default) :width 1 :height 1 :mipmaps 1
                              :format +pixelformat-uncompressed-r8g8b8a8+))

          (if (tobjm-diffuse-texname mat)
              (setf (material-map-texture (mmap +material-map-diffuse+)) (load-texture (tobjm-diffuse-texname mat))) ; map_Kd
              (setf (material-map-color (mmap +material-map-diffuse+)) (color (tobjm-diffuse mat)))) ; float diffuse[3]
          (setf (material-map-value (mmap +material-map-diffuse+)) 0.0)

          (when (tobjm-specular-texname mat)
            (setf (material-map-texture (mmap +material-map-specular+)) (load-texture (tobjm-specular-texname mat)))) ; map_Ks
          (setf (material-map-color (mmap +material-map-specular+)) (color (tobjm-specular mat)) ; float specular[3]
                (material-map-value (mmap +material-map-specular+)) 0.0)

          (when (tobjm-bump-texname mat)
            (setf (material-map-texture (mmap +material-map-normal+)) (load-texture (tobjm-bump-texname mat)))) ; map_bump, bump
          (setf (material-map-color (mmap +material-map-normal+)) (copy-list +white+)
                (material-map-value (mmap +material-map-normal+)) (float (tobjm-shininess mat) 1.0))

          (setf (material-map-color (mmap +material-map-emission+)) (color (tobjm-emission mat))) ; float emission[3]

          (when (tobjm-displacement-texname mat)
            (setf (material-map-texture (mmap +material-map-height+)) (load-texture (tobjm-displacement-texname mat))))))))) ; disp

;; Load materials from model file
(defun load-materials (file-name)
  "Load materials from model file, returns a vector of materials"
  (let ((materials (vector))
        (count 0))
    (when (is-file-extension file-name ".mtl")
      (multiple-value-bind (mats result) (tinyobj-parse-mtl-file file-name)
        (unless (eq result :success) (trace-log +log-warning+ "MATERIAL: [~a] Failed to parse materials file" file-name))
        (setf count (length mats)
              materials (make-array count))
        (%process-materials-obj materials mats count)))
    (values materials count)))

;; Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
(defun load-material-default ()
  "Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)"
  (let ((material (make-material)))
    (setf (material-maps material)
          (let ((maps (make-array +max-material-maps+)))
            (dotimes (i +max-material-maps+ maps) (setf (svref maps i) (make-material-map)))))

    ;; Using rlgl default shader
    (setf (material-shader material) (make-shader :id (rl-get-shader-id-default) :locs (rl-get-shader-locs-default)))

    ;; Using rlgl default texture (1x1 pixel, UNCOMPRESSED_R8G8B8A8, 1 mipmap)
    (setf (material-map-texture (%material-map material +material-map-diffuse+))
          (make-texture :id (rl-get-texture-id-default) :width 1 :height 1 :mipmaps 1
                        :format +pixelformat-uncompressed-r8g8b8a8+))
    ;;material.maps[MATERIAL_MAP_NORMAL].texture;         // NOTE: By default, not set
    ;;material.maps[MATERIAL_MAP_SPECULAR].texture;       // NOTE: By default, not set

    (setf (material-map-color (%material-map material +material-map-diffuse+)) (copy-list +white+) ; Diffuse color
          (material-map-color (%material-map material +material-map-specular+)) (copy-list +white+)) ; Specular color
    material))

;; Check if material is valid (map textures loaded in GPU)
(defun is-material-valid (material)
  "Check if a material is valid (shader assigned, map textures loaded in GPU)"
  (and (material-maps material)        ; Validate material contain some map
       (> (shader-id (material-shader material)) 0) ; Validate material shader is valid
       t))

;; Unload material from memory
(defun unload-material (material)
  "Unload material from GPU memory (VRAM)"
  ;; Unload material shader (avoid unloading default shader, managed by raylib)
  (when (/= (shader-id (material-shader material)) (rl-get-shader-id-default))
    (unload-shader (material-shader material)))

  ;; Unload loaded texture maps (avoid unloading default texture, managed by raylib)
  (when (material-maps material)
    (dotimes (i +max-material-maps+)
      (let ((id (texture-id (material-map-texture (%material-map material i)))))
        (when (/= id (rl-get-texture-id-default)) (rl-unload-texture id)))))

  (setf (material-maps material) nil))

;; Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
;; NOTE: Previous texture should be manually unloaded
(defun set-material-texture (material map-type texture)
  "Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)"
  (setf (material-map-texture (%material-map material map-type)) texture))

;; Set the material for a mesh
(defun set-model-mesh-material (model mesh-id material-id)
  "Set material for a mesh"
  (cond ((>= mesh-id (model-mesh-count model)) (trace-log +log-warning+ "MESH: Id greater than mesh count"))
        ((>= material-id (model-material-count model)) (trace-log +log-warning+ "MATERIAL: Id greater than material count"))
        (t (setf (aref (model-mesh-material model) mesh-id) material-id))))

;; Load model animations from file
(defun load-model-animations (file-name)
  "Load model animations from file, returns a vector of model-animation and the count"
  (let ((animations nil) (count 0))
    (when (is-file-extension file-name ".iqm") (multiple-value-setq (animations count) (%load-model-animations-iqm file-name)))
    (when (is-file-extension file-name ".m3d") (multiple-value-setq (animations count) (%load-model-animations-m3d file-name)))
    (when (is-file-extension file-name ".gltf;.glb") (multiple-value-setq (animations count) (%load-model-animations-gltf file-name)))
    (values animations count)))

(defun %transform-matrix (transform)
  "MatrixMultiply(MatrixMultiply(MatrixScale(scale), QuaternionToMatrix(rotation)), MatrixTranslate(translation))"
  (let ((s (transform-scale transform))
        (tr (transform-translation transform)))
    (matrix-multiply
     (matrix-multiply (matrix-scale (vx3 s) (vy3 s) (vz3 s))
                      (quaternion-to-matrix (transform-rotation transform)))
     (matrix-translate (vx3 tr) (vy3 tr) (vz3 tr)))))

(defun %update-bone-matrix (model bone-index)
  "Compute runtime bone matrix from model current pose"
  (let ((bind-pose-matrix (%transform-matrix (aref (model-skeleton-bind-pose (model-skeleton model)) bone-index)))
        (current-pose-matrix (%transform-matrix (aref (model-current-pose model) bone-index))))
    (setf (aref (model-bone-matrices model) bone-index)
          (matrix-multiply (matrix-invert bind-pose-matrix) current-pose-matrix))))

(defun %anim-pose (anim frame bone-index)
  (aref (aref (model-animation-keyframe-poses anim) frame) bone-index))

;; Update model animation data (vertex buffers / bone matrices) for a specific pose
;; NOTE 1: Request frame could be fractional, using a lerp interpolation between two frames
;; NOTE 2: Updated vertex animation data is uploaded to GPU in case of CPU skinning,
;; for GPU skinning, bone matrices are uploaded to shader on DrawModelEx()
(defun update-model-animation (model anim frame)
  "Update model animation pose (vertex buffers and bone matrices)"
  (unless (model-bone-matrices model) (return-from update-model-animation))

  ;;UpdateModelAnimationEx(model, anim, frame, anim, frame, 0.0f);

  ;; Update model animated bones transform matrices for a given frame
  (when (and (> (model-animation-keyframe-count anim) 0)
             (model-skeleton-bones (model-skeleton model))
             (model-animation-keyframe-poses anim))
    ;; Get frame and blending from frame factor required
    (let* ((frame (float frame 1.0))
           (count (model-animation-keyframe-count anim))
           (current-frame (truncate frame))
           (next-frame (+ current-frame 1))
           (blend (clamp (- frame current-frame) 0.0 1.0)))
      (when (>= current-frame count) (setf current-frame (rem current-frame count)))
      (when (>= next-frame count) (setf next-frame (rem next-frame count)))

      ;; Update all bones and bone matrices of model
      (dotimes (bone-index (model-skeleton-bone-count (model-skeleton model)))
        ;; Compute interpolated pose between current and next frame
        ;; NOTE: Storing animation frame data in model.currentPose
        (let ((current (%anim-pose anim current-frame bone-index))
              (next (%anim-pose anim next-frame bone-index))
              (pose (aref (model-current-pose model) bone-index)))
          (setf (transform-translation pose) (vector3-lerp (transform-translation current) (transform-translation next) blend)
                (transform-rotation pose) (quaternion-slerp (transform-rotation current) (transform-rotation next) blend)
                (transform-scale pose) (vector3-lerp (transform-scale current) (transform-scale next) blend)))
        (%update-bone-matrix model bone-index))

      ;; CPU skinning, updates CPU buffers and uploads them to GPU
      ;; NOTE: On GPU skinning not supported, use CPU skinning
      (%update-model-animation-vertex-buffers model))))

;; Update model animation data (vertex buffers / bone matrices) for a specific pose,
;; defined by two different animations at specific frames blended together
;; NOTE 1: Request frames could be fractional, using a lerp interpolation between two frames
;; NOTE 2: Updated vertex animation data is uploaded to GPU in case of CPU skinning,
;; for GPU skinning, bone matrices are uploaded to shader on DrawModelEx()
(defun update-model-animation-ex (model anim-a frame-a anim-b frame-b blend)
  "Update model animation pose, blending two animations"
  (unless (model-bone-matrices model) (return-from update-model-animation-ex))

  (let ((blend (float blend 1.0)))
    (when (and (> (model-animation-keyframe-count anim-a) 0) (model-animation-keyframe-poses anim-a)
               (> (model-animation-keyframe-count anim-b) 0) (model-animation-keyframe-poses anim-b)
               (>= blend 0.0) (<= blend 1.0))
      (flet ((frames (anim frame)
               ;; Inter-frame interpolation values for the animation
               (let* ((frame (float frame 1.0))
                      (count (model-animation-keyframe-count anim))
                      (current-frame (rem (truncate frame) count))
                      (next-frame (+ current-frame 1))
                      (frame-blend (clamp (- frame current-frame) 0.0 1.0)))
                 (when (>= current-frame count) (setf current-frame (rem current-frame count)))
                 (when (>= next-frame count) (setf next-frame (rem next-frame count)))
                 (values current-frame next-frame frame-blend))))
        (multiple-value-bind (current-frame-a next-frame-a blend-a) (frames anim-a frame-a)
          (multiple-value-bind (current-frame-b next-frame-b blend-b) (frames anim-b frame-b)
            (dotimes (bone-index (model-skeleton-bone-count (model-skeleton model)))
              (let* ((ca (%anim-pose anim-a current-frame-a bone-index))
                     (na (%anim-pose anim-a next-frame-a bone-index))
                     (cb (%anim-pose anim-b current-frame-b bone-index))
                     (nb (%anim-pose anim-b next-frame-b bone-index))
                     ;; Get frame-interpolation for first animation
                     (frame-a-translation (vector3-lerp (transform-translation ca) (transform-translation na) blend-a))
                     (frame-a-rotation (quaternion-slerp (transform-rotation ca) (transform-rotation na) blend-a))
                     (frame-a-scale (vector3-lerp (transform-scale ca) (transform-scale na) blend-a))
                     ;; Get frame-interpolation for second animation
                     (frame-b-translation (vector3-lerp (transform-translation cb) (transform-translation nb) blend-b))
                     (frame-b-rotation (quaternion-slerp (transform-rotation cb) (transform-rotation nb) blend-b))
                     (frame-b-scale (vector3-lerp (transform-scale cb) (transform-scale nb) blend-b))
                     (pose (aref (model-current-pose model) bone-index)))
                ;; Compute interpolated pose between both animations frames
                ;; NOTE: Storing animation frame data in model.currentPose
                (setf (transform-translation pose) (vector3-lerp frame-a-translation frame-b-translation blend)
                      (transform-rotation pose) (quaternion-slerp frame-a-rotation frame-b-rotation blend)
                      (transform-scale pose) (vector3-lerp frame-a-scale frame-b-scale blend))
                (%update-bone-matrix model bone-index)))

            ;; CPU skinning, updates CPU buffers and uploads them to GPU (if available)
            ;; NOTE: Fallback in case GPU skinning is not supported or enabled
            (%update-model-animation-vertex-buffers model)))))))

;; Update model vertex animation buffers (positions and normals)
;; NOTE: Required for CPU skinning, uploads animated vertex buffers to GPU
(defun %update-model-animation-vertex-buffers (model)
  (dotimes (m (model-mesh-count model))
    (let* ((mesh (aref (model-meshes model) m))
           (vertex-values-count (* (mesh-vertex-count mesh) 3))
           (bone-counter 0)
           (buffer-update-required nil)) ; Flag to check when anim vertex information is updated
      ;; Skip if missing bone data or missing anim buffers initialization
      (when (and (mesh-bone-weights mesh) (mesh-bone-indices mesh)
                 (mesh-anim-vertices mesh) (mesh-anim-normals mesh))
        (let ((anim-vertices (mesh-anim-vertices mesh))
              (anim-normals (mesh-anim-normals mesh))
              (vertices (mesh-vertices mesh))
              (normals (mesh-normals mesh)))
          (loop for v-counter from 0 below vertex-values-count by 3
                do (setf (aref anim-vertices v-counter) 0.0
                         (aref anim-vertices (+ v-counter 1)) 0.0
                         (aref anim-vertices (+ v-counter 2)) 0.0)
                   (when anim-normals
                     (setf (aref anim-normals v-counter) 0.0
                           (aref anim-normals (+ v-counter 1)) 0.0
                           (aref anim-normals (+ v-counter 2)) 0.0))
                   ;; Iterates over 4 bones per vertex
                   (dotimes (j 4)
                     (let ((bone-weight (aref (mesh-bone-weights mesh) bone-counter))
                           (bone-index (aref (mesh-bone-indices mesh) bone-counter)))
                       (incf bone-counter)
                       ;; Early stop when no transformation will be applied
                       (unless (= bone-weight 0.0)
                         (let ((anim-vertex (vector3-transform (vec3 (aref vertices v-counter) (aref vertices (+ v-counter 1))
                                                                     (aref vertices (+ v-counter 2)))
                                                               (aref (model-bone-matrices model) bone-index))))
                           (incf (aref anim-vertices v-counter) (* (vx3 anim-vertex) bone-weight))
                           (incf (aref anim-vertices (+ v-counter 1)) (* (vy3 anim-vertex) bone-weight))
                           (incf (aref anim-vertices (+ v-counter 2)) (* (vz3 anim-vertex) bone-weight))
                           (setf buffer-update-required t))
                         ;; Normals processing
                         ;; NOTE: Using meshes.baseNormals (default normal) to calculate meshes.normals (animated normals)
                         (when (and normals anim-normals)
                           (let ((anim-normal (vector3-transform (vec3 (aref normals v-counter) (aref normals (+ v-counter 1))
                                                                       (aref normals (+ v-counter 2)))
                                                                 (matrix-transpose (matrix-invert (aref (model-bone-matrices model) bone-index))))))
                             (incf (aref anim-normals v-counter) (* (vx3 anim-normal) bone-weight))
                             (incf (aref anim-normals (+ v-counter 1)) (* (vy3 anim-normal) bone-weight))
                             (incf (aref anim-normals (+ v-counter 2)) (* (vz3 anim-normal) bone-weight))))))))
          (when buffer-update-required
            ;; Update GPU vertex buffers with updated data (position + normals)
            (rl-update-vertex-buffer (aref (mesh-vbo-id mesh) +shader-loc-vertex-position+) anim-vertices (* (mesh-vertex-count mesh) 3 4) 0)
            ;; NOTE: C uses vboId[SHADER_LOC_VERTEX_NORMAL] (3, the colors buffer index), reproduced as is
            (when normals
              (rl-update-vertex-buffer (aref (mesh-vbo-id mesh) +shader-loc-vertex-normal+) anim-normals (* (mesh-vertex-count mesh) 3 4) 0))))))))

;; Unload animation array data
(defun unload-model-animations (animations anim-count)
  "Unload animation array data"
  (dotimes (a anim-count)
    (setf (model-animation-keyframe-poses (aref animations a)) nil))
  nil)

;; Check model animation skeleton match
;; NOTE: Only number of bones and parent connections are checked
(defun is-model-animation-valid (model anim)
  "Check model animation skeleton match"
  (= (model-skeleton-bone-count (model-skeleton model)) (model-animation-bone-count anim)))

;;;----------------------------------------------------------------------------------
;;; Mesh generation functions
;;;----------------------------------------------------------------------------------

;; Generate polygonal mesh
(defun gen-mesh-poly (sides radius)
  "Generate polygonal mesh"
  (let ((mesh (make-mesh)))
    (when (< sides 3) (return-from gen-mesh-poly mesh)) ; Security check
    (let* ((radius (float radius 1.0))
           (vertex-count (* sides 3))
           (vertices (%floats (* vertex-count 3)))
           (d 0.0)
           (d-step (/ 360.0 sides)))
      ;; Vertices definition
      (loop for v from 0 below (- vertex-count 2) by 3
            do (setf (aref vertices (* 3 (+ v 1))) (* (%sinf (* +deg2rad+ d)) radius)
                     (aref vertices (+ (* 3 (+ v 1)) 2)) (* (%cosf (* +deg2rad+ d)) radius)
                     (aref vertices (* 3 (+ v 2))) (* (%sinf (* +deg2rad+ (+ d d-step))) radius)
                     (aref vertices (+ (* 3 (+ v 2)) 2)) (* (%cosf (* +deg2rad+ (+ d d-step))) radius))
               (incf d d-step))
      (setf (mesh-vertex-count mesh) vertex-count
            (mesh-triangle-count mesh) sides
            (mesh-vertices mesh) vertices
            (mesh-texcoords mesh) (%floats (* vertex-count 2)) ; TexCoords definition
            (mesh-normals mesh) (let ((normals (%floats (* vertex-count 3)))) ; Normals definition
                                  (dotimes (n vertex-count normals) (setf (aref normals (+ (* 3 n) 1)) 1.0)))) ; Vector3.up;
      ;; Upload vertex data to GPU (static mesh)
      ;; NOTE: mesh.vboId array is allocated inside UploadMesh()
      (upload-mesh mesh nil)
      mesh)))

(defun %mesh-from-par-shapes (shape)
  "Unindexed mesh data from a par_shapes mesh, uploaded to GPU (static mesh)"
  (let* ((vertex-count (* (par-ntriangles shape) 3))
         (vertices (%floats (* vertex-count 3)))
         (texcoords (%floats (* vertex-count 2)))
         (normals (%floats (* vertex-count 3)))
         (points (par-points shape))
         (pnormals (par-normals shape))
         (tcoords (par-tcoords shape))
         (triangles (par-triangles shape)))
    (dotimes (k vertex-count)
      (let ((i (aref triangles k)))
        (setf (aref vertices (* k 3)) (aref points (* i 3))
              (aref vertices (+ (* k 3) 1)) (aref points (+ (* i 3) 1))
              (aref vertices (+ (* k 3) 2)) (aref points (+ (* i 3) 2))
              (aref normals (* k 3)) (aref pnormals (* i 3))
              (aref normals (+ (* k 3) 1)) (aref pnormals (+ (* i 3) 1))
              (aref normals (+ (* k 3) 2)) (aref pnormals (+ (* i 3) 2))
              (aref texcoords (* k 2)) (aref tcoords (* i 2))
              (aref texcoords (+ (* k 2) 1)) (aref tcoords (+ (* i 2) 1)))))
    (par-shapes-free-mesh shape)
    (let ((mesh (make-mesh :vertex-count vertex-count
                           :triangle-count (par-ntriangles shape)
                           :vertices vertices
                           :texcoords texcoords
                           :normals normals)))
      ;; Upload vertex data to GPU (static mesh)
      (upload-mesh mesh nil)
      mesh)))

(defparameter +x-axis+ '(1.0 0.0 0.0))
(defparameter +y-axis+ '(0.0 1.0 0.0))

;; Generate plane mesh (with subdivisions)
;; NOTE: CUSTOM_MESH_GEN_PLANE is defined in rmodels.c (par_shapes is not used)
(defun gen-mesh-plane (width length res-x res-z)
  "Generate plane mesh (with subdivisions)"
  (let* ((width (float width 1.0)) (length (float length 1.0))
         (res-x (1+ res-x))
         (res-z (1+ res-z))
         ;; Vertices definition
         (vertex-count (* res-x res-z)) ; vertices get reused for the faces
         (vertices (%floats (* vertex-count 3)))
         (normals (%floats (* vertex-count 3)))
         (texcoords (%floats (* vertex-count 2)))
         ;; Triangles definition (indices)
         (num-faces (* (- res-x 1) (- res-z 1)))
         (indices (make-array (* num-faces 6) :element-type '(unsigned-byte 16) :initial-element 0)))
    (dotimes (z res-z)
      ;; [-length/2, length/2]
      (let ((z-pos (* (- (/ (float z 1.0) (- res-z 1)) 0.5) length)))
        (dotimes (x res-x)
          ;; [-width/2, width/2]
          (let ((x-pos (* (- (/ (float x 1.0) (- res-x 1)) 0.5) width))
                (i (+ x (* z res-x))))
            (setf (aref vertices (* 3 i)) x-pos
                  (aref vertices (+ (* 3 i) 2)) z-pos)))))
    ;; Normals definition
    (dotimes (n vertex-count) (setf (aref normals (+ (* 3 n) 1)) 1.0)) ; Vector3.up;
    ;; TexCoords definition
    (dotimes (v res-z)
      (dotimes (u res-x)
        (setf (aref texcoords (* 2 (+ u (* v res-x)))) (/ (float u 1.0) (- res-x 1))
              (aref texcoords (+ (* 2 (+ u (* v res-x))) 1)) (/ (float v 1.0) (- res-z 1)))))
    (let ((tt 0))
      (dotimes (face num-faces)
        ;; Retrieve lower left corner from face ind
        (let ((i (+ face (truncate face (- res-x 1)))))
          (dolist (index (list (+ i res-x) (+ i 1) i (+ i res-x) (+ i res-x 1) (+ i 1)))
            (setf (aref indices tt) (logand index #xffff))
            (incf tt)))))
    (let ((mesh (make-mesh :vertex-count vertex-count
                           :triangle-count (* num-faces 2)
                           :vertices vertices
                           :texcoords texcoords
                           :normals normals
                           :indices indices)))
      ;; Upload vertex data to GPU (static mesh)
      (upload-mesh mesh nil)
      mesh)))

;; Generated cuboid mesh
;; NOTE: CUSTOM_MESH_GEN_CUBE is defined in rmodels.c (par_shapes is not used)
(defun gen-mesh-cube (width height length)
  "Generate cuboid mesh"
  (let* ((w (/ (float width 1.0) 2)) (h (/ (float height 1.0) 2)) (l (/ (float length 1.0) 2))
         (vertices (make-array 72 :element-type 'single-float
                                  :initial-contents
                                  (list (- w) (- h) l    w (- h) l    w h l    (- w) h l
                                        (- w) (- h) (- l)    (- w) h (- l)    w h (- l)    w (- h) (- l)
                                        (- w) h (- l)    (- w) h l    w h l    w h (- l)
                                        (- w) (- h) (- l)    w (- h) (- l)    w (- h) l    (- w) (- h) l
                                        w (- h) (- l)    w h (- l)    w h l    w (- h) l
                                        (- w) (- h) (- l)    (- w) (- h) l    (- w) h l    (- w) h (- l))))
         (texcoords (make-array 48 :element-type 'single-float
                                   :initial-contents
                                   '(0.0 0.0  1.0 0.0  1.0 1.0  0.0 1.0
                                     1.0 0.0  1.0 1.0  0.0 1.0  0.0 0.0
                                     0.0 1.0  0.0 0.0  1.0 0.0  1.0 1.0
                                     1.0 1.0  0.0 1.0  0.0 0.0  1.0 0.0
                                     1.0 0.0  1.0 1.0  0.0 1.0  0.0 0.0
                                     0.0 0.0  1.0 0.0  1.0 1.0  0.0 1.0)))
         (normals (make-array 72 :element-type 'single-float
                                 :initial-contents
                                 (loop for n in '((0.0 0.0 1.0) (0.0 0.0 -1.0) (0.0 1.0 0.0)
                                                  (0.0 -1.0 0.0) (1.0 0.0 0.0) (-1.0 0.0 0.0))
                                       append (loop repeat 4 append n))))
         (indices (make-array 36 :element-type '(unsigned-byte 16) :initial-element 0)))
    ;; Indices can be initialized right now
    (loop for i from 0 below 36 by 6
          for k from 0
          do (setf (aref indices i) (* 4 k)
                   (aref indices (+ i 1)) (+ (* 4 k) 1)
                   (aref indices (+ i 2)) (+ (* 4 k) 2)
                   (aref indices (+ i 3)) (* 4 k)
                   (aref indices (+ i 4)) (+ (* 4 k) 2)
                   (aref indices (+ i 5)) (+ (* 4 k) 3)))
    (let ((mesh (make-mesh :vertex-count 24
                           :triangle-count 12
                           :vertices vertices
                           :texcoords texcoords
                           :normals normals
                           :indices indices)))
      ;; Upload vertex data to GPU (static mesh)
      (upload-mesh mesh nil)
      mesh)))

;; Generate sphere mesh (standard sphere)
(defun gen-mesh-sphere (radius rings slices)
  "Generate sphere mesh (standard sphere)"
  (if (and (>= rings 3) (>= slices 3))
      (progn
        (par-shapes-set-epsilon-degenerate-sphere 0.0)
        (let ((sphere (par-shapes-create-parametric-sphere slices rings)))
          (par-shapes-scale sphere radius radius radius)
          ;; NOTE: Soft normals are computed internally
          (%mesh-from-par-shapes sphere)))
      (progn
        (trace-log +log-warning+ "MESH: Failed to generate mesh: sphere")
        (make-mesh))))

;; Generate hemisphere mesh (half sphere, no bottom cap)
(defun gen-mesh-hemi-sphere (radius rings slices)
  "Generate half-sphere mesh (no bottom cap)"
  (if (and (>= rings 3) (>= slices 3))
      (let ((radius (max (float radius 1.0) 0.0))
            (sphere (par-shapes-create-hemisphere slices rings)))
        (par-shapes-scale sphere radius radius radius)
        ;; NOTE: Soft normals are computed internally
        (%mesh-from-par-shapes sphere))
      (progn
        (trace-log +log-warning+ "MESH: Failed to generate mesh: hemisphere")
        (make-mesh))))

;; Generate cylinder mesh
(defun gen-mesh-cylinder (radius height slices)
  "Generate cylinder mesh"
  (if (>= slices 3)
      (let ((radius (float radius 1.0)) (height (float height 1.0)))
        ;; Instance a cylinder that sits on the Z=0 plane using the given tessellation
        ;; levels across the UV domain.  Think of "slices" like a number of pizza
        ;; slices, and "stacks" like a number of stacked rings
        ;; Height and radius are both 1.0, but they can easily be changed with par_shapes_scale
        (let ((cylinder (par-shapes-create-cylinder slices 8))
              (cap-top nil) (cap-bottom nil))
          (par-shapes-scale cylinder radius radius height)
          (par-shapes-rotate cylinder (/ (- +pi+) 2.0) +x-axis+)

          ;; Generate an orientable disk shape (top cap)
          (setf cap-top (par-shapes-create-disk radius slices '(0.0 0.0 0.0) '(0.0 0.0 1.0)))
          (setf (par-tcoords cap-top) (%floats (* 2 (par-npoints cap-top))))
          (par-shapes-rotate cap-top (/ (- +pi+) 2.0) +x-axis+)
          (par-shapes-rotate cap-top (* 90 +deg2rad+) +y-axis+)
          (par-shapes-translate cap-top 0.0 height 0.0)

          ;; Generate an orientable disk shape (bottom cap)
          (setf cap-bottom (par-shapes-create-disk radius slices '(0.0 0.0 0.0) '(0.0 0.0 -1.0)))
          (setf (par-tcoords cap-bottom) (make-array (* 2 (par-npoints cap-bottom)) :element-type 'single-float :initial-element 0.95))
          (par-shapes-rotate cap-bottom (/ +pi+ 2.0) +x-axis+)
          (par-shapes-rotate cap-bottom (* -90 +deg2rad+) +y-axis+)

          (par-shapes-merge-and-free cylinder cap-top)
          (par-shapes-merge-and-free cylinder cap-bottom)
          (%mesh-from-par-shapes cylinder)))
      (progn
        (trace-log +log-warning+ "MESH: Failed to generate mesh: cylinder")
        (make-mesh))))

;; Generate cone/pyramid mesh
(defun gen-mesh-cone (radius height slices)
  "Generate cone/pyramid mesh"
  (if (>= slices 3)
      (let ((radius (float radius 1.0)) (height (float height 1.0)))
        ;; Instance a cone that sits on the Z=0 plane using the given tessellation
        ;; levels across the UV domain.  Think of "slices" like a number of pizza
        ;; slices, and "stacks" like a number of stacked rings
        ;; Height and radius are both 1.0, but they can easily be changed with par_shapes_scale
        (let ((cone (par-shapes-create-cone slices 8))
              (cap-bottom nil))
          (par-shapes-scale cone radius radius height)
          (par-shapes-rotate cone (/ (- +pi+) 2.0) +x-axis+)
          (par-shapes-rotate cone (/ +pi+ 2.0) +y-axis+)

          ;; Generate an orientable disk shape (bottom cap)
          (setf cap-bottom (par-shapes-create-disk radius slices '(0.0 0.0 0.0) '(0.0 0.0 -1.0)))
          (setf (par-tcoords cap-bottom) (make-array (* 2 (par-npoints cap-bottom)) :element-type 'single-float :initial-element 0.95))
          (par-shapes-rotate cap-bottom (/ +pi+ 2.0) +x-axis+)

          (par-shapes-merge-and-free cone cap-bottom)
          (%mesh-from-par-shapes cone)))
      (progn
        (trace-log +log-warning+ "MESH: Failed to generate mesh: cone")
        (make-mesh))))

;; Generate torus mesh
;; NOTE: The distance between the center of the hole and the center of the
;; tube is half size of the radius of the tube (radius*size/2)
(defun gen-mesh-torus (radius size rad-seg sides)
  "Generate torus mesh"
  (if (and (>= sides 3) (>= rad-seg 3))
      (let ((radius (float radius 1.0)) (size (float size 1.0)))
        (cond ((> radius 1.0) (setf radius 1.0))
              ((< radius 0.1) (setf radius 0.1)))
        ;; Create a donut that sits on the Z=0 plane with the specified inner radius
        ;; The outer radius can be controlled with par_shapes_scale
        (let ((torus (par-shapes-create-torus rad-seg sides radius)))
          (par-shapes-scale torus (/ size 2) (/ size 2) (/ size 2))
          (%mesh-from-par-shapes torus)))
      (progn
        (trace-log +log-warning+ "MESH: Failed to generate mesh: torus")
        (make-mesh))))

;; Generate trefoil knot mesh
(defun gen-mesh-knot (radius size rad-seg sides)
  "Generate trefoil knot mesh"
  (if (and (>= sides 3) (>= rad-seg 3))
      (let ((radius (float radius 1.0)) (size (float size 1.0)))
        (cond ((> radius 3.0) (setf radius 3.0))
              ((< radius 0.5) (setf radius 0.5)))
        (let ((knot (par-shapes-create-trefoil-knot rad-seg sides radius)))
          (par-shapes-scale knot size size size)
          (%mesh-from-par-shapes knot)))
      (progn
        (trace-log +log-warning+ "MESH: Failed to generate mesh: knot")
        (make-mesh))))

;; Generate a mesh from heightmap
;; NOTE: Vertex data is uploaded to GPU
(defun gen-mesh-heightmap (heightmap size)
  "Generate heightmap mesh from image data"
  (let* ((map-x (image-width heightmap))
         (map-z (image-height heightmap))
         (pixels (%load-image-colors heightmap))
         (mesh (make-mesh))
         ;; NOTE: One vertex per pixel
         (triangle-count (* (- map-x 1) (- map-z 1) 2)) ; One quad every four pixels
         (vertex-count (* triangle-count 3))
         (vertices (%floats (* vertex-count 3)))
         (normals (%floats (* vertex-count 3)))
         (texcoords (%floats (* vertex-count 2)))
         (v-counter 0)                  ; Used to count vertices float by float
         (tc-counter 0)                 ; Used to count texcoords float by float
         (n-counter 0)                  ; Used to count normals float by float
         (scale-x (/ (%x size) (- map-x 1)))
         (scale-y (/ (%y size) 255.0))
         (scale-z (/ (%z size) (- map-z 1))))
    (flet ((gray (i)
             ;; GRAY_VALUE(c) ((float)(c.r + c.g + c.b)/3.0f)
             (/ (float (+ (aref pixels (* 4 i)) (aref pixels (+ (* 4 i) 1)) (aref pixels (+ (* 4 i) 2))) 1.0) 3.0)))
      (dotimes (z (- map-z 1))
        (dotimes (x (- map-x 1))
          ;; Fill vertices array with data
          ;;----------------------------------------------------------
          (let ((v v-counter))
            ;; one triangle - 3 vertex
            (setf (aref vertices v) (* (float x 1.0) scale-x)
                  (aref vertices (+ v 1)) (* (gray (+ x (* z map-x))) scale-y)
                  (aref vertices (+ v 2)) (* (float z 1.0) scale-z)

                  (aref vertices (+ v 3)) (* (float x 1.0) scale-x)
                  (aref vertices (+ v 4)) (* (gray (+ x (* (+ z 1) map-x))) scale-y)
                  (aref vertices (+ v 5)) (* (float (+ z 1) 1.0) scale-z)

                  (aref vertices (+ v 6)) (* (float (+ x 1) 1.0) scale-x)
                  (aref vertices (+ v 7)) (* (gray (+ (+ x 1) (* z map-x))) scale-y)
                  (aref vertices (+ v 8)) (* (float z 1.0) scale-z))

            ;; Another triangle - 3 vertex
            (setf (aref vertices (+ v 9)) (aref vertices (+ v 6))
                  (aref vertices (+ v 10)) (aref vertices (+ v 7))
                  (aref vertices (+ v 11)) (aref vertices (+ v 8))

                  (aref vertices (+ v 12)) (aref vertices (+ v 3))
                  (aref vertices (+ v 13)) (aref vertices (+ v 4))
                  (aref vertices (+ v 14)) (aref vertices (+ v 5))

                  (aref vertices (+ v 15)) (* (float (+ x 1) 1.0) scale-x)
                  (aref vertices (+ v 16)) (* (gray (+ (+ x 1) (* (+ z 1) map-x))) scale-y)
                  (aref vertices (+ v 17)) (* (float (+ z 1) 1.0) scale-z))
            (incf v-counter 18))        ; 6 vertex, 18 floats

          ;; Fill texcoords array with data
          ;;--------------------------------------------------------------
          (let ((tc tc-counter))
            (setf (aref texcoords tc) (/ (float x 1.0) (- map-x 1))
                  (aref texcoords (+ tc 1)) (/ (float z 1.0) (- map-z 1))

                  (aref texcoords (+ tc 2)) (/ (float x 1.0) (- map-x 1))
                  (aref texcoords (+ tc 3)) (/ (float (+ z 1) 1.0) (- map-z 1))

                  (aref texcoords (+ tc 4)) (/ (float (+ x 1) 1.0) (- map-x 1))
                  (aref texcoords (+ tc 5)) (/ (float z 1.0) (- map-z 1))

                  (aref texcoords (+ tc 6)) (aref texcoords (+ tc 4))
                  (aref texcoords (+ tc 7)) (aref texcoords (+ tc 5))

                  (aref texcoords (+ tc 8)) (aref texcoords (+ tc 2))
                  (aref texcoords (+ tc 9)) (aref texcoords (+ tc 3))

                  (aref texcoords (+ tc 10)) (/ (float (+ x 1) 1.0) (- map-x 1))
                  (aref texcoords (+ tc 11)) (/ (float (+ z 1) 1.0) (- map-z 1)))
            (incf tc-counter 12))       ; 6 texcoords, 12 floats

          ;; Fill normals array with data
          ;;--------------------------------------------------------------
          (loop for i from 0 below 18 by 9
                do (let* ((o (+ n-counter i))
                          (va (vec3 (aref vertices o) (aref vertices (+ o 1)) (aref vertices (+ o 2))))
                          (vb (vec3 (aref vertices (+ o 3)) (aref vertices (+ o 4)) (aref vertices (+ o 5))))
                          (vc (vec3 (aref vertices (+ o 6)) (aref vertices (+ o 7)) (aref vertices (+ o 8))))
                          (vn (vector3-normalize (vector3-cross-product (vector3-subtract vb va) (vector3-subtract vc va)))))
                     (dotimes (k 3)
                       (setf (aref normals (+ o (* 3 k))) (vx3 vn)
                             (aref normals (+ o (* 3 k) 1)) (vy3 vn)
                             (aref normals (+ o (* 3 k) 2)) (vz3 vn)))))
          (incf n-counter 18))))        ; 6 vertex, 18 floats

    (setf (mesh-triangle-count mesh) triangle-count
          (mesh-vertex-count mesh) vertex-count
          (mesh-vertices mesh) vertices
          (mesh-normals mesh) normals
          (mesh-texcoords mesh) texcoords
          (mesh-colors mesh) nil)

    ;; Upload vertex data to GPU (static mesh)
    (upload-mesh mesh nil)
    mesh))

;; Generate a cubes mesh from pixel data
;; NOTE: Vertex data is uploaded to GPU
(defun gen-mesh-cubicmap (cubicmap cube-size)
  "Generate cubes-based map mesh from image data"
  (let* ((pixels (%load-image-colors cubicmap))
         (width (image-width cubicmap))
         (height (image-height cubicmap))
         (w (%x cube-size))
         (h (%z cube-size))
         (h2 (%y cube-size))
         (map-vertices (make-array 64 :adjustable t :fill-pointer 0))
         (map-normals (make-array 64 :adjustable t :fill-pointer 0))
         (map-texcoords (make-array 64 :adjustable t :fill-pointer 0))
         ;; Define the 6 normals of the cube, combined accordingly later
         (n1 '(1.0 0.0 0.0))
         (n2 '(-1.0 0.0 0.0))
         (n3 '(0.0 1.0 0.0))
         (n4 '(0.0 -1.0 0.0))
         (n5 '(0.0 0.0 -1.0))
         (n6 '(0.0 0.0 1.0))
         ;; NOTE: Using texture rectangles to define different
         ;; textures for top-bottom-front-back-right-left (6)
         (right-tex-uv '(0.0 0.0 0.5 0.5))
         (left-tex-uv '(0.5 0.0 0.5 0.5))
         (front-tex-uv '(0.0 0.0 0.5 0.5))
         (back-tex-uv '(0.5 0.0 0.5 0.5))
         (top-tex-uv '(0.0 0.5 0.5 0.5))
         (bottom-tex-uv '(0.5 0.5 0.5 0.5)))
    (labels ((pixel-is (index color)
               ;; COLOR_EQUAL(pixels[index], color)
               (destructuring-bind (r g b a) color
                 (and (= (aref pixels (* 4 index)) r) (= (aref pixels (+ (* 4 index) 1)) g)
                      (= (aref pixels (+ (* 4 index) 2)) b) (= (aref pixels (+ (* 4 index) 3)) a))))
             (uv (rec corner)
               ;; Texcoord from a texture rectangle corner (:tl :bl :tr :br)
               (destructuring-bind (x y rw rh) rec
                 (ecase corner
                   (:tl (list x y))
                   (:bl (list x (+ y rh)))
                   (:tr (list (+ x rw) y))
                   (:br (list (+ x rw) (+ y rh))))))
             (face (vertices normal rec corners)
               ;; Define 2 triangles (6 vertex)
               (dolist (v vertices) (vector-push-extend v map-vertices))
               (dotimes (i 6) (vector-push-extend normal map-normals))
               (dolist (c corners) (vector-push-extend (uv rec c) map-texcoords))))
      (dotimes (z height)
        (dotimes (x width)
          ;; Define the 8 vertex of the cube, to be combined accordingly later
          (let ((v1 (list (* w (- x 0.5)) h2 (* h (- z 0.5))))
                (v2 (list (* w (- x 0.5)) h2 (* h (+ z 0.5))))
                (v3 (list (* w (+ x 0.5)) h2 (* h (+ z 0.5))))
                (v4 (list (* w (+ x 0.5)) h2 (* h (- z 0.5))))
                (v5 (list (* w (+ x 0.5)) 0.0 (* h (- z 0.5))))
                (v6 (list (* w (- x 0.5)) 0.0 (* h (- z 0.5))))
                (v7 (list (* w (- x 0.5)) 0.0 (* h (+ z 0.5))))
                (v8 (list (* w (+ x 0.5)) 0.0 (* h (+ z 0.5))))
                (index (+ (* z width) x)))
            (cond
              ;; Check pixel color to be WHITE -> draw full cube
              ((pixel-is index +white+)
               ;; Define triangles and checking collateral cubes
               ;;------------------------------------------------

               ;; Define top triangles (2 tris, 6 vertex --> v1-v2-v3, v1-v3-v4)
               ;; WARNING: Not required for a WHITE cubes, created to allow seeing the map from outside
               (face (list v1 v2 v3 v1 v3 v4) n3 top-tex-uv '(:tl :bl :br :tl :br :tr))

               ;; Define bottom triangles (2 tris, 6 vertex --> v6-v8-v7, v6-v5-v8)
               (face (list v6 v8 v7 v6 v5 v8) n4 bottom-tex-uv '(:tr :bl :br :tr :tl :bl))

               ;; Checking cube on bottom of current cube
               (when (or (and (< z (- height 1)) (pixel-is (+ (* (+ z 1) width) x) +black+)) (= z (- height 1)))
                 ;; Define front triangles (2 tris, 6 vertex) --> v2 v7 v3, v3 v7 v8
                 ;; NOTE: Collateral occluded faces are not generated
                 (face (list v2 v7 v3 v3 v7 v8) n6 front-tex-uv '(:tl :bl :tr :tr :bl :br)))

               ;; Checking cube on top of current cube
               (when (or (and (> z 0) (pixel-is (+ (* (- z 1) width) x) +black+)) (= z 0))
                 ;; Define back triangles (2 tris, 6 vertex) --> v1 v5 v6, v1 v4 v5
                 ;; NOTE: Collateral occluded faces are not generated
                 (face (list v1 v5 v6 v1 v4 v5) n5 back-tex-uv '(:tr :bl :br :tr :tl :bl)))

               ;; Checking cube on right of current cube
               (when (or (and (< x (- width 1)) (pixel-is (+ (* z width) (+ x 1)) +black+)) (= x (- width 1)))
                 ;; Define right triangles (2 tris, 6 vertex) --> v3 v8 v4, v4 v8 v5
                 ;; NOTE: Collateral occluded faces are not generated
                 (face (list v3 v8 v4 v4 v8 v5) n1 right-tex-uv '(:tl :bl :tr :tr :bl :br)))

               ;; Checking cube on left of current cube
               (when (or (and (> x 0) (pixel-is (+ (* z width) (- x 1)) +black+)) (= x 0))
                 ;; Define left triangles (2 tris, 6 vertex) --> v1 v7 v2, v1 v6 v7
                 ;; NOTE: Collateral occluded faces are not generated
                 (face (list v1 v7 v2 v1 v6 v7) n2 left-tex-uv '(:tl :br :tr :tl :bl :br))))
              ;; Check pixel color to be BLACK, in that case only drawing floor and roof
              ((pixel-is index +black+)
               ;; Define top triangles (2 tris, 6 vertex --> v1-v2-v3, v1-v3-v4)
               (face (list v1 v3 v2 v1 v4 v3) n4 top-tex-uv '(:tl :br :bl :tl :tr :br))
               ;; Define bottom triangles (2 tris, 6 vertex --> v6-v8-v7, v6-v5-v8)
               (face (list v6 v7 v8 v6 v8 v5) n3 bottom-tex-uv '(:tr :br :bl :tr :bl :tl))))))))

    ;; Move data from mapVertices temp arrays to vertices float array
    (let* ((v-counter (length map-vertices))
           (mesh (make-mesh :vertex-count v-counter
                            :triangle-count (truncate v-counter 3)
                            :vertices (%floats (* v-counter 3))
                            :normals (%floats (* v-counter 3))
                            :texcoords (%floats (* v-counter 2))
                            :colors nil)))
      ;; Move vertices data
      (loop for v across map-vertices for f from 0 by 3
            do (replace (mesh-vertices mesh) (mapcar (lambda (c) (float c 1.0)) v) :start1 f))
      ;; Move normals data
      (loop for n across map-normals for f from 0 by 3
            do (replace (mesh-normals mesh) n :start1 f))
      ;; Move texcoords data
      (loop for uv across map-texcoords for f from 0 by 2
            do (replace (mesh-texcoords mesh) uv :start1 f))

      ;; Upload vertex data to GPU (static mesh)
      (upload-mesh mesh nil)
      mesh)))

;; Compute mesh bounding box limits
;; NOTE: minVertex and maxVertex should be transformed by model transform matrix
(defun get-mesh-bounding-box (mesh)
  "Compute mesh bounding box limits"
  ;; Get min and max vertex to construct bounds (AABB)
  (let ((min-vertex (vec3 0.0 0.0 0.0))
        (max-vertex (vec3 0.0 0.0 0.0))
        (vertices (mesh-vertices mesh)))
    (when vertices
      (setf min-vertex (vec3 (aref vertices 0) (aref vertices 1) (aref vertices 2))
            max-vertex (vec3 (aref vertices 0) (aref vertices 1) (aref vertices 2)))
      (loop for i from 1 below (mesh-vertex-count mesh)
            do (let ((v (vec3 (aref vertices (* i 3)) (aref vertices (+ (* i 3) 1)) (aref vertices (+ (* i 3) 2)))))
                 (setf min-vertex (vector3-min min-vertex v)
                       max-vertex (vector3-max max-vertex v)))))
    ;; Create the bounding box
    (make-bounding-box :min min-vertex :max max-vertex)))

;; Compute mesh tangents
(defun gen-mesh-tangents (mesh)
  "Compute mesh tangents"
  ;; Check if input mesh data is useful
  (when (or (null mesh) (null (mesh-vertices mesh)) (null (mesh-texcoords mesh)) (null (mesh-normals mesh)))
    (trace-log +log-warning+ "MESH: Tangents generation requires vertices, texcoords and normals vertex attribute data")
    (return-from gen-mesh-tangents))

  (let* ((vertex-count (mesh-vertex-count mesh))
         ;; Allocate or reallocate tangents data
         (tangents (%floats (* vertex-count 4)))
         ;; Allocate temporary arrays for tangents calculation
         (tan1 (make-array vertex-count :initial-element (vec3 0.0 0.0 0.0)))
         (tan2 (make-array vertex-count :initial-element (vec3 0.0 0.0 0.0)))
         (vertices (mesh-vertices mesh))
         (texcoords (mesh-texcoords mesh))
         (normals (mesh-normals mesh))
         (indices (mesh-indices mesh)))
    (setf (mesh-tangents mesh) tangents)

    ;; Process all triangles of the mesh
    ;; 'triangleCount' must be always valid
    (dotimes (tr (mesh-triangle-count mesh))
      ;; Get triangle vertex indices
      (let* ((i0 (if indices (aref indices (+ (* tr 3) 0)) (+ (* tr 3) 0)))
             (i1 (if indices (aref indices (+ (* tr 3) 1)) (+ (* tr 3) 1)))
             (i2 (if indices (aref indices (+ (* tr 3) 2)) (+ (* tr 3) 2)))
             ;; Calculate triangle edges
             (x1 (- (aref vertices (+ (* i1 3) 0)) (aref vertices (+ (* i0 3) 0))))
             (y1 (- (aref vertices (+ (* i1 3) 1)) (aref vertices (+ (* i0 3) 1))))
             (z1 (- (aref vertices (+ (* i1 3) 2)) (aref vertices (+ (* i0 3) 2))))
             (x2 (- (aref vertices (+ (* i2 3) 0)) (aref vertices (+ (* i0 3) 0))))
             (y2 (- (aref vertices (+ (* i2 3) 1)) (aref vertices (+ (* i0 3) 1))))
             (z2 (- (aref vertices (+ (* i2 3) 2)) (aref vertices (+ (* i0 3) 2))))
             ;; Calculate texture coordinate differences
             (s1 (- (aref texcoords (+ (* i1 2) 0)) (aref texcoords (+ (* i0 2) 0))))
             (t1 (- (aref texcoords (+ (* i1 2) 1)) (aref texcoords (+ (* i0 2) 1))))
             (s2 (- (aref texcoords (+ (* i2 2) 0)) (aref texcoords (+ (* i0 2) 0))))
             (t2 (- (aref texcoords (+ (* i2 2) 1)) (aref texcoords (+ (* i0 2) 1))))
             ;; Calculate denominator and check for degenerate UV
             (div (- (* s1 t2) (* s2 t1)))
             (r (if (< (abs div) 0.0001) 0.0 (/ 1.0 div)))
             ;; Calculate tangent and bitangent directions
             (sdir (vec3 (* (- (* t2 x1) (* t1 x2)) r) (* (- (* t2 y1) (* t1 y2)) r) (* (- (* t2 z1) (* t1 z2)) r)))
             (tdir (vec3 (* (- (* s1 x2) (* s2 x1)) r) (* (- (* s1 y2) (* s2 y1)) r) (* (- (* s1 z2) (* s2 z1)) r))))
        ;; Accumulate tangents and bitangents for each vertex of the triangle
        (setf (aref tan1 i0) (vector3-add (aref tan1 i0) sdir)
              (aref tan1 i1) (vector3-add (aref tan1 i1) sdir)
              (aref tan1 i2) (vector3-add (aref tan1 i2) sdir)
              (aref tan2 i0) (vector3-add (aref tan2 i0) tdir)
              (aref tan2 i1) (vector3-add (aref tan2 i1) tdir)
              (aref tan2 i2) (vector3-add (aref tan2 i2) tdir))))

    ;; Calculate final tangents for each vertex
    (dotimes (i vertex-count)
      (let ((normal (vec3 (aref normals (+ (* i 3) 0)) (aref normals (+ (* i 3) 1)) (aref normals (+ (* i 3) 2))))
            (tangent (aref tan1 i)))
        (flet ((perpendicular ()
                 ;; Create a tangent perpendicular to the normal
                 (if (> (abs (vz3 normal)) 0.707)
                     (vec3 1.0 0.0 0.0)
                     (vector3-normalize (vec3 (- (vy3 normal)) (vx3 normal) 0.0)))))
          ;; Handle zero tangent (can happen with degenerate UVs)
          (if (< (vector3-length tangent) 0.0001)
              (let ((tg (perpendicular)))
                (setf (aref tangents (+ (* i 4) 0)) (vx3 tg)
                      (aref tangents (+ (* i 4) 1)) (vy3 tg)
                      (aref tangents (+ (* i 4) 2)) (vz3 tg)
                      (aref tangents (+ (* i 4) 3)) 1.0))
              ;; Gram-Schmidt orthogonalization to make tangent orthogonal to normal
              ;; T_prime = T - N*dot(N, T)
              (let ((orthogonalized (vector3-subtract tangent (vector3-scale normal (vector3-dot-product normal tangent)))))
                ;; Handle cases where orthogonalized vector is too small
                (setf orthogonalized
                      (if (< (vector3-length orthogonalized) 0.0001)
                          (perpendicular)
                          ;; Normalize the orthogonalized tangent
                          (vector3-normalize orthogonalized)))
                ;; Store the calculated tangent
                (setf (aref tangents (+ (* i 4) 0)) (vx3 orthogonalized)
                      (aref tangents (+ (* i 4) 1)) (vy3 orthogonalized)
                      (aref tangents (+ (* i 4) 2)) (vz3 orthogonalized))
                ;; Calculate the handedness (w component)
                (setf (aref tangents (+ (* i 4) 3))
                      (if (< (vector3-dot-product (vector3-cross-product normal orthogonalized) (aref tan2 i)) 0.0) -1.0 1.0)))))))

    ;; Update vertex buffers if available
    (when (mesh-vbo-id mesh)
      (if (/= (aref (mesh-vbo-id mesh) +shader-loc-vertex-tangent+) 0)
          ;; Update existing tangent vertex buffer
          (rl-update-vertex-buffer (aref (mesh-vbo-id mesh) +shader-loc-vertex-tangent+) tangents (* vertex-count 4 4) 0)
          ;; Create new tangent vertex buffer
          (setf (aref (mesh-vbo-id mesh) +shader-loc-vertex-tangent+) (rl-load-vertex-buffer tangents (* vertex-count 4 4) nil)))
      ;; Set up vertex attributes for shader
      (rl-enable-vertex-array (mesh-vao-id mesh))
      (rl-set-vertex-attribute +rl-default-shader-attrib-location-tangent+ 4 +rl-float+ 0 0 0)
      (rl-enable-vertex-attribute +rl-default-shader-attrib-location-tangent+)
      (rl-disable-vertex-array))

    (trace-log +log-info+ "MESH: Tangents data computed and uploaded for provided mesh")))

;; Draw a model (with texture if set)
(defun draw-model (model position scale tint)
  "Draw a model (with texture if set)"
  (let ((scale (float scale 1.0)))
    (draw-model-ex model position (vec3 0.0 1.0 0.0) 0.0 (vec3 scale scale scale) tint)))

;; Draw a model with custom transform
(defun draw-model-ex (model position rotation-axis rotation-angle scale tint)
  "Draw a model with extended parameters"
  ;; Calculate transformation matrix from function parameters
  ;; Get transform matrix (rotation -> scale -> translation)
  (let* ((mat-scale (matrix-scale (%x scale) (%y scale) (%z scale)))
         (mat-rotation (matrix-rotate (%v3 rotation-axis) (* (float rotation-angle 1.0) +deg2rad+)))
         (mat-translation (matrix-translate (%x position) (%y position) (%z position)))
         (mat-transform (matrix-multiply (matrix-multiply mat-scale mat-rotation) mat-translation))
         ;; Combine model transformation matrix (model.transform) with matrix generated by function parameters (matTransform)
         (transform (matrix-multiply (model-transform model) mat-transform)))
    (destructuring-bind (tr tg tb ta) (%col tint)
      (dotimes (i (model-mesh-count model))
        (let* ((mat (aref (model-materials model) (aref (model-mesh-material model) i)))
               (diffuse-map (%material-map mat +material-map-diffuse+))
               (col-diffuse (material-map-color diffuse-map)))
          ;; Applying color tint directly to material diffuse map,
          ;; because it comes as an input parameter to the function
          (destructuring-bind (dr dg db da) (%col col-diffuse)
            (setf (material-map-color diffuse-map)
                  (list (logand (truncate (* dr tr) 255) #xff) (logand (truncate (* dg tg) 255) #xff)
                        (logand (truncate (* db tb) 255) #xff) (logand (truncate (* da ta) 255) #xff))))

          ;; Upload runtime bone transforms matrices, to compute skinning on the shader (GPU-skinning)
          ;; NOTE: Required location must be found and Mesh bones indices and weights must be also uploaded to shader
          (let ((locs (shader-locs (material-shader mat))))
            (when (and locs (/= (aref locs +shader-loc-matrix-bonetransforms+) -1) (model-bone-matrices model))
              (rl-enable-shader (shader-id (material-shader mat))) ; Enable shader to set bone transform matrices
              (rl-set-uniform-matrices (aref locs +shader-loc-matrix-bonetransforms+) (model-bone-matrices model)
                                       (model-skeleton-bone-count (model-skeleton model)))))

          (draw-mesh (aref (model-meshes model) i) mat transform)

          ;; Restore material diffuse map color (before tint applied)
          (setf (material-map-color diffuse-map) col-diffuse))))))

;; Draw a model wires (with texture if set)
(defun draw-model-wires (model position scale tint)
  "Draw a model wires (with texture if set)"
  (rl-enable-wire-mode)
  (draw-model model position scale tint)
  (rl-disable-wire-mode))

;; Draw a model wires with custom transform
(defun draw-model-wires-ex (model position rotation-axis rotation-angle scale tint)
  "Draw a model wires (with texture if set) with extended parameters"
  (rl-enable-wire-mode)
  (draw-model-ex model position rotation-axis rotation-angle scale tint)
  (rl-disable-wire-mode))

;; Draw a billboard
(defun draw-billboard (camera texture position scale tint)
  "Draw a billboard texture"
  (let* ((scale (float scale 1.0))
         (rec (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width texture) 1.0) :height (float (texture-height texture) 1.0))))
    (draw-billboard-rec camera texture rec position
                        (vec2 (* scale (abs (/ (rectangle-width rec) (rectangle-height rec)))) scale) tint)))

;; Draw a billboard (part of a texture defined by a rectangle)
(defun draw-billboard-rec (camera texture rec position size tint)
  "Draw a billboard texture defined by source"
  ;; NOTE: Billboard locked on axis-Y
  (let ((up (vec3 0.0 1.0 0.0))
        (size (vec2 (%x size) (%y size))))
    (draw-billboard-pro camera texture rec position up size (vector2-scale size 0.5) 0.0 tint)))

;; Draw a billboard texture defined by source rectangle with scaling and rotation
(defun draw-billboard-pro (camera texture rec position up size origin rotation tint)
  "Draw a billboard texture defined by source and rotation"
  (multiple-value-bind (rx ry rw rh) (%rec rec)
    (let* ((rotation (float rotation 1.0))
           (size-x (%x size)) (size-y (%y size))
           (origin-x (%x origin)) (origin-y (%y origin))
           ;; Compute the up vector and the right vector
           (mat-view (matrix-look-at (camera3d-position camera) (camera3d-target camera) (camera3d-up camera)))
           (right (vector3-scale (vec3 (%m mat-view 0) (%m mat-view 4) (%m mat-view 8)) size-x))
           (up (vector3-scale (%v3 up) size-y))
           (position (%v3 position)))
      ;; Flip the content of the billboard while maintaining the counterclockwise edge rendering order
      (when (< size-x 0.0)
        (setf rx (- rx size-x)
              rw (* rw -1.0)
              right (vector3-negate right)
              origin-x (* origin-x -1.0)))
      (when (< size-y 0.0)
        (setf ry (- ry size-y)
              rh (* rh -1.0)
              up (vector3-negate up)
              origin-y (* origin-y -1.0)))

      ;; Draw the texture region described by rec on the following rectangle in 3D space:
      ;;
      ;;                size.x          <--.
      ;;  3 ^---------------------------+ 2 \ rotation
      ;;    |                           |   /
      ;;    |                           |
      ;;    |   origin.x   position     |
      ;; up |..............             | size.y
      ;;    |             .             |
      ;;    |             . origin.y    |
      ;;    |             .             |
      ;;  0 +---------------------------> 1
      ;;                right
      (let* ((forward (when (/= rotation 0.0) (vector3-cross-product right up)))
             (origin-3d (vector3-add (vector3-scale (vector3-normalize right) origin-x)
                                     (vector3-scale (vector3-normalize up) origin-y)))
             (points (vector (vector3-zero) right (vector3-add up right) up))
             (tw (texture-width texture))
             (th (texture-height texture))
             (texcoords (vector (list (/ rx tw) (/ (+ ry rh) th))
                                (list (/ (+ rx rw) tw) (/ (+ ry rh) th))
                                (list (/ (+ rx rw) tw) (/ ry th))
                                (list (/ rx tw) (/ ry th)))))
        (dotimes (i 4)
          (setf (svref points i) (vector3-subtract (svref points i) origin-3d))
          (when (/= rotation 0.0)
            (setf (svref points i) (vector3-rotate-by-axis-angle (svref points i) forward (* rotation +deg2rad+))))
          (setf (svref points i) (vector3-add (svref points i) position)))

        (rl-set-texture (texture-id texture))
        (rl-begin +rl-quads+)
        (%color tint)
        (dotimes (i 4)
          (rl-tex-coord2f (first (svref texcoords i)) (second (svref texcoords i)))
          (%vertex-v3 (svref points i)))
        (rl-end)
        (rl-set-texture 0)))))

;; Draw a bounding box with wires
(defun draw-bounding-box (box color)
  "Draw bounding box (wires)"
  (let* ((bmin (bounding-box-min box))
         (bmax (bounding-box-max box))
         (size-x (abs (- (vx3 bmax) (vx3 bmin))))
         (size-y (abs (- (vy3 bmax) (vy3 bmin))))
         (size-z (abs (- (vz3 bmax) (vz3 bmin))))
         (center (vec3 (+ (vx3 bmin) (/ size-x 2.0)) (+ (vy3 bmin) (/ size-y 2.0)) (+ (vz3 bmin) (/ size-z 2.0)))))
    (draw-cube-wires center size-x size-y size-z color)))

;;;----------------------------------------------------------------------------------
;;; Collision detection functions
;;;----------------------------------------------------------------------------------

;; Check collision between two spheres
(defun check-collision-spheres (center1 radius1 center2 radius2)
  "Check collision between two spheres"
  ;; Check for distances squared to avoid sqrtf()
  (let ((rad-sum (+ (float radius1 1.0) (float radius2 1.0))))
    (<= (vector3-distance-sqr (%v3 center1) (%v3 center2)) (* rad-sum rad-sum))))

;; Check collision between two boxes
;; NOTE: Boxes are defined by two points minimum and maximum
(defun check-collision-boxes (box1 box2)
  "Check collision between two bounding boxes"
  (let ((min1 (bounding-box-min box1)) (max1 (bounding-box-max box1))
        (min2 (bounding-box-min box2)) (max2 (bounding-box-max box2))
        (collision t))
    (if (and (>= (vx3 max1) (vx3 min2)) (<= (vx3 min1) (vx3 max2)))
        (progn
          (when (or (< (vy3 max1) (vy3 min2)) (> (vy3 min1) (vy3 max2))) (setf collision nil))
          (when (or (< (vz3 max1) (vz3 min2)) (> (vz3 min1) (vz3 max2))) (setf collision nil)))
        (setf collision nil))
    collision))

;; Check collision between box and sphere
(defun check-collision-box-sphere (box center radius)
  "Check collision between box and sphere"
  (let* ((radius (float radius 1.0))
         (bmin (bounding-box-min box)) (bmax (bounding-box-max box))
         (closest-point (vec3 (clamp (%x center) (vx3 bmin) (vx3 bmax))
                              (clamp (%y center) (vy3 bmin) (vy3 bmax))
                              (clamp (%z center) (vz3 bmin) (vz3 bmax)))))
    (<= (vector3-distance-sqr (%v3 center) closest-point) (* radius radius))))

;; Get collision info between ray and sphere
(defun get-ray-collision-sphere (ray center radius)
  "Get collision info between ray and sphere"
  (float-features:with-float-traps-masked t
    (let* ((collision (make-ray-collision))
           (radius (float radius 1.0))
           (center (%v3 center))
           (ray-position (%v3 (ray-position ray)))
           (ray-direction (%v3 (ray-direction ray)))
           (ray-sphere-pos (vector3-subtract center ray-position))
           (vector (vector3-dot-product ray-sphere-pos ray-direction))
           (distance (vector3-length ray-sphere-pos))
           (d (- (* radius radius) (- (* distance distance) (* vector vector)))))
      (setf (ray-collision-hit collision) (>= d 0.0))
      ;; Check if ray origin is inside the sphere to calculate the correct collision point
      (if (< distance radius)
          (progn
            (setf (ray-collision-distance collision) (+ vector (%sqrtf d)))
            ;; Calculate collision point
            (setf (ray-collision-point collision)
                  (vector3-add ray-position (vector3-scale ray-direction (ray-collision-distance collision))))
            ;; Calculate collision normal (pointing outwards)
            (setf (ray-collision-normal collision)
                  (vector3-negate (vector3-normalize (vector3-subtract (ray-collision-point collision) center)))))
          (progn
            (setf (ray-collision-distance collision) (- vector (%sqrtf d)))
            ;; Calculate collision point
            (setf (ray-collision-point collision)
                  (vector3-add ray-position (vector3-scale ray-direction (ray-collision-distance collision))))
            ;; Calculate collision normal (pointing inwards)
            (setf (ray-collision-normal collision)
                  (vector3-normalize (vector3-subtract (ray-collision-point collision) center)))))
      collision)))

(defun %fmin (a b)
  "C fmin(): NaN arguments are ignored"
  (cond ((float-features:float-nan-p a) b)
        ((float-features:float-nan-p b) a)
        (t (min a b))))

(defun %fmax (a b)
  "C fmax(): NaN arguments are ignored"
  (cond ((float-features:float-nan-p a) b)
        ((float-features:float-nan-p b) a)
        (t (max a b))))

(defun %c-float-to-int (x)
  "C (int) conversion of a float (truncation, 0x80000000 for NaN/out of range as on x86-64)"
  (if (or (float-features:float-nan-p x) (float-features:float-infinity-p x) (>= (abs x) 2147483648.0))
      -2147483648
      (truncate x)))

;; Get collision info between ray and box
(defun get-ray-collision-box (ray box)
  "Get collision info between ray and box"
  (float-features:with-float-traps-masked t
    (let* ((collision (make-ray-collision))
           (position (%v3 (ray-position ray)))
           (direction (%v3 (ray-direction ray)))
           (bmin (bounding-box-min box))
           (bmax (bounding-box-max box))
           ;; NOTE: If ray.position is inside the box, the distance is negative (as if the ray was reversed)
           ;; Reversing ray.direction will give use the correct result
           (inside-box (and (> (vx3 position) (vx3 bmin)) (< (vx3 position) (vx3 bmax))
                            (> (vy3 position) (vy3 bmin)) (< (vy3 position) (vy3 bmax))
                            (> (vz3 position) (vz3 bmin)) (< (vz3 position) (vz3 bmax))))
           (tt (make-array 11 :element-type 'single-float :initial-element 0.0)))
      (when inside-box (setf direction (vector3-negate direction)))

      (setf (aref tt 8) (/ 1.0 (vx3 direction))
            (aref tt 9) (/ 1.0 (vy3 direction))
            (aref tt 10) (/ 1.0 (vz3 direction)))

      (setf (aref tt 0) (* (- (vx3 bmin) (vx3 position)) (aref tt 8))
            (aref tt 1) (* (- (vx3 bmax) (vx3 position)) (aref tt 8))
            (aref tt 2) (* (- (vy3 bmin) (vy3 position)) (aref tt 9))
            (aref tt 3) (* (- (vy3 bmax) (vy3 position)) (aref tt 9))
            (aref tt 4) (* (- (vz3 bmin) (vz3 position)) (aref tt 10))
            (aref tt 5) (* (- (vz3 bmax) (vz3 position)) (aref tt 10)))
      (setf (aref tt 6) (%fmax (%fmax (%fmin (aref tt 0) (aref tt 1)) (%fmin (aref tt 2) (aref tt 3))) (%fmin (aref tt 4) (aref tt 5)))
            (aref tt 7) (%fmin (%fmin (%fmax (aref tt 0) (aref tt 1)) (%fmax (aref tt 2) (aref tt 3))) (%fmax (aref tt 4) (aref tt 5))))

      (setf (ray-collision-hit collision) (not (or (< (aref tt 7) 0) (> (aref tt 6) (aref tt 7))))
            (ray-collision-distance collision) (aref tt 6)
            (ray-collision-point collision) (vector3-add position (vector3-scale direction (aref tt 6))))

      ;; Get box center point
      (let ((normal (vector3-lerp bmin bmax 0.5)))
        ;; Get vector center point->hit point
        (setf normal (vector3-subtract (ray-collision-point collision) normal))
        ;; Scale vector to unit cube
        ;; NOTE: Use an additional .01 to fix numerical errors
        (setf normal (vector3-scale normal 2.01))
        (setf normal (vector3-divide normal (vector3-subtract bmax bmin)))
        ;; The relevant elements of the vector are now slightly larger than 1.0f (or smaller than -1.0f)
        ;; and the others are somewhere between -1.0 and 1.0 casting to int is exactly our wanted normal!
        (setf normal (vec3 (float (%c-float-to-int (vx3 normal)) 1.0)
                           (float (%c-float-to-int (vy3 normal)) 1.0)
                           (float (%c-float-to-int (vz3 normal)) 1.0)))
        (setf (ray-collision-normal collision) (vector3-normalize normal)))

      (when inside-box
        ;; Fix result
        (setf (ray-collision-distance collision) (* (ray-collision-distance collision) -1.0)
              (ray-collision-normal collision) (vector3-negate (ray-collision-normal collision))))
      collision)))

;; Get collision info between ray and mesh
(defun get-ray-collision-mesh (ray mesh transform)
  "Get collision info between ray and mesh"
  (let ((collision (make-ray-collision))
        (vertdata (mesh-vertices mesh)))
    ;; Check if mesh vertex data on CPU for testing
    (when vertdata
      ;; Test against all triangles in mesh
      (dotimes (i (mesh-triangle-count mesh))
        (flet ((vertex (k)
                 (vec3 (aref vertdata (* k 3)) (aref vertdata (+ (* k 3) 1)) (aref vertdata (+ (* k 3) 2)))))
          (let* ((indices (mesh-indices mesh))
                 (a (vertex (if indices (aref indices (+ (* i 3) 0)) (+ (* i 3) 0))))
                 (b (vertex (if indices (aref indices (+ (* i 3) 1)) (+ (* i 3) 1))))
                 (c (vertex (if indices (aref indices (+ (* i 3) 2)) (+ (* i 3) 2))))
                 (tri-hit-info (get-ray-collision-triangle ray
                                                           (vector3-transform a transform)
                                                           (vector3-transform b transform)
                                                           (vector3-transform c transform))))
            (when (ray-collision-hit tri-hit-info)
              ;; Save the closest hit triangle
              (when (or (not (ray-collision-hit collision))
                        (> (ray-collision-distance collision) (ray-collision-distance tri-hit-info)))
                (setf collision tri-hit-info)))))))
    collision))

;; Get collision info between ray and triangle
;; NOTE: The points are expected to be in counter-clockwise winding
;; NOTE: Based on https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
(defun get-ray-collision-triangle (ray p1 p2 p3)
  "Get collision info between ray and triangle"
  (let* ((epsilon 0.000001)             ; A small number
         (collision (make-ray-collision))
         (p1 (%v3 p1)) (p2 (%v3 p2)) (p3 (%v3 p3))
         (ray-position (%v3 (ray-position ray)))
         (ray-direction (%v3 (ray-direction ray)))
         ;; Find vectors for two edges sharing V1
         (edge1 (vector3-subtract p2 p1))
         (edge2 (vector3-subtract p3 p1))
         ;; Begin calculating determinant - also used to calculate u parameter
         (p (vector3-cross-product ray-direction edge2))
         ;; If determinant is near zero, ray lies in plane of triangle or ray is parallel to plane of triangle
         (det (vector3-dot-product edge1 p)))
    ;; Avoid culling!
    (when (and (> det (- epsilon)) (< det epsilon)) (return-from get-ray-collision-triangle collision))

    (let* ((inv-det (/ 1.0 det))
           ;; Calculate distance from V1 to ray origin
           (tv (vector3-subtract ray-position p1))
           ;; Calculate u parameter and test bound
           (u (* (vector3-dot-product tv p) inv-det)))
      ;; The intersection lies outside the triangle
      (when (or (< u 0.0) (> u 1.0)) (return-from get-ray-collision-triangle collision))

      ;; Prepare to test v parameter
      (let* ((q (vector3-cross-product tv edge1))
             ;; Calculate V parameter and test bound
             (v (* (vector3-dot-product ray-direction q) inv-det)))
        ;; The intersection lies outside the triangle
        (when (or (< v 0.0) (> (+ u v) 1.0)) (return-from get-ray-collision-triangle collision))

        (let ((tt (* (vector3-dot-product edge2 q) inv-det)))
          (when (> tt epsilon)
            ;; Ray hit, get hit point and normal
            (setf (ray-collision-hit collision) t
                  (ray-collision-distance collision) tt
                  (ray-collision-normal collision) (vector3-normalize (vector3-cross-product edge1 edge2))
                  (ray-collision-point collision) (vector3-add ray-position (vector3-scale ray-direction tt)))))))
    collision))

;; Get collision info between ray and quad
;; NOTE: The points are expected to be in counter-clockwise winding
(defun get-ray-collision-quad (ray p1 p2 p3 p4)
  "Get collision info between ray and quad"
  (let ((collision (get-ray-collision-triangle ray p1 p2 p4)))
    (unless (ray-collision-hit collision)
      (setf collision (get-ray-collision-triangle ray p2 p3 p4)))
    collision))

;;;----------------------------------------------------------------------------------
;;; Module Internal Functions Definition
;;;----------------------------------------------------------------------------------

;; Build pose from parent joints
;; NOTE: Required for animations loading (required by IQM and GLTF)
(defun %build-pose-from-parent-joints (bones bone-count transforms)
  (dotimes (i bone-count)
    (let ((parent (bone-info-parent (aref bones i))))
      (when (>= parent 0)
        (if (> parent i)
            (trace-log +log-warning+ "Skipping bone not topologically sorted: Bone ~d has parent ~d" i parent)
            (let ((tr (aref transforms i))
                  (pt (aref transforms parent)))
              (setf (transform-rotation tr) (quaternion-multiply (transform-rotation pt) (transform-rotation tr))
                    (transform-scale tr) (vector3-multiply (transform-scale tr) (transform-scale pt))
                    (transform-translation tr) (vector3-multiply (transform-translation tr) (transform-scale pt))
                    (transform-translation tr) (vector3-rotate-by-quaternion (transform-translation tr) (transform-rotation pt))
                    (transform-translation tr) (vector3-add (transform-translation tr) (transform-translation pt)))))))))

;;;----------------------------------------------------------------------------------
;;; Model file formats loading
;;;----------------------------------------------------------------------------------

(defun %chdir (dir)
  "CHDIR(dir), returns 0 on success"
  (handler-case
      (let ((path (uiop:ensure-directory-pathname (truename dir))))
        (uiop:chdir path)
        (setf *default-pathname-defaults* path)
        0)
    (error () -1)))

(defun %load-file-text-bytes (file-name)
  "LoadFileText() as an octet vector (up to the first NUL), NIL on failure"
  (multiple-value-bind (data size) (load-file-data file-name)
    (when (and data (> size 0))
      (subseq data 0 (or (position 0 data) size)))))

;; Load OBJ mesh data
;; Notes to keep in mind:
;;  - A mesh is created for every material present in the obj file
;;  - The model.meshCount is therefore the materialCount returned from tinyobj
;;  - The mesh is automatically triangulated by tinyobj
(defun %load-obj (file-name)
  (let ((model (make-model :transform (matrix-identity)))
        (file-text (%load-file-text-bytes file-name)))
    (unless file-text
      (trace-log +log-warning+ "MODEL: [~a] Unable to read obj file" file-name)
      (return-from %load-obj model))

    (let ((current-dir (get-working-directory)) ; Save current working directory
          (working-dir (get-directory-path file-name))) ; Switch to OBJ directory for material path correctness
      (when (/= (%chdir working-dir) 0)
        (trace-log +log-warning+ "MODEL: [~a] Failed to change working directory" working-dir))

      (multiple-value-bind (obj-attributes obj-shapes obj-materials ret)
          (tinyobj-parse-obj file-text (length file-text) +tinyobj-flag-triangulate+)
        (unless (eq ret :success)
          (trace-log +log-warning+ "MODEL: Unable to read obj data ~a" file-name)
          (return-from %load-obj model))

        (let* ((obj-shape-count (length obj-shapes))
               (obj-material-count (length obj-materials))
               (num-faces (tobja-num-faces obj-attributes))
               (material-ids (tobja-material-ids obj-attributes))
               (face-num-verts (tobja-face-num-verts obj-attributes))
               (next-shape 1)
               (last-material -1)
               (mesh-index 0)
               ;; Count meshes
               (next-shape-end (tobja-num-face-num-verts obj-attributes)))
          (flet ((reset-walk ()
                   (setf next-shape 1
                         last-material -1
                         mesh-index 0
                         next-shape-end (tobja-num-face-num-verts obj-attributes))
                   ;; See how many verts till the next shape
                   (when (> obj-shape-count 1) (setf next-shape-end (tobjs-face-offset (aref obj-shapes next-shape)))))
                 (next-shape-p (face-id)
                   ;; Try to find the last vert in the next shape
                   (when (>= face-id next-shape-end)
                     (incf next-shape)
                     (setf next-shape-end (if (< next-shape obj-shape-count)
                                              (tobjs-face-offset (aref obj-shapes next-shape))
                                              ;; This is actually the total number of face verts in the file, not faces
                                              (tobja-num-face-num-verts obj-attributes)))
                     t)))
            (reset-walk)
            ;; Walk all the faces
            (dotimes (face-id num-faces)
              (cond ((next-shape-p face-id) (incf mesh-index))
                    ;; If this is a new material, a new mesh is allocated
                    ((and (/= last-material -1) (/= (aref material-ids face-id) last-material)) (incf mesh-index)))
              (setf last-material (aref material-ids face-id)))

            ;; Allocate the base meshes and materials
            (setf (model-mesh-count model) (+ mesh-index 1)
                  (model-meshes model) (let ((v (make-array (model-mesh-count model))))
                                         (dotimes (i (length v) v) (setf (aref v i) (make-mesh)))))
            (if (> obj-material-count 0)
                (setf (model-material-count model) obj-material-count
                      (model-materials model) (make-array obj-material-count))
                ;; Allocate at least one material
                (setf (model-material-count model) 1
                      (model-materials model) (make-array 1)))
            (setf (model-mesh-material model) (make-array (model-mesh-count model) :initial-element 0))

            ;; See how many verts are in each mesh
            (let ((local-mesh-vertex-counts (make-array (model-mesh-count model) :initial-element 0))
                  (local-mesh-vertex-count 0))
              (reset-walk)
              ;; Walk all the faces
              (dotimes (face-id num-faces)
                (let ((new-mesh nil))   ; Is a new mesh required?
                  (cond ((next-shape-p face-id) (setf new-mesh t))
                        ((and (/= last-material -1) (/= (aref material-ids face-id) last-material)) (setf new-mesh t)))
                  (setf last-material (aref material-ids face-id))
                  (when new-mesh
                    (setf (aref local-mesh-vertex-counts mesh-index) local-mesh-vertex-count
                          local-mesh-vertex-count 0)
                    (incf mesh-index))
                  (incf local-mesh-vertex-count (aref face-num-verts face-id))))
              (setf (aref local-mesh-vertex-counts mesh-index) local-mesh-vertex-count)

              (dotimes (i (model-mesh-count model))
                ;; Allocate the buffers for each mesh
                (let ((vertex-count (aref local-mesh-vertex-counts i))
                      (mesh (aref (model-meshes model) i)))
                  (setf (mesh-vertex-count mesh) vertex-count
                        (mesh-triangle-count mesh) (truncate vertex-count 3)
                        (mesh-vertices mesh) (%floats (* vertex-count 3))
                        (mesh-normals mesh) (%floats (* vertex-count 3))
                        (mesh-texcoords mesh) (%floats (* vertex-count 2))
                        (mesh-colors mesh) (make-array (* vertex-count 4) :element-type '(unsigned-byte 8) :initial-element 0)))))

            ;; Fill meshes
            (let ((face-vert-index 0)
                  (local-mesh-vertex-count 0)
                  (faces (tobja-faces obj-attributes))
                  (vertices (tobja-vertices obj-attributes))
                  (normals (tobja-normals obj-attributes))
                  (texcoords (tobja-texcoords obj-attributes)))
              (reset-walk)
              ;; Walk all the faces
              (dotimes (face-id num-faces)
                (let ((new-mesh (next-shape-p face-id))) ; Is a new mesh required?
                  ;; If this is a new material, a new mesh is allocated
                  (when (and (/= last-material -1) (/= (aref material-ids face-id) last-material)) (setf new-mesh t))
                  (setf last-material (aref material-ids face-id))
                  (when new-mesh
                    (setf local-mesh-vertex-count 0)
                    (incf mesh-index))
                  (let ((mat-id (if (and (>= last-material 0) (< last-material obj-material-count)) last-material 0))
                        (mesh (aref (model-meshes model) mesh-index)))
                    (setf (aref (model-mesh-material model) mesh-index) mat-id)
                    (dotimes (f (aref face-num-verts face-id))
                      (destructuring-bind (vert-index texcord-index normal-index) (svref faces face-vert-index)
                        ;; NOTE: Out-of-range indices from malformed files are skipped, keeping zeroed values
                        (when (and (>= vert-index 0) (< vert-index (tobja-num-vertices obj-attributes)))
                          (dotimes (i 3)
                            (setf (aref (mesh-vertices mesh) (+ (* local-mesh-vertex-count 3) i)) (aref vertices (+ (* vert-index 3) i)))))
                        (when (and (> (tobja-num-texcoords obj-attributes) 0) (/= texcord-index +tinyobj-invalid-index+)
                                   (>= texcord-index 0) (< texcord-index (tobja-num-texcoords obj-attributes)))
                          (dotimes (i 2)
                            (setf (aref (mesh-texcoords mesh) (+ (* local-mesh-vertex-count 2) i)) (aref texcoords (+ (* texcord-index 2) i))))
                          (setf (aref (mesh-texcoords mesh) (+ (* local-mesh-vertex-count 2) 1))
                                (- 1.0 (aref (mesh-texcoords mesh) (+ (* local-mesh-vertex-count 2) 1)))))
                        (if (and (> (tobja-num-normals obj-attributes) 0) (/= normal-index +tinyobj-invalid-index+)
                                 (>= normal-index 0) (< normal-index (tobja-num-normals obj-attributes)))
                            (dotimes (i 3)
                              (setf (aref (mesh-normals mesh) (+ (* local-mesh-vertex-count 3) i)) (aref normals (+ (* normal-index 3) i))))
                            (setf (aref (mesh-normals mesh) (+ (* local-mesh-vertex-count 3) 0)) 0.0
                                  (aref (mesh-normals mesh) (+ (* local-mesh-vertex-count 3) 1)) 1.0
                                  (aref (mesh-normals mesh) (+ (* local-mesh-vertex-count 3) 2)) 0.0))
                        (dotimes (i 4) (setf (aref (mesh-colors mesh) (+ (* local-mesh-vertex-count 4) i)) 255))
                        (incf face-vert-index)
                        (incf local-mesh-vertex-count)))))))

            (if (> obj-material-count 0)
                (%process-materials-obj (model-materials model) obj-materials obj-material-count)
                (setf (aref (model-materials model) 0) (load-material-default)))))) ; Set default material for the mesh

      ;; Restore current working directory
      (when (/= (%chdir current-dir) 0)
        (trace-log +log-warning+ "MODEL: [~a] Failed to change working directory" current-dir)))
    model))
;; Load VOX (MagicaVoxel) mesh data
(defun %load-vox (file-name)
  (let ((model (make-model))
        (nbvertices 0)
        (meshescount 0))
    ;; Read vox file into buffer
    (multiple-value-bind (file-data data-size) (load-file-data file-name)
      (unless file-data
        (trace-log +log-warning+ "MODEL: [~a] Failed to load VOX file" file-name)
        (return-from %load-vox model))

      ;; Read and build voxarray description
      (let* ((voxarray (make-vox-array-3d))
             (ret (vox-load-from-memory file-data data-size voxarray)))
        (if (/= ret +vox-success+)
            (progn
              ;; Error
              (trace-log +log-warning+ "MODEL: [~a] Failed to load VOX data" file-name)
              (return-from %load-vox model))
            (progn
              ;; Success: Compute meshes count
              (setf nbvertices (truncate (length (voxa-vertices voxarray)) 3)
                    meshescount (+ 1 (truncate nbvertices 65536)))
              (trace-log +log-info+ "MODEL: [~a] VOX data loaded successfully : ~d vertices/~d meshes" file-name nbvertices meshescount)))

        ;; Build models from meshes
        (setf (model-transform model) (matrix-identity)
              (model-mesh-count model) meshescount
              (model-meshes model) (let ((v (make-array meshescount))) (dotimes (i meshescount v) (setf (aref v i) (make-mesh))))
              (model-mesh-material model) (make-array meshescount :initial-element 0)
              (model-material-count model) 1
              (model-materials model) (vector (load-material-default)))

        ;; Init model meshes
        (let ((vertices-remain nbvertices)
              (vertices-max 65532)      ; 5461 voxels x 12 vertices per voxel -> 65532 (must be inf 65536)
              ;; 6*4 = 12 vertices per voxel
              (first-vertex 0)
              (all-indices (voxa-indices voxarray)))
          (dotimes (i meshescount)
            (let* ((mesh (aref (model-meshes model) i))
                   (vertex-count (min vertices-max vertices-remain)))
              ;; Copy vertices
              (setf (mesh-vertex-count mesh) vertex-count
                    (mesh-vertices mesh) (subseq (voxa-vertices voxarray) (* 3 first-vertex) (* 3 (+ first-vertex vertex-count)))
                    ;; Copy normals
                    (mesh-normals mesh) (subseq (voxa-normals voxarray) (* 3 first-vertex) (* 3 (+ first-vertex vertex-count)))
                    ;; Copy indices
                    ;; NOTE: All the voxarray indices are copied to every mesh, like C
                    (mesh-indices mesh) (coerce all-indices '(simple-array (unsigned-byte 16) (*)))
                    (mesh-triangle-count mesh) (* (truncate vertex-count 4) 2)
                    ;; Copy colors
                    (mesh-colors mesh) (subseq (voxa-colors voxarray) (* 4 first-vertex) (* 4 (+ first-vertex vertex-count))))
              ;; First material index
              (setf (aref (model-mesh-material model) i) 0)
              (decf vertices-remain vertices-max)
              (incf first-vertex vertices-max))))))
    model))

;;; IQM file data readers (little endian, 0 past the end of the data like a zeroed buffer)
(defun %iqm-u8 (data offset)
  (if (< -1 offset (length data)) (aref data offset) 0))
(defun %iqm-u16 (data offset)
  (logior (%iqm-u8 data offset) (ash (%iqm-u8 data (+ offset 1)) 8)))
(defun %iqm-u32 (data offset)
  (logior (%iqm-u16 data offset) (ash (%iqm-u16 data (+ offset 2)) 16)))
(defun %iqm-s32 (data offset)
  (%i32 (%iqm-u32 data offset)))
(defun %iqm-f32 (data offset)
  (ieee-floats:decode-float32 (%iqm-u32 data offset)))
(defun %iqm-name (data offset &optional (length 32))
  "char[LENGTH] copied from DATA as a string (up to the first NUL)"
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length) (setf (aref bytes i) (%iqm-u8 data (+ offset i))))
    (babel:octets-to-string bytes :end (or (position 0 bytes) length) :encoding :utf-8 :errorp nil)))

(defconstant +iqm-version+ 2 "Only IQM version 2 supported")
(defparameter +iqm-magic+ "INTERQUAKEMODEL" "IQM file magic number")

;; IQM header fields (unsigned int) offsets
(defmacro %iqm-header (data field)
  (let ((index (position field '(version data-size flags num-text ofs-text num-meshes ofs-meshes
                                 num-vertexarrays num-vertexes ofs-vertexarrays num-triangles ofs-triangles ofs-adjacency
                                 num-joints ofs-joints num-poses ofs-poses num-anims ofs-anims
                                 num-frames num-framechannels ofs-frames ofs-bounds num-comment ofs-comment
                                 num-extensions ofs-extensions))))
    `(%iqm-u32 ,data ,(+ 16 (* 4 index)))))

(defun %iqm-check-header (data file-name)
  "Check IQM magic and version, returns T if valid"
  (cond ((not (and (>= (length data) 16)
                   ;; memcmp(magic, IQM_MAGIC, sizeof(IQM_MAGIC)) (16 bytes including NUL)
                   (loop for i below 16
                         always (= (aref data i) (if (< i 15) (char-code (char +iqm-magic+ i)) 0)))))
         (trace-log +log-warning+ "MODEL: [~a] IQM file is not a valid model" file-name)
         nil)
        ((/= (%iqm-header data version) +iqm-version+)
         (trace-log +log-warning+ "MODEL: [~a] IQM file version not supported (~d)" file-name (%iqm-header data version))
         nil)
        (t t)))

;; Load IQM mesh data
(defun %load-iqm (file-name)
  (let ((model (make-model))
        (file-data (load-file-data file-name)))
    ;; In case file can not be read, return an empty model
    (unless file-data (return-from %load-iqm model))

    (let ((base-path (get-directory-path file-name)))
      ;; Read IQM header
      (unless (%iqm-check-header file-data file-name) (return-from %load-iqm model))

      (let* ((d file-data)
             (num-meshes (%iqm-header d num-meshes))
             (ofs-meshes (%iqm-header d ofs-meshes))
             (ofs-text (%iqm-header d ofs-text))
             (num-vertexes (%iqm-header d num-vertexes))
             ;; Meshes data processing: IQMMesh { name, material, first_vertex, num_vertexes, first_triangle, num_triangles }
             (imesh (coerce (loop for i below num-meshes
                                  collect (loop for k below 6 collect (%iqm-u32 d (+ ofs-meshes (* i 24) (* k 4)))))
                            'vector)))
        (setf (model-mesh-count model) num-meshes
              (model-meshes model) (let ((v (make-array num-meshes))) (dotimes (i num-meshes v) (setf (aref v i) (make-mesh))))
              (model-material-count model) num-meshes
              (model-materials model) (make-array num-meshes)
              (model-mesh-material model) (make-array num-meshes :initial-element 0))

        (dotimes (i num-meshes)
          (destructuring-bind (name material first-vertex num-vert first-triangle num-triangles) (aref imesh i)
            (declare (ignore first-vertex first-triangle))
            (let ((name (%iqm-name d (+ ofs-text name)))
                  (material (%iqm-name d (+ ofs-text material)))
                  (mesh (aref (model-meshes model) i)))
              (setf (aref (model-materials model) i) (load-material-default))
              (when (> (length material) 0)
                (setf (material-map-texture (%material-map (aref (model-materials model) i) +material-map-albedo+))
                      (load-texture (format nil "~a/~a" base-path material))))

              (setf (aref (model-mesh-material model) i) i)

              (trace-log +log-debug+ "MODEL: [~a] mesh name (~a), material (~a)" file-name name material)

              (setf (mesh-vertex-count mesh) num-vert
                    (mesh-vertices mesh) (%floats (* num-vert 3))       ; Default vertex positions
                    (mesh-normals mesh) (%floats (* num-vert 3))        ; Default vertex normals
                    (mesh-texcoords mesh) (%floats (* num-vert 2))      ; Default vertex texcoords
                    (mesh-bone-indices mesh) (make-array (* num-vert 4) :element-type '(unsigned-byte 8) :initial-element 0) ; Up-to 4 bones supported!
                    (mesh-bone-weights mesh) (%floats (* num-vert 4))   ; Up-to 4 bones supported!
                    (mesh-triangle-count mesh) num-triangles
                    (mesh-indices mesh) (make-array (* num-triangles 3) :element-type '(unsigned-byte 16) :initial-element 0)
                    ;; Animated vertex data, processed for rendering
                    ;; NOTE: Animated vertex should be re-uploaded to GPU (if not using GPU skinning)
                    (mesh-anim-vertices mesh) (%floats (* num-vert 3))
                    (mesh-anim-normals mesh) (%floats (* num-vert 3))))))

        ;; Triangles data processing
        (let ((ofs-triangles (%iqm-header d ofs-triangles)))
          (dotimes (m num-meshes)
            (destructuring-bind (name material first-vertex num-vert first-triangle num-triangles) (aref imesh m)
              (declare (ignore name material num-vert))
              (let ((tcounter 0)
                    (indices (mesh-indices (aref (model-meshes model) m))))
                (loop for i from first-triangle below (+ first-triangle num-triangles)
                      do (flet ((vertex (k) (logand (- (%iqm-u32 d (+ ofs-triangles (* i 12) (* k 4))) first-vertex) #xffff)))
                           ;; IQM triangles indexes are stored in counter-clockwise, but raylib processes the index in linear order,
                           ;; expecting they point to the counter-clockwise vertex triangle, so triangle indexes need to be reversed
                           ;; NOTE: raylib renders vertex data in counter-clockwise order (standard convention) by default
                           (setf (aref indices (+ tcounter 2)) (vertex 0)
                                 (aref indices (+ tcounter 1)) (vertex 1)
                                 (aref indices tcounter) (vertex 2))
                           (incf tcounter 3)))))))

        ;; Vertex arrays data processing: IQMVertexArray { type, flags, format, size, offset }
        (let ((ofs-vertexarrays (%iqm-header d ofs-vertexarrays)))
          (dotimes (i (%iqm-header d num-vertexarrays))
            (let ((type (%iqm-u32 d (+ ofs-vertexarrays (* i 20))))
                  (offset (%iqm-u32 d (+ ofs-vertexarrays (* i 20) 16))))
              (flet ((copy-attribute (components reader accessor &optional anim-accessor)
                       (dotimes (m num-meshes)
                         (let* ((first-vertex (third (aref imesh m)))
                                (num-vert (fourth (aref imesh m)))
                                (mesh (aref (model-meshes model) m))
                                (target (funcall accessor mesh))
                                (anim (when anim-accessor (funcall anim-accessor mesh)))
                                (v-counter 0))
                           (loop for k from (* first-vertex components) below (* (+ first-vertex num-vert) components)
                                 do (let ((value (if (< k (* num-vertexes components)) (funcall reader k) 0)))
                                      (setf (aref target v-counter) value)
                                      (when anim (setf (aref anim v-counter) value))
                                      (incf v-counter)))))))
                (case type
                  (0 (copy-attribute 3 (lambda (k) (%iqm-f32 d (+ offset (* k 4)))) #'mesh-vertices #'mesh-anim-vertices)) ; IQM_POSITION
                  (2 (copy-attribute 3 (lambda (k) (%iqm-f32 d (+ offset (* k 4)))) #'mesh-normals #'mesh-anim-normals)) ; IQM_NORMAL
                  (1 (copy-attribute 2 (lambda (k) (%iqm-f32 d (+ offset (* k 4)))) #'mesh-texcoords)) ; IQM_TEXCOORD
                  (4 (copy-attribute 4 (lambda (k) (%iqm-u8 d (+ offset k))) #'mesh-bone-indices)) ; IQM_BLENDINDEXES
                  (5 (copy-attribute 4 (lambda (k) (/ (float (%iqm-u8 d (+ offset k)) 1.0) 255.0)) #'mesh-bone-weights)) ; IQM_BLENDWEIGHTS
                  (6 (dotimes (m num-meshes)                           ; IQM_COLOR
                       (let ((mesh (aref (model-meshes model) m)))
                         (setf (mesh-colors mesh) (make-array (* (mesh-vertex-count mesh) 4) :element-type '(unsigned-byte 8) :initial-element 0))))
                     (copy-attribute 4 (lambda (k) (%iqm-u8 d (+ offset k))) #'mesh-colors)))))))

        ;; Bones (joints) data processing: IQMJoint { name, parent, translate[3], rotate[4], scale[3] }
        (let* ((num-joints (%iqm-header d num-joints))
               (ofs-joints (%iqm-header d ofs-joints))
               (bones (make-array num-joints))
               (bind-pose (make-array num-joints)))
          (dotimes (i num-joints)
            (let ((o (+ ofs-joints (* i 48))))
              (flet ((f (k) (%iqm-f32 d (+ o 8 (* k 4)))))
                ;; Bones
                (setf (aref bones i) (make-bone-info :name (%iqm-name d (+ ofs-text (%iqm-u32 d o)))
                                                     :parent (%iqm-s32 d (+ o 4))))
                ;; Bind pose (base pose)
                (setf (aref bind-pose i) (make-transform :translation (vec3 (f 0) (f 1) (f 2))
                                                         :rotation (vec4 (f 3) (f 4) (f 5) (f 6))
                                                         :scale (vec3 (f 7) (f 8) (f 9)))))))
          (setf (model-skeleton model) (make-model-skeleton :bone-count num-joints :bones bones :bind-pose bind-pose))

          (%build-pose-from-parent-joints bones num-joints bind-pose)

          ;; Initialize runtime animation data: current pose and bone matrices
          (setf (model-current-pose model)
                (let ((v (make-array num-joints)))
                  (dotimes (j num-joints v)
                    (setf (aref v j) (make-transform :translation (vec3 0.0 0.0 0.0) :rotation (vec4 0.0 0.0 0.0 0.0)
                                                     :scale (vec3 0.0 0.0 0.0)))))
                (model-bone-matrices model)
                (let ((v (make-array num-joints)))
                  (dotimes (j num-joints v) (setf (aref v j) (matrix-identity))))))))
    model))

;; Load IQM animation data
(defun %load-model-animations-iqm (file-name)
  (let ((file-data (load-file-data file-name)))
    (unless file-data (return-from %load-model-animations-iqm (values nil 0)))
    (unless (%iqm-check-header file-data file-name) (return-from %load-model-animations-iqm (values nil 0)))

    (let* ((d file-data)
           (num-poses (%iqm-header d num-poses))
           (ofs-poses (%iqm-header d ofs-poses))
           (num-anims (%iqm-header d num-anims))
           (ofs-anims (%iqm-header d ofs-anims))
           (ofs-text (%iqm-header d ofs-text))
           (num-framechannels (%iqm-header d num-framechannels))
           (num-frames (%iqm-header d num-frames))
           (ofs-frames (%iqm-header d ofs-frames))
           ;; IQMPose { parent, mask, channeloffset[10], channelscale[10] }
           (poses (coerce (loop for i below num-poses
                                collect (let ((o (+ ofs-poses (* i 88))))
                                          (list (%iqm-s32 d o) (%iqm-u32 d (+ o 4))
                                                (coerce (loop for k below 10 collect (%iqm-f32 d (+ o 8 (* k 4)))) 'vector)
                                                (coerce (loop for k below 10 collect (%iqm-f32 d (+ o 48 (* k 4)))) 'vector))))
                          'vector))
           (animations (make-array num-anims)))
      (flet ((framedata (k)
               (if (< k (* num-frames num-framechannels)) (%iqm-u16 d (+ ofs-frames (* k 2))) 0)))
        (dotimes (a num-anims)
          ;; IQMAnim { name, first_frame, num_frames, framerate, flags }
          (let* ((o (+ ofs-anims (* a 20)))
                 (first-frame (%iqm-u32 d (+ o 4)))
                 (anim-num-frames (%iqm-u32 d (+ o 8)))
                 (framerate (%iqm-f32 d (+ o 12)))
                 (keyframe-poses (make-array anim-num-frames))
                 (animation (make-model-animation :bone-count num-poses
                                                  :keyframe-count anim-num-frames
                                                  :keyframe-poses keyframe-poses
                                                  :name (%iqm-name d (+ ofs-text (%iqm-u32 d o)))))
                 (dcounter (* first-frame num-framechannels)))
            (setf (aref animations a) animation)

            (trace-log +log-info+ "MODEL: [~a] Loaded animation: ~a | Frames: ~d | Framerate: ~a" file-name
                       (model-animation-name animation) anim-num-frames (%sprintf "%f" framerate))

            (dotimes (frame anim-num-frames)
              (setf (aref keyframe-poses frame)
                    (let ((v (make-array num-poses))) (dotimes (i num-poses v) (setf (aref v i) (make-transform))))))

            (dotimes (frame anim-num-frames)
              (dotimes (i num-poses)
                (destructuring-bind (parent mask channeloffset channelscale) (aref poses i)
                  (declare (ignore parent))
                  (let ((values (make-array 10 :element-type 'single-float)))
                    (dotimes (c 10)
                      (setf (aref values c) (aref channeloffset c))
                      (when (logtest mask (ash 1 c))
                        (setf (aref values c) (+ (aref values c) (* (float (framedata dcounter) 1.0) (aref channelscale c))))
                        (incf dcounter)))
                    (let ((pose (aref (aref keyframe-poses frame) i)))
                      (setf (transform-translation pose) (vec3 (aref values 0) (aref values 1) (aref values 2))
                            (transform-rotation pose) (quaternion-normalize (vec4 (aref values 3) (aref values 4) (aref values 5) (aref values 6)))
                            (transform-scale pose) (vec3 (aref values 7) (aref values 8) (aref values 9))))))))

            (dotimes (frame anim-num-frames)
              (let ((frame-poses (aref keyframe-poses frame)))
                (dotimes (i num-poses)
                  (let ((parent (first (aref poses i))))
                    (when (>= parent 0)
                      (let ((pose (aref frame-poses i))
                            (parent-pose (aref frame-poses parent)))
                        (setf (transform-rotation pose) (quaternion-multiply (transform-rotation parent-pose) (transform-rotation pose))
                              (transform-translation pose) (vector3-rotate-by-quaternion (transform-translation pose) (transform-rotation parent-pose))
                              (transform-translation pose) (vector3-add (transform-translation pose) (transform-translation parent-pose))
                              (transform-scale pose) (vector3-multiply (transform-scale pose) (transform-scale parent-pose)))))))))))
        (values animations num-anims)))))


;; Load image from different glTF provided methods (uri, path, buffer_view)
(defun %load-image-from-cgltf-image (cgltf-image tex-path)
  (let ((image (make-image)))
    (unless cgltf-image (return-from %load-image-from-cgltf-image image))

    (let ((uri (cgltf-image-uri cgltf-image))
          (view (cgltf-image-buffer-view cgltf-image)))
      (cond (uri                        ; Check if image data is provided as an uri (base64 or path)
             (if (and (> (length uri) 5) (string= uri "data:" :end1 5)) ; Check if image is provided as base64 text data
                 ;; Data URI Format: data:<mediatype>;base64,<data>
                 ;; Find the comma
                 (let ((i (position #\, uri)))
                   (if (null i)
                       (trace-log +log-warning+ "IMAGE: glTF data URI is not a valid image")
                       (let ((base64-size (- (length uri) i 1)))
                         (loop while (char= (char uri (+ i base64-size)) #\=) do (decf base64-size)) ; Ignore optional paddings
                         (let* ((number-of-encoded-bits (- (* base64-size 6) (mod (* base64-size 6) 8))) ; Encoded bits minus extra bits, so it becomes a multiple of 8 bits
                                (out-size (ash number-of-encoded-bits -3))) ; Actual encoded bytes
                           (multiple-value-bind (result data) (cgltf-load-buffer-base64 out-size uri (1+ i))
                             (when (eq result :success)
                               (setf image (load-image-from-memory ".png" data out-size))))))))
                 ;; Check if image is provided as image path
                 (setf image (load-image (format nil "~a/~a" tex-path uri)))))
            ;; Check if image is provided as data buffer
            ((and view (cgltf-buffer-data (cgltf-buffer-view-buffer view)))
             (let* ((size (cgltf-buffer-view-size view))
                    (data (make-array size :element-type '(unsigned-byte 8)))
                    (buffer (cgltf-buffer-data (cgltf-buffer-view-buffer view)))
                    (offset (cgltf-buffer-view-offset view))
                    (stride (if (/= (cgltf-buffer-view-stride view) 0) (cgltf-buffer-view-stride view) 1))
                    (mime-type (cgltf-image-mime-type cgltf-image)))
               ;; Copy buffer data to memory for loading
               (dotimes (i size)
                 (setf (aref data i) (%gltf-u8 buffer offset))
                 (incf offset stride))

               ;; Check mime_type for image: (cgltfImage->mime_type == "image/png")
               ;; NOTE: Detected that some models define mime_type as "image\\/png"
               (cond ((member mime-type '("image\\/png" "image/png") :test #'equal)
                      (setf image (load-image-from-memory ".png" data size)))
                     ((member mime-type '("image\\/jpeg" "image/jpeg") :test #'equal)
                      (setf image (load-image-from-memory ".jpg" data size)))
                     (t (trace-log +log-warning+ "MODEL: glTF image data MIME type not recognized")))))))
    image))

(defun %c-name32 (string)
  "snprintf(char[32], \"%s\", STRING)"
  (let ((bytes (babel:string-to-octets string :encoding :utf-8)))
    (babel:octets-to-string bytes :end (min (length bytes) 31) :encoding :utf-8 :errorp nil)))

;; Load bone info from GLTF skin data
(defun %load-bone-info-gltf (skin)
  "Returns (values bones bone-count)"
  (let* ((joints (or (cgltf-skin-joints skin) #()))
         (joints-count (length joints))
         (bones (make-array joints-count)))
    (dotimes (i joints-count)
      (let ((node (svref joints i))
            (bone (make-bone-info)))
        (setf (svref bones i) bone)
        (when (cgltf-node-name node) (setf (bone-info-name bone) (%c-name32 (cgltf-node-name node))))

        ;; Find parent bone index by walking up the node tree past any
        ;; non-joint ancestors (intermediate transform nodes used by some
        ;; DCC exporters), until we hit a node that is also in skin.joints.
        (let ((parent-index -1)
              (ancestor (cgltf-node-parent node)))
          (loop while (and ancestor (= parent-index -1))
                do (let ((j (position ancestor joints)))
                     (when j (setf parent-index j)))
                   (when (= parent-index -1) (setf ancestor (cgltf-node-parent ancestor))))
          (setf (bone-info-parent bone) parent-index))))
    (values bones joints-count)))

(defun %gltf-matrix (m)
  "Matrix from a column-major cgltf float[16]"
  (%matrix (aref m 0) (aref m 1) (aref m 2) (aref m 3) (aref m 4) (aref m 5) (aref m 6) (aref m 7)
           (aref m 8) (aref m 9) (aref m 10) (aref m 11) (aref m 12) (aref m 13) (aref m 14) (aref m 15)))

(defun %gltf-load-attribute (accessor num-comp src-type)
  "LOAD_ATTRIBUTE(): ACCESSOR count*NUM-COMP raw values of SRC-TYPE (:float, :u8, :s8, :u16, :s16, :u32)"
  (let* ((view (cgltf-accessor-buffer-view accessor))
         (data (and view (cgltf-buffer-data (cgltf-buffer-view-buffer view))))
         (size (ecase src-type ((:u8 :s8) 1) ((:u16 :s16) 2) ((:float :u32) 4)))
         (reader (ecase src-type
                   (:float #'%gltf-f32) (:u8 #'%gltf-u8) (:s8 #'%gltf-s8)
                   (:u16 #'%gltf-u16) (:s16 #'%gltf-s16) (:u32 #'%gltf-u32)))
         (count (cgltf-accessor-count accessor))
         (out (make-array (* count num-comp)))
         ;; buffer = (srcType *)buffer->data + view->offset/sizeof(srcType) + accessor->offset/sizeof(srcType)
         (base (if view (+ (floor (cgltf-buffer-view-offset view) size) (floor (cgltf-accessor-offset accessor) size)) 0))
         (step (floor (cgltf-accessor-stride accessor) size))
         (n 0))
    (dotimes (k count)
      (dotimes (l num-comp)
        (setf (svref out (+ (* num-comp k) l)) (funcall reader data (* size (+ base n l)))))
      (incf n step))
    out))

(defun %gltf-copy-into (target values &optional (key #'identity))
  "Copy VALUES into TARGET (C writes past a too small allocation are dropped)"
  (dotimes (i (min (length target) (length values)) target)
    (setf (aref target i) (funcall key (svref values i)))))

(defun %c-float-to-uchar (x)
  "C (unsigned char) conversion of a float"
  (logand (%c-float-to-int x) #xff))

;; Load glTF file into model struct, .gltf and .glb supported
(defun %load-gltf (file-name)
  "
    Function implemented by Wilhem Barbier(@wbrbr), with modifications by Tyler Bezera(@gamerfiend)
    Transform handling implemented by Paul Melis (@paulmelis)
    Reviewed by Ramon Santamaria (@raysan5)

    FEATURES:
      - Supports .gltf and .glb files
      - Supports embedded (base64) or external textures
      - Supports PBR metallic/roughness flow, loads material textures, values and colors
                 PBR specular/glossiness flow and extended texture flows not supported
      - Supports multiple meshes per model (every primitives is loaded as a separate mesh)
      - Supports basic animations
      - Transforms, including parent-child relations, are applied on the mesh data,
        but the hierarchy is not kept (as it can't be represented)
      - Mesh instances in the glTF file (a.e. same mesh linked from multiple nodes)
        are turned into separate raylib Meshes

    RESTRICTIONS:
      - Only triangle meshes supported
      - Vertex attribute types and formats supported:
          > Vertices (position): vec3: float
          > Normals: vec3: float
          > Texcoords: vec2: float
          > Colors: vec4: u8, u16, f32 (normalized)
          > Indices: u16, u32 (truncated to u16)
      - Scenes defined in the glTF file are ignored. All nodes in the file are used
"
  (let ((model (make-model)))
    ;; glTF file loading
    (multiple-value-bind (file-data data-size) (load-file-data file-name)
      (unless file-data (return-from %load-gltf model))

      ;; glTF data loading
      (multiple-value-bind (result data) (cgltf-parse file-data data-size)
        (unless (eq result :success)
          (trace-log +log-warning+ "MODEL: [~a] Failed to load glTF data" file-name)
          (return-from %load-gltf model))

        (case (cgltf-data-file-type data)
          (:glb (trace-log +log-info+ "MODEL: [~a] Model basic data (glb) loaded successfully" file-name))
          (:gltf (trace-log +log-info+ "MODEL: [~a] Model basic data (glTF) loaded successfully" file-name))
          (t (trace-log +log-warning+ "MODEL: [~a] Model format not recognized" file-name)))

        (trace-log +log-info+ "    > Meshes count: ~d" (length (cgltf-data-meshes data)))
        (trace-log +log-info+ "    > Materials count: ~d (+1 default)" (length (cgltf-data-materials data)))
        (trace-log +log-debug+ "    > Buffers count: ~d" (length (cgltf-data-buffers data)))
        (trace-log +log-debug+ "    > Images count: ~d" (length (cgltf-data-images data)))
        (trace-log +log-debug+ "    > Textures count: ~d" (length (cgltf-data-textures data)))

        ;; Force reading data buffers (fills buffer_view->buffer->data)
        ;; NOTE: If an uri is defined to base64 data or external path, it's automatically loaded
        (unless (eq (cgltf-load-buffers data file-name) :success)
          (trace-log +log-info+ "MODEL: [~a] Failed to load mesh/material buffers" file-name))

        (let ((primitives-count 0)
              (draco-compression nil)
              (nodes (cgltf-data-nodes data))
              (materials (cgltf-data-materials data)))
          ;; NOTE: Load every primitive in the glTF as a separate raylib Mesh
          ;; Determine total number of meshes needed from the node hierarchy
          (loop for node across nodes
                for mesh = (cgltf-node-mesh node)
                when mesh
                  do (loop for prim across (or (cgltf-mesh-primitives mesh) #())
                           do (cond ((cgltf-primitive-has-draco-mesh-compression prim)
                                     (setf draco-compression t)
                                     (trace-log +log-warning+ "MODEL: [~a] Failed to load mesh data, Draco compression not supported" file-name)
                                     (return))
                                    ((eq (cgltf-primitive-type prim) :triangles) (incf primitives-count)))))

          (when draco-compression
            (trace-log +log-warning+ "MODEL: [~a] Failed to load glTF data" file-name)
            (return-from %load-gltf model))

          (trace-log +log-debug+ "    > Primitives (triangles only) count based on hierarchy : ~d" primitives-count)

          ;; Load our model data: meshes and materials
          (setf (model-mesh-count model) primitives-count
                (model-meshes model) (let ((v (make-array primitives-count)))
                                       (dotimes (i primitives-count v) (setf (aref v i) (make-mesh)))))

          ;; NOTE: Keep an extra slot for default material, in case some mesh requires it
          (setf (model-material-count model) (+ (length materials) 1)
                (model-materials model) (make-array (model-material-count model)))
          (setf (aref (model-materials model) 0) (load-material-default)) ; Load default material (index: 0)

          ;; Load mesh-material indices, by default all meshes are mapped to material index: 0
          (setf (model-mesh-material model) (make-array primitives-count :initial-element 0))

          ;; Load materials data
          ;;----------------------------------------------------------------------------------------------------
          (loop for i from 0 below (length materials)
                for j from 1
                do (let ((material (load-material-default))
                         (gltf-material (aref materials i))
                         (tex-path (get-directory-path file-name)))
                     (setf (aref (model-materials model) j) material)
                     (flet ((mmap (index) (%material-map material index))
                            (view-image (view)
                              (let ((texture (cgltf-texture-view-texture view)))
                                (and texture (%load-image-from-cgltf-image (cgltf-texture-image texture) tex-path)))))
                       ;; Check glTF material flow: PBR metallic/roughness flow
                       ;; NOTE: Alternatively, materials can follow PBR specular/glossiness flow
                       (when (cgltf-material-has-pbr-metallic-roughness gltf-material)
                         ;; Load base color texture (albedo)
                         (let ((im-albedo (view-image (cgltf-material-base-color-texture gltf-material))))
                           (when (and im-albedo (image-data im-albedo))
                             (setf (material-map-texture (mmap +material-map-albedo+)) (load-texture-from-image im-albedo))
                             (unload-image im-albedo)))
                         ;; Load base color factor (tint)
                         (let ((factor (cgltf-material-base-color-factor gltf-material)))
                           (setf (material-map-color (mmap +material-map-albedo+))
                                 (loop for k below 4 collect (%c-float-to-uchar (* (aref factor k) 255.0)))))

                         ;; Load metallic/roughness texture
                         (when (cgltf-texture-view-texture (cgltf-material-metallic-roughness-texture gltf-material))
                           (let ((im-metallic-roughness (view-image (cgltf-material-metallic-roughness-texture gltf-material))))
                             (when (image-data im-metallic-roughness)
                               (let* ((width (image-width im-metallic-roughness))
                                      (height (image-height im-metallic-roughness))
                                      (im-metallic (make-image :data (make-array (* width height) :element-type '(unsigned-byte 8) :initial-element 0)
                                                               :width width :height height :mipmaps 1
                                                               :format +pixelformat-uncompressed-grayscale+))
                                      (im-roughness (make-image :data (make-array (* width height) :element-type '(unsigned-byte 8) :initial-element 0)
                                                                :width width :height height :mipmaps 1
                                                                :format +pixelformat-uncompressed-grayscale+)))
                                 (dotimes (x width)
                                   (dotimes (y height)
                                     (let ((color (get-image-color im-metallic-roughness x y)))
                                       (setf (aref (image-data im-roughness) (+ (* y width) x)) (second color) ; Roughness color channel
                                             (aref (image-data im-metallic) (+ (* y width) x)) (third color))))) ; Metallic color channel

                                 (setf (material-map-texture (mmap +material-map-roughness+)) (load-texture-from-image im-roughness)
                                       (material-map-texture (mmap +material-map-metalness+)) (load-texture-from-image im-metallic))

                                 (unload-image im-roughness)
                                 (unload-image im-metallic)
                                 (unload-image im-metallic-roughness))))

                           ;; Load metallic/roughness material properties
                           (setf (material-map-value (mmap +material-map-roughness+)) (cgltf-material-roughness-factor gltf-material)
                                 (material-map-value (mmap +material-map-metalness+)) (cgltf-material-metallic-factor gltf-material)))

                         ;; Load normal texture
                         (let ((im-normal (view-image (cgltf-material-normal-texture gltf-material))))
                           (when (and im-normal (image-data im-normal))
                             (setf (material-map-texture (mmap +material-map-normal+)) (load-texture-from-image im-normal))
                             (unload-image im-normal)))

                         ;; Load ambient occlusion texture
                         (let ((im-occlusion (view-image (cgltf-material-occlusion-texture gltf-material))))
                           (when (and im-occlusion (image-data im-occlusion))
                             (setf (material-map-texture (mmap +material-map-occlusion+)) (load-texture-from-image im-occlusion))
                             (unload-image im-occlusion)))

                         ;; Load emissive texture
                         (when (cgltf-texture-view-texture (cgltf-material-emissive-texture gltf-material))
                           (let ((im-emissive (view-image (cgltf-material-emissive-texture gltf-material))))
                             (when (image-data im-emissive)
                               (setf (material-map-texture (mmap +material-map-emission+)) (load-texture-from-image im-emissive))
                               (unload-image im-emissive)))

                           ;; Load emissive color factor
                           (let ((factor (cgltf-material-emissive-factor gltf-material)))
                             (setf (material-map-color (mmap +material-map-emission+))
                                   (list (%c-float-to-uchar (* (aref factor 0) 255.0))
                                         (%c-float-to-uchar (* (aref factor 1) 255.0))
                                         (%c-float-to-uchar (* (aref factor 2) 255.0))
                                         255))))))

                     ;; Other possible materials not supported by raylib pipeline:
                     ;; has_clearcoat, has_transmission, has_volume, has_ior, has specular, has_sheen
                     ))
          ;;----------------------------------------------------------------------------------------------------

          ;; Load meshes data
          ;;
          ;; NOTE: Visit each node in the hierarchy and process any mesh linked from it
          ;;  - Each primitive within a glTF node becomes a raylib Mesh
          ;;  - The local-to-world transform of each node is used to transform the points/normals/tangents of the created Mesh(es)
          ;;  - Any glTF mesh linked from more than one Node (a.e. instancing) is turned into multiple Mesh's, as each Node will have its own transform applied
          ;;
          ;; WARNING: The code below disregards the scenes defined in the file, all nodes are used
          ;;----------------------------------------------------------------------------------------------------
          (let ((mesh-index 0))
            (loop for node across nodes
                  for gltf-mesh = (cgltf-node-mesh node)
                  when gltf-mesh
                    do (let* ((world-matrix (%gltf-matrix (cgltf-node-transform-world node)))
                              (world-matrix-normals (matrix-transpose (matrix-invert world-matrix))))
                         (loop for prim across (or (cgltf-mesh-primitives gltf-mesh) #())
                               ;; NOTE: Only support primitives defined by triangles
                               ;; Other alternatives: points, lines, line_strip, triangle_strip
                               when (eq (cgltf-primitive-type prim) :triangles)
                                 do (let ((mesh (aref (model-meshes model) mesh-index)))
                                      ;; NOTE: Attributes data could be provided in several data formats (8, 8u, 16u, 32...),
                                      ;; Only some formats for each attribute type are supported, read info at the top of this function!
                                      (loop for attr across (or (cgltf-primitive-attributes prim) #())
                                            do (%load-gltf-mesh-attribute file-name mesh attr world-matrix world-matrix-normals))

                                      ;; Load primitive indices data (if provided)
                                      (let ((attribute (cgltf-primitive-indices prim)))
                                        (if (and attribute (cgltf-accessor-buffer-view attribute))
                                            (progn
                                              (setf (mesh-triangle-count mesh) (truncate (%i32 (logand (cgltf-accessor-count attribute) #xffffffff)) 3))
                                              (if (mesh-indices mesh)
                                                  (trace-log +log-warning+ "MODEL: [~a] Indices attribute data already loaded" file-name)
                                                  (flet ((load-indices (src-type)
                                                           ;; Init raylib mesh indices to copy glTF attribute data
                                                           (setf (mesh-indices mesh)
                                                                 (%gltf-copy-into (make-array (cgltf-accessor-count attribute) :element-type '(unsigned-byte 16))
                                                                                  (%gltf-load-attribute attribute 1 src-type)
                                                                                  (lambda (v) (logand v #xffff))))))
                                                    (case (cgltf-accessor-component-type attribute)
                                                      ;; Load unsigned short data type into mesh.indices
                                                      (:r-16u (load-indices :u16))
                                                      (:r-8u (load-indices :u8))
                                                      (:r-32u
                                                       (load-indices :u32)
                                                       (trace-log +log-warning+ "MODEL: [~a] Indices data converted from u32 to u16, possible loss of data" file-name))
                                                      (t (trace-log +log-warning+ "MODEL: [~a] Indices data format not supported, use u16" file-name))))))
                                            ;; Unindexed mesh
                                            (setf (mesh-triangle-count mesh) (truncate (mesh-vertex-count mesh) 3))))

                                      ;; Assign to the primitive mesh the corresponding material index
                                      ;; NOTE: If no material defined, mesh uses the already assigned default material (index: 0)
                                      (let ((m (position (cgltf-primitive-material prim) materials)))
                                        ;; The primitive actually keeps the pointer to the corresponding material,
                                        ;; raylib instead assigns to the mesh the by its index, as loaded in model.materials array
                                        ;; To get the index, check if material pointers match, and assign the corresponding index,
                                        ;; skipping index 0, the default material
                                        (when (and m (cgltf-primitive-material prim))
                                          (setf (aref (model-mesh-material model) mesh-index) (+ m 1))))

                                      (incf mesh-index))))))     ; Move to next mesh
          ;;----------------------------------------------------------------------------------------------------

          ;; Load animation data
          ;; REF: https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#skins
          ;; REF: https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#skinned-mesh-attributes
          ;;
          ;; LIMITATIONS:
          ;;  - Only supports 1 armature per file, and skips loading it if there are multiple armatures
          ;;  - Only supports linear interpolation (default method in Blender when checked "Always Sample Animations" when exporting a GLTF file)
          ;;  - Only supports translation/rotation/scale animation channel.path, weights not considered (a.e. morph targets)
          ;;----------------------------------------------------------------------------------------------------
          (let ((skins (cgltf-data-skins data)))
            (when (> (length skins) 0)
              (let ((skin (aref skins 0)))
                (multiple-value-bind (bones bone-count) (%load-bone-info-gltf skin)
                  (let ((bind-pose (make-array bone-count))
                        (joints (cgltf-skin-joints skin))
                        (inverse-bind-matrices (cgltf-skin-inverse-bind-matrices skin)))
                    (setf (model-skeleton model) (make-model-skeleton :bone-count bone-count :bones bones :bind-pose bind-pose))
                    (dotimes (i bone-count)
                      (let ((bind-matrix nil)
                            (inverse-bind-transform (%cgltf-floats 16)))
                        (if (and inverse-bind-matrices
                                 (>= (cgltf-accessor-count inverse-bind-matrices) (length joints))
                                 (cgltf-accessor-read-float inverse-bind-matrices i inverse-bind-transform 16))
                            (setf bind-matrix (matrix-invert (%gltf-matrix inverse-bind-transform)))
                            (setf bind-matrix (%gltf-matrix (cgltf-node-transform-world (svref joints i)))))
                        (multiple-value-bind (translation rotation scale) (matrix-decompose bind-matrix)
                          (setf (svref bind-pose i)
                                (make-transform :translation translation :rotation rotation :scale scale)))))))

                (when (> (length skins) 1)
                  (trace-log +log-warning+ "MODEL: [~a] can only load one skin (armature) per model, but gltf skins_count == ~d"
                             file-name (length skins))))))

          (let ((mesh-index 0)
                (bone-count (model-skeleton-bone-count (model-skeleton model))))
            (loop for node across nodes
                  for gltf-mesh = (cgltf-node-mesh node)
                  when gltf-mesh
                    do (loop for prim across (or (cgltf-mesh-primitives gltf-mesh) #())
                             ;; NOTE: Only support primitives defined by triangles
                             when (eq (cgltf-primitive-type prim) :triangles)
                               do (let ((mesh (aref (model-meshes model) mesh-index))
                                        (has-joints nil))
                                    (loop for attr across (or (cgltf-primitive-attributes prim) #())
                                          do (case (cgltf-attribute-type attr)
                                               ;; NOTE: JOINTS_1 + WEIGHT_1 will be used for +4 joints influencing a vertex -> Not supported by raylib
                                               (:joints
                                                (setf has-joints t)
                                                (%load-gltf-mesh-joints file-name mesh (cgltf-attribute-data attr)))
                                               (:weights
                                                (%load-gltf-mesh-weights file-name mesh (cgltf-attribute-data attr)))))

                                    ;; Check if animated, and the mesh was not given any bone assignments, but is the child of a bone node
                                    ;; in this case, all the verts need to be attached to the parent bone so it will animate with the bone
                                    (when (and (> (length (cgltf-data-skins data)) 0) (not has-joints)
                                               (cgltf-node-parent node) (null (cgltf-node-mesh (cgltf-node-parent node))))
                                      (let ((parent-bone-id (or (loop for joint below bone-count
                                                                      when (eq (svref (cgltf-skin-joints (aref (cgltf-data-skins data) 0)) joint)
                                                                               (cgltf-node-parent node))
                                                                        return joint)
                                                                -1)))
                                        (when (>= parent-bone-id 0)
                                          (let ((n (* (mesh-vertex-count mesh) 4)))
                                            (setf (mesh-bone-indices mesh) (make-array n :element-type '(unsigned-byte 8) :initial-element 0)
                                                  (mesh-bone-weights mesh) (%floats n))
                                            (loop for vertex-index from 0 below n by 4
                                                  do (setf (aref (mesh-bone-indices mesh) vertex-index) (logand parent-bone-id #xff)
                                                           (aref (mesh-bone-weights mesh) vertex-index) 1.0))))))

                                    ;; Animated vertex data (CPU skinning)
                                    (let ((n (* (mesh-vertex-count mesh) 3)))
                                      (setf (mesh-anim-vertices mesh) (%gltf-copy-into (%floats n) (coerce (or (mesh-vertices mesh) #()) 'simple-vector))
                                            (mesh-anim-normals mesh) (%floats n))
                                      (when (mesh-normals mesh)
                                        (%gltf-copy-into (mesh-anim-normals mesh) (coerce (mesh-normals mesh) 'simple-vector))))
                                    (setf (mesh-bone-count mesh) bone-count)

                                    (incf mesh-index))))) ; Move to next mesh

          ;; Initialize runtime animation data: current pose and bone matrices
          ;; NOTE: Unused allocated memory is not kept in case of no bones defined
          (let ((bone-count (model-skeleton-bone-count (model-skeleton model))))
            (when (> bone-count 0)
              (setf (model-current-pose model)
                    (let ((v (make-array bone-count)))
                      (dotimes (j bone-count v)
                        (setf (aref v j) (make-transform :translation (vec3 0.0 0.0 0.0) :rotation (vec4 0.0 0.0 0.0 0.0)
                                                         :scale (vec3 0.0 0.0 0.0)))))
                    (model-bone-matrices model)
                    (let ((v (make-array bone-count)))
                      (dotimes (j bone-count v) (setf (aref v j) (matrix-identity)))))))
          ;;----------------------------------------------------------------------------------------------------
          )))
    model))

(defun %load-gltf-mesh-attribute (file-name mesh attr world-matrix world-matrix-normals)
  "LoadGLTF() mesh attributes loading: POSITION, NORMAL, TANGENT, TEXCOORD_n, COLOR_n"
  (let* ((attribute (cgltf-attribute-data attr))
         (type (cgltf-accessor-type attribute))
         (component-type (cgltf-accessor-component-type attribute))
         (count (cgltf-accessor-count attribute)))
    (flet ((load-floats (num-comp src-type &optional (convert (lambda (v) (float v 1.0))))
             (%gltf-copy-into (%floats (* count num-comp)) (%gltf-load-attribute attribute num-comp src-type) convert))
           (transform (array stride matrix &optional normalize)
             (dotimes (k count array)
               (let ((vt (vector3-transform (vec3 (aref array (* stride k)) (aref array (+ (* stride k) 1)) (aref array (+ (* stride k) 2)))
                                            matrix)))
                 (when normalize (setf vt (vector3-normalize vt)))
                 (setf (aref array (* stride k)) (vx vt)
                       (aref array (+ (* stride k) 1)) (vy vt)
                       (aref array (+ (* stride k) 2)) (vz vt))))))
      (case (cgltf-attribute-type attr)
        (:position                      ; POSITION, vec3, float
         ;; WARNING: SPECS: POSITION accessor MUST have its min and max properties defined
         (if (mesh-vertices mesh)
             (trace-log +log-warning+ "MODEL: [~a] Vertices attribute data already loaded" file-name)
             (let ((src-type (and (eq type :vec3) (case component-type (:r-32f :float) (:r-16u :u16) (:r-16 :s16)))))
               (if src-type
                   ;; Init raylib mesh vertices to copy glTF attribute data
                   (setf (mesh-vertex-count mesh) (%i32 (logand count #xffffffff))
                         ;; Load 3 components into mesh.vertices, converted to float
                         ;; Transform the vertices
                         (mesh-vertices mesh) (transform (load-floats 3 src-type) 3 world-matrix))
                   (trace-log +log-warning+ "MODEL: [~a] Vertices attribute data format not supported, use vec3 float" file-name)))))
        (:normal                        ; NORMAL, vec3, float
         (if (mesh-normals mesh)
             (trace-log +log-warning+ "MODEL: [~a] Normals attribute data already loaded" file-name)
             (let ((src-type (and (eq type :vec3) (case component-type (:r-32f :float) (:r-16 :s16) (:r-8u :u8) (:r-8 :s8)))))
               (if src-type
                   ;; Init raylib mesh normals to copy glTF attribute data
                   ;; Transform the normals (normalized for integer data)
                   (setf (mesh-normals mesh) (transform (load-floats 3 src-type) 3 world-matrix-normals (not (eq src-type :float))))
                   (trace-log +log-warning+ "MODEL: [~a] Normals attribute data format not supported, use vec3 float" file-name)))))
        (:tangent                       ; TANGENT, vec4, float, w is tangent basis sign
         (if (mesh-tangents mesh)
             (trace-log +log-warning+ "MODEL: [~a] Tangents attribute data already loaded" file-name)
             (if (and (eq type :vec4) (eq component-type :r-32f))
                 ;; Load 4 components of float data type into mesh.tangents
                 ;; Transform the tangents
                 (setf (mesh-tangents mesh) (transform (load-floats 4 :float) 4 world-matrix))
                 (trace-log +log-warning+ "MODEL: [~a] Tangents attribute data format not supported, use vec4 float" file-name))))
        (:texcoord                      ; TEXCOORD_n, vec2, float/u8n/u16n
         ;; Support up to 2 texture coordinates attributes
         (let ((texcoord-ptr nil))
           (if (eq type :vec2)
               (case component-type
                 (:r-32f (setf texcoord-ptr (load-floats 2 :float))) ; vec2, float
                 (:r-8u (setf texcoord-ptr (load-floats 2 :u8 (lambda (v) (/ (float v 1.0) 255.0))))) ; vec2, u8n
                 (:r-16u (setf texcoord-ptr (load-floats 2 :u16 (lambda (v) (/ (float v 1.0) 65535.0))))) ; vec2, u16n
                 (t (trace-log +log-warning+ "MODEL: [~a] Texcoords attribute data format not supported" file-name)))
               (trace-log +log-warning+ "MODEL: [~a] Texcoords attribute data format not supported, use vec2 float" file-name))

           (case (cgltf-attribute-index attr)
             (0 (setf (mesh-texcoords mesh) texcoord-ptr))
             (1 (setf (mesh-texcoords2 mesh) texcoord-ptr))
             (t (trace-log +log-warning+ "MODEL: [~a] No more than 2 texture coordinates attributes supported" file-name)))))
        (:color                         ; COLOR_n, vec3/vec4, float/u8n/u16n
         ;; WARNING: SPECS: All components of each COLOR_n accessor element MUST be clamped to [0.0, 1.0] range
         (if (mesh-colors mesh)
             (trace-log +log-warning+ "MODEL: [~a] Colors attribute data already loaded" file-name)
             (let ((convert (case component-type
                              (:r-8u #'identity)
                              (:r-16u (lambda (v) (%c-float-to-uchar (* (/ (float v 1.0) 65535.0) 255.0))))
                              (:r-32f (lambda (v) (%c-float-to-uchar (* v 255.0))))))
                   (src-type (case component-type (:r-8u :u8) (:r-16u :u16) (:r-32f :float))))
               (cond ((not (member type '(:vec3 :vec4)))
                      (trace-log +log-warning+ "MODEL: [~a] Color attribute data format not supported" file-name))
                     ((null convert)
                      (trace-log +log-warning+ "MODEL: [~a] Color attribute data format not supported" file-name))
                     ((eq type :vec3)   ; RGB
                      ;; Convert data to raylib color data type (4 bytes)
                      (let ((temp (%gltf-load-attribute attribute 3 src-type))
                            (colors (make-array (* count 4) :element-type '(unsigned-byte 8) :initial-element 0)))
                        (loop for c from 0 by 4
                              for k from 0 by 3
                              while (< c (- (* count 4) 3))
                              do (setf (aref colors c) (funcall convert (svref temp k))
                                       (aref colors (+ c 1)) (funcall convert (svref temp (+ k 1)))
                                       (aref colors (+ c 2)) (funcall convert (svref temp (+ k 2)))
                                       (aref colors (+ c 3)) 255))
                        (setf (mesh-colors mesh) colors)))
                     (t                 ; RGBA
                      (setf (mesh-colors mesh)
                            (%gltf-copy-into (make-array (* count 4) :element-type '(unsigned-byte 8) :initial-element 0)
                                             (%gltf-load-attribute attribute 4 src-type) convert)))))))
        ;; NOTE: Attributes related to animations data are processed after mesh data loading
        ))))

(defun %load-gltf-mesh-joints (file-name mesh attribute)
  "JOINTS_n (vec4: 4 bones max per vertex / u8, u16)"
  ;; NOTE: JOINTS_n can only be vec4 and u8/u16
  ;; SPECS: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#meshes-overview

  ;; WARNING: raylib only supports model.meshes[].boneIndices as u8 (unsigned char),
  ;; if data is provided in any other format, it is converted to supported format but
  ;; it could imply data loss (a warning message is issued in that case)
  (if (eq (cgltf-accessor-type attribute) :vec4)
      (case (cgltf-accessor-component-type attribute)
        (:r-8u
         ;; Load attribute: vec4, u8 (unsigned char)
         (setf (mesh-bone-indices mesh)
               (%gltf-copy-into (make-array (* (mesh-vertex-count mesh) 4) :element-type '(unsigned-byte 8) :initial-element 0)
                                (%gltf-load-attribute attribute 4 :u8))))
        (:r-16u
         ;; Load data into a temp buffer to be converted to raylib data type
         (let ((temp (%gltf-copy-into (make-array (* (mesh-vertex-count mesh) 4) :initial-element 0)
                                      (%gltf-load-attribute attribute 4 :u16)))
               (bone-indices (make-array (* (mesh-vertex-count mesh) 4) :element-type '(unsigned-byte 8) :initial-element 0))
               (bone-id-overflow-warning nil))
           ;; Convert data to raylib color data type (4 bytes)
           (dotimes (b (length bone-indices))
             (when (and (> (svref temp b) 255) (not bone-id-overflow-warning))
               (trace-log +log-warning+ "MODEL: [~a] Joint attribute data format (u16) overflow" file-name)
               (setf bone-id-overflow-warning t))
             ;; Despite the possible overflow, convert data to unsigned char
             (setf (aref bone-indices b) (logand (svref temp b) #xff)))
           (setf (mesh-bone-indices mesh) bone-indices)))
        (t (trace-log +log-warning+ "MODEL: [~a] Joint attribute data format not supported" file-name)))
      (trace-log +log-warning+ "MODEL: [~a] Joint attribute data format not supported" file-name)))

(defun %load-gltf-mesh-weights (file-name mesh attribute)
  "WEIGHTS_n (vec4, u8n/u16n/f32)"
  (if (eq (cgltf-accessor-type attribute) :vec4)
      (let ((weights (%floats (* (mesh-vertex-count mesh) 4))))
        (case (cgltf-accessor-component-type attribute)
          (:r-8u
           ;; Convert data to raylib bone weight data type (4 bytes)
           (setf (mesh-bone-weights mesh)
                 (%gltf-copy-into weights (%gltf-load-attribute attribute 4 :u8) (lambda (v) (/ (float v 1.0) 255.0)))))
          (:r-16u
           ;; Convert data to raylib bone weight data type
           (setf (mesh-bone-weights mesh)
                 (%gltf-copy-into weights (%gltf-load-attribute attribute 4 :u16) (lambda (v) (/ (float v 1.0) 65535.0)))))
          (:r-32f
           ;; Load 4 components of float data type into mesh.boneWeights
           (setf (mesh-bone-weights mesh) (%gltf-copy-into weights (%gltf-load-attribute attribute 4 :float))))
          (t (trace-log +log-warning+ "MODEL: [~a] Joint weight attribute data format not supported, use vec4 float" file-name))))
      (trace-log +log-warning+ "MODEL: [~a] Joint weight attribute data format not supported, use vec4 float" file-name)))

;; Get interpolated pose for bone sampler at a specific time
(defun %get-pose-at-time-gltf (interpolation-type input output time value)
  "Returns (values success value), VALUE (vec3 or vec4) is returned unchanged when not computed"
  (when (eq interpolation-type :max-enum) (return-from %get-pose-at-time-gltf (values nil value)))

  ;; Input and output should have the same count
  (let ((tstart 0.0) (tend 0.0)
        (keyframe 0)                    ; Defaults to first pose
        (found nil)
        (tmp1 (%cgltf-floats 1))
        (input-count (%i32 (logand (cgltf-accessor-count input) #xffffffff))))
    (flet ((read-time (index)
             (unless (cgltf-accessor-read-float input index tmp1 1)
               (return-from %get-pose-at-time-gltf (values nil value)))
             (aref tmp1 0)))
      (loop for i from 0 below (- input-count 1)
            do (setf tstart (read-time i)
                     tend (read-time (+ i 1)))
               (when (and (<= tstart time) (< time tend))
                 (setf keyframe i found t)
                 (return)))

      ;; No interval contains a time at (or past) the last keyframe, because the
      ;; search above requires time < tend: clamp to the edge interval instead of
      ;; falling back to keyframe 0, which returns a pose from the start
      (when (and (not found) (>= input-count 2))
        (setf keyframe (- input-count 2))
        (let ((tfirst (read-time 0)))
          (when (< time tfirst) (setf keyframe 0)))
        (setf tstart (read-time keyframe)
              tend (read-time (+ keyframe 1)))))

    ;; Constant animation, no need to interpolate
    (when (float-equals tend tstart) (setf interpolation-type :step))

    (let* ((duration (%fmax (- tend tstart) +epsilon+))
           (tt (/ (- time tstart) duration)))
      (setf tt (if (< tt 0.0) 0.0 tt))
      (setf tt (if (> tt 1.0) 1.0 tt))

      (unless (eq (cgltf-accessor-component-type output) :r-32f)
        (return-from %get-pose-at-time-gltf (values nil value)))

      (case (cgltf-accessor-type output)
        (:vec3
         (let ((tmp (%cgltf-floats 3)))
           (flet ((read-v3 (index)
                    (cgltf-accessor-read-float output index tmp 3)
                    (vec3 (aref tmp 0) (aref tmp 1) (aref tmp 2))))
             (case interpolation-type
               (:step (setf value (read-v3 keyframe)))
               (:linear
                (let* ((v1 (read-v3 keyframe))
                       (v2 (read-v3 (+ keyframe 1))))
                  (setf value (vector3-lerp v1 v2 tt))))
               (:cubic-spline
                (let* ((v1 (read-v3 (+ (* 3 keyframe) 1)))
                       (tangent1 (read-v3 (+ (* 3 keyframe) 2)))
                       (v2 (read-v3 (+ (* 3 (+ keyframe 1)) 1)))
                       (tangent2 (read-v3 (* 3 (+ keyframe 1)))))
                  (setf value (vector3-cubic-hermite v1 tangent1 v2 tangent2 tt))))))))
        (:vec4
         ;; Only v4 is for rotations, so it's a quaternion
         (let ((tmp (%cgltf-floats 4)))
           (flet ((read-v4 (index &optional tangent)
                    (cgltf-accessor-read-float output index tmp 4)
                    (vec4 (aref tmp 0) (aref tmp 1) (aref tmp 2) (if tangent 0.0 (aref tmp 3)))))
             (case interpolation-type
               (:step (setf value (read-v4 keyframe)))
               (:linear
                (let* ((v1 (read-v4 keyframe))
                       (v2 (read-v4 (+ keyframe 1))))
                  (setf value (quaternion-slerp v1 v2 tt))))
               (:cubic-spline
                (let* ((v1 (read-v4 (+ (* 3 keyframe) 1)))
                       (out-tangent1 (read-v4 (+ (* 3 keyframe) 2) t))
                       (v2 (read-v4 (+ (* 3 (+ keyframe 1)) 1)))
                       (in-tangent2 (read-v4 (* 3 (+ keyframe 1)) t)))
                  (setf v1 (quaternion-normalize v1)
                        v2 (quaternion-normalize v2))
                  (when (< (vector4-dot-product v1 v2) 0.0)
                    (setf v2 (vector4-negate v2)))
                  (setf out-tangent1 (vector4-scale out-tangent1 duration)
                        in-tangent2 (vector4-scale in-tangent2 duration))
                  (setf value (quaternion-cubic-hermite-spline v1 out-tangent1 v2 in-tangent2 tt)))))))))
      (values t value))))

(defconstant +gltf-framerate+ 60.0 "glTF animation framerate (frames per second)")

(defun %load-model-animations-gltf (file-name)
  (multiple-value-bind (file-data data-size) (load-file-data file-name)
    ;; glTF data loading
    (multiple-value-bind (result data) (cgltf-parse file-data data-size)
      (unless (eq result :success)
        (trace-log +log-warning+ "MODEL: [~a] Failed to load glTF data" file-name)
        (return-from %load-model-animations-gltf (values nil 0)))

      (let ((animations nil) (anim-count 0)
            (result (cgltf-load-buffers data file-name)))
        (unless (eq result :success) (trace-log +log-info+ "MODEL: [~a] Failed to load animation buffers" file-name))

        (when (eq result :success)
          (let ((skins (cgltf-data-skins data)))
            (when (> (length skins) 0)
              (let* ((skin (aref skins 0))
                     (joints (cgltf-skin-joints skin))
                     ;; Precompute, per joint, the static transform contributed by any
                     ;; intermediate non-joint nodes between the joint and its nearest
                     ;; joint ancestor. This handles exporters (e.g. wow.export) that
                     ;; store bone offsets on dummy parent nodes rather than on the
                     ;; joints themselves. Depends only on the skin, not the animation.
                     (joint-count (length joints))
                     (ext-offset (make-array joint-count)))
                (setf anim-count (length (cgltf-data-animations data))
                      animations (make-array anim-count))

                (dotimes (k joint-count)
                  (setf (svref ext-offset k) (matrix-identity))
                  (loop for n = (cgltf-node-parent (svref joints k)) then (cgltf-node-parent n)
                        while n
                        do (when (find n joints) (return))
                           ;; Compose the intermediate node's local TRS (scale, then rotation, then translation)
                           (let* ((s (cgltf-node-scale n)) (r (cgltf-node-rotation n)) (tr (cgltf-node-translation n))
                                  (node-scale (matrix-scale (aref s 0) (aref s 1) (aref s 2)))
                                  (node-rotation (quaternion-to-matrix (vec4 (aref r 0) (aref r 1) (aref r 2) (aref r 3))))
                                  (node-translation (matrix-translate (aref tr 0) (aref tr 1) (aref tr 2)))
                                  (node-transform (matrix-multiply (matrix-multiply node-scale node-rotation) node-translation)))
                             (setf (svref ext-offset k) (matrix-multiply (svref ext-offset k) node-transform)))))

                (dotimes (a anim-count)
                  (multiple-value-bind (bones bone-count) (%load-bone-info-gltf skin)
                    (let* ((anim-data (aref (cgltf-data-animations data) a))
                           (animation (make-model-animation :bone-count bone-count))
                           ;; struct Channels { translate, rotate, scale }
                           (bone-channels (make-array bone-count :initial-element nil))
                           (anim-duration 0.0)
                           (tmp1 (%cgltf-floats 1)))
                      (setf (svref animations a) animation)
                      (dotimes (k bone-count) (setf (svref bone-channels k) (list nil nil nil)))

                      (loop for channel across (or (cgltf-animation-channels anim-data) #())
                            for j from 0
                            do (let ((bone-index (or (position (cgltf-animation-channel-target-node channel) joints) -1)))
                                 (unless (or (= bone-index -1) ; Animation channel for a node not in the skeleton
                                             (null (cgltf-animation-channel-target-node channel)))
                                   (let ((sampler (cgltf-animation-channel-sampler channel)))
                                     (if (not (eq (cgltf-animation-sampler-interpolation sampler) :max-enum))
                                         (case (cgltf-animation-channel-target-path channel)
                                           (:translation (setf (first (svref bone-channels bone-index)) channel))
                                           (:rotation (setf (second (svref bone-channels bone-index)) channel))
                                           (:scale (setf (third (svref bone-channels bone-index)) channel))
                                           (t (trace-log +log-warning+ "MODEL: [~a] Unsupported target_path on channel ~d's sampler for animation ~d. Skipping."
                                                         file-name j a)))
                                         (trace-log +log-warning+ "MODEL: [~a] Invalid interpolation curve encountered for GLTF animation." file-name))

                                     (let ((input (cgltf-animation-sampler-input sampler)))
                                       (if (not (cgltf-accessor-read-float input (1- (cgltf-accessor-count input)) tmp1 1))
                                           (trace-log +log-warning+ "MODEL: [~a] Failed to load input time" file-name)
                                           (let ((time (aref tmp1 0)))
                                             (setf anim-duration (if (> time anim-duration) time anim-duration)))))))))

                      (when (cgltf-animation-name anim-data)
                        (setf (model-animation-name animation) (%c-name32 (cgltf-animation-name anim-data))))

                      (let* ((keyframe-count (+ (%c-float-to-int (* anim-duration +gltf-framerate+)) 1))
                             (keyframe-poses (make-array keyframe-count)))
                        (setf (model-animation-keyframe-count animation) keyframe-count
                              (model-animation-keyframe-poses animation) keyframe-poses)

                        (dotimes (j keyframe-count)
                          (let ((poses (make-array bone-count))
                                (time (/ (float j 1.0) +gltf-framerate+)))
                            (setf (svref keyframe-poses j) poses)
                            (dotimes (k bone-count)
                              (let* ((joint (svref joints k))
                                     (translation (let ((v (cgltf-node-translation joint))) (vec3 (aref v 0) (aref v 1) (aref v 2))))
                                     (rotation (let ((v (cgltf-node-rotation joint))) (vec4 (aref v 0) (aref v 1) (aref v 2) (aref v 3))))
                                     (scale (let ((v (cgltf-node-scale joint))) (vec3 (aref v 0) (aref v 1) (aref v 2)))))
                                (destructuring-bind (translate rotate scale-channel) (svref bone-channels k)
                                  (flet ((pose (channel current what)
                                           (let ((sampler (cgltf-animation-channel-sampler channel)))
                                             (multiple-value-bind (ok new-value)
                                                 (%get-pose-at-time-gltf (cgltf-animation-sampler-interpolation sampler)
                                                                         (cgltf-animation-sampler-input sampler)
                                                                         (cgltf-animation-sampler-output sampler)
                                                                         time current)
                                               (unless ok
                                                 (trace-log +log-info+ "MODEL: [~a] Failed to load ~a pose data for bone ~a"
                                                            file-name what (bone-info-name (svref bones k))))
                                               new-value))))
                                    (when translate (setf translation (pose translate translation "translate")))
                                    (when rotate (setf rotation (pose rotate rotation "rotate")))
                                    (when scale-channel (setf scale (pose scale-channel scale "scale")))))

                                ;; Compose joint local TRS, then prepend the static
                                ;; intermediate non-joint offsets so the final TRS is
                                ;; expressed relative to the joint's skeleton parent.
                                (let* ((s (matrix-scale (vx scale) (vy scale) (vz scale)))
                                       (r (quaternion-to-matrix rotation))
                                       (tm (matrix-translate (vx translation) (vy translation) (vz translation)))
                                       (joint-local (matrix-multiply (matrix-multiply s r) tm))
                                       (combined (matrix-multiply joint-local (svref ext-offset k))))
                                  (multiple-value-bind (tr-translation tr-rotation tr-scale) (matrix-decompose combined)
                                    (setf (svref poses k) (make-transform :translation tr-translation :rotation tr-rotation
                                                                          :scale tr-scale))))))

                            (%build-pose-from-parent-joints bones bone-count poses))))

                      (trace-log +log-info+ "MODEL: [~a] Loaded animation: ~a | Frames: ~d | Duration: ~as" file-name
                                 (or (cgltf-animation-name anim-data) "NULL") (model-animation-keyframe-count animation)
                                 (%sprintf "%f" anim-duration)))))))

            (when (> (length skins) 1)
              (trace-log +log-warning+ "MODEL: [~a] Expected one unique skin to load animation data from, but found ~d"
                         file-name (length skins)))))

        (values animations anim-count)))))

;; Hook LoadFileData() calls to M3D loaders
(defun %m3d-loaderhook (fn)
  (load-file-data (%m3d-octets-string fn)))

(defun %m3d-name32 (name)
  "snprintf(char[32], \"%s\", NAME) of a M3D (latin-1 kept bytes) string"
  (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code (or name ""))))
    (babel:octets-to-string bytes :end (min (length bytes) 31) :encoding :utf-8 :errorp nil)))

(defun %m3d-color (color)
  "memcpy() of an uint32_t color into a Color (r in the lowest byte)"
  (list (ldb (byte 8 0) color) (ldb (byte 8 8) color) (ldb (byte 8 16) color) (ldb (byte 8 24) color)))

(defun %m3d-vertex-vec3 (m3d index &optional (scale 1.0))
  (let ((v (aref (m3d-vertex m3d) index)))
    (vec3 (* (m3dv-x v) scale) (* (m3dv-y v) scale) (* (m3dv-z v) scale))))

(defun %m3d-vertex-quaternion (m3d index)
  (let ((v (aref (m3d-vertex m3d) index)))
    (vec4 (m3dv-x v) (m3dv-y v) (m3dv-z v) (m3dv-w v))))

(defun %m3d-bone-to-model-space (pose parent-pose)
  "Child bones are stored in parent bone relative space, convert that into model space"
  (setf (transform-rotation pose) (quaternion-multiply (transform-rotation parent-pose) (transform-rotation pose))
        (transform-translation pose) (vector3-rotate-by-quaternion (transform-translation pose) (transform-rotation parent-pose))
        (transform-translation pose) (vector3-add (transform-translation pose) (transform-translation parent-pose))
        (transform-scale pose) (vector3-multiply (transform-scale pose) (transform-scale parent-pose))))

(defun %m3d-no-bone-transform ()
  (make-transform :translation (vec3 0.0 0.0 0.0) :rotation (vec4 0.0 0.0 0.0 1.0) :scale (vec3 1.0 1.0 1.0)))

;; Load M3D mesh data
(defun %load-m3d (file-name)
  (let ((model (make-model))
        (mi #xfffffffe)                 ; int mi = -2, compared as unsigned
        (vcolor nil))
    (multiple-value-bind (file-data data-size) (load-file-data file-name)
      (declare (ignore data-size))
      (unless file-data (return-from %load-m3d model))

      (let ((m3d (m3d-load file-data #'%m3d-loaderhook)))
        (if (or (null m3d) (m3d-err-isfatal (m3d-errcode m3d)))
            (progn
              (trace-log +log-warning+ "MODEL: [~a] Failed to load M3D data, error code ~d" file-name (if m3d (m3d-errcode m3d) -2))
              (return-from %load-m3d model))
            (trace-log +log-info+ "MODEL: [~a] M3D data loaded successfully: ~d faces/~d materials" file-name
                       (m3d-numface m3d) (m3d-nummaterial m3d)))

        ;; Check if face is found, if not, probably just a material library
        (when (zerop (m3d-numface m3d)) (return-from %load-m3d model))

        (let ((mesh-count 0) (material-count 0)
              (faces (m3d-face m3d))
              (vertices (m3d-vertex m3d))
              (scale (m3d-scale m3d))
              (skinned (and (plusp (m3d-numbone m3d)) (plusp (m3d-numskin m3d)))))
          (if (> (m3d-nummaterial m3d) 0)
              (progn
                (setf mesh-count (m3d-nummaterial m3d) material-count (m3d-nummaterial m3d))
                (trace-log +log-info+ "MODEL: model has ~d material meshes" material-count))
              (progn
                (setf mesh-count 1 material-count 0)
                (trace-log +log-info+ "MODEL: No materials, putting all meshes in a default material")))

          ;; A default material is always required, so adding +1
          (incf material-count)

          ;; NOTE: Faces must be in non-decreasing materialid order, sorting is not needed,
          ;; valid M3D model files should already be sorted (Check PR #3363 #3385)

          (let ((meshes (make-array mesh-count :adjustable t :fill-pointer mesh-count))
                (mesh-material (make-array mesh-count :adjustable t :fill-pointer mesh-count :initial-element 0))
                (materials (make-array (+ material-count 1) :initial-element nil)))
            (dotimes (i mesh-count) (setf (aref meshes i) (make-mesh)))
            (setf (model-mesh-count model) mesh-count
                  (model-material-count model) material-count)

            ;; Map no material to index 0 with default shader, everything else materialid + 1
            (setf (aref materials 0) (load-material-default))

            (let ((k -1) (l 0))
              (dotimes (i (m3d-numface m3d))
                (let ((face (aref faces i)))
                  ;; Materials are grouped together
                  (when (/= mi (m3df-materialid face))
                    ;; There should be only one material switch per material kind,
                    ;; but be bulletproof for non-optimal model files
                    (when (>= (+ k 1) (model-mesh-count model))
                      (incf (model-mesh-count model))
                      (vector-push-extend (make-mesh) meshes)
                      (vector-push-extend 0 mesh-material))

                    (incf k)
                    (setf mi (m3df-materialid face))

                    ;; Only allocate colors VertexBuffer if there's a color vertex in the model for this material batch
                    ;; if all colors are fully transparent black for all vertices of this material, then assuming no vertex colors
                    (setf l 0 vcolor nil)
                    (loop for j from i below (m3d-numface m3d)
                          while (= mi (m3df-materialid (aref faces j)))
                          do (let ((fv (m3df-vertex (aref faces j))))
                               (when (or (zerop (m3dv-color (aref vertices (aref fv 0))))
                                         (zerop (m3dv-color (aref vertices (aref fv 1))))
                                         (zerop (m3dv-color (aref vertices (aref fv 2)))))
                                 (setf vcolor t)))
                             (incf l))

                    (let* ((mesh (aref meshes k))
                           (vertex-count (* l 3)))
                      (setf (mesh-vertex-count mesh) vertex-count
                            (mesh-triangle-count mesh) l
                            (mesh-vertices mesh) (%floats (* vertex-count 3))
                            (mesh-texcoords mesh) (%floats (* vertex-count 2))
                            (mesh-normals mesh) (%floats (* vertex-count 3)))

                      ;; If no map is provided, or colors are defined, allocate storage for vertex colors
                      ;; M3D specs only consider vertex colors if no material is provided, however raylib uses both and mixes the colors
                      (when (or (= mi +m3d-undef+) vcolor)
                        (setf (mesh-colors mesh) (make-array (* vertex-count 4) :element-type '(unsigned-byte 8) :initial-element 0)))

                      ;; If no map is provided and vertex colors are allocated, set them to white
                      (when (and (= mi +m3d-undef+) (mesh-colors mesh))
                        (fill (mesh-colors mesh) 255))

                      (when skinned
                        (setf (mesh-bone-indices mesh) (make-array (* vertex-count 4) :element-type '(unsigned-byte 8) :initial-element 0)
                              (mesh-bone-weights mesh) (%floats (* vertex-count 4))
                              ;; NOTE: SUPPORT_GPU_SKINNING is disabled: vertex buffers for CPU skinning
                              (mesh-anim-vertices mesh) (%floats (* vertex-count 3))
                              (mesh-anim-normals mesh) (%floats (* vertex-count 3)))))

                    (setf (aref mesh-material k) (%i32 (logand (+ mi 1) #xffffffff)))
                    (setf l 0))

                  ;; Process meshes per material, add triangles
                  (let* ((mesh (aref meshes k))
                         (fv (m3df-vertex face))
                         (ft (m3df-texcoord face))
                         (fnorm (m3df-normal face)))
                    (float-features:with-float-traps-masked t
                      (dotimes (c 3)
                        (let ((v (aref vertices (aref fv c))))
                          (setf (aref (mesh-vertices mesh) (+ (* l 9) (* c 3) 0)) (* (m3dv-x v) scale)
                                (aref (mesh-vertices mesh) (+ (* l 9) (* c 3) 1)) (* (m3dv-y v) scale)
                                (aref (mesh-vertices mesh) (+ (* l 9) (* c 3) 2)) (* (m3dv-z v) scale)))))

                    ;; Without vertex color (full transparency), using the default color
                    (when (mesh-colors mesh)
                      (dotimes (c 3)
                        (let ((color (m3dv-color (aref vertices (aref fv c)))))
                          (when (logtest color #xff000000)
                            (loop for b from 0 for byte in (%m3d-color color)
                                  do (setf (aref (mesh-colors mesh) (+ (* l 12) (* c 4) b)) byte))))))

                    (when (/= (aref ft 0) +m3d-undef+)
                      (dotimes (c 3)
                        (let ((uv (svref (m3d-tmap m3d) (aref ft c))))
                          (setf (aref (mesh-texcoords mesh) (+ (* l 6) (* c 2) 0)) (car uv)
                                (aref (mesh-texcoords mesh) (+ (* l 6) (* c 2) 1)) (- 1.0 (cdr uv))))))

                    (when (/= (aref fnorm 0) +m3d-undef+)
                      (dotimes (c 3)
                        (let ((v (aref vertices (aref fnorm c))))
                          (setf (aref (mesh-normals mesh) (+ (* l 9) (* c 3) 0)) (m3dv-x v)
                                (aref (mesh-normals mesh) (+ (* l 9) (* c 3) 1)) (m3dv-y v)
                                (aref (mesh-normals mesh) (+ (* l 9) (* c 3) 2)) (m3dv-z v)))))

                    ;; Add skin (vertex / bone weight pairs)
                    (when skinned
                      (dotimes (n 3)
                        (let ((skinid (%i32 (m3dv-skinid (aref vertices (aref fv n))))))
                          ;; Check if there is a skin for this mesh
                          (if (and (/= skinid -1) (< skinid (m3d-numskin m3d)))
                              (let ((skin (svref (m3d-skin m3d) skinid)))
                                (dotimes (j 4)
                                  (setf (aref (mesh-bone-indices mesh) (+ (* l 12) (* n 4) j)) (logand (aref (m3ds-boneid skin) j) #xff)
                                        (aref (mesh-bone-weights mesh) (+ (* l 12) (* n 4) j)) (aref (m3ds-weight skin) j))))
                              ;; Boneless meshes with skeletal animations are not supported, so
                              ;; putting all vertices without a bone into a special "no bone" bone
                              (setf (aref (mesh-bone-indices mesh) (+ (* l 12) (* n 4))) (logand (m3d-numbone m3d) #xff)
                                    (aref (mesh-bone-weights mesh) (+ (* l 12) (* n 4))) 1.0))))))
                  (incf l))))

            ;; Load materials
            (dotimes (i (m3d-nummaterial m3d))
              (let ((material (load-material-default)))
                (setf (aref materials (+ i 1)) material)
                (flet ((mmap (index) (%material-map material index)))
                  (loop for prop across (m3dm-props (aref (m3d-material m3d) i))
                        do (let ((type (m3dp-type prop)) (value (m3dp-value prop)))
                             (cond
                               ((= type +m3dp-kd+)
                                (setf (material-map-color (mmap +material-map-diffuse+)) (%m3d-color value)
                                      (material-map-value (mmap +material-map-diffuse+)) 0.0))
                               ((= type +m3dp-ks+)
                                (setf (material-map-color (mmap +material-map-specular+)) (%m3d-color value)))
                               ((= type +m3dp-ns+)
                                (setf (material-map-value (mmap +material-map-specular+)) (%m3d-prop-float value)))
                               ((= type +m3dp-ke+)
                                (setf (material-map-color (mmap +material-map-emission+)) (%m3d-color value)
                                      (material-map-value (mmap +material-map-emission+)) 0.0))
                               ((= type +m3dp-pm+)
                                (setf (material-map-value (mmap +material-map-metalness+)) (%m3d-prop-float value)))
                               ((= type +m3dp-pr+)
                                (setf (material-map-value (mmap +material-map-roughness+)) (%m3d-prop-float value)))
                               ((= type +m3dp-ps+)
                                (setf (material-map-color (mmap +material-map-normal+)) (copy-list +white+)
                                      (material-map-value (mmap +material-map-normal+)) (%m3d-prop-float value)))
                               ((>= type 128)
                                (let* ((texture (aref (m3d-texture m3d) value))
                                       (tx (m3dtx-image texture))
                                       (image (if tx
                                                  (make-image :data (image-data tx) :width (logand (image-width tx) #xffff)
                                                              :height (logand (image-height tx) #xffff) :mipmaps 1
                                                              :format (image-pixel-format tx))
                                                  (make-image :data nil :width 0 :height 0 :mipmaps 1
                                                              :format +pixelformat-uncompressed-grayscale+)))
                                       (map-index (case type
                                                    (#.+m3dp-map-kd+ +material-map-diffuse+)
                                                    (#.+m3dp-map-ks+ +material-map-specular+)
                                                    (#.+m3dp-map-ke+ +material-map-emission+)
                                                    (#.+m3dp-map-km+ +material-map-normal+)
                                                    (#.+m3dp-map-ka+ +material-map-occlusion+)
                                                    (#.+m3dp-map-pm+ +material-map-roughness+))))
                                  (when map-index
                                    (setf (material-map-texture (mmap map-index)) (load-texture-from-image image)))))))))))

            ;; Load bones
            (when (plusp (m3d-numbone m3d))
              (let* ((bone-count (+ (m3d-numbone m3d) 1))
                     (bones (make-array bone-count))
                     (bind-pose (make-array bone-count)))
                (dotimes (i bone-count) (setf (svref bind-pose i) (make-transform)))
                (setf (model-skeleton model) (make-model-skeleton :bone-count bone-count :bones bones :bind-pose bind-pose))
                (dotimes (i (m3d-numbone m3d))
                  (let* ((m3d-bone (svref (m3d-bone m3d) i))
                         (parent (%i32 (m3db-parent m3d-bone)))
                         (pose (make-transform :translation (%m3d-vertex-vec3 m3d (m3db-pos m3d-bone) scale)
                                               ;; NOTE: If the orientation quaternion is not normalized, then that's encoding scaling
                                               :rotation (quaternion-normalize (%m3d-vertex-quaternion m3d (m3db-ori m3d-bone)))
                                               :scale (vec3 1.0 1.0 1.0))))
                    (setf (svref bones i) (make-bone-info :name (%m3d-name32 (m3db-name m3d-bone)) :parent parent)
                          (svref bind-pose i) pose)
                    ;; Child bones are stored in parent bone relative space, convert that into model space
                    (when (>= parent 0)
                      (%m3d-bone-to-model-space pose (svref bind-pose parent)))))
                ;; Add a special "no bone" bone
                (setf (svref bones (m3d-numbone m3d)) (make-bone-info :name "NO BONE" :parent -1)
                      (svref bind-pose (m3d-numbone m3d)) (%m3d-no-bone-transform))))

            ;; Load bone-pose default mesh into animation vertices. These will be updated when UpdateModelAnimation gets
            ;; called, but not before, however DrawMesh uses these if they exist (so not good if they are left empty)
            (when skinned
              (let ((bone-count (model-skeleton-bone-count (model-skeleton model))))
                (loop for mesh across meshes
                      do (setf (mesh-bone-count mesh) bone-count)
                         ;; Initialize vertex buffers for CPU skinning
                         (when (mesh-anim-vertices mesh)
                           (replace (mesh-anim-vertices mesh) (mesh-vertices mesh))
                           (replace (mesh-anim-normals mesh) (mesh-normals mesh))))
                ;; Initialize runtime animation data: current pose and bone matrices
                (setf (model-current-pose model)
                      (let ((v (make-array bone-count)))
                        (dotimes (j bone-count v)
                          (setf (aref v j) (make-transform :translation (vec3 0.0 0.0 0.0) :rotation (vec4 0.0 0.0 0.0 0.0)
                                                           :scale (vec3 0.0 0.0 0.0)))))
                      (model-bone-matrices model)
                      (let ((v (make-array bone-count)))
                        (dotimes (j bone-count v) (setf (aref v j) (matrix-identity)))))))

            (setf (model-meshes model) (coerce meshes 'simple-vector)
                  (model-mesh-material model) (coerce mesh-material 'simple-vector)
                  (model-materials model) materials)
            (m3d-free m3d)))))
    model))

(defun %m3d-prop-float (value)
  "prop->value.fnum: the property value as a float (union with the integer values)"
  (if (floatp value) value (%bits->f32 value)))

(defconstant +m3d-animdelay+ 17 "Animation frames delay, (~1000 ms/60 FPS = 16.666666 ms)")

;; Load M3D animation data
(defun %load-model-animations-m3d (file-name)
  (let ((animations nil) (anim-count 0))
    (multiple-value-bind (file-data data-size) (load-file-data file-name)
      (declare (ignore data-size))
      (when file-data
        (let ((m3d (m3d-load file-data #'%m3d-loaderhook)))
          (if (or (null m3d) (m3d-err-isfatal (m3d-errcode m3d)))
              (progn
                (trace-log +log-warning+ "MODEL: [~a] Failed to load M3D data, error code ~d" file-name (if m3d (m3d-errcode m3d) -2))
                (return-from %load-model-animations-m3d (values nil 0)))
              (trace-log +log-info+ "MODEL: [~a] M3D data loaded successfully: ~d animations, ~d bones, ~d skins" file-name
                         (m3d-numaction m3d) (m3d-numbone m3d) (m3d-numskin m3d)))

          ;; No animation or bones, exit out. skins are not required because some people use one animation for N models
          (when (or (zerop (m3d-numaction m3d)) (zerop (m3d-numbone m3d)))
            (return-from %load-model-animations-m3d (values nil 0)))

          (let ((numbone (m3d-numbone m3d))
                (scale (m3d-scale m3d)))
            (setf anim-count (m3d-numaction m3d)
                  animations (make-array anim-count))
            (dotimes (a anim-count)
              (let* ((action (aref (m3d-action m3d) a))
                     (keyframe-count (floor (m3da-durationmsec action) +m3d-animdelay+))
                     (keyframe-poses (make-array keyframe-count))
                     (bones (make-array (+ numbone 1)))
                     (animation (make-model-animation :bone-count (+ numbone 1)
                                                      :keyframe-count keyframe-count
                                                      :keyframe-poses keyframe-poses
                                                      :name (%m3d-name32 (m3da-name action)))))
                (setf (svref animations a) animation)
                ;; NOTE: C prints the unsigned durationmsec with %f
                (trace-log +log-info+ "MODEL: [~a] Loaded animation: ~a | Frames: ~d | Duration: ~as" file-name
                           (model-animation-name animation) keyframe-count (%sprintf "%f" (float (m3da-durationmsec action) 1.0)))

                (dotimes (i numbone)
                  (let ((bone (svref (m3d-bone m3d) i)))
                    (setf (svref bones i) (make-bone-info :name (%m3d-name32 (m3db-name bone)) :parent (%i32 (m3db-parent bone))))))

                ;; A special, never transformed "no bone" bone, used for boneless vertices
                (setf (svref bones numbone) (make-bone-info :name "NO BONE" :parent -1))

                ;; M3D stores frames at arbitrary intervals with sparse skeletons; Full skeletons is required at
                ;; regular intervals, so let the M3D SDK do the heavy lifting and calculate interpolated bones
                (dotimes (i keyframe-count)
                  (let ((poses (make-array (+ numbone 1)))
                        (pose (m3d-pose m3d a (* i +m3d-animdelay+))))
                    (dotimes (j (+ numbone 1)) (setf (svref poses j) (make-transform)))
                    (setf (svref keyframe-poses i) poses)
                    (when pose
                      (dotimes (j numbone)
                        (let ((transform (make-transform :translation (%m3d-vertex-vec3 m3d (m3db-pos (aref pose j)) scale)
                                                         :rotation (quaternion-normalize (%m3d-vertex-quaternion m3d (m3db-ori (aref pose j))))
                                                         :scale (vec3 1.0 1.0 1.0))))
                          (setf (svref poses j) transform)
                          ;; Child bones are stored in parent bone relative space, convert that into model space
                          (when (>= (bone-info-parent (svref bones j)) 0)
                            (%m3d-bone-to-model-space transform (svref poses (bone-info-parent (svref bones j)))))))
                      ;; Default transform for the "no bone" bone
                      (setf (svref poses numbone) (%m3d-no-bone-transform)))))))
            (m3d-free m3d)))))
    (values animations anim-count)))

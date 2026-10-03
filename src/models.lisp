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
             (rl-vertex3f (* (sin (* +deg2rad+ i)) radius) (* (cos (* +deg2rad+ i)) radius) 0.0)
             (rl-vertex3f (* (sin (* +deg2rad+ (+ i 10))) radius) (* (cos (* +deg2rad+ (+ i 10))) radius) 0.0))
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
            (,cosring (cos ,ringangle))
            (,sinring (sin ,ringangle))
            (,cosslice (cos ,sliceangle))
            (,sinslice (sin ,sliceangle))
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
             (rl-vertex3f (* (sin (* +deg2rad+ i angle-step)) radius) y (* (cos (* +deg2rad+ i angle-step)) radius))))
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
                collect (let ((s1 (* (sin (* base-angle (+ i 0))) start-radius))
                              (c1 (* (cos (* base-angle (+ i 0))) start-radius))
                              (s2 (* (sin (* base-angle (+ i 1))) start-radius))
                              (c2 (* (cos (* base-angle (+ i 1))) start-radius))
                              (s3 (* (sin (* base-angle (+ i 0))) end-radius))
                              (c3 (* (cos (* base-angle (+ i 0))) end-radius))
                              (s4 (* (sin (* base-angle (+ i 1))) end-radius))
                              (c4 (* (cos (* base-angle (+ i 1))) end-radius)))
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
             (rl-vertex3f (* (sin (* +deg2rad+ i angle-step)) radius) y (* (cos (* +deg2rad+ i angle-step)) radius))))
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
                     (let ((ring-sin (* (sin (* base-slice-angle jj)) (cos (* base-ring-angle ii))))
                           (ring-cos (* (cos (* base-slice-angle jj)) (cos (* base-ring-angle ii))))
                           (s (sin (* base-ring-angle ii))))
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
                   (let ((ring-sin (* (sin (* base-slice-angle jj)) radius))
                         (ring-cos (* (cos (* base-slice-angle jj)) radius)))
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

;; Load materials from model file
(defun load-materials (file-name)
  "Load materials from model file, returns a vector of materials"
  (let ((materials (vector))
        (count 0))
    (when (is-file-extension file-name ".mtl")
      (multiple-value-bind (mats result) (%tinyobj-parse-mtl-file file-name)
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
            do (setf (aref vertices (* 3 (+ v 1))) (* (sin (* +deg2rad+ d)) radius)
                     (aref vertices (+ (* 3 (+ v 1)) 2)) (* (cos (* +deg2rad+ d)) radius)
                     (aref vertices (* 3 (+ v 2))) (* (sin (* +deg2rad+ (+ d d-step))) radius)
                     (aref vertices (+ (* 3 (+ v 2)) 2)) (* (cos (* +deg2rad+ (+ d d-step))) radius))
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
  (cond ((sb-ext:float-nan-p a) b)
        ((sb-ext:float-nan-p b) a)
        (t (min a b))))

(defun %fmax (a b)
  "C fmax(): NaN arguments are ignored"
  (cond ((sb-ext:float-nan-p a) b)
        ((sb-ext:float-nan-p b) a)
        (t (max a b))))

(defun %c-float-to-int (x)
  "C (int) conversion of a float (truncation, 0x80000000 for NaN/out of range as on x86-64)"
  (if (or (sb-ext:float-nan-p x) (sb-ext:float-infinity-p x) (>= (abs x) 2147483648.0))
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
;; TODO: OBJ/MTL (tinyobj_loader_c), IQM, GLTF (cgltf), VOX and M3D loaders
(defun %unsupported-model-format (file-name)
  (trace-log +log-warning+ "MODEL: [~a] Model file format loading not implemented yet" file-name)
  (make-model))

(defun %load-obj (file-name) (%unsupported-model-format file-name))
(defun %load-iqm (file-name) (%unsupported-model-format file-name))
(defun %load-gltf (file-name) (%unsupported-model-format file-name))
(defun %load-vox (file-name) (%unsupported-model-format file-name))
(defun %load-m3d (file-name) (%unsupported-model-format file-name))
(defun %load-model-animations-iqm (file-name) (declare (ignore file-name)) (values nil 0))
(defun %load-model-animations-gltf (file-name) (declare (ignore file-name)) (values nil 0))
(defun %load-model-animations-m3d (file-name) (declare (ignore file-name)) (values nil 0))
(defun %tinyobj-parse-mtl-file (file-name) (declare (ignore file-name)) (values nil :error))
(defun %process-materials-obj (materials mats count) (declare (ignore materials mats count)) nil)

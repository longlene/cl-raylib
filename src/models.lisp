(in-package #:cl-raylib)

;;; 3D Models and Meshes System
;;; This module provides comprehensive 3D model loading, manipulation, and rendering

;;; Note: raylib doesn't have a standalone Vertex struct
;;; This is a cl-raylib internal helper structure for convenience
(defstruct vertex
  "Internal vertex structure for cl-raylib convenience (not in original raylib)"
  (position (vec3 0.0 0.0 0.0) :type vec3)
  (normal (vec3 0.0 1.0 0.0) :type vec3) 
  (texcoord (vec2 0.0 0.0) :type vec2)
  (color +white+ :type list))

;;; Global model management
(defvar *model-registry* (make-hash-table) "Registry of loaded models")
(defvar *mesh-registry* (make-hash-table) "Registry of loaded meshes")

;;; Mesh creation functions

(defun create-mesh (vertices indices)
  "Create a mesh from vertex and index data"
  (let ((mesh (make-mesh :vertices vertices
                         :indices indices
                         :vertex-count (length vertices)
                         :triangle-count (/ (length indices) 3))))
    (calculate-mesh-bounds mesh)
    mesh))

(defun create-vertex (position &optional normal texcoord color)
  "Create a vertex with optional normal, texcoord, and color"
  (make-vertex :position position
               :normal (or normal (vec3 0.0 1.0 0.0))
               :texcoord (or texcoord (vec2 0.0 0.0))
               :color (or color +white+)))

;;; Mesh generation functions

(defun gen-mesh-cube (width height length)
  "Generate a cube mesh"
  (let ((vertices (list
                   ;; Front face
                   (create-vertex (vec3 -0.5 -0.5  0.5) (vec3 0.0 0.0 1.0) (vec2 0.0 0.0))
                   (create-vertex (vec3  0.5 -0.5  0.5) (vec3 0.0 0.0 1.0) (vec2 1.0 0.0))
                   (create-vertex (vec3  0.5  0.5  0.5) (vec3 0.0 0.0 1.0) (vec2 1.0 1.0))
                   (create-vertex (vec3 -0.5  0.5  0.5) (vec3 0.0 0.0 1.0) (vec2 0.0 1.0))
                   
                   ;; Back face
                   (create-vertex (vec3 -0.5 -0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 1.0 0.0))
                   (create-vertex (vec3 -0.5  0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 1.0 1.0))
                   (create-vertex (vec3  0.5  0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 0.0 1.0))
                   (create-vertex (vec3  0.5 -0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 0.0 0.0))
                   
                   ;; Top face
                   (create-vertex (vec3 -0.5  0.5 -0.5) (vec3 0.0 1.0 0.0) (vec2 0.0 1.0))
                   (create-vertex (vec3 -0.5  0.5  0.5) (vec3 0.0 1.0 0.0) (vec2 0.0 0.0))
                   (create-vertex (vec3  0.5  0.5  0.5) (vec3 0.0 1.0 0.0) (vec2 1.0 0.0))
                   (create-vertex (vec3  0.5  0.5 -0.5) (vec3 0.0 1.0 0.0) (vec2 1.0 1.0))
                   
                   ;; Bottom face
                   (create-vertex (vec3 -0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 1.0))
                   (create-vertex (vec3  0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 0.0 1.0))
                   (create-vertex (vec3  0.5 -0.5  0.5) (vec3 0.0 -1.0 0.0) (vec2 0.0 0.0))
                   (create-vertex (vec3 -0.5 -0.5  0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                   
                   ;; Right face
                   (create-vertex (vec3  0.5 -0.5 -0.5) (vec3 1.0 0.0 0.0) (vec2 1.0 0.0))
                   (create-vertex (vec3  0.5  0.5 -0.5) (vec3 1.0 0.0 0.0) (vec2 1.0 1.0))
                   (create-vertex (vec3  0.5  0.5  0.5) (vec3 1.0 0.0 0.0) (vec2 0.0 1.0))
                   (create-vertex (vec3  0.5 -0.5  0.5) (vec3 1.0 0.0 0.0) (vec2 0.0 0.0))
                   
                   ;; Left face
                   (create-vertex (vec3 -0.5 -0.5 -0.5) (vec3 -1.0 0.0 0.0) (vec2 0.0 0.0))
                   (create-vertex (vec3 -0.5 -0.5  0.5) (vec3 -1.0 0.0 0.0) (vec2 1.0 0.0))
                   (create-vertex (vec3 -0.5  0.5  0.5) (vec3 -1.0 0.0 0.0) (vec2 1.0 1.0))
                   (create-vertex (vec3 -0.5  0.5 -0.5) (vec3 -1.0 0.0 0.0) (vec2 0.0 1.0))))
        
        (indices (list
                  ;; Front face
                  0 1 2   2 3 0
                  ;; Back face
                  4 5 6   6 7 4
                  ;; Top face
                  8 9 10  10 11 8
                  ;; Bottom face
                  12 13 14  14 15 12
                  ;; Right face
                  16 17 18  18 19 16
                  ;; Left face
                  20 21 22  22 23 20)))
    
    ;; Scale vertices
    (loop for vertex in vertices do
      (setf (vertex-position vertex)
            (vec3 (* (vx3 (vertex-position vertex)) width)
                  (* (vy3 (vertex-position vertex)) height)
                  (* (vz3 (vertex-position vertex)) length))))
    
    (create-mesh vertices indices)))

(defun gen-mesh-sphere (radius rings slices)
  "Generate a sphere mesh"
  (let ((vertices nil)
        (indices nil))
    
    ;; Generate vertices
    (loop for i from 0 to rings do
      (let* ((lat (* (/ i rings) +pi+))
             (sin-lat (sin lat))
             (cos-lat (cos lat)))
        (loop for j from 0 to slices do
          (let* ((lon (* (/ j slices) 2.0 +pi+))
                 (sin-lon (sin lon))
                 (cos-lon (cos lon))
                 (x (* radius sin-lat cos-lon))
                 (y (* radius cos-lat))
                 (z (* radius sin-lat sin-lon))
                 (u (/ j (float slices)))
                 (v (/ i (float rings))))
            (push (create-vertex (vec3 x y z)
                                 (vec3 (/ x radius) (/ y radius) (/ z radius))
                                 (vec2 u v))
                  vertices)))))
    
    ;; Generate indices
    (loop for i from 0 below rings do
      (loop for j from 0 below slices do
        (let ((first-vertex (+ (* i (1+ slices)) j))
              (second-vertex (+ first-vertex slices 1)))
          ;; First triangle
          (push first-vertex indices)
          (push second-vertex indices)
          (push (1+ first-vertex) indices)
          
          ;; Second triangle
          (push second-vertex indices)
          (push (1+ second-vertex) indices)
          (push (1+ first-vertex) indices))))
    
    (create-mesh (reverse vertices) (reverse indices))))

(defun gen-mesh-plane (width length res-x res-z)
  "Generate a plane mesh with specified resolution"
  (let ((vertices nil)
        (indices nil)
        (half-width (/ width 2.0))
        (half-length (/ length 2.0)))
    
    ;; Generate vertices
    (loop for i from 0 to res-z do
      (loop for j from 0 to res-x do
        (let* ((x (- (* (/ j (float res-x)) width) half-width))
               (z (- (* (/ i (float res-z)) length) half-length))
               (u (/ j (float res-x)))
               (v (/ i (float res-z))))
          (push (create-vertex (vec3 x 0.0 z)
                               (vec3 0.0 1.0 0.0)
                               (vec2 u v))
                vertices))))
    
    ;; Generate indices
    (loop for i from 0 below res-z do
      (loop for j from 0 below res-x do
        (let ((vertex-index (+ (* i (1+ res-x)) j)))
          ;; First triangle
          (push vertex-index indices)
          (push (+ vertex-index res-x 1) indices)
          (push (1+ vertex-index) indices)
          
          ;; Second triangle
          (push (1+ vertex-index) indices)
          (push (+ vertex-index res-x 1) indices)
          (push (+ vertex-index res-x 2) indices))))
    
    (create-mesh (reverse vertices) (reverse indices))))

;;; Mesh manipulation functions

(defun calculate-mesh-bounds (mesh)
  "Calculate bounding box for mesh"
  (when (mesh-vertices mesh)
    (let* ((first-vertex (first (mesh-vertices mesh)))
           (first-pos (vertex-position first-vertex))
           (min-point first-pos)
           (max-point first-pos))
      
      (loop for vertex in (mesh-vertices mesh) do
        (let ((pos (vertex-position vertex)))
          (setf (vx3 min-point) (min (vx3 min-point) (vx3 pos)))
          (setf (vy3 min-point) (min (vy3 min-point) (vy3 pos)))
          (setf (vz3 min-point) (min (vz3 min-point) (vz3 pos)))
          (setf (vx3 max-point) (max (vx3 max-point) (vx3 pos)))
          (setf (vy3 max-point) (max (vy3 max-point) (vy3 pos)))
          (setf (vz3 max-point) (max (vz3 max-point) (vz3 pos)))))
      
      (make-bounding-box :min min-point :max max-point))))

(defun mesh-calculate-normals (mesh)
  "Calculate vertex normals for mesh (flat shading)"
  (let ((vertices (mesh-vertices mesh))
        (indices (mesh-indices mesh)))
    
    ;; Reset all normals
    (loop for vertex in vertices do
      (setf (vertex-normal vertex) (vec3 0.0 0.0 0.0)))
    
    ;; Calculate face normals and accumulate to vertices
    (loop for i from 0 below (length indices) by 3 do
      (let* ((i1 (nth i indices))
             (i2 (nth (1+ i) indices))
             (i3 (nth (+ i 2) indices))
             (v1 (nth i1 vertices))
             (v2 (nth i2 vertices))
             (v3 (nth i3 vertices))
             (edge1 (v- (vertex-position v2) (vertex-position v1)))
             (edge2 (v- (vertex-position v3) (vertex-position v1)))
             (face-normal (vunit (vc edge1 edge2))))
        
        ;; Add face normal to each vertex normal
        (setf (vertex-normal v1) (v+ (vertex-normal v1) face-normal))
        (setf (vertex-normal v2) (v+ (vertex-normal v2) face-normal))
        (setf (vertex-normal v3) (v+ (vertex-normal v3) face-normal))))
    
    ;; Normalize all vertex normals
    (loop for vertex in vertices do
      (setf (vertex-normal vertex) (vunit (vertex-normal vertex))))
    
    mesh))

(defun mesh-transform (mesh transform-matrix)
  "Transform mesh vertices by matrix"
  (loop for vertex in (mesh-vertices mesh) do
    (setf (vertex-position vertex)
          (m* transform-matrix (vertex-position vertex)))
    ;; Transform normals (without translation)
    (let ((normal-matrix (mtranspose (minv transform-matrix))))
      (setf (vertex-normal vertex)
            (vunit (m* normal-matrix (vertex-normal vertex))))))
  mesh)

;;; Model creation and manipulation

(defun create-model (meshes materials)
  "Create a model from meshes and materials"
  (make-model :meshes meshes
              :materials materials
              :mesh-count (length meshes)
              :material-count (length materials)))

;;; Material functions

(defun create-material (&key shader)
  "Create a material with optional shader"
  (let ((material (make-material :shader shader)))
    ;; Initialize default material maps
    (loop for i from 0 below +max-material-maps+ do
      (setf (aref (material-maps material) i)
            (make-material-map :color +white+ :value 1.0)))
    material))

(defun load-material-default ()
  "Load default material (matches raylib LoadMaterialDefault)"
  (create-material))

(defun set-material-texture (material map-type texture)
  "Set texture for a material map type (matches raylib SetMaterialTexture)"
  (when (and material (>= map-type 0) (< map-type +max-material-maps+))
    (let ((material-map (aref (material-maps material) map-type)))
      (unless material-map
        (setf material-map (make-material-map :color +white+ :value 1.0))
        (setf (aref (material-maps material) map-type) material-map))
      (setf (material-map-texture material-map) texture)))
  material)

(defun get-material-texture (material map-type)
  "Get texture from material map type"
  (when (and material (>= map-type 0) (< map-type +max-material-maps+))
    (let ((material-map (aref (material-maps material) map-type)))
      (when material-map
        (material-map-texture material-map)))))

(defun set-material-color (material map-type color)
  "Set color for a material map type"
  (when (and material (>= map-type 0) (< map-type +max-material-maps+))
    (let ((material-map (aref (material-maps material) map-type)))
      (unless material-map
        (setf material-map (make-material-map :color +white+ :value 1.0))
        (setf (aref (material-maps material) map-type) material-map))
      (setf (material-map-color material-map) color)))
  material)

(defun get-material-color (material map-type)
  "Get color from material map type"
  (when (and material (>= map-type 0) (< map-type +max-material-maps+))
    (let ((material-map (aref (material-maps material) map-type)))
      (if material-map
          (material-map-color material-map)
          +white+))))

(defun material-set-texture (material texture)
  "Set diffuse texture for material (legacy compatibility function)"
  (set-material-texture material +material-map-diffuse+ texture))

(defun is-material-valid (material)
  "Check if a material is valid (matches raylib IsMaterialValid)"
  (and material (material-p material)))

(defun unload-material (material)
  "Unload material from GPU memory (matches raylib UnloadMaterial)"
  (when (is-material-valid material)
    ;; Unload shader if it exists
    (when (material-shader material)
      ;; TODO: Implement shader unloading
      )
    ;; Clear material maps
    (loop for i from 0 below +max-material-maps+ do
      (setf (aref (material-maps material) i) nil))
    ;; Reset parameters
    (loop for i from 0 below 4 do
      (setf (aref (material-params material) i) 0.0))))

;;; Mesh GPU upload functions

(defun upload-mesh (mesh)
  "Upload mesh data to GPU (VBO/VAO)"
  (unless (mesh-uploaded mesh)
    ;; Generate VAO
    (setf (mesh-vao mesh) (gen-vertex-array))
    (bind-vertex-array (mesh-vao mesh))
    
    ;; Generate and upload vertex data
    (setf (mesh-vbo-vertices mesh) (gl:gen-buffer))
    (%gl:bind-buffer :array-buffer (mesh-vbo-vertices mesh))
    
    ;; Interleave vertex data: position(3) + normal(3) + texcoord(2) + color(4)
    (let ((vertex-data (make-array (* (mesh-vertex-count mesh) 12) :element-type 'single-float)))
      (loop for i from 0 below (mesh-vertex-count mesh) do
        (let* ((vertex (nth i (mesh-vertices mesh)))
               (base-index (* i 12))
               (pos (vertex-position vertex))
               (normal (vertex-normal vertex))
               (texcoord (vertex-texcoord vertex))
               (color (color-normalize (vertex-color vertex))))
          
          ;; Position (3 floats)
          (setf (aref vertex-data base-index) (vx3 pos))
          (setf (aref vertex-data (+ base-index 1)) (vy3 pos))
          (setf (aref vertex-data (+ base-index 2)) (vz3 pos))
          
          ;; Normal (3 floats)
          (setf (aref vertex-data (+ base-index 3)) (vx3 normal))
          (setf (aref vertex-data (+ base-index 4)) (vy3 normal))
          (setf (aref vertex-data (+ base-index 5)) (vz3 normal))
          
          ;; Texture coordinates (2 floats)
          (setf (aref vertex-data (+ base-index 6)) (vx2 texcoord))
          (setf (aref vertex-data (+ base-index 7)) (vy2 texcoord))
          
          ;; Color (4 floats)
          (setf (aref vertex-data (+ base-index 8)) (first color))
          (setf (aref vertex-data (+ base-index 9)) (second color))
          (setf (aref vertex-data (+ base-index 10)) (third color))
          (setf (aref vertex-data (+ base-index 11)) (fourth color))))
      
      ;; Upload vertex data using cffi with-pointer-to-vector-data
      (cffi:with-pointer-to-vector-data (vertex-ptr vertex-data)
        (%gl:buffer-data :array-buffer 
                         (* (length vertex-data) (cffi:foreign-type-size :float))
                         vertex-ptr :static-draw)))
    
    ;; Setup vertex attributes
    (let ((stride (* 12 4))) ; 12 floats * 4 bytes per float
      ;; Position attribute
      (gl:enable-vertex-attrib-array 0)
      (vertex-attrib-pointer 0 3 :float nil stride 0)
      
      ;; Normal attribute
      (gl:enable-vertex-attrib-array 1)
      (vertex-attrib-pointer 1 3 :float nil stride (* 3 4))
      
      ;; Texture coordinate attribute
      (gl:enable-vertex-attrib-array 2)
      (vertex-attrib-pointer 2 2 :float nil stride (* 6 4))
      
      ;; Color attribute
      (gl:enable-vertex-attrib-array 3)
      (vertex-attrib-pointer 3 4 :float nil stride (* 8 4)))
    
    ;; Generate and upload index data
    (when (mesh-indices mesh)
      (setf (mesh-vbo-indices mesh) (gl:gen-buffer))
      (%gl:bind-buffer :element-array-buffer (mesh-vbo-indices mesh))
      (let ((index-data (make-array (length (mesh-indices mesh)) :element-type '(unsigned-byte 32))))
        (loop for i from 0 below (length (mesh-indices mesh)) do
          (setf (aref index-data i) (nth i (mesh-indices mesh))))
        ;; Upload index data using cffi with-pointer-to-vector-data
        (cffi:with-pointer-to-vector-data (index-ptr index-data)
          (%gl:buffer-data :element-array-buffer 
                           (* (length index-data) (cffi:foreign-type-size :uint32))
                           index-ptr :static-draw))))
    
    ;; Unbind
    (bind-vertex-array 0)
    (%gl:bind-buffer :array-buffer 0)
    (%gl:bind-buffer :element-array-buffer 0)
    
    (setf (mesh-uploaded mesh) t))
  
  mesh)

(defun unload-mesh (mesh)
  "Unload mesh from GPU memory"
  (when (mesh-uploaded mesh)
    ;; Delete VBOs
    (when (/= (mesh-vbo-vertices mesh) 0)
      (gl:delete-buffers (list (mesh-vbo-vertices mesh)))
      (setf (mesh-vbo-vertices mesh) 0))
    (when (/= (mesh-vbo-indices mesh) 0)
      (gl:delete-buffers (list (mesh-vbo-indices mesh)))
      (setf (mesh-vbo-indices mesh) 0))
    
    ;; Delete VAO
    (when (/= (mesh-vao mesh) 0)
      (delete-vertex-arrays (list (mesh-vao mesh)))
      (setf (mesh-vao mesh) 0))
    
    (setf (mesh-uploaded mesh) nil))
  
  mesh)

;;; Utility functions

(defun is-mesh-valid (mesh)
  "Check if mesh is valid"
  (and mesh
       (mesh-p mesh)
       (> (mesh-vertex-count mesh) 0)))

(defun is-model-valid (model)
  "Check if model is valid"
  (and model
       (model-p model)
       (> (model-mesh-count model) 0)))

;;; Cleanup functions

(defun cleanup-models ()
  "Cleanup all loaded models and meshes"
  (loop for model being the hash-values of *model-registry* do
    (loop for mesh in (model-meshes model) do
      (unload-mesh mesh)))
  (clrhash *model-registry*)
  
  (loop for mesh being the hash-values of *mesh-registry* do
    (unload-mesh mesh))
  (clrhash *mesh-registry*))

;;; Additional mesh manipulation functions (from rmodels.c)

(defun draw-mesh (mesh material transform)
  "Draw a 3D mesh with material and transform"
  (when (and (is-mesh-valid mesh) (mesh-uploaded mesh))
    ;; Set up material properties
    (let* ((diffuse-map (when material (aref (material-maps material) +material-map-diffuse+)))
           (diffuse-color (if diffuse-map (material-map-color diffuse-map) +white+))
           (diffuse-texture (when diffuse-map (material-map-texture diffuse-map))))
      
      ;; Apply transform matrix
      (gl:with-pushed-matrix
        ;; Apply transformation
        (gl:mult-matrix (marr4 transform))
        
        ;; Bind texture if available
        (when diffuse-texture
          (gl:enable :texture-2d)
          (gl:bind-texture :texture-2d (texture-id diffuse-texture)))
        
        ;; Set material color
        (gl:color (/ (first diffuse-color) 255.0)
                  (/ (second diffuse-color) 255.0)
                  (/ (third diffuse-color) 255.0)
                  (/ (fourth diffuse-color) 255.0))
        
        ;; Bind VAO and draw
        (bind-vertex-array (mesh-vao mesh))
        
        ;; Draw elements if indices exist, otherwise draw arrays
        (if (mesh-indices mesh)
            (%gl:draw-elements :triangles (length (mesh-indices mesh)) :unsigned-int 0)
            (gl:draw-arrays :triangles 0 (mesh-vertex-count mesh)))
        
        (bind-vertex-array 0)
        
        ;; Unbind texture
        (when diffuse-texture
          (gl:bind-texture :texture-2d 0)
          (gl:disable :texture-2d))))))

(defun draw-mesh-instanced (mesh material transforms instances)
  "Draw mesh multiple times with different transforms"
  (loop for i from 0 below instances do
    (when (< i (length transforms))
      (draw-mesh mesh material (nth i transforms)))))

(defun update-mesh-buffer (mesh index data data-size offset)
  "Update mesh vertex buffer data"
  (declare (ignore index data data-size offset))
  ;; Simplified implementation - would need proper buffer update logic
  (when (mesh-uploaded mesh)
    (%gl:bind-buffer :array-buffer (mesh-vbo-vertices mesh))
    ;; Buffer update logic would go here
    (%gl:bind-buffer :array-buffer 0)))

(defun get-mesh-bounding-box (mesh)
  "Get bounding box for mesh"
  (calculate-mesh-bounds mesh))

(defun gen-mesh-tangents (mesh)
  "Generate tangents for mesh (simplified implementation)"
  ;; This would calculate tangent vectors for normal mapping
  ;; For now, just ensure the mesh is marked as modified
  (when mesh
    (setf (mesh-uploaded mesh) nil)) ; Mark for re-upload
  mesh)

;;; Advanced mesh generation functions

(defun gen-mesh-poly (sides radius)
  "Generate regular polygon mesh"
  (let* ((vertices '())
         (indices '())
         (angle-step (/ (* 2.0 +pi+) sides)))
    
    ;; Center vertex
    (push (create-vertex (vec3 0.0 0.0 0.0) (vec3 0.0 1.0 0.0) (vec2 0.5 0.5) +white+) vertices)
    
    ;; Perimeter vertices
    (loop for i from 0 below sides do
      (let ((angle (* i angle-step)))
        (push (create-vertex 
               (vec3 (* radius (cos angle)) 0.0 (* radius (sin angle)))
               (vec3 0.0 1.0 0.0)
               (vec2 (+ 0.5 (* 0.5 (cos angle))) (+ 0.5 (* 0.5 (sin angle))))
               +white+) vertices)))
    
    ;; Generate triangle indices
    (loop for i from 0 below sides do
      (let ((next (mod (1+ i) sides)))
        (push 0 indices)           ; Center
        (push (1+ i) indices)     ; Current vertex
        (push (1+ next) indices)))  ; Next vertex
    
    (create-mesh (reverse vertices) (reverse indices))))

(defun gen-mesh-hemisphere (radius rings slices)
  "Generate hemisphere mesh"
  ;; Simplified implementation - generate half of a sphere
  (let* ((vertices '())
         (indices '())
         (ring-step (/ +pi+ (* 2.0 rings)))  ; Only half sphere
         (slice-step (/ (* 2.0 +pi+) slices)))
    
    ;; Generate vertices
    (loop for ring from 0 to rings do
      (let ((ring-angle (* ring ring-step)))
        (loop for slice from 0 to slices do
          (let ((slice-angle (* slice slice-step)))
            (push (create-vertex
                   (vec3 (* radius (sin ring-angle) (cos slice-angle))
                         (* radius (cos ring-angle))
                         (* radius (sin ring-angle) (sin slice-angle)))
                   (vec3 (sin ring-angle) (cos ring-angle) (sin slice-angle))
                   (vec2 (/ slice slices) (/ ring rings))
                   +white+) vertices)))))
    
    ;; Generate indices (simplified)
    (loop for ring from 0 below rings do
      (loop for slice from 0 below slices do
        (let ((current (+ (* ring (1+ slices)) slice))
              (next (+ (* ring (1+ slices)) (1+ slice)))
              (below (+ (* (1+ ring) (1+ slices)) slice))
              (below-next (+ (* (1+ ring) (1+ slices)) (1+ slice))))
          
          ;; First triangle
          (push current indices)
          (push below indices)
          (push next indices)
          
          ;; Second triangle
          (push next indices)
          (push below indices)
          (push below-next indices))))
    
    (create-mesh (reverse vertices) (reverse indices))))

(defun gen-mesh-cylinder (radius height slices)
  "Generate cylinder mesh"
  (let* ((vertices '())
         (indices '())
         (angle-step (/ (* 2.0 +pi+) slices)))
    
    ;; Bottom center
    (push (create-vertex (vec3 0.0 (- (/ height 2.0)) 0.0) (vec3 0.0 -1.0 0.0) (vec2 0.5 0.5) +white+) vertices)
    ;; Top center  
    (push (create-vertex (vec3 0.0 (/ height 2.0) 0.0) (vec3 0.0 1.0 0.0) (vec2 0.5 0.5) +white+) vertices)
    
    ;; Side vertices (bottom and top rings)
    (loop for i from 0 below slices do
      (let ((angle (* i angle-step))
            (x (* radius (cos angle)))
            (z (* radius (sin angle))))
        ;; Bottom ring
        (push (create-vertex 
               (vec3 x (- (/ height 2.0)) z)
               (vec3 (cos angle) 0.0 (sin angle))
               (vec2 (/ i slices) 0.0)
               +white+) vertices)
        ;; Top ring
        (push (create-vertex
               (vec3 x (/ height 2.0) z)
               (vec3 (cos angle) 0.0 (sin angle))
               (vec2 (/ i slices) 1.0)
               +white+) vertices)))
    
    ;; Generate indices for caps and sides
    (loop for i from 0 below slices do
      (let ((next (mod (1+ i) slices)))
        ;; Bottom cap
        (push 0 indices)
        (push (+ 2 (* 2 next)) indices)
        (push (+ 2 (* 2 i)) indices)
        
        ;; Top cap
        (push 1 indices)
        (push (+ 3 (* 2 i)) indices)
        (push (+ 3 (* 2 next)) indices)
        
        ;; Side quad (two triangles)
        (let ((bottom-current (+ 2 (* 2 i)))
              (bottom-next (+ 2 (* 2 next)))
              (top-current (+ 3 (* 2 i)))
              (top-next (+ 3 (* 2 next))))
          
          ;; First triangle
          (push bottom-current indices)
          (push top-current indices)
          (push bottom-next indices)
          
          ;; Second triangle
          (push bottom-next indices)
          (push top-current indices)
          (push top-next indices))))
    
    (create-mesh (reverse vertices) (reverse indices))))

(defun gen-mesh-cone (radius height slices)
  "Generate cone mesh"
  (let* ((vertices '())
         (indices '())
         (angle-step (/ (* 2.0 +pi+) slices)))
    
    ;; Bottom center
    (push (create-vertex (vec3 0.0 0.0 0.0) (vec3 0.0 -1.0 0.0) (vec2 0.5 0.5) +white+) vertices)
    ;; Top point
    (push (create-vertex (vec3 0.0 height 0.0) (vec3 0.0 1.0 0.0) (vec2 0.5 0.0) +white+) vertices)
    
    ;; Bottom ring vertices
    (loop for i from 0 below slices do
      (let ((angle (* i angle-step)))
        (push (create-vertex
               (vec3 (* radius (cos angle)) 0.0 (* radius (sin angle)))
               (vec3 (cos angle) 0.0 (sin angle))
               (vec2 (+ 0.5 (* 0.5 (cos angle))) (+ 0.5 (* 0.5 (sin angle))))
               +white+) vertices)))
    
    ;; Generate indices
    (loop for i from 0 below slices do
      (let ((next (mod (1+ i) slices)))
        ;; Bottom cap
        (push 0 indices)
        (push (+ 2 next) indices)
        (push (+ 2 i) indices)
        
        ;; Side triangle
        (push 1 indices)
        (push (+ 2 i) indices)
        (push (+ 2 next) indices)))
    
    (create-mesh (reverse vertices) (reverse indices))))

(defun gen-mesh-torus (radius size rad-seg sides)
  "Generate torus mesh"
  (let* ((vertices '())
         (indices '())
         (rad-step (/ (* 2.0 +pi+) rad-seg))
         (side-step (/ (* 2.0 +pi+) sides)))
    
    ;; Generate vertices
    (loop for i from 0 below rad-seg do
      (let ((rad-angle (* i rad-step)))
        (loop for j from 0 below sides do
          (let ((side-angle (* j side-step))
                (center-x (* radius (cos rad-angle)))
                (center-z (* radius (sin rad-angle))))
            (push (create-vertex
                   (vec3 (+ center-x (* size (cos side-angle) (cos rad-angle)))
                         (* size (sin side-angle))
                         (+ center-z (* size (cos side-angle) (sin rad-angle))))
                   (vec3 (* (cos side-angle) (cos rad-angle))
                         (sin side-angle)
                         (* (cos side-angle) (sin rad-angle)))
                   (vec2 (/ i rad-seg) (/ j sides))
                   +white+) vertices)))))
    
    ;; Generate indices
    (loop for i from 0 below rad-seg do
      (loop for j from 0 below sides do
        (let ((current (+ (* i sides) j))
              (next-i (mod (1+ i) rad-seg))
              (next-j (mod (1+ j) sides)))
          
          (let ((v1 current)
                (v2 (+ (* i sides) next-j))
                (v3 (+ (* next-i sides) j))
                (v4 (+ (* next-i sides) next-j)))
            
            ;; First triangle
            (push v1 indices)
            (push v3 indices)
            (push v2 indices)
            
            ;; Second triangle
            (push v2 indices)
            (push v3 indices)
            (push v4 indices)))))
    
    (create-mesh (reverse vertices) (reverse indices))))

;;; Model drawing functions (matching raylib rmodels.c)

;;; Basic 3D drawing functions
(defun draw-line-3d (start-pos end-pos color)
  "Draw a line in 3D world space"
  (let ((actual-color (keyword-to-color color)))
    (gl:color (/ (first actual-color) 255.0)
              (/ (second actual-color) 255.0)
              (/ (third actual-color) 255.0)
              (/ (fourth actual-color) 255.0))
    (gl:begin :lines)
    (gl:vertex (vx3 start-pos) (vy3 start-pos) (vz3 start-pos))
    (gl:vertex (vx3 end-pos) (vy3 end-pos) (vz3 end-pos))
    (gl:end)))

(defun draw-point-3d (position color)
  "Draw a point in 3D space, actually a small line"
  (let ((actual-color (keyword-to-color color)))
    (gl:color (/ (first actual-color) 255.0)
              (/ (second actual-color) 255.0)
              (/ (third actual-color) 255.0)
              (/ (fourth actual-color) 255.0))
    (gl:point-size 4.0)
    (gl:begin :points)
    (gl:vertex (vx3 position) (vy3 position) (vz3 position))
    (gl:end)))

(defun draw-triangle-3d (v1 v2 v3 color)
  "Draw a color-filled triangle (vertex in counter-clockwise order!)"
  (let ((actual-color (keyword-to-color color)))
    (gl:color (/ (first actual-color) 255.0)
              (/ (second actual-color) 255.0)
              (/ (third actual-color) 255.0)
              (/ (fourth actual-color) 255.0))
    (gl:begin :triangles)
    (gl:vertex (vx3 v1) (vy3 v1) (vz3 v1))
    (gl:vertex (vx3 v2) (vy3 v2) (vz3 v2))
    (gl:vertex (vx3 v3) (vy3 v3) (vz3 v3))
    (gl:end)))


(defun draw-grid (slices spacing)
  "Draw a 3D grid centered at (0, 0, 0) - simplified implementation"
  (let ((half-slices (floor slices 2)))
    (gl:with-primitive :lines
      (loop for i from (- half-slices) to half-slices do
        ;; Set color: darker for center lines (i=0), lighter for others
        (if (= i 0)
            (gl:color 0.5 0.5 0.5 1.0)    ; Darker gray for center lines
            (gl:color 0.75 0.75 0.75 1.0)) ; Lighter gray for other lines
        
        ;; Draw vertical lines (parallel to Z axis)
        (gl:vertex (* i spacing) 0.0 (* (- half-slices) spacing))
        (gl:vertex (* i spacing) 0.0 (* half-slices spacing))
        
        ;; Draw horizontal lines (parallel to X axis)  
        (gl:vertex (* (- half-slices) spacing) 0.0 (* i spacing))
        (gl:vertex (* half-slices spacing) 0.0 (* i spacing))))))

;;; Billboard drawing functions (from rmodels.c)

(defun draw-billboard (camera texture position scale tint)
  "Draw a billboard texture facing the camera"
  ;; Create source rectangle for entire texture
  (let ((source (list 0.0 0.0 (texture-width texture) (texture-height texture))))
    (draw-billboard-rec camera texture source position 
                        (list (* scale (abs (/ (texture-width texture) (texture-height texture))))
                              scale) 
                        tint)))

(defun draw-billboard-rec (camera texture source position size tint)
  "Draw a billboard with a specified texture rectangle"
  ;; Billboard locked on Y axis
  (let ((up (vec3 0.0 1.0 0.0)))
    (draw-billboard-pro camera texture source position up size 
                        (list (/ (first size) 2.0) (/ (second size) 2.0))
                        0.0 tint)))

(defun draw-billboard-pro (camera texture source position up size origin rotation tint)
  "Draw a billboard with full control parameters"
  (declare (ignore rotation)) ; TODO: Implement rotation functionality
  (let* (;; Calculate view matrix directly
         (view-matrix (get-camera-matrix camera))
         
         ;; Extract right vector from view matrix (first column)
         (right (mcol view-matrix 0))
         (right-scaled (vscale right (first size)))
         (up-scaled (vscale up (second size)))
         
         ;; Handle negative scaling (flip billboard)
         (actual-right (if (< (first size) 0.0)
                          (v- right-scaled)
                          right-scaled))
         (actual-up (if (< (second size) 0.0)
                       (v- up-scaled)
                       up-scaled))
         
         ;; Calculate origin offset
         (right-norm (vunit actual-right))
         (up-norm (vunit actual-up))
         (origin-3d (v+ (vscale right-norm (first origin))
                                 (vscale up-norm (second origin))))
         
         ;; Calculate four corners of billboard
         (p0 (vec3))
         (p1 actual-right)
         (p2 (v+ actual-up actual-right))
         (p3 actual-up)
         
         ;; Apply rotation if needed
         (points (list p0 p1 p2 p3)))
    
    ;; Apply origin offset and position to all points
    (setf points (mapcar (lambda (p)
                          (v+ position
                                       (v- p origin-3d)))
                        points))
    
    ;; Calculate texture coordinates
    (let* ((tex-width (texture-width texture))
           (tex-height (texture-height texture))
           (s0 (/ (first source) tex-width))
           (t0 (/ (+ (second source) (fourth source)) tex-height))
           (s1 (/ (+ (first source) (third source)) tex-width))
           (t1 (/ (second source) tex-height))
           (texcoords (list (vec2 s0 t0)
                           (vec2 s1 t0)
                           (vec2 s1 t1)
                           (vec2 s0 t1))))
      
      ;; Render billboard quad
      (gl:enable :texture-2d)
      (gl:bind-texture :texture-2d (texture-id texture))
      (gl:begin :quads)
      
      (set-gl-color tint)
      (loop for i from 0 to 3 do
        (let ((point (nth i points))
              (texcoord (nth i texcoords)))
          (gl:tex-coord (vx2 texcoord) (vy2 texcoord))
          (gl:vertex (vx3 point) (vy3 point) (vz3 point))))
      
      (gl:end)
      (gl:bind-texture :texture-2d 0)
      (gl:disable :texture-2d))))

;;; Additional mesh generation functions (completing rmodels.c)

(defun gen-mesh-knot (radius size rad-seg sides)
  "Generate trefoil knot mesh"
  (let* ((vertices '())
         (indices '())
         (rad-step (/ (* 2.0 +pi+) rad-seg))
         (side-step (/ (* 2.0 +pi+) sides)))
    
    ;; Generate vertices using trefoil knot parametric equations
    (loop for i from 0 below rad-seg do
      (let* ((t-param (* 3.0 i rad-step))  ; Parameter for knot curve
             ;; Trefoil knot parametric equations
             (knot-x (* radius (+ (sin t-param) (* 2.0 (sin (* 2.0 t-param))))))
             (knot-y (* radius (- (cos t-param) (* 2.0 (cos (* 2.0 t-param))))))
             (knot-z (* radius (* -1.0 (sin (* 3.0 t-param)))))
             ;; Calculate tangent for normal computation
             (dx (* radius (+ (cos t-param) (* 4.0 (cos (* 2.0 t-param))))))
             (dy (* radius (+ (sin t-param) (* 4.0 (sin (* 2.0 t-param))))))
             (dz (* radius (* -3.0 (cos (* 3.0 t-param)))))
             ;; Normalize tangent
             (tangent-length (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
             (tx (/ dx tangent-length))
             (ty (/ dy tangent-length))
             (tz (/ dz tangent-length))
             ;; Create perpendicular vectors for tube cross-section
             (nx (if (> (abs tz) 0.1) 0.0 1.0))
             (ny (if (> (abs tz) 0.1) 1.0 0.0))
             (nz (if (> (abs tz) 0.1) tz 0.0))
             ;; Normal vector perpendicular to tangent
             (norm-length (sqrt (+ (* nx nx) (* ny ny) (* nz nz))))
             (normal-x (/ nx norm-length))
             (normal-y (/ ny norm-length))
             (normal-z (/ nz norm-length))
             ;; Binormal for complete frame
             (bx (- (* ty normal-z) (* tz normal-y)))
             (by (- (* tz normal-x) (* tx normal-z)))
             (bz (- (* tx normal-y) (* ty normal-x))))
        
        ;; Generate tube cross-section
        (loop for j from 0 below sides do
          (let* ((angle (* j side-step))
                 (cos-a (cos angle))
                 (sin-a (sin angle))
                 ;; Position on tube surface
                 (tube-x (+ knot-x (* size (+ (* cos-a normal-x) (* sin-a bx)))))
                 (tube-y (+ knot-y (* size (+ (* cos-a normal-y) (* sin-a by)))))
                 (tube-z (+ knot-z (* size (+ (* cos-a normal-z) (* sin-a bz)))))
                 ;; Surface normal
                 (surf-nx (+ (* cos-a normal-x) (* sin-a bx)))
                 (surf-ny (+ (* cos-a normal-y) (* sin-a by)))
                 (surf-nz (+ (* cos-a normal-z) (* sin-a bz))))
            
            (push (create-vertex
                   (vec3 tube-x tube-y tube-z)
                   (vec3 surf-nx surf-ny surf-nz)
                   (vec2 (/ i rad-seg) (/ j sides))
                   +white+) vertices)))))
    
    ;; Generate indices for tube surface
    (loop for i from 0 below rad-seg do
      (loop for j from 0 below sides do
        (let* ((current (+ (* i sides) j))
               (next-i (mod (1+ i) rad-seg))
               (next-j (mod (1+ j) sides))
               (v1 current)
               (v2 (+ (* i sides) next-j))
               (v3 (+ (* next-i sides) j))
               (v4 (+ (* next-i sides) next-j)))
          
          ;; Two triangles per quad
          (push v1 indices)
          (push v3 indices)
          (push v2 indices)
          
          (push v2 indices)
          (push v3 indices)
          (push v4 indices))))
    
    (create-mesh (reverse vertices) (reverse indices))))

(defun gen-mesh-heightmap (heightmap size)
  "Generate mesh from heightmap data"
  (unless (and heightmap (> (image-width heightmap) 1) (> (image-height heightmap) 1))
    (error "Invalid heightmap for mesh generation"))
  
  (let* ((map-x (image-width heightmap))
         (map-z (image-height heightmap))
         (vertices '())
         (indices '())
         (scale-x (/ (first size) (1- map-x)))
         (scale-y (second size))
         (scale-z (/ (third size) (1- map-z))))
    
    ;; Generate vertices from heightmap
    (loop for z from 0 below map-z do
      (loop for x from 0 below map-x do
        (let* ((pixel-index (+ (* z map-x) x))
               ;; Get height from heightmap (assuming grayscale)
               (height-value (if (< pixel-index (length (image-data heightmap)))
                               (/ (aref (image-data heightmap) pixel-index) 255.0)
                               0.0))
               (world-x (* x scale-x))
               (world-y (* height-value scale-y))
               (world-z (* z scale-z))
               (tex-u (/ x (1- map-x)))
               (tex-v (/ z (1- map-z))))
          
          (push (create-vertex
                 (vec3 world-x world-y world-z)
                 (vec3 0.0 1.0 0.0)  ; Will be recalculated
                 (vec2 tex-u tex-v)
                 +white+) vertices))))
    
    ;; Generate indices for triangles
    (loop for z from 0 below (1- map-z) do
      (loop for x from 0 below (1- map-x) do
        (let ((i1 (+ (* z map-x) x))
              (i2 (+ (* z map-x) (1+ x)))
              (i3 (+ (* (1+ z) map-x) x))
              (i4 (+ (* (1+ z) map-x) (1+ x))))
          
          ;; First triangle
          (push i1 indices)
          (push i3 indices)
          (push i2 indices)
          
          ;; Second triangle
          (push i2 indices)
          (push i3 indices)
          (push i4 indices))))
    
    (let ((mesh (create-mesh (reverse vertices) (reverse indices))))
      ;; Recalculate normals for proper lighting
      (mesh-calculate-normals mesh)
      mesh)))

(defun gen-mesh-cubicmap (cubicmap cube-size)
  "Generate mesh from cubic map (voxel-based)"
  (unless (and cubicmap (> (image-width cubicmap) 0) (> (image-height cubicmap) 0))
    (error "Invalid cubicmap for mesh generation"))
  
  (let* ((map-width (image-width cubicmap))
         (map-height (image-height cubicmap))
         (vertices '())
         (indices '())
         (vertex-count 0))
    
    ;; Process each pixel in the cubicmap
    (loop for y from 0 below map-height do
      (loop for x from 0 below map-width do
        (let* ((pixel-index (+ (* y map-width) x))
               ;; Check if this pixel represents a solid voxel
               (is-solid (and (< pixel-index (length (image-data cubicmap)))
                             (> (aref (image-data cubicmap) pixel-index) 0))))
          
          (when is-solid
            ;; Generate cube at this position
            (let* ((cube-x (* x (first cube-size)))
                   (cube-y 0.0)
                   (cube-z (* y (third cube-size)))
                   (half-x (/ (first cube-size) 2.0))
                   (half-y (/ (second cube-size) 2.0))
                   (half-z (/ (third cube-size) 2.0)))
              
              ;; Add 8 vertices for cube
              (let ((cube-vertices
                     (list
                      ;; Bottom face
                      (create-vertex (vec3 (- cube-x half-x) (- cube-y half-y) (- cube-z half-z)) (vec3 0.0 -1.0 0.0) (vec2 0.0 0.0) +white+)
                      (create-vertex (vec3 (+ cube-x half-x) (- cube-y half-y) (- cube-z half-z)) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0) +white+)
                      (create-vertex (vec3 (+ cube-x half-x) (- cube-y half-y) (+ cube-z half-z)) (vec3 0.0 -1.0 0.0) (vec2 1.0 1.0) +white+)
                      (create-vertex (vec3 (- cube-x half-x) (- cube-y half-y) (+ cube-z half-z)) (vec3 0.0 -1.0 0.0) (vec2 0.0 1.0) +white+)
                      ;; Top face
                      (create-vertex (vec3 (- cube-x half-x) (+ cube-y half-y) (- cube-z half-z)) (vec3 0.0 1.0 0.0) (vec2 0.0 0.0) +white+)
                      (create-vertex (vec3 (+ cube-x half-x) (+ cube-y half-y) (- cube-z half-z)) (vec3 0.0 1.0 0.0) (vec2 1.0 0.0) +white+)
                      (create-vertex (vec3 (+ cube-x half-x) (+ cube-y half-y) (+ cube-z half-z)) (vec3 0.0 1.0 0.0) (vec2 1.0 1.0) +white+)
                      (create-vertex (vec3 (- cube-x half-x) (+ cube-y half-y) (+ cube-z half-z)) (vec3 0.0 1.0 0.0) (vec2 0.0 1.0) +white+))))
                
                ;; Add vertices to main list
                (setf vertices (append vertices cube-vertices))
                
                ;; Add indices for cube faces (12 triangles)
                (let ((base-index vertex-count))
                  ;; Bottom face (0,1,2) (2,3,0)
                  (push (+ base-index 0) indices) (push (+ base-index 1) indices) (push (+ base-index 2) indices)
                  (push (+ base-index 2) indices) (push (+ base-index 3) indices) (push (+ base-index 0) indices)
                  ;; Top face (4,6,5) (6,4,7)
                  (push (+ base-index 4) indices) (push (+ base-index 6) indices) (push (+ base-index 5) indices)
                  (push (+ base-index 6) indices) (push (+ base-index 4) indices) (push (+ base-index 7) indices)
                  ;; Front face (0,4,5) (5,1,0)
                  (push (+ base-index 0) indices) (push (+ base-index 4) indices) (push (+ base-index 5) indices)
                  (push (+ base-index 5) indices) (push (+ base-index 1) indices) (push (+ base-index 0) indices)
                  ;; Back face (2,6,7) (7,3,2)
                  (push (+ base-index 2) indices) (push (+ base-index 6) indices) (push (+ base-index 7) indices)
                  (push (+ base-index 7) indices) (push (+ base-index 3) indices) (push (+ base-index 2) indices)
                  ;; Left face (3,7,4) (4,0,3)
                  (push (+ base-index 3) indices) (push (+ base-index 7) indices) (push (+ base-index 4) indices)
                  (push (+ base-index 4) indices) (push (+ base-index 0) indices) (push (+ base-index 3) indices)
                  ;; Right face (1,5,6) (6,2,1)
                  (push (+ base-index 1) indices) (push (+ base-index 5) indices) (push (+ base-index 6) indices)
                  (push (+ base-index 6) indices) (push (+ base-index 2) indices) (push (+ base-index 1) indices))
                
                (incf vertex-count 8))))))
    
    (create-mesh vertices (reverse indices)))))

;;; Model loading and rendering functions

(defun load-model (filename)
  "Load model from file (simplified implementation)"
  (declare (ignore filename)) ; TODO: Implement actual file loading
  ;; This would need full implementation for different formats
  ;; For now, return a default model
  (let ((default-mesh (gen-mesh-cube 1.0 1.0 1.0))
        (default-material (load-material-default)))
    (upload-mesh default-mesh)
    (create-model (list default-mesh) (list default-material))))

(defun load-model-from-mesh (mesh)
  "Create model from single mesh"
  (let ((material (make-material)))
    (upload-mesh mesh)
    (create-model (list mesh) (list material))))

(defun unload-model (model)
  "Unload model from memory"
  (when (is-model-valid model)
    ;; Unload all meshes
    (loop for mesh in (model-meshes model) do
      (unload-mesh mesh))
    ;; Clear model data
    (setf (model-meshes model) nil)
    (setf (model-materials model) nil)
    (setf (model-mesh-count model) 0)
    (setf (model-material-count model) 0)))

(defun draw-model (model position scale tint)
  "Draw a model (with texture if set)"
  (when (is-model-valid model)
    (let ((transform (m* (mtranslation position)
                         (mscaling (vec3 scale scale scale)))))
      (loop for i from 0 below (model-mesh-count model) do
        (let ((mesh (nth i (model-meshes model)))
              (material (if (< i (model-material-count model))
                           (nth i (model-materials model))
                           (first (model-materials model)))))
          (when material
            ;; Apply material and draw mesh
            (draw-mesh mesh material transform)))))))

(defun draw-model-ex (model position rotation-axis rotation-angle scale tint)
  "Draw a model with extended parameters"
  (when (is-model-valid model)
    (let ((transform (m* (m* (mtranslation position)
                             (mrotation rotation-axis rotation-angle))
                         (mscaling (vec3 scale scale scale)))))
      (loop for i from 0 below (model-mesh-count model) do
        (let ((mesh (nth i (model-meshes model)))
              (material (if (< i (model-material-count model))
                           (nth i (model-materials model))
                           (first (model-materials model)))))
          (when material
            ;; Apply material and draw mesh
            (draw-mesh mesh material transform)))))))

(defun draw-model-wires (model position scale tint)
  "Draw a model wires (wireframe mode)"
  (when (is-model-valid model)
    (gl:polygon-mode :front-and-back :line)
    (draw-model model position scale tint)
    (gl:polygon-mode :front-and-back :fill)))

(defun draw-model-wires-ex (model position rotation-axis rotation-angle scale tint)
  "Draw model wires with extended parameters"
  (when (is-model-valid model)
    (gl:polygon-mode :front-and-back :line)
    (draw-model-ex model position rotation-axis rotation-angle scale tint)
    (gl:polygon-mode :front-and-back :fill)))

;;; Utility helper functions

(defun color-multiply (color1 color2)
  "Multiply two colors component-wise"
  (list (* (first color1) (first color2))
        (* (second color1) (second color2))
        (* (third color1) (third color2))
        (* (fourth color1) (fourth color2))))

(defun matrix4-rotate-axis (axis angle)
  "Create rotation matrix around arbitrary axis"
  ;; Use 3d-matrices mrotation function
  (cond
    ((vec3-p axis) (mrotation axis angle))
    ((and (listp axis) (= (length axis) 3))
     (mrotation (vec3 (first axis) (second axis) (third axis)) angle))
    (t (meye 4))))

;;; Model bounding box functions

(defun get-model-bounding-box (model)
  "Get model bounding box"
  (when (is-model-valid model)
    (let ((min-point (vec3 most-positive-single-float most-positive-single-float most-positive-single-float))
          (max-point (vec3 most-negative-single-float most-negative-single-float most-negative-single-float)))
      
      ;; Calculate combined bounding box from all meshes
      (loop for mesh in (model-meshes model) do
        (let ((mesh-bounds (get-mesh-bounding-box mesh)))
          (when mesh-bounds
            (let ((mesh-min (bounding-box-min mesh-bounds))
                  (mesh-max (bounding-box-max mesh-bounds)))
              (setf (vx3 min-point) (min (vx3 min-point) (vx3 mesh-min)))
              (setf (vy3 min-point) (min (vy3 min-point) (vy3 mesh-min)))
              (setf (vz3 min-point) (min (vz3 min-point) (vz3 mesh-min)))
              (setf (vx3 max-point) (max (vx3 max-point) (vx3 mesh-max)))
              (setf (vy3 max-point) (max (vy3 max-point) (vy3 mesh-max)))
              (setf (vz3 max-point) (max (vz3 max-point) (vz3 mesh-max)))))))
      
      (make-bounding-box :min min-point :max max-point))))

;;; Collision detection functions (matches raylib rmodels.c)

(defun check-collision-point-triangle (point a b c)
  "Check if point is inside a triangle in 3D space"
  (let* ((v0 (v- c a))
         (v1 (v- b a))
         (v2 (v- point a))
         (dot00 (v. v0 v0))
         (dot01 (v. v0 v1))
         (dot02 (v. v0 v2))
         (dot11 (v. v1 v1))
         (dot12 (v. v1 v2))
         (inv-denom (/ 1.0 (- (* dot00 dot11) (* dot01 dot01))))
         (u (* (- (* dot11 dot02) (* dot01 dot12)) inv-denom))
         (v (* (- (* dot00 dot12) (* dot01 dot02)) inv-denom)))
    (and (>= u 0) (>= v 0) (<= (+ u v) 1))))

(defun check-collision-point-box (point box-min box-max)
  "Check if point is inside a 3D box (matches raylib CheckCollisionPointBoundingBox)"
  (and (>= (vx point) (vx box-min)) (<= (vx point) (vx box-max))
       (>= (vy point) (vy box-min)) (<= (vy point) (vy box-max))
       (>= (vz point) (vz box-min)) (<= (vz point) (vz box-max))))

;;; Ray collision detection functions

(defun get-ray-collision-sphere (ray center radius)
  "Get ray collision info with sphere (matches raylib GetRayCollisionSphere)"
  (let* ((ray-to-center (v- center (ray-position ray)))
         (ray-dir (ray-direction ray))
         (closest-point (v. ray-to-center ray-dir))
         (closest-on-ray (if (< closest-point 0.0)
                             (ray-position ray)
                             (v+ (ray-position ray) (v* ray-dir closest-point))))
         (distance-to-center (vlength (v- center closest-on-ray))))
    (if (<= distance-to-center radius)
        (let* ((distance-to-sphere (- closest-point (sqrt (- (* radius radius) 
                                                            (* distance-to-center distance-to-center)))))
               (hit-point (v+ (ray-position ray) (v* ray-dir distance-to-sphere))))
          (make-ray-collision :hit t :distance distance-to-sphere :point hit-point :normal (vunit (v- hit-point center))))
        (make-ray-collision :hit nil :distance 0.0 :point (vec3 0 0 0) :normal (vec3 0 0 0)))))

(defun get-ray-collision-box (ray box-or-min &optional box-max)
  "Get ray collision info with box (matches raylib GetRayCollisionBox)"
  (let* ((box-min (if box-max box-or-min (bounding-box-min box-or-min)))
         (box-max (or box-max (bounding-box-max box-or-min)))
         (ray-pos (ray-position ray))
         (ray-dir (ray-direction ray))
         (t-min-x (/ (- (vx box-min) (vx ray-pos)) (vx ray-dir)))
         (t-max-x (/ (- (vx box-max) (vx ray-pos)) (vx ray-dir)))
         (t-min-y (/ (- (vy box-min) (vy ray-pos)) (vy ray-dir)))
         (t-max-y (/ (- (vy box-max) (vy ray-pos)) (vy ray-dir)))
         (t-min-z (/ (- (vz box-min) (vz ray-pos)) (vz ray-dir)))
         (t-max-z (/ (- (vz box-max) (vz ray-pos)) (vz ray-dir))))
    
    (when (> t-min-x t-max-x) (rotatef t-min-x t-max-x))
    (when (> t-min-y t-max-y) (rotatef t-min-y t-max-y))
    (when (> t-min-z t-max-z) (rotatef t-min-z t-max-z))
    
    (let ((t-min (max t-min-x t-min-y t-min-z))
          (t-max (min t-max-x t-max-y t-max-z)))
      
      (if (and (>= t-max 0) (<= t-min t-max))
          (let* ((t-hit (if (>= t-min 0) t-min t-max))
                 (hit-point (v+ ray-pos (v* ray-dir t-hit)))
                 (normal (cond
                          ((= t-hit t-min-x) (vec3 (if (< (vx ray-dir) 0) 1 -1) 0 0))
                          ((= t-hit t-max-x) (vec3 (if (> (vx ray-dir) 0) 1 -1) 0 0))
                          ((= t-hit t-min-y) (vec3 0 (if (< (vy ray-dir) 0) 1 -1) 0))
                          ((= t-hit t-max-y) (vec3 0 (if (> (vy ray-dir) 0) 1 -1) 0))
                          ((= t-hit t-min-z) (vec3 0 0 (if (< (vz ray-dir) 0) 1 -1)))
                          (t (vec3 0 0 (if (> (vz ray-dir) 0) 1 -1))))))
            (make-ray-collision :hit t :distance t-hit :point hit-point :normal normal))
          (make-ray-collision :hit nil :distance 0.0 :point (vec3 0 0 0) :normal (vec3 0 0 0))))))

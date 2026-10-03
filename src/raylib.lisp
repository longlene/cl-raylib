(in-package #:cl-raylib)

;;; Rectangle structure for texture operations
(defstruct rectangle
  "Rectangle structure for texture regions"
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (width 0.0 :type single-float)
  (height 0.0 :type single-float))

;;; Argument helpers shared by the drawing modules: Vector2 arguments accept a vec2
;;; or an (x y) list, Rectangle arguments a rectangle struct or an (x y w h) list
(declaim (inline %x %y))
(defun %x (v) (float (if (consp v) (first v) (vx v)) 1.0))
(defun %y (v) (float (if (consp v) (second v) (vy v)) 1.0))

(defun %rec (rec)
  "Return rectangle x, y, width, height as single-floats"
  (if (consp rec)
      (values (float (first rec) 1.0) (float (second rec) 1.0)
              (float (third rec) 1.0) (float (fourth rec) 1.0))
      (values (rectangle-x rec) (rectangle-y rec)
              (rectangle-width rec) (rectangle-height rec))))

;;; Enum arguments accept the raylib integer value or the cl-raylib.cffi keyword
;;; (:key-a, :mouse-button-left, :flag-window-resizable, ...); the enum prefix can be omitted (:a)
(defun %enum-value (value prefixes)
  "Resolve an enum keyword to its raylib integer value"
  (if (keywordp value)
      (let ((name (symbol-name value)))
        (dolist (prefix (cons "" prefixes) (error "Unknown raylib enum value: ~s" value))
          (let ((sym (find-symbol (concatenate 'string "+" prefix name "+") '#:cl-raylib)))
            (when (and sym (boundp sym)) (return (symbol-value sym))))))
      value))

(declaim (inline %key %mouse-button %gamepad-button %gamepad-axis))
(defun %key (key) (if (integerp key) key (%enum-value key '("KEY-"))))
(defun %mouse-button (button) (if (integerp button) button (%enum-value button '("MOUSE-BUTTON-"))))
(defun %gamepad-button (button) (if (integerp button) button (%enum-value button '("GAMEPAD-BUTTON-"))))
(defun %gamepad-axis (axis) (if (integerp axis) axis (%enum-value axis '("GAMEPAD-AXIS-"))))

(defun keyword-to-key (key)
  "Convert a key keyword (:key-a or :a) to its KeyboardKey value"
  (%key key))

(defun keyword-to-mouse-button (button)
  "Convert a mouse button keyword to its MouseButton value"
  (%mouse-button button))

(defun %flags (flags &optional (prefix "FLAG-"))
  "Resolve bit flags given as an integer, a keyword or a list of keywords (cffi defbitfield style)"
  (cond ((integerp flags) flags)
        ((listp flags) (reduce #'logior flags :key (lambda (f) (%flags f prefix)) :initial-value 0))
        (t (%enum-value flags (list prefix)))))

;;; Automation event (raylib AutomationEvent)
(defstruct automation-event
  (frame 0 :type (unsigned-byte 32))            ; Event frame
  (type 0 :type (unsigned-byte 32))             ; Event type (AutomationEventType)
  (params (make-array 4 :element-type '(signed-byte 32) :initial-element 0)
   :type (simple-array (signed-byte 32) (4))))  ; Event parameters (if required)

;;; Automation event list (raylib AutomationEventList)
(defstruct automation-event-list
  (capacity 0 :type (unsigned-byte 32))         ; Events max entries (MAX_AUTOMATION_EVENTS)
  (count 0 :type (unsigned-byte 32))            ; Events entries count
  (events #() :type simple-vector))             ; Events entries

;;; Camera system modes (raylib CameraMode enum)
(defconstant +camera-custom+ 0 "Camera custom, controlled by user (UpdateCamera() does nothing)")
(defconstant +camera-free+ 1 "Camera free mode")
(defconstant +camera-orbital+ 2 "Camera orbital, around target, zoom supported")
(defconstant +camera-first-person+ 3 "Camera first person")
(defconstant +camera-third-person+ 4 "Camera third person")

;;; Camera projection (raylib CameraProjection enum)
(defconstant +camera-perspective+ 0 "Perspective projection")
(defconstant +camera-orthographic+ 1 "Orthographic projection")

;;; Color blending modes (raylib BlendMode enum, pre-defined)
(defconstant +blend-alpha+ 0 "Blend textures considering alpha (default)")
(defconstant +blend-additive+ 1 "Blend textures adding colors")
(defconstant +blend-multiplied+ 2 "Blend textures multiplying colors")
(defconstant +blend-add-colors+ 3 "Blend textures adding colors (alternative)")
(defconstant +blend-subtract-colors+ 4 "Blend textures subtracting colors (alternative)")
(defconstant +blend-alpha-premultiply+ 5 "Blend premultiplied textures considering alpha")
(defconstant +blend-custom+ 6 "Blend textures using custom src/dst factors (use rl-set-blend-factors)")
(defconstant +blend-custom-separate+ 7 "Blend textures using custom rgb/alpha separate src/dst factors (use rl-set-blend-factors-separate)")

;;; Pixel formats (raylib PixelFormat enum)
;;; NOTE: Support depends on OpenGL version and platform
(defconstant +pixelformat-uncompressed-grayscale+ 1 "8 bit per pixel (no alpha)")
(defconstant +pixelformat-uncompressed-gray-alpha+ 2 "8*2 bpp (2 channels)")
(defconstant +pixelformat-uncompressed-r5g6b5+ 3 "16 bpp")
(defconstant +pixelformat-uncompressed-r8g8b8+ 4 "24 bpp")
(defconstant +pixelformat-uncompressed-r5g5b5a1+ 5 "16 bpp (1 bit alpha)")
(defconstant +pixelformat-uncompressed-r4g4b4a4+ 6 "16 bpp (4 bit alpha)")
(defconstant +pixelformat-uncompressed-r8g8b8a8+ 7 "32 bpp")
(defconstant +pixelformat-uncompressed-r32+ 8 "32 bpp (1 channel - float)")
(defconstant +pixelformat-uncompressed-r32g32b32+ 9 "32*3 bpp (3 channels - float)")
(defconstant +pixelformat-uncompressed-r32g32b32a32+ 10 "32*4 bpp (4 channels - float)")
(defconstant +pixelformat-uncompressed-r16+ 11 "16 bpp (1 channel - half float)")
(defconstant +pixelformat-uncompressed-r16g16b16+ 12 "16*3 bpp (3 channels - half float)")
(defconstant +pixelformat-uncompressed-r16g16b16a16+ 13 "16*4 bpp (4 channels - half float)")
(defconstant +pixelformat-compressed-dxt1-rgb+ 14 "4 bpp (no alpha)")
(defconstant +pixelformat-compressed-dxt1-rgba+ 15 "4 bpp (1 bit alpha)")
(defconstant +pixelformat-compressed-dxt3-rgba+ 16 "8 bpp")
(defconstant +pixelformat-compressed-dxt5-rgba+ 17 "8 bpp")
(defconstant +pixelformat-compressed-etc1-rgb+ 18 "4 bpp")
(defconstant +pixelformat-compressed-etc2-rgb+ 19 "4 bpp")
(defconstant +pixelformat-compressed-etc2-eac-rgba+ 20 "8 bpp")
(defconstant +pixelformat-compressed-pvrt-rgb+ 21 "4 bpp")
(defconstant +pixelformat-compressed-pvrt-rgba+ 22 "4 bpp")
(defconstant +pixelformat-compressed-astc-4x4-rgba+ 23 "8 bpp")
(defconstant +pixelformat-compressed-astc-8x8-rgba+ 24 "2 bpp")
(defconstant +pixelformat-uncompressed-rgba+ 7 "32-bit RGBA (alias for compatibility)")

;;; Image structure (CPU-side data)
;;; NOTE: The format and mipmaps slots are named PIXEL-FORMAT and MIPMAP-COUNT so that
;;; IMAGE-FORMAT and IMAGE-MIPMAPS can be raylib's ImageFormat() and ImageMipmaps();
;;; (image-format image) with one argument still reads the format
(defstruct (image (:constructor %make-image))
  "Image, pixel data stored in CPU memory (RAM), data layout given by pixel-format"
  (data nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (mipmap-count 1 :type fixnum)
  (pixel-format +pixelformat-uncompressed-r8g8b8a8+ :type fixnum))

(defun make-image (&key data (width 0) (height 0) (mipmaps 1) (format +pixelformat-uncompressed-r8g8b8a8+))
  "Create an image structure"
  (%make-image :data data :width width :height height :mipmap-count mipmaps :pixel-format format))

;;; Texture structure (GPU-side data)
(defstruct texture
  "Texture data structure for GPU operations"
  (id 0 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (mipmaps 1 :type fixnum)
  (format +pixelformat-uncompressed-rgba+ :type fixnum))

;;; Render texture structure (matches raylib RenderTexture)
(defstruct render-texture
  "RenderTexture structure for framebuffer rendering - matches raylib RenderTexture"
  (id 0 :type fixnum)           ; OpenGL framebuffer object id
  (texture nil :type (or null texture))  ; Color buffer attachment texture
  (depth nil :type (or null texture)))   ; Depth buffer attachment texture

;;; RenderTexture2D is just a typedef alias in raylib
(deftype render-texture-2d () 'render-texture)

;;; Shader constants (from rlgl.h)
(defconstant +max-shader-locations+ 32 "Maximum number of shader locations")

;;; Shader structure (matches raylib Shader)
(defstruct shader
  "Shader program structure"
  (id 0 :type fixnum)                   ; Shader program id
  (locs nil :type (or null simple-vector))) ; Shader locations array (RL_MAX_SHADER_LOCATIONS)

;;; Shader location index
(defconstant +shader-loc-vertex-position+ 0 "Shader location: vertex attribute: position")
(defconstant +shader-loc-vertex-texcoord01+ 1 "Shader location: vertex attribute: texcoord01")
(defconstant +shader-loc-vertex-texcoord02+ 2 "Shader location: vertex attribute: texcoord02")
(defconstant +shader-loc-vertex-normal+ 3 "Shader location: vertex attribute: normal")
(defconstant +shader-loc-vertex-tangent+ 4 "Shader location: vertex attribute: tangent")
(defconstant +shader-loc-vertex-color+ 5 "Shader location: vertex attribute: color")
(defconstant +shader-loc-matrix-mvp+ 6 "Shader location: matrix uniform: model-view-projection")
(defconstant +shader-loc-matrix-view+ 7 "Shader location: matrix uniform: view (camera transform)")
(defconstant +shader-loc-matrix-projection+ 8 "Shader location: matrix uniform: projection")
(defconstant +shader-loc-matrix-model+ 9 "Shader location: matrix uniform: model (transform)")
(defconstant +shader-loc-matrix-normal+ 10 "Shader location: matrix uniform: normal")
(defconstant +shader-loc-vector-view+ 11 "Shader location: vector uniform: view")
(defconstant +shader-loc-color-diffuse+ 12 "Shader location: vector uniform: diffuse color")
(defconstant +shader-loc-color-specular+ 13 "Shader location: vector uniform: specular color")
(defconstant +shader-loc-color-ambient+ 14 "Shader location: vector uniform: ambient color")
(defconstant +shader-loc-map-albedo+ 15 "Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)")
(defconstant +shader-loc-map-metalness+ 16 "Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)")
(defconstant +shader-loc-map-normal+ 17 "Shader location: sampler2d texture: normal")
(defconstant +shader-loc-map-roughness+ 18 "Shader location: sampler2d texture: roughness")
(defconstant +shader-loc-map-occlusion+ 19 "Shader location: sampler2d texture: occlusion")
(defconstant +shader-loc-map-emission+ 20 "Shader location: sampler2d texture: emission")
(defconstant +shader-loc-map-height+ 21 "Shader location: sampler2d texture: height")
(defconstant +shader-loc-map-cubemap+ 22 "Shader location: samplerCube texture: cubemap")
(defconstant +shader-loc-map-irradiance+ 23 "Shader location: samplerCube texture: irradiance")
(defconstant +shader-loc-map-prefilter+ 24 "Shader location: samplerCube texture: prefilter")
(defconstant +shader-loc-map-brdf+ 25 "Shader location: sampler2d texture: brdf")
(defconstant +shader-loc-vertex-boneids+ 26 "Shader location: vertex attribute: bone indices")
(defconstant +shader-loc-vertex-boneweights+ 27 "Shader location: vertex attribute: bone weights")
(defconstant +shader-loc-matrix-bonetransforms+ 28 "Shader location: matrix attribute: bone transforms (animation)")
(defconstant +shader-loc-vertex-instancetransform+ 29 "Shader location: vertex attribute: instance transforms")

(defconstant +shader-loc-map-diffuse+ +shader-loc-map-albedo+)
(defconstant +shader-loc-map-specular+ +shader-loc-map-metalness+)

;;; Shader uniform data type
(defconstant +shader-uniform-float+ 0 "Shader uniform type: float")
(defconstant +shader-uniform-vec2+ 1 "Shader uniform type: vec2 (2 float)")
(defconstant +shader-uniform-vec3+ 2 "Shader uniform type: vec3 (3 float)")
(defconstant +shader-uniform-vec4+ 3 "Shader uniform type: vec4 (4 float)")
(defconstant +shader-uniform-int+ 4 "Shader uniform type: int")
(defconstant +shader-uniform-ivec2+ 5 "Shader uniform type: ivec2 (2 int)")
(defconstant +shader-uniform-ivec3+ 6 "Shader uniform type: ivec3 (3 int)")
(defconstant +shader-uniform-ivec4+ 7 "Shader uniform type: ivec4 (4 int)")
(defconstant +shader-uniform-uint+ 8 "Shader uniform type: unsigned int")
(defconstant +shader-uniform-uivec2+ 9 "Shader uniform type: uivec2 (2 unsigned int)")
(defconstant +shader-uniform-uivec3+ 10 "Shader uniform type: uivec3 (3 unsigned int)")
(defconstant +shader-uniform-uivec4+ 11 "Shader uniform type: uivec4 (4 unsigned int)")
(defconstant +shader-uniform-sampler2d+ 12 "Shader uniform type: sampler2d")

;;; Shader attribute data types
(defconstant +shader-attrib-float+ 0 "Shader attribute type: float")
(defconstant +shader-attrib-vec2+ 1 "Shader attribute type: vec2 (2 float)")
(defconstant +shader-attrib-vec3+ 2 "Shader attribute type: vec3 (3 float)")
(defconstant +shader-attrib-vec4+ 3 "Shader attribute type: vec4 (4 float)")

;;; VrDeviceInfo, Head-Mounted-Display device parameters
(defstruct vr-device-info
  (h-resolution 0 :type fixnum)         ; Horizontal resolution in pixels
  (v-resolution 0 :type fixnum)         ; Vertical resolution in pixels
  (h-screen-size 0.0 :type single-float) ; Horizontal size in meters
  (v-screen-size 0.0 :type single-float) ; Vertical size in meters
  (eye-to-screen-distance 0.0 :type single-float) ; Distance between eye and display in meters
  (lens-separation-distance 0.0 :type single-float) ; Lens separation distance in meters
  (interpupillary-distance 0.0 :type single-float) ; IPD (distance between pupils) in meters
  (lens-distortion-values (make-array 4 :element-type 'single-float :initial-element 0.0)) ; Lens distortion constant parameters
  (chroma-ab-correction (make-array 4 :element-type 'single-float :initial-element 0.0))) ; Chromatic aberration correction parameters

;;; VrStereoConfig, VR stereo rendering configuration for simulator
(defstruct vr-stereo-config
  (projection (vector (meye 4) (meye 4)))  ; VR projection matrices (per eye)
  (view-offset (vector (meye 4) (meye 4))) ; VR view offset matrices (per eye)
  (left-lens-center (make-array 2 :element-type 'single-float :initial-element 0.0))   ; VR left lens center
  (right-lens-center (make-array 2 :element-type 'single-float :initial-element 0.0))  ; VR right lens center
  (left-screen-center (make-array 2 :element-type 'single-float :initial-element 0.0)) ; VR left screen center
  (right-screen-center (make-array 2 :element-type 'single-float :initial-element 0.0)) ; VR right screen center
  (scale (make-array 2 :element-type 'single-float :initial-element 0.0))    ; VR distortion scale
  (scale-in (make-array 2 :element-type 'single-float :initial-element 0.0))) ; VR distortion scale in

;; Note: raylib doesn't have a standalone Vertex struct - 
;; vertex data is stored as arrays within the Mesh struct

;;; Nine-patch texture info (matches raylib NPatchInfo)
(defstruct npatch-info
  "Nine-patch texture info"
  (source nil :type (or null rectangle))  ; Texture source rectangle
  (left 0 :type fixnum)                   ; Left border offset
  (top 0 :type fixnum)                    ; Top border offset  
  (right 0 :type fixnum)                  ; Right border offset
  (bottom 0 :type fixnum)                 ; Bottom border offset
  (layout 0 :type fixnum))                ; Layout of the n-patch: 3x3, 1x3 or 3x1

;;; Data structures exactly matching raylib's Font and GlyphInfo
(defstruct glyph-info
  "Character glyph information - matches raylib GlyphInfo exactly"
  (value 0 :type fixnum)         ; Unicode codepoint (int)
  (offset-x 0 :type fixnum)      ; Character offset X when drawing (int)
  (offset-y 0 :type fixnum)      ; Character offset Y when drawing (int)
  (advance-x 0 :type fixnum)     ; Character advance position X (int)
  (image nil :type (or null image))) ; Character image data (Image)

(defstruct font
  "Font structure matching raylib Font exactly"
  (base-size 0 :type fixnum)          ; Base size (default chars height) (int)
  (glyph-count 0 :type fixnum)        ; Number of glyph characters (int)
  (glyph-padding 0 :type fixnum)      ; Padding around the glyph characters (int)
  (texture nil :type (or null texture)) ; Texture atlas containing the glyphs (Texture)
  (recs nil :type (or null simple-vector)) ; Rectangles in texture for the glyphs (Rectangle *)
  (glyphs nil :type (or null simple-vector))) ; Glyphs info data (GlyphInfo *)

;;; Camera3D structure
(defstruct (camera3d (:constructor %make-camera3d))
  "3D Camera representation"
  (position (vec3 0.0 0.0 0.0))                 ; Camera position (3d-math vec3)
  (target (vec3 0.0 0.0 -1.0))                  ; Camera target point (3d-math vec3)
  (up (vec3 0.0 1.0 0.0))                       ; Camera up vector (3d-math vec3)
  (fovy 45.0 :type single-float)                ; Camera field-of-view Y
  (projection 0 :type fixnum))                   ; Camera projection type


;;; Camera2D structure - matches raylib exactly
(defstruct camera2d
  "2D Camera representation - matches raylib Camera2D"
  (offset (vec2 0.0 0.0) :type vec2)    ; Camera offset (displacement from target)
  (target (vec2 0.0 0.0) :type vec2)    ; Camera target (rotation and zoom origin) 
  (rotation 0.0 :type single-float)      ; Camera rotation in degrees
  (zoom 1.0 :type single-float))         ; Camera zoom (scaling), should be 1.0f by default

;;; Mesh structure
(defstruct mesh
  "3D mesh containing vertices and indices"
  (vertex-count 0 :type fixnum)                 ; Number of vertices
  (triangle-count 0 :type fixnum)               ; Number of triangles
  (vertices nil :type list)                     ; List of vertex structures
  (indices nil :type list)                      ; List of triangle indices (groups of 3)
  (vbo-vertices 0 :type fixnum)                 ; OpenGL VBO for vertices
  (vbo-indices 0 :type fixnum)                  ; OpenGL VBO for indices
  (vao 0 :type fixnum)                          ; OpenGL VAO
  (uploaded nil :type boolean))                 ; Whether mesh is uploaded to GPU

;;; MaterialMap structure (matches raylib MaterialMap)
(defstruct material-map
  "Material map structure matching raylib MaterialMap"
  (texture nil :type (or null texture))         ; Material map texture
  (color (list 255 255 255 255) :type list)     ; Material map color (WHITE)
  (value 1.0 :type single-float))               ; Material map value

;;; Material map index constants (matching raylib)
(defconstant +material-map-albedo+ 0)          ; Albedo material (same as MATERIAL_MAP_DIFFUSE)
(defconstant +material-map-metalness+ 1)       ; Metalness material (same as MATERIAL_MAP_SPECULAR)
(defconstant +material-map-normal+ 2)          ; Normal material
(defconstant +material-map-roughness+ 3)       ; Roughness material
(defconstant +material-map-occlusion+ 4)       ; Ambient occlusion material
(defconstant +material-map-emission+ 5)        ; Emission material
(defconstant +material-map-height+ 6)          ; Heightmap material
(defconstant +material-map-cubemap+ 7)         ; Cubemap material
(defconstant +material-map-irradiance+ 8)      ; Irradiance material
(defconstant +material-map-prefilter+ 9)       ; Prefilter material
(defconstant +material-map-brdf+ 10)           ; Brdf material

;;; Aliases for compatibility
(defconstant +material-map-diffuse+ +material-map-albedo+)
(defconstant +material-map-specular+ +material-map-metalness+)
(defconstant +max-material-maps+ 12)           ; Maximum number of material maps

;;; Material structure (matches raylib Material)
(defstruct material
  "Material structure matching raylib Material"
  (shader nil :type (or null shader))                                    ; Material shader
  (maps (make-array +max-material-maps+ :initial-element nil) :type simple-vector) ; Material maps array
  (params (make-array 4 :initial-element 0.0 :element-type 'single-float) :type (simple-array single-float (4)))) ; Material generic parameters


;;; Model structure
(defstruct model
  "3D model containing mesh and material data"
  (meshes nil :type list)                       ; List of mesh structures
  (materials nil :type list)                    ; List of material structures
  (mesh-count 0 :type fixnum)                   ; Number of meshes
  (material-count 0 :type fixnum)               ; Number of materials
  (transform (meye 4) :type mat4)               ; Model transformation matrix
  (bounding-box nil :type (or null list)))      ; Bounding box (min-max Vector3 pair)

;;; Ray for casting
(defstruct ray
  "Ray structure for 3D ray casting"
  (position (vec3 0.0 0.0 0.0))                 ; Ray position (origin)
  (direction (vec3 0.0 0.0 -1.0)))              ; Ray direction (normalized)

;;; Ray collision result
(defstruct ray-collision
  "Ray collision result structure (matches raylib RayCollision)"
  (hit nil :type boolean)                       ; Did the ray hit something?
  (distance 0.0 :type single-float)             ; Distance to the nearest hit
  (point (vec3 0.0 0.0 0.0) :type vec3)         ; Point of the nearest hit
  (normal (vec3 0.0 0.0 0.0) :type vec3))       ; Surface normal of hit

;;; Bounding box structure
(defstruct bounding-box
  "3D bounding box"
  (min (vec3 0.0 0.0 0.0) :type vec3)           ; Minimum point
  (max (vec3 0.0 0.0 0.0) :type vec3))          ; Maximum point

;;; 2D geometry structures for collision detection

;;; Circle structure for 2D collision detection
(defstruct circle
  "2D circle structure for collision detection"
  (center (vec2 0.0 0.0) :type vec2)            ; Circle center
  (radius 0.0 :type single-float))              ; Circle radius

;;; AABB (Axis-Aligned Bounding Box) structure for 2D collision detection
(defstruct aabb
  "2D axis-aligned bounding box for collision detection"
  (min (vec2 0.0 0.0) :type vec2)               ; Minimum point
  (max (vec2 0.0 0.0) :type vec2))              ; Maximum point

;;; Line segment structure for 2D collision detection
(defstruct line-segment
  "2D line segment for collision detection"
  (start (vec2 0.0 0.0) :type vec2)             ; Start point
  (end (vec2 0.0 0.0) :type vec2))              ; End point

;;; 3D geometry structures

;;; Sphere structure for 3D collision detection
(defstruct sphere
  "3D sphere structure for collision detection"
  (center (vec3 0.0 0.0 0.0) :type vec3)        ; Sphere center
  (radius 0.0 :type single-float))              ; Sphere radius

;;; AABB3D structure for 3D collision detection
(defstruct aabb3d
  "3D axis-aligned bounding box for collision detection"
  (min (vec3 0.0 0.0 0.0) :type vec3)           ; Minimum point
  (max (vec3 0.0 0.0 0.0) :type vec3))          ; Maximum point

;;; File path list structure for file drop handling
(defstruct file-path-list
  "File path list structure for dropped files (matches raylib FilePathList)"
  (capacity 0 :type fixnum)                     ; Filepaths max entries
  (count 0 :type fixnum)                        ; Filepaths entries count
  (paths nil :type list))                       ; Filepaths entries (list of strings)

;;; Audio structures (matches raylib audio types)

;;; Wave structure - audio wave data (matches raylib Wave)
;;; NOTE: DATA is a typed array depending on SAMPLE-SIZE: 8 -> (unsigned-byte 8),
;;; 16 -> (signed-byte 16), 32 -> single-float
(defstruct wave
  "Wave data structure for audio waveform (matches raylib Wave)"
  (frame-count 0 :type fixnum)                  ; Total number of frames (considering channels)
  (sample-rate 0 :type fixnum)                  ; Frequency (samples per second)
  (sample-size 0 :type fixnum)                  ; Bit depth (bits per sample): 8, 16, 32 (24 not supported)
  (channels 0 :type fixnum)                     ; Number of channels (1-mono, 2-stereo, ...)
  (data nil))                                   ; Buffer data

;;; Audio Stream structure (matches raylib AudioStream)
(defstruct audio-stream
  "Audio stream for custom audio streaming (matches raylib AudioStream)"
  (buffer nil)                                  ; Pointer to internal data used by the audio system
  (processor nil)                               ; Pointer to internal data processor, useful for audio effects
  (sample-rate 0 :type fixnum)                  ; Frequency (samples per second)
  (sample-size 0 :type fixnum)                  ; Bit depth (bits per sample): 8, 16, 32 (24 not supported)
  (channels 0 :type fixnum))                    ; Number of channels (1-mono, 2-stereo, ...)

;;; Sound structure (matches raylib Sound)
(defstruct sound
  "Sound structure for short audio samples (matches raylib Sound)"
  (stream (make-audio-stream) :type audio-stream) ; Audio stream
  (frame-count 0 :type fixnum))                 ; Total number of frames (considering channels)

;;; Music structure (matches raylib Music)
(defstruct music
  "Music structure for long audio streams (matches raylib Music)"
  (stream (make-audio-stream) :type audio-stream) ; Audio stream
  (frame-count 0 :type fixnum)                  ; Total number of frames (considering channels)
  (looping nil :type boolean)                   ; Music looping enable
  (ctx-type 0 :type fixnum)                     ; Type of music context (audio filetype)
  (ctx-data nil))                               ; Audio context data, depends on type

;;; ConfigFlags enum (raylib.h)
(defconstant +flag-vsync-hint+ 64 "Set to try enabling V-Sync on GPU")
(defconstant +flag-fullscreen-mode+ 2 "Set to run program in fullscreen")
(defconstant +flag-window-resizable+ 4 "Set to allow resizable window")
(defconstant +flag-window-undecorated+ 8 "Set to disable window decoration (frame and buttons)")
(defconstant +flag-window-hidden+ 128 "Set to hide window")
(defconstant +flag-window-minimized+ 512 "Set to minimize window (iconify)")
(defconstant +flag-window-maximized+ 1024 "Set to maximize window (expanded to monitor)")
(defconstant +flag-window-unfocused+ 2048 "Set to window non focused")
(defconstant +flag-window-topmost+ 4096 "Set to window always on top")
(defconstant +flag-window-always-run+ 256 "Set to allow windows running while minimized")
(defconstant +flag-window-transparent+ 16 "Set to allow transparent framebuffer")
(defconstant +flag-window-highdpi+ 8192 "Set to support HighDPI")
(defconstant +flag-window-mouse-passthrough+ 16384 "Set to support mouse passthrough, only supported when FLAG_WINDOW_UNDECORATED")
(defconstant +flag-borderless-windowed-mode+ 32768 "Set to run program in borderless windowed mode")
(defconstant +flag-msaa-4x-hint+ 32 "Set to try enabling MSAA 4X")
(defconstant +flag-interlaced-hint+ 65536 "Set to try enabling interlaced video format (for V3D)")
(defconstant +flag-window-borderless-windowed-mode+ +flag-borderless-windowed-mode+ "Old pure cl-raylib name")

;;; KeyboardKey enum (raylib.h)
(defconstant +key-null+ 0 "Key: NULL, used for no key pressed")
(defconstant +key-apostrophe+ 39 "Key: '")
(defconstant +key-comma+ 44 "Key: ,")
(defconstant +key-minus+ 45 "Key: -")
(defconstant +key-period+ 46 "Key: .")
(defconstant +key-slash+ 47 "Key: /")
(defconstant +key-zero+ 48 "Key: 0")
(defconstant +key-one+ 49 "Key: 1")
(defconstant +key-two+ 50 "Key: 2")
(defconstant +key-three+ 51 "Key: 3")
(defconstant +key-four+ 52 "Key: 4")
(defconstant +key-five+ 53 "Key: 5")
(defconstant +key-six+ 54 "Key: 6")
(defconstant +key-seven+ 55 "Key: 7")
(defconstant +key-eight+ 56 "Key: 8")
(defconstant +key-nine+ 57 "Key: 9")
(defconstant +key-semicolon+ 59 "Key: ;")
(defconstant +key-equal+ 61 "Key: =")
(defconstant +key-a+ 65 "Key: A | a")
(defconstant +key-b+ 66 "Key: B | b")
(defconstant +key-c+ 67 "Key: C | c")
(defconstant +key-d+ 68 "Key: D | d")
(defconstant +key-e+ 69 "Key: E | e")
(defconstant +key-f+ 70 "Key: F | f")
(defconstant +key-g+ 71 "Key: G | g")
(defconstant +key-h+ 72 "Key: H | h")
(defconstant +key-i+ 73 "Key: I | i")
(defconstant +key-j+ 74 "Key: J | j")
(defconstant +key-k+ 75 "Key: K | k")
(defconstant +key-l+ 76 "Key: L | l")
(defconstant +key-m+ 77 "Key: M | m")
(defconstant +key-n+ 78 "Key: N | n")
(defconstant +key-o+ 79 "Key: O | o")
(defconstant +key-p+ 80 "Key: P | p")
(defconstant +key-q+ 81 "Key: Q | q")
(defconstant +key-r+ 82 "Key: R | r")
(defconstant +key-s+ 83 "Key: S | s")
(defconstant +key-t+ 84 "Key: T | t")
(defconstant +key-u+ 85 "Key: U | u")
(defconstant +key-v+ 86 "Key: V | v")
(defconstant +key-w+ 87 "Key: W | w")
(defconstant +key-x+ 88 "Key: X | x")
(defconstant +key-y+ 89 "Key: Y | y")
(defconstant +key-z+ 90 "Key: Z | z")
(defconstant +key-left-bracket+ 91 "Key: [")
(defconstant +key-backslash+ 92 "Key: '\'")
(defconstant +key-right-bracket+ 93 "Key: ]")
(defconstant +key-grave+ 96 "Key: `")
(defconstant +key-space+ 32 "Key: Space")
(defconstant +key-escape+ 256 "Key: Esc")
(defconstant +key-enter+ 257 "Key: Enter")
(defconstant +key-tab+ 258 "Key: Tab")
(defconstant +key-backspace+ 259 "Key: Backspace")
(defconstant +key-insert+ 260 "Key: Ins")
(defconstant +key-delete+ 261 "Key: Del")
(defconstant +key-right+ 262 "Key: Cursor right")
(defconstant +key-left+ 263 "Key: Cursor left")
(defconstant +key-down+ 264 "Key: Cursor down")
(defconstant +key-up+ 265 "Key: Cursor up")
(defconstant +key-page-up+ 266 "Key: Page up")
(defconstant +key-page-down+ 267 "Key: Page down")
(defconstant +key-home+ 268 "Key: Home")
(defconstant +key-end+ 269 "Key: End")
(defconstant +key-caps-lock+ 280 "Key: Caps lock")
(defconstant +key-scroll-lock+ 281 "Key: Scroll down")
(defconstant +key-num-lock+ 282 "Key: Num lock")
(defconstant +key-print-screen+ 283 "Key: Print screen")
(defconstant +key-pause+ 284 "Key: Pause")
(defconstant +key-f1+ 290 "Key: F1")
(defconstant +key-f2+ 291 "Key: F2")
(defconstant +key-f3+ 292 "Key: F3")
(defconstant +key-f4+ 293 "Key: F4")
(defconstant +key-f5+ 294 "Key: F5")
(defconstant +key-f6+ 295 "Key: F6")
(defconstant +key-f7+ 296 "Key: F7")
(defconstant +key-f8+ 297 "Key: F8")
(defconstant +key-f9+ 298 "Key: F9")
(defconstant +key-f10+ 299 "Key: F10")
(defconstant +key-f11+ 300 "Key: F11")
(defconstant +key-f12+ 301 "Key: F12")
(defconstant +key-left-shift+ 340 "Key: Shift left")
(defconstant +key-left-control+ 341 "Key: Control left")
(defconstant +key-left-alt+ 342 "Key: Alt left")
(defconstant +key-left-super+ 343 "Key: Super left")
(defconstant +key-right-shift+ 344 "Key: Shift right")
(defconstant +key-right-control+ 345 "Key: Control right")
(defconstant +key-right-alt+ 346 "Key: Alt right")
(defconstant +key-right-super+ 347 "Key: Super right")
(defconstant +key-kb-menu+ 348 "Key: KB menu")
(defconstant +key-kp-0+ 320 "Key: Keypad 0")
(defconstant +key-kp-1+ 321 "Key: Keypad 1")
(defconstant +key-kp-2+ 322 "Key: Keypad 2")
(defconstant +key-kp-3+ 323 "Key: Keypad 3")
(defconstant +key-kp-4+ 324 "Key: Keypad 4")
(defconstant +key-kp-5+ 325 "Key: Keypad 5")
(defconstant +key-kp-6+ 326 "Key: Keypad 6")
(defconstant +key-kp-7+ 327 "Key: Keypad 7")
(defconstant +key-kp-8+ 328 "Key: Keypad 8")
(defconstant +key-kp-9+ 329 "Key: Keypad 9")
(defconstant +key-kp-decimal+ 330 "Key: Keypad .")
(defconstant +key-kp-divide+ 331 "Key: Keypad /")
(defconstant +key-kp-multiply+ 332 "Key: Keypad *")
(defconstant +key-kp-subtract+ 333 "Key: Keypad -")
(defconstant +key-kp-add+ 334 "Key: Keypad +")
(defconstant +key-kp-enter+ 335 "Key: Keypad Enter")
(defconstant +key-kp-equal+ 336 "Key: Keypad =")
(defconstant +key-back+ 4 "Key: Android back button")
(defconstant +key-menu+ 5 "Key: Android menu button")
(defconstant +key-volume-up+ 24 "Key: Android volume up button")
(defconstant +key-volume-down+ 25 "Key: Android volume down button")

;; Mouse button constants
(defconstant +mouse-button-left+ 0)
(defconstant +mouse-button-right+ 1)
(defconstant +mouse-button-middle+ 2)
(defconstant +mouse-button-side+ 3)
(defconstant +mouse-button-extra+ 4)
(defconstant +mouse-button-forward+ 5)
(defconstant +mouse-button-back+ 6)

;; Mouse cursor types
(defconstant +mouse-cursor-default+ 0)
(defconstant +mouse-cursor-arrow+ 1)
(defconstant +mouse-cursor-ibeam+ 2)
(defconstant +mouse-cursor-crosshair+ 3)
(defconstant +mouse-cursor-pointing-hand+ 4)
(defconstant +mouse-cursor-resize-ew+ 5)
(defconstant +mouse-cursor-resize-ns+ 6)
(defconstant +mouse-cursor-resize-nwse+ 7)
(defconstant +mouse-cursor-resize-nesw+ 8)
(defconstant +mouse-cursor-resize-all+ 9)
(defconstant +mouse-cursor-not-allowed+ 10)

;;; GamepadButton enum (raylib.h)
(defconstant +gamepad-button-unknown+ 0 "Unknown button, for error checking")
(defconstant +gamepad-button-left-face-up+ 1 "Gamepad left DPAD up button")
(defconstant +gamepad-button-left-face-right+ 2 "Gamepad left DPAD right button")
(defconstant +gamepad-button-left-face-down+ 3 "Gamepad left DPAD down button")
(defconstant +gamepad-button-left-face-left+ 4 "Gamepad left DPAD left button")
(defconstant +gamepad-button-right-face-up+ 5 "Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)")
(defconstant +gamepad-button-right-face-right+ 6 "Gamepad right button right (i.e. PS3: Circle, Xbox: B)")
(defconstant +gamepad-button-right-face-down+ 7 "Gamepad right button down (i.e. PS3: Cross, Xbox: A)")
(defconstant +gamepad-button-right-face-left+ 8 "Gamepad right button left (i.e. PS3: Square, Xbox: X)")
(defconstant +gamepad-button-left-trigger-1+ 9 "Gamepad top/back trigger left (first), it could be a trailing button")
(defconstant +gamepad-button-left-trigger-2+ 10 "Gamepad top/back trigger left (second), it could be a trailing button")
(defconstant +gamepad-button-right-trigger-1+ 11 "Gamepad top/back trigger right (first), it could be a trailing button")
(defconstant +gamepad-button-right-trigger-2+ 12 "Gamepad top/back trigger right (second), it could be a trailing button")
(defconstant +gamepad-button-middle-left+ 13 "Gamepad center buttons, left one (i.e. PS3: Select)")
(defconstant +gamepad-button-middle+ 14 "Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)")
(defconstant +gamepad-button-middle-right+ 15 "Gamepad center buttons, right one (i.e. PS3: Start)")
(defconstant +gamepad-button-left-thumb+ 16 "Gamepad joystick pressed button left")
(defconstant +gamepad-button-right-thumb+ 17 "Gamepad joystick pressed button right")

;;; GamepadAxis enum (raylib.h)
(defconstant +gamepad-axis-left-x+ 0 "Gamepad left stick X axis")
(defconstant +gamepad-axis-left-y+ 1 "Gamepad left stick Y axis")
(defconstant +gamepad-axis-right-x+ 2 "Gamepad right stick X axis")
(defconstant +gamepad-axis-right-y+ 3 "Gamepad right stick Y axis")
(defconstant +gamepad-axis-left-trigger+ 4 "Gamepad back trigger left, pressure level: [1..-1]")
(defconstant +gamepad-axis-right-trigger+ 5 "Gamepad back trigger right, pressure level: [1..-1]")

;;; Gesture enum (raylib.h)
(defconstant +gesture-none+ 0 "No gesture")
(defconstant +gesture-tap+ 1 "Tap gesture")
(defconstant +gesture-doubletap+ 2 "Double tap gesture")
(defconstant +gesture-hold+ 4 "Hold gesture")
(defconstant +gesture-drag+ 8 "Drag gesture")
(defconstant +gesture-swipe-right+ 16 "Swipe right gesture")
(defconstant +gesture-swipe-left+ 32 "Swipe left gesture")
(defconstant +gesture-swipe-up+ 64 "Swipe up gesture")
(defconstant +gesture-swipe-down+ 128 "Swipe down gesture")
(defconstant +gesture-pinch-in+ 256 "Pinch in gesture")
(defconstant +gesture-pinch-out+ 512 "Pinch out gesture")

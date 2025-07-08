(in-package #:cl-raylib)

;;; Rectangle structure for texture operations
(defstruct rectangle
  "Rectangle structure for texture regions"
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (width 0.0 :type single-float)
  (height 0.0 :type single-float))

;;; Image structure (CPU-side data)
(defstruct image
  "Image data structure for CPU-side operations"
  (data nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (mipmaps 1 :type fixnum)
  (format +pixelformat-uncompressed-rgba+ :type fixnum))

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
  (id 0 :type fixnum)                                                      ; Shader program id
  (locs (make-array +max-shader-locations+ :initial-element -1) :type simple-vector)) ; Shader locations array

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
  (color +white+ :type list)                    ; Material map color
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
  (params (make-array 4 :initial-element 0.0 :element-type 'single-float) :type simple-vector)) ; Material generic parameters


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
(defstruct wave
  "Wave data structure for audio waveform (matches raylib Wave)"
  (frame-count 0 :type fixnum)                  ; Total number of frames (considering channels)
  (sample-rate 44100 :type fixnum)              ; Frequency (samples per second)
  (sample-size 16 :type fixnum)                 ; Bit depth (bits per sample): 8, 16, 32
  (channels 2 :type fixnum)                     ; Number of channels (1-mono, 2-stereo, ...)
  (data nil :type (or null (simple-array (unsigned-byte 8) (*))))) ; Buffer data pointer

;;; Audio Stream structure (matches raylib AudioStream)
(defstruct audio-stream
  "Audio stream for custom audio streaming (matches raylib AudioStream)" 
  (buffer nil)                                  ; Pointer to internal data used by the audio system
  (processor nil)                               ; Pointer to internal data processor, useful for audio effects
  (sample-rate 44100 :type fixnum)              ; Frequency (samples per second)
  (sample-size 16 :type fixnum)                 ; Bit depth (bits per sample): 8, 16, 32
  (channels 2 :type fixnum))                    ; Number of channels (1-mono, 2-stereo, ...)

;;; Sound structure (matches raylib Sound)
(defstruct sound
  "Sound structure for short audio samples (matches raylib Sound)"
  (stream nil :type (or null audio-stream))     ; Audio stream
  (frame-count 0 :type fixnum))                 ; Total number of frames (considering channels)

;;; Music structure (matches raylib Music)
(defstruct music
  "Music structure for long audio streams (matches raylib Music)"
  (stream nil :type (or null audio-stream))     ; Audio stream
  (frame-count 0 :type fixnum)                  ; Total number of frames (considering channels)
  (looping nil :type boolean)                   ; Music looping enable
  (ctx-type 0 :type fixnum)                     ; Type of music context (audio filetype)
  (ctx-data nil))                               ; Audio context data, depends on type

;;; System/Window config flags (matching raylib)
(defconstant +flag-vsync-hint+ (ash 1 6))
(defconstant +flag-fullscreen-mode+ (ash 1 1))
(defconstant +flag-window-resizable+ (ash 1 2))
(defconstant +flag-window-undecorated+ (ash 1 3))
(defconstant +flag-window-hidden+ (ash 1 7))
(defconstant +flag-window-minimized+ (ash 1 9))
(defconstant +flag-window-maximized+ (ash 1 10))
(defconstant +flag-window-unfocused+ (ash 1 11))
(defconstant +flag-window-topmost+ (ash 1 12))
(defconstant +flag-window-always-run+ (ash 1 8))
(defconstant +flag-window-transparent+ (ash 1 4))
(defconstant +flag-window-highdpi+ (ash 1 13))
(defconstant +flag-window-mouse-passthrough+ #x4000)
(defconstant +flag-window-borderless-windowed-mode+ #x8000)
(defconstant +flag-msaa-4x-hint+ (ash 1 5))
(defconstant +flag-interlaced-hint+ (ash 1 5))

;;; Key constants (matching raylib key codes)
(defconstant +key-null+ 0)
(defconstant +key-apostrophe+ 39)
(defconstant +key-comma+ 44)
(defconstant +key-minus+ 45)
(defconstant +key-period+ 46)
(defconstant +key-slash+ 47)
(defconstant +key-zero+ 48)
(defconstant +key-one+ 49)
(defconstant +key-two+ 50)
(defconstant +key-three+ 51)
(defconstant +key-four+ 52)
(defconstant +key-five+ 53)
(defconstant +key-six+ 54)
(defconstant +key-seven+ 55)
(defconstant +key-eight+ 56)
(defconstant +key-nine+ 57)
(defconstant +key-semicolon+ 59)
(defconstant +key-equal+ 61)
(defconstant +key-a+ 65)
(defconstant +key-b+ 66)
(defconstant +key-c+ 67)
(defconstant +key-d+ 68)
(defconstant +key-e+ 69)
(defconstant +key-f+ 70)
(defconstant +key-g+ 71)
(defconstant +key-h+ 72)
(defconstant +key-i+ 73)
(defconstant +key-j+ 74)
(defconstant +key-k+ 75)
(defconstant +key-l+ 76)
(defconstant +key-m+ 77)
(defconstant +key-n+ 78)
(defconstant +key-o+ 79)
(defconstant +key-p+ 80)
(defconstant +key-q+ 81)
(defconstant +key-r+ 82)
(defconstant +key-s+ 83)
(defconstant +key-t+ 84)
(defconstant +key-u+ 85)
(defconstant +key-v+ 86)
(defconstant +key-w+ 87)
(defconstant +key-x+ 88)
(defconstant +key-y+ 89)
(defconstant +key-z+ 90)

;; Function keys
(defconstant +key-space+ 32)
(defconstant +key-escape+ 256)
(defconstant +key-enter+ 257)
(defconstant +key-tab+ 258)
(defconstant +key-backspace+ 259)
(defconstant +key-insert+ 260)
(defconstant +key-delete+ 261)
(defconstant +key-right+ 262)
(defconstant +key-left+ 263)
(defconstant +key-down+ 264)
(defconstant +key-up+ 265)
(defconstant +key-page-up+ 266)
(defconstant +key-page-down+ 267)
(defconstant +key-home+ 268)
(defconstant +key-end+ 269)
(defconstant +key-caps-lock+ 280)
(defconstant +key-scroll-lock+ 281)
(defconstant +key-num-lock+ 282)
(defconstant +key-print-screen+ 283)
(defconstant +key-pause+ 284)
(defconstant +key-f1+ 290)
(defconstant +key-f2+ 291)
(defconstant +key-f3+ 292)
(defconstant +key-f4+ 293)
(defconstant +key-f5+ 294)
(defconstant +key-f6+ 295)
(defconstant +key-f7+ 296)
(defconstant +key-f8+ 297)
(defconstant +key-f9+ 298)
(defconstant +key-f10+ 299)
(defconstant +key-f11+ 300)
(defconstant +key-f12+ 301)

;; Modifier keys
(defconstant +key-left-shift+ 340)
(defconstant +key-left-control+ 341)
(defconstant +key-left-alt+ 342)
(defconstant +key-left-super+ 343)
(defconstant +key-right-shift+ 344)
(defconstant +key-right-control+ 345)
(defconstant +key-right-alt+ 346)
(defconstant +key-right-super+ 347)

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

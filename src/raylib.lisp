(in-package #:cl-raylib)
;;/**********************************************************************************************
;;*
;;*   raylib - A simple and easy-to-use library to enjoy videogames programming (www.raylib.com)
;;*
;;*   FEATURES:
;;*       - NO external dependencies, all required libraries included with raylib
;;*       - Multiplatform: Windows, Linux, FreeBSD, OpenBSD, NetBSD, DragonFly, MacOS, UWP, Android, Raspberry Pi, HTML5.
;;*       - Written in plain C code (C99) in PascalCase/camelCase notation
;;*       - Hardware accelerated with OpenGL (1.1, 2.1, 3.3 or ES2 - choose at compile)
;;*       - Unique OpenGL abstraction layer (usable as standalone module): [rlgl]
;;*       - Powerful fonts module (XNA SpriteFonts, BMFonts, TTF)
;;*       - Outstanding texture formats support, including compressed formats (DXT, ETC, ASTC)
;;*       - Full 3d support for 3d Shapes, Models, Billboards, Heightmaps and more!
;;*       - Flexible Materials system, supporting classic maps and PBR maps
;;*       - Skeletal Animation support (CPU bones-based animation)
;;*       - Shaders support, including Model shaders and Postprocessing shaders
;;*       - Powerful math module for Vector, Matrix and Quaternion operations: [raymath]
;;*       - Audio loading and playing with streaming support (WAV, OGG, MP3, FLAC, XM, MOD)
;;*       - VR stereo rendering with configurable HMD device parameters
;;*       - Bindings to multiple programming languages available!
;;*
;;*   NOTES:
;;*       One custom font is loaded by default when InitWindow() [core]
;;*       If using OpenGL 3.3 or ES2, one default shader is loaded automatically (internally defined) [rlgl]
;;*       If using OpenGL 3.3 or ES2, several vertex buffers (VAO/VBO) are created to manage lines-triangles-quads
;;*
;;*   DEPENDENCIES (included):
;;*       [core] rglfw (github.com/glfw/glfw) for window/context management and input (only PLATFORM_DESKTOP)
;;*       [rlgl] glad (github.com/Dav1dde/glad) for OpenGL 3.3 extensions loading (only PLATFORM_DESKTOP)
;;*       [raudio] miniaudio (github.com/dr-soft/miniaudio) for audio device/context management
;;*
;;*   OPTIONAL DEPENDENCIES (included):
;;*       [core] rgif (Charlie Tangora, Ramon Santamaria) for GIF recording
;;*       [textures] stb_image (Sean Barret) for images loading (BMP, TGA, PNG, JPEG, HDR...)
;;*       [textures] stb_image_write (Sean Barret) for image writting (BMP, TGA, PNG, JPG)
;;*       [textures] stb_image_resize (Sean Barret) for image resizing algorythms
;;*       [textures] stb_perlin (Sean Barret) for Perlin noise image generation
;;*       [text] stb_truetype (Sean Barret) for ttf fonts loading
;;*       [text] stb_rect_pack (Sean Barret) for rectangles packing
;;*       [models] par_shapes (Philip Rideout) for parametric 3d shapes generation
;;*       [models] tinyobj_loader_c (Syoyo Fujita) for models loading (OBJ, MTL)
;;*       [models] cgltf (Johannes Kuhlmann) for models loading (glTF)
;;*       [raudio] stb_vorbis (Sean Barret) for OGG audio loading
;;*       [raudio] dr_flac (David Reid) for FLAC audio file loading
;;*       [raudio] dr_mp3 (David Reid) for MP3 audio file loading
;;*       [raudio] jar_xm (Joshua Reisenauer) for XM audio module loading
;;*       [raudio] jar_mod (Joshua Reisenauer) for MOD audio module loading
;;*
;;*
;;*   LICENSE: zlib/libpng
;;*
;;*   raylib is licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;*   BSD-like license that allows static linking with closed source software:
;;*
;;*   Copyright (c) 2013-2019 Ramon Santamaria (@raysan5)
;;*
;;*   This software is provided "as-is", without any express or implied warranty. In no event
;;*   will the authors be held liable for any damages arising from the use of this software.
;;*
;;*   Permission is granted to anyone to use this software for any purpose, including commercial
;;*   applications, and to alter it and redistribute it freely, subject to the following restrictions:
;;*
;;*     1. The origin of this software must not be misrepresented; you must not claim that you
;;*     wrote the original software. If you use this software in a product, an acknowledgment
;;*     in the product documentation would be appreciated but is not required.
;;*
;;*     2. Altered source versions must be plainly marked as such, and must not be misrepresented
;;*     as being the original software.
;;*
;;*     3. This notice may not be removed or altered from any source distribution.
;;*
;;**********************************************************************************************/
;;
;;#ifndef RAYLIB_H
;;#define RAYLIB_H
;;
;;#include <stdarg.h>                             // Required for: va_list - Only used by TraceLogCallback
;;
;;#if defined(_WIN32) && defined(BUILD_LIBTYPE_SHARED)
;;    #define RLAPI __declspec(dllexport)         // We are building raylib as a Win32 shared library (.dll)
;;#elif defined(_WIN32) && defined(USE_LIBTYPE_SHARED)
;;    #define RLAPI __declspec(dllimport)         // We are using raylib as a Win32 shared library (.dll)
;;#else
;;    #define RLAPI   // We are building or using raylib as a static library (or Linux shared library)
;;#endif
;;
;;//----------------------------------------------------------------------------------
;;// Some basic Defines
;;//----------------------------------------------------------------------------------
;;#ifndef PI
;;    #define PI 3.14159265358979323846f
;;#endif
;;
;;#define DEG2RAD (PI/180.0f)
;;#define RAD2DEG (180.0f/PI)
;;
;;#define MAX_TOUCH_POINTS        10      // Maximum number of touch points supported
;;
;;// Shader and material limits
;;#define MAX_SHADER_LOCATIONS    32      // Maximum number of predefined locations stored in shader struct
;;#define MAX_MATERIAL_MAPS       12      // Maximum number of texture maps stored in shader struct
;;
;;// Allow custom memory allocators
;;#ifndef RL_MALLOC
;;    #define RL_MALLOC(sz)       malloc(sz)
;;#endif
;;#ifndef RL_CALLOC
;;    #define RL_CALLOC(n,sz)     calloc(n,sz)
;;#endif
;;#ifndef RL_FREE
;;    #define RL_FREE(p)          free(p)
;;#endif
;;
;;// NOTE: MSC C++ compiler does not support compound literals (C99 feature)
;;// Plain structures in C++ (without constructors) can be initialized from { } initializers.
;;#if defined(__cplusplus)
;;    #define CLITERAL
;;#else
;;    #define CLITERAL    (Color)
;;#endif
;;
;;// Some Basic Colors
;;// NOTE: Custom raylib color palette for amazing visuals on WHITE background
;;#define LIGHTGRAY  CLITERAL{ 200, 200, 200, 255 }   // Light Gray
(define-constant +lightgray+ '(200 200 200 255) :test #'equal)
;;#define GRAY       CLITERAL{ 130, 130, 130, 255 }   // Gray
(define-constant +gray+ '(130 130 130 255) :test #'equal)
;;#define DARKGRAY   CLITERAL{ 80, 80, 80, 255 }      // Dark Gray
(define-constant +darkgray+ '(80 80 80 255) :test #'equal)
;;#define YELLOW     CLITERAL{ 253, 249, 0, 255 }     // Yellow
(define-constant +yellow+ '(253 249 0 255) :test #'equal)
;;#define GOLD       CLITERAL{ 255, 203, 0, 255 }     // Gold
(define-constant +gold+ '(255 203 0 255) :test #'equal)
;;#define ORANGE     CLITERAL{ 255, 161, 0, 255 }     // Orange
(define-constant +orange+     '(255 161 0 255 ) :test #'equal)
;;#define PINK       CLITERAL{ 255, 109, 194, 255 }   // Pink
(define-constant +pink+       '(255 109 194 255) :test #'equal)
;;#define RED        CLITERAL{ 230, 41, 55, 255 }     // Red
(define-constant +red+        '( 230 41 55 255 ) :test #'equal)     
;;#define MAROON     CLITERAL{ 190, 33, 55, 255 }     // Maroon
(define-constant +maroon+      '(190 33 55 255) :test #'equal) 
;;#define GREEN      CLITERAL{ 0, 228, 48, 255 }      // Green
(define-constant +green+       '(0 228 48 255) :test #'equal) 
;;#define LIME       CLITERAL{ 0, 158, 47, 255 }      // Lime
(define-constant +lime+        '(0 158 47 255) :test #'equal) 
;;#define DARKGREEN  CLITERAL{ 0, 117, 44, 255 }      // Dark Green
(define-constant +darkgreen+   '(0 117 44 255) :test #'equal) 
;;#define SKYBLUE    CLITERAL{ 102, 191, 255, 255 }   // Sky Blue
(define-constant +skyblue+     '(102 191 255 255) :test #'equal) 
;;#define BLUE       CLITERAL{ 0, 121, 241, 255 }     // Blue
(define-constant +blue+        '(0 121 241 255) :test #'equal) 
;;#define DARKBLUE   CLITERAL{ 0, 82, 172, 255 }      // Dark Blue
(define-constant +darkblue+    '(0 82 172 255) :test #'equal) 
;;#define PURPLE     CLITERAL{ 200, 122, 255, 255 }   // Purple
(define-constant +purple+      '(200 122 255 255) :test #'equal) 
;;#define VIOLET     CLITERAL{ 135, 60, 190, 255 }    // Violet
(define-constant +violet+      '(135 60 190 255) :test #'equal) 
;;#define DARKPURPLE CLITERAL{ 112, 31, 126, 255 }    // Dark Purple
(define-constant +darkpurple+  '(112 31 126 255) :test #'equal) 
;;#define BEIGE      CLITERAL{ 211, 176, 131, 255 }   // Beige
(define-constant +beige+       '(211 176 131 255) :test #'equal) 
;;#define BROWN      CLITERAL{ 127, 106, 79, 255 }    // Brown
(define-constant +brown+       '(127 106 79 255) :test #'equal) 
;;#define DARKBROWN  CLITERAL{ 76, 63, 47, 255 }      // Dark Brown
(define-constant +darkbrown+   '(76 63 47 255) :test #'equal) 
;;
;;#define WHITE      CLITERAL{ 255, 255, 255, 255 }   // White
(define-constant +white+       '(255 255 255 255) :test #'equal) 
;;#define BLACK      CLITERAL{ 0, 0, 0, 255 }         // Black
(define-constant +black+       '(0 0 0 255) :test #'equal) 
;;#define BLANK      CLITERAL{ 0, 0, 0, 0 }           // Blank (Transparent)
(define-constant +blank+       '(0 0 0 0) :test #'equal) 
;;#define MAGENTA    CLITERAL{ 255, 0, 255, 255 }     // Magenta
(define-constant +magenta+     '(255 0 255 255) :test #'equal) 
;;#define RAYWHITE   CLITERAL{ 245, 245, 245, 255 }   // My own White (raylib logo)
(define-constant +raywhite+    '(245 245 245 255) :test #'equal)
;;
;;// Temporal hack to avoid breaking old codebases using
;;// deprecated raylib implementation of these functions
;;#define FormatText  TextFormat
;;#define SubText     TextSubtext
;;#define ShowWindow  UnhideWindow
;;
;;//----------------------------------------------------------------------------------
;;// Structures Definition
;;//----------------------------------------------------------------------------------
;;// Boolean type
;;#if defined(__STDC__) && __STDC_VERSION__ >= 199901L
;;    #include <stdbool.h>
;;#elif !defined(__cplusplus) && !defined(bool)
;;    typedef enum { false, true } bool;
;;#endif
;;
;;// Vector2 type
;;typedef struct Vector2 {
;;    float x;
;;    float y;
;;} Vector2;
(defcstruct (%vector2 :class vector2-type)
 "Vector2 type"
 (x :float)
 (y :float))

(defstruct vector2
 x
 y)

(defmethod translate-into-foreign-memory (object (type vector2-type) pointer)
  (with-foreign-slots ((x y) pointer (:struct %vector2))
                      (setf x (vector2-x object))
                      (setf y (vector2-y object))))

(defmethod translate-from-foreign (pointer (type vector2-type))
  (with-foreign-slots ((x y) pointer (:struct %vector2))
                      (make-vector2 :x x :y y)))

;;// Vector3 type
;;typedef struct Vector3 {
;;    float x;
;;    float y;
;;    float z;
;;} Vector3;
(defcstruct (%vector3 :class vector3-type)
 "Vector3 type"
 (x :float)
 (y :float)
 (z :float))

(defmethod translate-into-foreign-memory (object (type vector3-type) pointer)
  (with-foreign-slots ((x y z) pointer (:struct %vector3))
                      (setf x (nth 0 object))
                      (setf y (nth 1 object))
                      (setf z (nth 2 object))))

(defmethod translate-from-foreign (pointer (type vector3-type))
  (with-foreign-slots ((x y z) pointer (:struct %vector3))
                      (list x y z)))

;;// Vector4 type
;;typedef struct Vector4 {
;;    float x;
;;    float y;
;;    float z;
;;    float w;
;;} Vector4;
(defcstruct (%vector4 :class vector4-type)
 "Vector4 type"
 (x :float)
 (y :float)
 (z :float)
 (w :float))

(defmethod translate-into-foreign-memory (object (type vector4-type) pointer)
  (with-foreign-slots ((x y z w) pointer (:struct %vector4))
                      (setf x (nth 0 object))
                      (setf y (nth 1 object))
                      (setf z (nth 2 object))
		      (setf w (nth 3 object))))

(defmethod translate-from-foreign (pointer (type vector4-type))
  (with-foreign-slots ((x y z w) pointer (:struct %vector4))
                      (list x y z w)))
;;
;;// Quaternion type, same as Vector4
;;typedef Vector4 Quaternion;
;;
;;// Matrix type (OpenGL style 4x4 - right handed, column major)
;;typedef struct Matrix {
;;    float m0, m4, m8, m12;
;;    float m1, m5, m9, m13;
;;    float m2, m6, m10, m14;
;;    float m3, m7, m11, m15;
;;} Matrix;
(defcstruct (%matrix :class matrix-type)
  "Matrix type (OpenGL style 4x4"
  (m0 :float) (m4 :float) (m8 :float) (m12 :float)
  (m1 :float) (m5 :float) (m9 :float) (m13 :float)
  (m2 :float) (m6 :float) (m10 :float) (m14 :float)
  (m3 :float) (m7 :float) (m11 :float) (m15 :float))

(defmethod translate-into-foreign-memory (object (type matrix-type) pointer)
  (with-foreign-slots ((m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) pointer (:struct %matrix))
                      (setf m0 (nth 0 object))
                      (setf m1 (nth 1 object))
                      (setf m2 (nth 2 object))
                      (setf m3 (nth 3 object))
                      (setf m4 (nth 4 object))
                      (setf m5 (nth 5 object))
                      (setf m6 (nth 6 object))
                      (setf m7 (nth 7 object))
                      (setf m8 (nth 8 object))
                      (setf m9 (nth 9 object))
                      (setf m10 (nth 10 object))
                      (setf m11 (nth 11 object))
                      (setf m12 (nth 12 object))
                      (setf m13 (nth 13 object))
                      (setf m14 (nth 14 object))
                      (setf m15 (nth 15 object))))

(defmethod translate-from-foreign (pointer (type matrix-type))
  (with-foreign-slots ((m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) pointer (:struct %matrix))
                      (list m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15)))

;;// Color type, RGBA (32bit)
;;typedef struct Color {
;;    unsigned char r;
;;    unsigned char g;
;;    unsigned char b;
;;    unsigned char a;
;;} Color;
(defcstruct (%color :class color-type)
  "Color type, RGBA (32bit)"
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(defmethod translate-into-foreign-memory (object (type color-type) pointer)
  (with-foreign-slots ((r g b a) pointer (:struct %color))
                      (setf r (nth 0 object))
                      (setf g (nth 1 object))
                      (setf b (nth 2 object))
                      (setf a (nth 3 object))))

(defmethod translate-from-foreign (pointer (type color-type))
  (with-foreign-slots ((r g b a) pointer (:struct %color))
   (list r g b a)))

;;// Rectangle type
;;typedef struct Rectangle {
;;    float x;
;;    float y;
;;    float width;
;;    float height;
;;} Rectangle;
(defcstruct (%rectangle :class rectangle-type)
  "Rectangle type"
  (x :float)
  (y :float)
  (width :float)
  (height :float))

(defmethod translate-into-foreign-memory (object (type rectangle-type) pointer)
  (with-foreign-slots ((x y width height) pointer (:struct %rectangle))
                      (setf x (nth 0 object))
                      (setf y (nth 1 object))
                      (setf width (nth 2 object))
                      (setf height (nth 3 object))))

(defmethod translate-from-foreign (pointer (type rectangle-type))
  (with-foreign-slots ((x y width height) pointer (:struct %rectangle))
                      (list x y width height)))

;;// Image type, bpp always RGBA (32bit)
;;// NOTE: Data stored in CPU memory (RAM)
;;typedef struct Image {
;;    void *data;             // Image raw data
;;    int width;              // Image base width
;;    int height;             // Image base height
;;    int mipmaps;            // Mipmap levels, 1 by default
;;    int format;             // Data format (PixelFormat type)
;;} Image;
(defcstruct (%image :class image-type)
"Image type, bpp always RGBA (32bit)"
  (data :pointer)
  (width :int)
  (height :int)
  (mipmaps :int)
  (format :int))

(defmethod translate-into-foreign-memory (object (type image-type) pointer)
  (with-foreign-slots ((data width height mipmaps format) pointer (:struct %image))
                      (setf data (nth 0 object))
                      (setf width (nth 1 object))
                      (setf height (nth 2 object))
                      (setf mipmaps (nth 3 object))
                      (setf format (nth 4 object))))

(defmethod translate-from-foreign (pointer (type image-type))
  (with-foreign-slots ((data width height mipmaps format) pointer (:struct %image))
  (list data width height mipmaps format)))

;;// Texture2D type
;;// NOTE: Data stored in GPU memory
;;typedef struct Texture2D {
;;    unsigned int id;        // OpenGL texture id
;;    int width;              // Texture base width
;;    int height;             // Texture base height
;;    int mipmaps;            // Mipmap levels, 1 by default
;;    int format;             // Data format (PixelFormat type)
;;} Texture2D;
;;
;;// Texture type, same as Texture2D
;;typedef Texture2D Texture;
;;
(defcstruct (%texture :class texture-type)
  "Texture type"
  (id :unsigned-int)
  (width :int)
  (height :int)
  (mipmaps :int)
  (format :int))

(defmethod translate-into-foreign-memory (object (type texture-type) pointer)
  (with-foreign-slots ((id width height mipmaps format) pointer (:struct %texture))
                      (setf id (nth 0 object))
                      (setf width (nth 1 object))
                      (setf height (nth 2 object))
                      (setf mipmaps (nth 3 object))
                      (setf format (nth 4 object))))

(defmethod translate-from-foreign (pointer (type texture-type))
  (with-foreign-slots ((id width height mipmaps format) pointer (:struct %texture))
                      (list id width height mipmaps format)))

;;// TextureCubemap type, actually, same as Texture2D
;;typedef Texture2D TextureCubemap;
(defctype texture-cubemap (:struct %texture))

;;// RenderTexture2D type, for texture rendering
;;typedef struct RenderTexture2D {
;;    unsigned int id;        // OpenGL Framebuffer Object (FBO) id
;;    Texture2D texture;      // Color buffer attachment texture
;;    Texture2D depth;        // Depth buffer attachment texture
;;    bool depthTexture;      // Track if depth attachment is a texture or renderbuffer
;;} RenderTexture2D;
;;
;;// RenderTexture type, same as RenderTexture2D
;;typedef RenderTexture2D RenderTexture;
(defcstruct (%render-texture :class render-texture-type)
 "RenderTexture2D type, for texture rendering"
 (id :unsigned-int)
 (texture (:struct %texture))
 (depth (:struct %texture))
 (depth-texture :boolean))

(defmethod translate-into-foreign-memory (object (type render-texture-type) pointer)
  (with-foreign-slots ((id texture depth depth-texture) pointer (:struct %render-texture))
                      (setf id (nth 0 object))
                      (setf texture (nth 1 object))
                      (setf depth (nth 2 object))
                      (setf depth-texture (nth 3 object))))

(defmethod translate-from-foreign (pointer (type render-texture-type))
  (with-foreign-slots ((id texture depth depth-texture) pointer (:struct %render-texture))
                      (list id texture depth depth-texture)))
;;
;;// N-Patch layout info
;;typedef struct NPatchInfo {
;;    Rectangle sourceRec;   // Region in the texture
;;    int left;              // left border offset
;;    int top;               // top border offset
;;    int right;             // right border offset
;;    int bottom;            // bottom border offset
;;    int type;              // layout of the n-patch: 3x3, 1x3 or 3x1
;;} NPatchInfo;
(defcstruct (%patch-info :class patch-info-type)
 "N-Patch layout info"
 (source-rec (:struct %rectangle))
 (left :int)
 (top :int)
 (right :int)
 (bottom :int)
 (type :int))

(defmethod translate-into-foreign-memory (object (type patch-info-type) pointer)
  (with-foreign-slots ((source-rec left top right bottom type) pointer (:struct %patch-info))
                      (setf source-rec (nth 0 object))
                      (setf left (nth 1 object))
                      (setf top (nth 2 object))
                      (setf right (nth 3 object))
                      (setf bottom (nth 4 object))
                      (setf type (nth 5 object))))

(defmethod translate-from-foreign (pointer (type patch-info-type))
  (with-foreign-slots ((source-rec left top right bottom type) pointer (:struct %patch-info))
                      (list source-rec left top right bottom type)))

;;// Font character info
;;typedef struct CharInfo {
;;    int value;              // Character value (Unicode)
;;    Rectangle rec;          // Character rectangle in sprite font
;;    int offsetX;            // Character offset X when drawing
;;    int offsetY;            // Character offset Y when drawing
;;    int advanceX;           // Character advance position X
;;    unsigned char *data;    // Character pixel data (grayscale)
;;} CharInfo;
(defcstruct (%char-info :class char-info-type)
 "Font character info"
 (value :int)
 (rec (:struct %rectangle))
 (offset-x :int)
 (offset-y :int)
 (advance-x :int)
 (data :pointer))

(defmethod translate-into-foreign-memory (object (type char-info-type) pointer)
  (with-foreign-slots ((value rec offset-x offset-y advance-x data) pointer (:struct %char-info))
                      (setf value (nth 0 object))
                      (setf rec (nth 1 object))
                      (setf offset-x (nth 2 object))
                      (setf offset-y (nth 3 object))
                      (setf advance-x (nth 4 object))
                      (setf data (nth 5 object))))

(defmethod translate-from-foreign (pointer (type char-info-type))
  (with-foreign-slots ((value rec offset-x offset-y advance-x data) pointer (:struct %char-info))
                      (list value rec offset-x offset-y advance-x data)))
 
;;
;;// Font type, includes texture and charSet array data
;;typedef struct Font {
;;    Texture2D texture;      // Font texture
;;    int baseSize;           // Base size (default chars height)
;;    int charsCount;         // Number of characters
;;    CharInfo *chars;        // Characters info data
;;} Font;
;;
;;#define SpriteFont Font     // SpriteFont type fallback, defaults to Font
(defcstruct (%font :class font-type)
 "Font type, includes texture and charSet array data"
 (texture (:struct %texture))
 (base-size :int)
 (chars-count :int)
 (chars (:pointer (:struct %char-info))))

(defmethod translate-into-foreign-memory (object (type font-type) pointer)
  (with-foreign-slots ((texture base-size chars-count chars) pointer (:struct %font))
                      (setf texture (nth 0 object))
                      (setf base-size (nth 1 object))
                      (setf chars-count (nth 2 object))
                      (setf chars (nth 3 object))))

(defmethod translate-from-foreign (pointer (type font-type))
  (with-foreign-slots ((texture base-size chars-count chars) pointer (:struct %font))
                      (list texture base-size chars-count chars)))

;;// Camera type, defines a camera position/orientation in 3d space
;;typedef struct Camera3D {
;;    Vector3 position;       // Camera position
;;    Vector3 target;         // Camera target it looks-at
;;    Vector3 up;             // Camera up vector (rotation over its axis)
;;    float fovy;             // Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
;;    int type;               // Camera type, defines projection type: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
;;} Camera3D;
;;
;;#define Camera Camera3D     // Camera type fallback, defaults to Camera3D
(defcstruct (%camera3d :class camera3d-type)
 "Camera type, defines a camera position/orientation in 3d space"
 (position (:struct %vector3))
 (target (:struct %vector3))
 (up (:struct %vector3))
 (fovy :float)
 (type :int))

(defmethod translate-into-foreign-memory (object (type camera3d-type) pointer)
  (with-foreign-slots ((position target up fovy type) pointer (:struct %camera3d))
                      (setf position (nth 0 object))
                      (setf target (nth 1 object))
                      (setf up (nth 2 object))
                      (setf fovy (nth 3 object))
                      (setf type (nth 4 object))))

(defmethod translate-from-foreign (pointer (type camera3d-type))
  (with-foreign-slots ((position target up fovy type) pointer (:struct %camera3d))
                      (list position target up fovy type)))

;;// Camera2D type, defines a 2d camera
;;typedef struct Camera2D {
;;    Vector2 offset;         // Camera offset (displacement from target)
;;    Vector2 target;         // Camera target (rotation and zoom origin)
;;    float rotation;         // Camera rotation in degrees
;;    float zoom;             // Camera zoom (scaling), should be 1.0f by default
;;} Camera2D;
(defcstruct (%camera2d :class camera2d-type)
 "Camera2D type, defines a 2d camera"
 (offset (:struct %vector2))
 (target (:struct %vector2))
 (rotation :float)
 (zoom :float))

(defmethod translate-into-foreign-memory (object (type camera2d-type) pointer)
  (with-foreign-slots ((offset target rotation zoom) pointer (:struct %camera2d))
                      (setf offset (nth 0 object))
                      (setf target (nth 1 object))
                      (setf rotation (nth 2 object))
                      (setf zoom (nth 3 object))))

(defmethod translate-from-foreign (pointer (type camera2d-type))
  (with-foreign-slots ((offset target rotation zoom) pointer (:struct %camera2d))
                      (list offset target rotation zoom)))

;;// Vertex data definning a mesh
;;// NOTE: Data stored in CPU memory (and GPU)
;;typedef struct Mesh {
;;    int vertexCount;        // Number of vertices stored in arrays
;;    int triangleCount;      // Number of triangles stored (indexed or not)
;;
;;    // Default vertex data
;;    float *vertices;        // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
;;    float *texcoords;       // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
;;    float *texcoords2;      // Vertex second texture coordinates (useful for lightmaps) (shader-location = 5)
;;    float *normals;         // Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
;;    float *tangents;        // Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
;;    unsigned char *colors;  // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
;;    unsigned short *indices;// Vertex indices (in case vertex data comes indexed)
;;
;;    // Animation vertex data
;;    float *animVertices;    // Animated vertex positions (after bones transformations)
;;    float *animNormals;     // Animated normals (after bones transformations)
;;    int *boneIds;           // Vertex bone ids, up to 4 bones influence by vertex (skinning)
;;    float *boneWeights;     // Vertex bone weight, up to 4 bones influence by vertex (skinning)
;;
;;    // OpenGL identifiers
;;    unsigned int vaoId;     // OpenGL Vertex Array Object id
;;    unsigned int vboId[7];  // OpenGL Vertex Buffer Objects id (default vertex data)
;;} Mesh;
(defcstruct (%mesh :class mesh-type)
 "Vertex data definning a mesh"
  (vertex-count :int)
  (triangle-count :int)
  (vertices (:pointer :float))
  (texcoords (:pointer :float))
  (texcoords2 (:pointer :float))
  (normals (:pointer :float))
  (tangents (:pointer :float))
  (colors (:pointer :unsigned-char))
  (indices (:pointer :unsigned-short))
  (anim-vertices (:pointer :float))
  (anim-normals (:pointer :float))
  (bone-ids (:pointer :int))
  (bone-weights (:pointer :float))
  (vao-id :unsigned-int)
  (vbo-id :int :count 7))

(defmethod translate-into-foreign-memory (object (type mesh-type) pointer)
  (with-foreign-slots ((vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id) pointer (:struct %mesh))
                      (setf vertex-count (nth 0 object))
                      (setf triangle-count (nth 1 object))
                      (setf vertices (nth 2 object))
                      (setf texcoords (nth 3 object))
                      (setf texcoords2 (nth 4 object))
                      (setf normals (nth 5 object))
                      (setf tangents (nth 6 object))
                      (setf colors (nth 7 object))
                      (setf indices (nth 8 object))
                      (setf anim-vertices (nth 9 object))
                      (setf anim-normals (nth 10 object))
                      (setf bone-ids (nth 11 object))
                      (setf bone-weights (nth 12 object))
                      (setf vao-id (nth 13 object))
                      (setf vbo-id (nth 14 object))))

(defmethod translate-from-foreign (pointer (type mesh-type))
  (with-foreign-slots ((vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id) pointer (:struct %mesh))
                      (list vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id)))

;;// Shader type (generic)
;;typedef struct Shader {
;;    unsigned int id;                // Shader program id
;;    int locs[MAX_SHADER_LOCATIONS]; // Shader locations array
;;} Shader;
(defcstruct (%shader :class shader-type)
 "Shader type"
 (id :unsigned-int)
 (locs :int :count 32))
 
 (defmethod translate-into-foreign-memory (object (type shader-type) pointer)
 (with-foreign-slots ((id locs) pointer (:struct %shader))
                      (setf id (nth 0 object))
                      (setf locs (nth 1 object))))

(defmethod translate-from-foreign (pointer (type shader-type))
 (with-foreign-slots ((id locs) pointer (:struct %shader))
 (list id locs)))

;;// Material texture map
;;typedef struct MaterialMap {
;;    Texture2D texture;      // Material map texture
;;    Color color;            // Material map color
;;    float value;            // Material map value
;;} MaterialMap;
(defcstruct (%material-map :class material-map-type)
 "Material texture map"
 (texture (:struct %texture))
 (color (:struct %color))
 (value :float))
 
(defmethod translate-into-foreign-memory (object (type material-map-type) pointer)
 (with-foreign-slots ((texture color value) pointer (:struct %material-map))
                      (setf texture (nth 0 object))
                      (setf color (nth 1 object))
                      (setf value (nth 2 object))))

(defmethod translate-from-foreign (pointer (type material-map-type))
 (with-foreign-slots ((texture color value) pointer (:struct %material-map))
 (list texture color value)))
;;
;;// Material type (generic)
;;typedef struct Material {
;;    Shader shader;          // Material shader
;;    MaterialMap maps[MAX_MATERIAL_MAPS]; // Material maps
;;    float *params;          // Material generic parameters (if required)
;;} Material;
(defcstruct (%material :class material-type)
 "Material type"
 (shader (:struct %shader))
 (maps (:struct %material-map) :count 12)
 (params (:pointer :float)))

(defmethod translate-into-foreign-memory (object (type material-type) pointer)
 (with-foreign-slots ((shader maps params) pointer (:struct %material))
                      (setf shader (nth 0 object))
                      (setf maps (nth 1 object))
                      (setf params (nth 2 object))))

(defmethod translate-from-foreign (pointer (type material-type))
  (with-foreign-slots ((shader maps params) pointer (:struct %material))
                      (list shader maps params)))

;;
;;// Transformation properties
;;typedef struct Transform {
;;    Vector3 translation;    // Translation
;;    Quaternion rotation;    // Rotation
;;    Vector3 scale;          // Scale
;;} Transform;
;;
;;// Bone information
;;typedef struct BoneInfo {
;;    char name[32];          // Bone name
;;    int parent;             // Bone parent
;;} BoneInfo;
;;
;;// Model type
;;typedef struct Model {
;;    Matrix transform;       // Local transform matrix
;;
;;    int meshCount;          // Number of meshes
;;    Mesh *meshes;           // Meshes array
;;
;;    int materialCount;      // Number of materials
;;    Material *materials;    // Materials array
;;    int *meshMaterial;      // Mesh material number
;;
;;    // Animation data
;;    int boneCount;          // Number of bones
;;    BoneInfo *bones;        // Bones information (skeleton)
;;    Transform *bindPose;    // Bones base transformation (pose)
;;} Model;
(defcstruct (%model :class model-type)
 "Model type"
 (mesh (:struct %mesh))
 (transform (:struct %matrix))
 (material (:struct %material)))

(defmethod translate-into-foreign-memory (object (type model-type) pointer)
 (with-foreign-slots ((mesh transform material) pointer (:struct %model))
                      (setf mesh (nth 0 object))
                      (setf transform (nth 1 object))
                      (setf material (nth 2 object))))

(defmethod translate-from-foreign (pointer (type model-type))
 (with-foreign-slots ((mesh transform material) pointer (:struct %model))
                     (list mesh transform material)))
;;
;;// Model animation
;;typedef struct ModelAnimation {
;;    int boneCount;          // Number of bones
;;    BoneInfo *bones;        // Bones information (skeleton)
;;
;;    int frameCount;         // Number of animation frames
;;    Transform **framePoses; // Poses array by frame
;;} ModelAnimation;
;;
;;// Ray type (useful for raycast)
;;typedef struct Ray {
;;    Vector3 position;       // Ray position (origin)
;;    Vector3 direction;      // Ray direction
;;} Ray;
(defcstruct (%ray :class ray-type)
 "Ray type (useful for raycast)"
 (position (:struct %vector3))
 (direction (:struct %vector3)))

(defmethod translate-into-foreign-memory (object (type ray-type) pointer)
 (with-foreign-slots ((position direction) pointer (:struct %ray))
                      (setf position (nth 0 object))
                      (setf direction (nth 1 object))))

(defmethod translate-from-foreign (pointer (type ray-type))
 (with-foreign-slots ((position direction) pointer (:struct %ray))
 (list position direction)))
;;
;;// Raycast hit information
;;typedef struct RayHitInfo {
;;    bool hit;               // Did the ray hit something?
;;    float distance;         // Distance to nearest hit
;;    Vector3 position;       // Position of nearest hit
;;    Vector3 normal;         // Surface normal of hit
;;} RayHitInfo;
(defcstruct (%ray-hit-info :class ray-hit-info-type)
 "Raycast hit information"
 (hit :boolean)
 (distance :float)
 (position (:struct %vector3))
 (normal (:struct %vector3)))

(defmethod translate-into-foreign-memory (object (type ray-hit-info-type) pointer)
 (with-foreign-slots ((hit distance position normal) pointer (:struct %ray-hit-info))
                      (setf hit (nth 0 object))
                      (setf distance (nth 1 object))
                      (setf position (nth 2 object))
                      (setf normal (nth 3 object))))

(defmethod translate-from-foreign (pointer (type ray-hit-info-type))
 (with-foreign-slots ((hit distance position normal) pointer (:struct %ray-hit-info))
                     (list hit distance position normal)))

;;
;;// Bounding box type
;;typedef struct BoundingBox {
;;    Vector3 min;            // Minimum vertex box-corner
;;    Vector3 max;            // Maximum vertex box-corner
;;} BoundingBox;
(defcstruct (%bounding-box :class bounding-box-type)
 "Bounding box type"
 (min (:struct %vector3))
 (max (:struct %vector3)))

(defmethod translate-into-foreign-memory (object (type bounding-box-type) pointer)
 (with-foreign-slots ((min max) pointer (:struct %bounding-box))
                      (setf min (nth 0 object))
                      (setf max (nth 1 object))))

(defmethod translate-from-foreign (pointer (type bounding-box-type))
 (with-foreign-slots ((min max) pointer (:struct %bounding-box))
  (list min max)))

;;
;;// Wave type, defines audio wave data
;;typedef struct Wave {
;;    unsigned int sampleCount;   // Number of samples
;;    unsigned int sampleRate;    // Frequency (samples per second)
;;    unsigned int sampleSize;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
;;    unsigned int channels;      // Number of channels (1-mono, 2-stereo)
;;    void *data;                 // Buffer data pointer
;;} Wave;
(defcstruct (%wave :class wave-type)
 "Wave type, defines audio wave data"
 (sample-count :unsigned-int)
 (sample-rate :unsigned-int)
 (sample-size :unsigned-int)
 (channels :unsigned-int)
 (data :pointer))

(defmethod translate-into-foreign-memory (object (type wave-type) pointer)
 (with-foreign-slots ((sample-count sample-rate sample-size channels data) pointer (:struct %wave))
                      (setf sample-count (nth 0 object))
                      (setf sample-rate (nth 1 object))
                      (setf sample-size (nth 2 object))
                      (setf channels (nth 3 object))
                      (setf data (nth 4 object))))

(defmethod translate-from-foreign (pointer (type wave-type))
 (with-foreign-slots ((sample-count sample-rate sample-size channels data) pointer (:struct %wave))
 (list sample-count sample-rate sample-size channels data)))

;;// Sound source type
;;typedef struct Sound {
;;    void *audioBuffer;      // Pointer to internal data used by the audio system
;;
;;    unsigned int source;    // Audio source id
;;    unsigned int buffer;    // Audio buffer id
;;    int format;             // Audio format specifier
;;} Sound;
(defcstruct (%sound :class sound-type)
 "Sound source type"
 (audio-buffer :pointer)
 (source :unsigned-int)
 (buffer :unsigned-int)
 (format :int))

(defmethod translate-into-foreign-memory (object (type sound-type) pointer)
 (with-foreign-slots ((audio-buffer source buffer format) pointer (:struct %sound))
                      (setf audio-buffer (nth 0 object))
                      (setf source (nth 1 object))
                      (setf buffer (nth 2 object))
                      (setf format (nth 3 object))))

(defmethod translate-from-foreign (pointer (type sound-type))
 (with-foreign-slots ((audio-buffer source buffer format) pointer (:struct %sound))
 (list audio-buffer source buffer format)))

;;// Music type (file streaming from memory)
;;// NOTE: Anything longer than ~10 seconds should be streamed
;;typedef struct MusicData *Music;
(defctype music :pointer)
;;
;;// Audio stream type
;;// NOTE: Useful to create custom audio streams not bound to a specific file
;;typedef struct AudioStream {
;;    unsigned int sampleRate;    // Frequency (samples per second)
;;    unsigned int sampleSize;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
;;    unsigned int channels;      // Number of channels (1-mono, 2-stereo)
;;
;;    void *audioBuffer;          // Pointer to internal data used by the audio system.
;;
;;    int format;                 // Audio format specifier
;;    unsigned int source;        // Audio source id
;;    unsigned int buffers[2];    // Audio buffers (double buffering)
;;} AudioStream;
(defcstruct (%audio-stream :class audio-stream-type)
 "Audio stream type"
  (sample-rate :unsigned-int)
  (sample-size :unsigned-int)
  (channels :unsigned-int)
  (audio-buffer :pointer)
  (format :int)
  (source :unsigned-int)
  (buffers :unsigned-int :count 2))

(defmethod translate-into-foreign-memory (object (type audio-stream-type) pointer)
 (with-foreign-slots ((sample-rate sample-size channels audio-buffer format source buffers) pointer (:struct %audio-stream))
                      (setf sample-rate (nth 0 object))
                      (setf sample-size (nth 1 object))
                      (setf channels (nth 2 object))
                      (setf audio-buffer (nth 3 object))
                      (setf format (nth 4 object))
                      (setf source (nth 5 object))
                      (setf (mem-aref buffers :unsigned-int 0) (nth 6 object))
                      (setf (mem-aref buffers :unsigned-int 1) (nth 7 object))))

(defmethod translate-from-foreign (pointer (type audio-stream-type))
  (with-foreign-slots ((sample-rate sample-size channels audio-buffer format source buffers) pointer (:struct %audio-stream))
                      (list sample-rate sample-size channels audio-buffer format source (mem-aref buffers :unsigned-int 0) (mem-aref buffers :unsigned-int 1))))

;;// Head-Mounted-Display device parameters
;;typedef struct VrDeviceInfo {
;;    int hResolution;                // HMD horizontal resolution in pixels
;;    int vResolution;                // HMD vertical resolution in pixels
;;    float hScreenSize;              // HMD horizontal size in meters
;;    float vScreenSize;              // HMD vertical size in meters
;;    float vScreenCenter;            // HMD screen center in meters
;;    float eyeToScreenDistance;      // HMD distance between eye and display in meters
;;    float lensSeparationDistance;   // HMD lens separation distance in meters
;;    float interpupillaryDistance;   // HMD IPD (distance between pupils) in meters
;;    float lensDistortionValues[4];  // HMD lens distortion constant parameters
;;    float chromaAbCorrection[4];    // HMD chromatic aberration correction parameters
;;} VrDeviceInfo;
(defcstruct (%vr-device-info :class vr-device-info-type)
 "Head-Mounted-Display device parameters"
 (h-resolution :int)
 (v-resolution :int)
 (h-screen-size :float)
 (v-screen-size :float)
 (v-screen-center :float)
 (eye-to-screen-distance :float)
 (lens-separation-distance :float)
 (interpupillary-distance :float)
 (lens-distortion-values :float :count 4)
 (chroma-ab-correction :float :count 4))

(defmethod translate-into-foreign-memory (object (type vr-device-info-type) pointer)
 (with-foreign-slots ((h-resolution v-resolution h-screen-size v-screen-size v-screen-center eye-to-screen-distance lens-separation-distance interpupillary-distance lens-distortion-values chroma-ab-correction) pointer (:struct %vr-device-info))
                      (setf h-resolution (nth 0 object))
                      (setf v-resolution (nth 1 object))
                      (setf h-screen-size (nth 2 object))
                      (setf v-screen-size (nth 3 object))
                      (setf v-screen-center (nth 4 object))
                      (setf eye-to-screen-distance (nth 5 object))
                      (setf lens-separation-distance (nth 6 object))
                      (setf interpupillary-distance (nth 7 object))
                      (setf lens-distortion-values (nth 8 object))
                      (setf chroma-ab-correction (nth 9 object))))

(defmethod translate-from-foreign (pointer (type vr-device-info-type))
  (with-foreign-slots ((h-resolution v-resolution h-screen-size v-screen-size v-screen-center eye-to-screen-distance lens-separation-distance interpupillary-distance lens-distortion-values chroma-ab-correction) pointer (:struct %vr-device-info))
                      (list h-resolution v-resolution h-screen-size v-screen-size v-screen-center eye-to-screen-distance lens-separation-distance interpupillary-distance lens-distortion-values chroma-ab-correction)))
                       
;;//----------------------------------------------------------------------------------
;;// Enumerators Definition
;;//----------------------------------------------------------------------------------
;;// System config flags
;;// NOTE: Used for bit masks
;;typedef enum {
;;    FLAG_SHOW_LOGO          = 1,    // Set to show raylib logo at startup
;;    FLAG_FULLSCREEN_MODE    = 2,    // Set to run program in fullscreen
;;    FLAG_WINDOW_RESIZABLE   = 4,    // Set to allow resizable window
;;    FLAG_WINDOW_UNDECORATED = 8,    // Set to disable window decoration (frame and buttons)
;;    FLAG_WINDOW_TRANSPARENT = 16,   // Set to allow transparent window
;;    FLAG_WINDOW_HIDDEN      = 128,  // Set to create the window initially hidden
;;    FLAG_MSAA_4X_HINT       = 32,   // Set to try enabling MSAA 4X
;;    FLAG_VSYNC_HINT         = 64    // Set to try enabling V-Sync on GPU
;;} ConfigFlag;
;;
;;// Trace log type
;;typedef enum {
;;    LOG_ALL = 0,        // Display all logs
;;    LOG_TRACE,
;;    LOG_DEBUG,
;;    LOG_INFO,
;;    LOG_WARNING,
;;    LOG_ERROR,
;;    LOG_FATAL,
;;    LOG_NONE            // Disable logging
;;} TraceLogType;
;;
;;// Keyboard keys
;;typedef enum {
;;    // Alphanumeric keys
;;    KEY_APOSTROPHE      = 39,
;;    KEY_COMMA           = 44,
;;    KEY_MINUS           = 45,
;;    KEY_PERIOD          = 46,
;;    KEY_SLASH           = 47,
;;    KEY_ZERO            = 48,
;;    KEY_ONE             = 49,
;;    KEY_TWO             = 50,
;;    KEY_THREE           = 51,
;;    KEY_FOUR            = 52,
;;    KEY_FIVE            = 53,
;;    KEY_SIX             = 54,
;;    KEY_SEVEN           = 55,
;;    KEY_EIGHT           = 56,
;;    KEY_NINE            = 57,
;;    KEY_SEMICOLON       = 59,
;;    KEY_EQUAL           = 61,
;;    KEY_A               = 65,
;;    KEY_B               = 66,
;;    KEY_C               = 67,
;;    KEY_D               = 68,
;;    KEY_E               = 69,
;;    KEY_F               = 70,
;;    KEY_G               = 71,
;;    KEY_H               = 72,
;;    KEY_I               = 73,
;;    KEY_J               = 74,
;;    KEY_K               = 75,
;;    KEY_L               = 76,
;;    KEY_M               = 77,
;;    KEY_N               = 78,
;;    KEY_O               = 79,
;;    KEY_P               = 80,
;;    KEY_Q               = 81,
;;    KEY_R               = 82,
;;    KEY_S               = 83,
;;    KEY_T               = 84,
;;    KEY_U               = 85,
;;    KEY_V               = 86,
;;    KEY_W               = 87,
;;    KEY_X               = 88,
;;    KEY_Y               = 89,
;;    KEY_Z               = 90,
;;
;;    // Function keys
;;    KEY_SPACE           = 32,
;;    KEY_ESCAPE          = 256,
;;    KEY_ENTER           = 257,
;;    KEY_TAB             = 258,
;;    KEY_BACKSPACE       = 259,
;;    KEY_INSERT          = 260,
;;    KEY_DELETE          = 261,
;;    KEY_RIGHT           = 262,
;;    KEY_LEFT            = 263,
;;    KEY_DOWN            = 264,
;;    KEY_UP              = 265,
;;    KEY_PAGE_UP         = 266,
;;    KEY_PAGE_DOWN       = 267,
;;    KEY_HOME            = 268,
;;    KEY_END             = 269,
;;    KEY_CAPS_LOCK       = 280,
;;    KEY_SCROLL_LOCK     = 281,
;;    KEY_NUM_LOCK        = 282,
;;    KEY_PRINT_SCREEN    = 283,
;;    KEY_PAUSE           = 284,
;;    KEY_F1              = 290,
;;    KEY_F2              = 291,
;;    KEY_F3              = 292,
;;    KEY_F4              = 293,
;;    KEY_F5              = 294,
;;    KEY_F6              = 295,
;;    KEY_F7              = 296,
;;    KEY_F8              = 297,
;;    KEY_F9              = 298,
;;    KEY_F10             = 299,
;;    KEY_F11             = 300,
;;    KEY_F12             = 301,
;;    KEY_LEFT_SHIFT      = 340,
;;    KEY_LEFT_CONTROL    = 341,
;;    KEY_LEFT_ALT        = 342,
;;    KEY_LEFT_SUPER      = 343,
;;    KEY_RIGHT_SHIFT     = 344,
;;    KEY_RIGHT_CONTROL   = 345,
;;    KEY_RIGHT_ALT       = 346,
;;    KEY_RIGHT_SUPER     = 347,
;;    KEY_KB_MENU         = 348,
;;    KEY_LEFT_BRACKET    = 91,
;;    KEY_BACKSLASH       = 92,
;;    KEY_RIGHT_BRACKET   = 93,
;;    KEY_GRAVE           = 96,
;;
;;    // Keypad keys
;;    KEY_KP_0            = 320,
;;    KEY_KP_1            = 321,
;;    KEY_KP_2            = 322,
;;    KEY_KP_3            = 323,
;;    KEY_KP_4            = 324,
;;    KEY_KP_5            = 325,
;;    KEY_KP_6            = 326,
;;    KEY_KP_7            = 327,
;;    KEY_KP_8            = 328,
;;    KEY_KP_9            = 329,
;;    KEY_KP_DECIMAL      = 330,
;;    KEY_KP_DIVIDE       = 331,
;;    KEY_KP_MULTIPLY     = 332,
;;    KEY_KP_SUBTRACT     = 333,
;;    KEY_KP_ADD          = 334,
;;    KEY_KP_ENTER        = 335,
;;    KEY_KP_EQUAL        = 336
;;} KeyboardKey;
;;
;;// Android buttons
;;typedef enum {
;;    KEY_BACK            = 4,
;;    KEY_MENU            = 82,
;;    KEY_VOLUME_UP       = 24,
;;    KEY_VOLUME_DOWN     = 25
;;} AndroidButton;
;;
;;// Mouse buttons
;;typedef enum {
;;    MOUSE_LEFT_BUTTON   = 0,
;;    MOUSE_RIGHT_BUTTON  = 1,
;;    MOUSE_MIDDLE_BUTTON = 2
;;} MouseButton;
;;
;;// Gamepad number
;;typedef enum {
;;    GAMEPAD_PLAYER1     = 0,
;;    GAMEPAD_PLAYER2     = 1,
;;    GAMEPAD_PLAYER3     = 2,
;;    GAMEPAD_PLAYER4     = 3
;;} GamepadNumber;
;;
;;// Gamepad Buttons
;;typedef enum {
;;    // This is here just for error checking
;;    GAMEPAD_BUTTON_UNKNOWN = 0,
;;
;;    // This is normally [A,B,X,Y]/[Circle,Triangle,Square,Cross]
;;    // No support for 6 button controllers though..
;;    GAMEPAD_BUTTON_LEFT_FACE_UP,
;;    GAMEPAD_BUTTON_LEFT_FACE_RIGHT,
;;    GAMEPAD_BUTTON_LEFT_FACE_DOWN,
;;    GAMEPAD_BUTTON_LEFT_FACE_LEFT,
;;  
;;    // This is normally a DPAD
;;    GAMEPAD_BUTTON_RIGHT_FACE_UP,
;;    GAMEPAD_BUTTON_RIGHT_FACE_RIGHT,
;;    GAMEPAD_BUTTON_RIGHT_FACE_DOWN,
;;    GAMEPAD_BUTTON_RIGHT_FACE_LEFT,
;;
;;    // Triggers
;;    GAMEPAD_BUTTON_LEFT_TRIGGER_1,
;;    GAMEPAD_BUTTON_LEFT_TRIGGER_2,
;;    GAMEPAD_BUTTON_RIGHT_TRIGGER_1,
;;    GAMEPAD_BUTTON_RIGHT_TRIGGER_2,
;;
;;    // These are buttons in the center of the gamepad
;;    GAMEPAD_BUTTON_MIDDLE_LEFT,     //PS3 Select
;;    GAMEPAD_BUTTON_MIDDLE,          //PS Button/XBOX Button
;;    GAMEPAD_BUTTON_MIDDLE_RIGHT,    //PS3 Start
;;
;;    // These are the joystick press in buttons
;;    GAMEPAD_BUTTON_LEFT_THUMB,
;;    GAMEPAD_BUTTON_RIGHT_THUMB
;;} GamepadButton;
;;
;;typedef enum {
;;    // This is here just for error checking
;;    GAMEPAD_AXIS_UNKNOWN = 0,
;;
;;    // Left stick
;;    GAMEPAD_AXIS_LEFT_X,
;;    GAMEPAD_AXIS_LEFT_Y,
;;
;;    // Right stick
;;    GAMEPAD_AXIS_RIGHT_X,
;;    GAMEPAD_AXIS_RIGHT_Y,
;;
;;    // Pressure levels for the back triggers
;;    GAMEPAD_AXIS_LEFT_TRIGGER,      // [1..-1] (pressure-level)
;;    GAMEPAD_AXIS_RIGHT_TRIGGER      // [1..-1] (pressure-level)
;;} GamepadAxis;
;;
;;// Shader location point type
;;typedef enum {
;;    LOC_VERTEX_POSITION = 0,
;;    LOC_VERTEX_TEXCOORD01,
;;    LOC_VERTEX_TEXCOORD02,
;;    LOC_VERTEX_NORMAL,
;;    LOC_VERTEX_TANGENT,
;;    LOC_VERTEX_COLOR,
;;    LOC_MATRIX_MVP,
;;    LOC_MATRIX_MODEL,
;;    LOC_MATRIX_VIEW,
;;    LOC_MATRIX_PROJECTION,
;;    LOC_VECTOR_VIEW,
;;    LOC_COLOR_DIFFUSE,
;;    LOC_COLOR_SPECULAR,
;;    LOC_COLOR_AMBIENT,
;;    LOC_MAP_ALBEDO,          // LOC_MAP_DIFFUSE
;;    LOC_MAP_METALNESS,       // LOC_MAP_SPECULAR
;;    LOC_MAP_NORMAL,
;;    LOC_MAP_ROUGHNESS,
;;    LOC_MAP_OCCLUSION,
;;    LOC_MAP_EMISSION,
;;    LOC_MAP_HEIGHT,
;;    LOC_MAP_CUBEMAP,
;;    LOC_MAP_IRRADIANCE,
;;    LOC_MAP_PREFILTER,
;;    LOC_MAP_BRDF
;;} ShaderLocationIndex;
;;
;;#define LOC_MAP_DIFFUSE      LOC_MAP_ALBEDO
;;#define LOC_MAP_SPECULAR     LOC_MAP_METALNESS
;;
;;// Shader uniform data types
;;typedef enum {
;;    UNIFORM_FLOAT = 0,
;;    UNIFORM_VEC2,
;;    UNIFORM_VEC3,
;;    UNIFORM_VEC4,
;;    UNIFORM_INT,
;;    UNIFORM_IVEC2,
;;    UNIFORM_IVEC3,
;;    UNIFORM_IVEC4,
;;    UNIFORM_SAMPLER2D
;;} ShaderUniformDataType;
;;
;;// Material map type
;;typedef enum {
;;    MAP_ALBEDO    = 0,       // MAP_DIFFUSE
;;    MAP_METALNESS = 1,       // MAP_SPECULAR
;;    MAP_NORMAL    = 2,
;;    MAP_ROUGHNESS = 3,
;;    MAP_OCCLUSION,
;;    MAP_EMISSION,
;;    MAP_HEIGHT,
;;    MAP_CUBEMAP,             // NOTE: Uses GL_TEXTURE_CUBE_MAP
;;    MAP_IRRADIANCE,          // NOTE: Uses GL_TEXTURE_CUBE_MAP
;;    MAP_PREFILTER,           // NOTE: Uses GL_TEXTURE_CUBE_MAP
;;    MAP_BRDF
;;} MaterialMapType;
;;
;;#define MAP_DIFFUSE      MAP_ALBEDO
;;#define MAP_SPECULAR     MAP_METALNESS
;;
;;// Pixel formats
;;// NOTE: Support depends on OpenGL version and platform
;;typedef enum {
;;    UNCOMPRESSED_GRAYSCALE = 1,     // 8 bit per pixel (no alpha)
;;    UNCOMPRESSED_GRAY_ALPHA,        // 8*2 bpp (2 channels)
;;    UNCOMPRESSED_R5G6B5,            // 16 bpp
;;    UNCOMPRESSED_R8G8B8,            // 24 bpp
;;    UNCOMPRESSED_R5G5B5A1,          // 16 bpp (1 bit alpha)
;;    UNCOMPRESSED_R4G4B4A4,          // 16 bpp (4 bit alpha)
;;    UNCOMPRESSED_R8G8B8A8,          // 32 bpp
;;    UNCOMPRESSED_R32,               // 32 bpp (1 channel - float)
;;    UNCOMPRESSED_R32G32B32,         // 32*3 bpp (3 channels - float)
;;    UNCOMPRESSED_R32G32B32A32,      // 32*4 bpp (4 channels - float)
;;    COMPRESSED_DXT1_RGB,            // 4 bpp (no alpha)
;;    COMPRESSED_DXT1_RGBA,           // 4 bpp (1 bit alpha)
;;    COMPRESSED_DXT3_RGBA,           // 8 bpp
;;    COMPRESSED_DXT5_RGBA,           // 8 bpp
;;    COMPRESSED_ETC1_RGB,            // 4 bpp
;;    COMPRESSED_ETC2_RGB,            // 4 bpp
;;    COMPRESSED_ETC2_EAC_RGBA,       // 8 bpp
;;    COMPRESSED_PVRT_RGB,            // 4 bpp
;;    COMPRESSED_PVRT_RGBA,           // 4 bpp
;;    COMPRESSED_ASTC_4x4_RGBA,       // 8 bpp
;;    COMPRESSED_ASTC_8x8_RGBA        // 2 bpp
;;} PixelFormat;
;;
;;// Texture parameters: filter mode
;;// NOTE 1: Filtering considers mipmaps if available in the texture
;;// NOTE 2: Filter is accordingly set for minification and magnification
;;typedef enum {
;;    FILTER_POINT = 0,               // No filter, just pixel aproximation
;;    FILTER_BILINEAR,                // Linear filtering
;;    FILTER_TRILINEAR,               // Trilinear filtering (linear with mipmaps)
;;    FILTER_ANISOTROPIC_4X,          // Anisotropic filtering 4x
;;    FILTER_ANISOTROPIC_8X,          // Anisotropic filtering 8x
;;    FILTER_ANISOTROPIC_16X,         // Anisotropic filtering 16x
;;} TextureFilterMode;
;;
;;// Cubemap layout type
;;typedef enum {
;;    CUBEMAP_AUTO_DETECT = 0,        // Automatically detect layout type
;;    CUBEMAP_LINE_VERTICAL,          // Layout is defined by a vertical line with faces
;;    CUBEMAP_LINE_HORIZONTAL,        // Layout is defined by an horizontal line with faces
;;    CUBEMAP_CROSS_THREE_BY_FOUR,    // Layout is defined by a 3x4 cross with cubemap faces
;;    CUBEMAP_CROSS_FOUR_BY_THREE,    // Layout is defined by a 4x3 cross with cubemap faces
;;    CUBEMAP_PANORAMA                // Layout is defined by a panorama image (equirectangular map)
;;} CubemapLayoutType;
;;
;;// Texture parameters: wrap mode
;;typedef enum {
;;    WRAP_REPEAT = 0,        // Repeats texture in tiled mode
;;    WRAP_CLAMP,             // Clamps texture to edge pixel in tiled mode
;;    WRAP_MIRROR_REPEAT,     // Mirrors and repeats the texture in tiled mode
;;    WRAP_MIRROR_CLAMP       // Mirrors and clamps to border the texture in tiled mode
;;} TextureWrapMode;
;;
;;// Font type, defines generation method
;;typedef enum {
;;    FONT_DEFAULT = 0,       // Default font generation, anti-aliased
;;    FONT_BITMAP,            // Bitmap font generation, no anti-aliasing
;;    FONT_SDF                // SDF font generation, requires external shader
;;} FontType;
;;
;;// Color blending modes (pre-defined)
;;typedef enum {
;;    BLEND_ALPHA = 0,        // Blend textures considering alpha (default)
;;    BLEND_ADDITIVE,         // Blend textures adding colors
;;    BLEND_MULTIPLIED        // Blend textures multiplying colors
;;} BlendMode;
;;
;;// Gestures type
;;// NOTE: It could be used as flags to enable only some gestures
;;typedef enum {
;;    GESTURE_NONE        = 0,
;;    GESTURE_TAP         = 1,
;;    GESTURE_DOUBLETAP   = 2,
;;    GESTURE_HOLD        = 4,
;;    GESTURE_DRAG        = 8,
;;    GESTURE_SWIPE_RIGHT = 16,
;;    GESTURE_SWIPE_LEFT  = 32,
;;    GESTURE_SWIPE_UP    = 64,
;;    GESTURE_SWIPE_DOWN  = 128,
;;    GESTURE_PINCH_IN    = 256,
;;    GESTURE_PINCH_OUT   = 512
;;} GestureType;
;;
;;// Camera system modes
;;typedef enum {
;;    CAMERA_CUSTOM = 0,
;;    CAMERA_FREE,
;;    CAMERA_ORBITAL,
;;    CAMERA_FIRST_PERSON,
;;    CAMERA_THIRD_PERSON
;;} CameraMode;
;;
;;// Camera projection modes
;;typedef enum {
;;    CAMERA_PERSPECTIVE = 0,
;;    CAMERA_ORTHOGRAPHIC
;;} CameraType;
;;
;;// Type of n-patch
;;typedef enum {
;;    NPT_9PATCH = 0,         // Npatch defined by 3x3 tiles
;;    NPT_3PATCH_VERTICAL,    // Npatch defined by 1x3 tiles
;;    NPT_3PATCH_HORIZONTAL   // Npatch defined by 3x1 tiles
;;} NPatchType;
;;
;;// Callbacks to be implemented by users
;;typedef void (*TraceLogCallback)(int logType, const char *text, va_list args);
;;
;;#if defined(__cplusplus)
;;extern "C" {            // Prevents name mangling of functions
;;#endif
;;
;;//------------------------------------------------------------------------------------
;;// Global Variables Definition
;;//------------------------------------------------------------------------------------
;;// It's lonely here...
;;
;;//------------------------------------------------------------------------------------
;;// Window and Graphics Device Functions (Module: core)
;;//------------------------------------------------------------------------------------
;;
;;// Window-related functions
;;RLAPI void InitWindow(int width, int height, const char *title);  // Initialize window and OpenGL context
(defcfun "InitWindow" :void
 "Initialize window and OpenGL context"
 (width :int)
 (height :int)
 (title :string))

;;RLAPI bool WindowShouldClose(void);                               // Check if KEY_ESCAPE pressed or Close icon pressed
(defcfun "WindowShouldClose" :boolean
 "Check if KEY_ESCAPE pressed or Close icon pressed")

;;RLAPI void CloseWindow(void);                                     // Close window and unload OpenGL context
(defcfun "CloseWindow" :void
 "Close window and unload OpenGL context")

;;RLAPI bool IsWindowReady(void);                                   // Check if window has been initialized successfully
(defcfun "IsWindowReady" :boolean
 "Check if window has been initialized successfully")

;;RLAPI bool IsWindowMinimized(void);                               // Check if window has been minimized (or lost focus)
(defcfun "IsWindowMinimized" :boolean
 "Check if window has been minimized (or lost focus)")

;;RLAPI bool IsWindowResized(void);                                 // Check if window has been resized
(defcfun "IsWindowResized" :boolean
 "Check if window has been resized")

;;RLAPI bool IsWindowHidden(void);                                  // Check if window is currently hidden
(defcfun "IsWindowHidden" :boolean
 "Check if window is currently hidden")

;;RLAPI void ToggleFullscreen(void);                                // Toggle fullscreen mode (only PLATFORM_DESKTOP)
(defcfun "ToggleFullscreen" :void
 "Toggle fullscreen mode (only PLATFORM_DESKTOP)")

;;RLAPI void UnhideWindow(void);                                    // Show the window
(defcfun "UnhideWindow" :void
 "Show the window")

;;RLAPI void HideWindow(void);                                      // Hide the window
(defcfun "HideWindow" :void
 "Hide the window")

;;RLAPI void SetWindowIcon(Image image);                            // Set icon for window (only PLATFORM_DESKTOP)
(defcfun "SetWindowIcon" :void
 "Set icon for window (only PLATFORM_DESKTOP)"
 (image (:struct %image)))

;;RLAPI void SetWindowTitle(const char *title);                     // Set title for window (only PLATFORM_DESKTOP)
(defcfun "SetWindowTitle" :void
 "Set title for window (only PLATFORM_DESKTOP)"
 (title :string))

;;RLAPI void SetWindowPosition(int x, int y);                       // Set window position on screen (only PLATFORM_DESKTOP)
(defcfun "SetWindowPosition" :void
 "Set window position on screen (only PLATFORM_DESKTOP)"
 (x :int)
 (y :int))

;;RLAPI void SetWindowMonitor(int monitor);                         // Set monitor for the current window (fullscreen mode)
(defcfun "SetWindowMonitor" :void
 "Set monitor for the current window (fullscreen mode)"
 (monitor :int))

;;RLAPI void SetWindowMinSize(int width, int height);               // Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
(defcfun "SetWindowMinSize" :void
 "Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)"
 (width :int)
 (height :int))

;;RLAPI void SetWindowSize(int width, int height);                  // Set window dimensions
(defcfun "SetWindowSize" :void
 "Set window dimensions"
 (width :int)
 (height :int))

;;RLAPI void *GetWindowHandle(void);                                // Get native window handle
(defcfun "GetWindowHandle" :pointer
 "Get native window handle")

;;RLAPI int GetScreenWidth(void);                                   // Get current screen width
(defcfun "GetScreenWidth" :int
 "Get current screen width")

;;RLAPI int GetScreenHeight(void);                                  // Get current screen height
(defcfun "GetScreenHeight" :int
 "Get current screen height")

;;RLAPI int GetMonitorCount(void);                                  // Get number of connected monitors
(defcfun "GetMonitorCount" :int
 "Get number of connected monitors")

;;RLAPI int GetMonitorWidth(int monitor);                           // Get primary monitor width
(defcfun "GetMonitorWidth" :int
 "Get primary monitor width"
 (monitor :int))

;;RLAPI int GetMonitorHeight(int monitor);                          // Get primary monitor height
(defcfun "GetMonitorHeight" :int
 "Get primary monitor height"
 (monitor :int))

;;RLAPI int GetMonitorPhysicalWidth(int monitor);                   // Get primary monitor physical width in millimetres
(defcfun "GetMonitorPhysicalWidth" :int
 "Get primary monitor physical width in millimetres"
 (monitor :int))

;;RLAPI int GetMonitorPhysicalHeight(int monitor);                  // Get primary monitor physical height in millimetres
(defcfun "GetMonitorPhysicalHeight" :int
 "Get primary monitor physical height in millimetres"
 (monitor :int))

;;RLAPI const char *GetMonitorName(int monitor);                    // Get the human-readable, UTF-8 encoded name of the primary monitor
(defcfun "GetMonitorName" :string
 "Get the human-readable, UTF-8 encoded name of the primary monitor"
 (monitor :int))

;;RLAPI const char *GetClipboardText(void);                         // Get clipboard text content
(defcfun "GetClipboardText" :string
 "Get clipboard text content")

;;RLAPI void SetClipboardText(const char *text);                    // Set clipboard text content
(defcfun "SetClipboardText" :void
 "Set clipboard text content"
 (text :string))

;;// Cursor-related functions
;;RLAPI void ShowCursor(void);                                      // Shows cursor
(defcfun "ShowCursor" :void
 "Shows cursor")

;;RLAPI void HideCursor(void);                                      // Hides cursor
(defcfun "HideCursor" :void
 "Hides cursor")

;;RLAPI bool IsCursorHidden(void);                                  // Check if cursor is not visible
(defcfun "IsCursorHidden" :boolean
 "Check if cursor is not visible")

;;RLAPI void EnableCursor(void);                                    // Enables cursor (unlock cursor)
(defcfun "EnableCursor" :void
 "Enables cursor (unlock cursor)")

;;RLAPI void DisableCursor(void);                                   // Disables cursor (lock cursor)
(defcfun "DisableCursor" :void
 "Disables cursor (lock cursor)")

;;// Drawing-related functions
;;RLAPI void ClearBackground(Color color);                          // Set background color (framebuffer clear color)
(defcfun "ClearBackground" :void
 "Set background color (framebuffer clear color)"
 (color (:struct %color)))

;;RLAPI void BeginDrawing(void);                                    // Setup canvas (framebuffer) to start drawing
(defcfun "BeginDrawing" :void
 "Setup canvas (framebuffer) to start drawing")

;;RLAPI void EndDrawing(void);                                      // End canvas drawing and swap buffers (double buffering)
(defcfun "EndDrawing" :void
 "End canvas drawing and swap buffers (double buffering)")

;;RLAPI void BeginMode2D(Camera2D camera);                          // Initialize 2D mode with custom camera (2D)
(defcfun "BeginMode2D" :void
 "Initialize 2D mode with custom camera (2D)"
 (camera (:struct %camera2d)))

;;RLAPI void EndMode2D(void);                                       // Ends 2D mode with custom camera
(defcfun "EndMode2D" :void
 "Ends 2D mode with custom camera")

;;RLAPI void BeginMode3D(Camera3D camera);                          // Initializes 3D mode with custom camera (3D)
(defcfun "BeginMode3D" :void
 "Initializes 3D mode with custom camera (3D)"
 (camera (:struct %camera3d)))

;;RLAPI void EndMode3D(void);                                       // Ends 3D mode and returns to default 2D orthographic mode
(defcfun "EndMode3D" :void
 "Ends 3D mode and returns to default 2D orthographic mode")

;;RLAPI void BeginTextureMode(RenderTexture2D target);              // Initializes render texture for drawing
(defcfun "BeginTextureMode" :void
 "Initializes render texture for drawing"
 (target (:struct %render-texture)))

;;RLAPI void EndTextureMode(void);                                  // Ends drawing to render texture
(defcfun "EndTextureMode" :void
 "Ends drawing to render texture")

;;// Screen-space-related functions
;;RLAPI Ray GetMouseRay(Vector2 mousePosition, Camera camera);      // Returns a ray trace from mouse position
(defcfun "GetMouseRay" (:struct %ray)
 "Returns a ray trace from mouse position"
 (mouse-position (:struct %vector2))
 (camera (:struct %camera3d)))

;;RLAPI Vector2 GetWorldToScreen(Vector3 position, Camera camera);  // Returns the screen space position for a 3d world space position
(defcfun "GetWorldToScreen" (:struct %vector2)
 "Returns the screen space position for a 3d world space position"
 (position (:struct %vector3))
 (camera (:struct %camera3d)))

;;RLAPI Matrix GetCameraMatrix(Camera camera);                      // Returns camera transform matrix (view matrix)
(defcfun "GetCameraMatrix" (:struct %matrix)
 "Returns camera transform matrix (view matrix)"
 (camera (:struct %camera3d)))
;;
;;// Timing-related functions
;;RLAPI void SetTargetFPS(int fps);                                 // Set target FPS (maximum)
(defcfun "SetTargetFPS" :void
 "Set target FPS (maximum)"
 (fps :int))

;;RLAPI int GetFPS(void);                                           // Returns current FPS
(defcfun "GetFPS" :int
 "Returns current FPS")

;;RLAPI float GetFrameTime(void);                                   // Returns time in seconds for last frame drawn
(defcfun "GetFrameTime" :float
 "Returns time in seconds for last frame drawn")

;;RLAPI double GetTime(void);                                       // Returns elapsed time in seconds since InitWindow()
(defcfun "GetTime" :double
 "Returns elapsed time in seconds since InitWindow()")

;;// Color-related functions
;;RLAPI int ColorToInt(Color color);                                // Returns hexadecimal value for a Color
(defcfun "ColorToInt" :int
 "Returns hexadecimal value for a Color"
 (color (:struct %color)))

;;RLAPI Vector4 ColorNormalize(Color color);                        // Returns color normalized as float [0..1]
(defcfun "ColorNormalize" (:struct %vector4)
 "Returns color normalized as float [0..1]"
 (color (:struct %color)))

;;RLAPI Vector3 ColorToHSV(Color color);                            // Returns HSV values for a Color
(defcfun "ColorToHSV" (:struct %vector3)
 "Returns HSV values for a Color"
 (color (:struct %color)))

;;RLAPI Color ColorFromHSV(Vector3 hsv);                            // Returns a Color from HSV values
(defcfun "ColorFromHSV" (:struct %color)
 "Returns a Color from HSV values"
 (hsv (:struct %vector3)))

;;RLAPI Color GetColor(int hexValue);                               // Returns a Color struct from hexadecimal value
(defcfun "GetColor" (:struct %color)
 "Returns a Color struct from hexadecimal value"
 (hex-value :int))

;;RLAPI Color Fade(Color color, float alpha);                       // Color fade-in or fade-out, alpha goes from 0.0f to 1.0f
(defcfun "Fade" (:struct %color)
 "Color fade-in or fade-out, alpha goes from 0.0f to 1.0f"
 (color (:struct %color))
 (alpha :float))

;;// Misc. functions
;;RLAPI void SetConfigFlags(unsigned char flags);                   // Setup window configuration flags (view FLAGS)
(defcfun "SetConfigFlags" :void
 "Setup window configuration flags (view FLAGS)"
 (flags :unsigned-char))

;;RLAPI void SetTraceLogLevel(int logType);                         // Set the current threshold (minimum) log level
(defcfun "SetTraceLogLevel" :void
 "Set the current threshold (minimum) log level"
 (log-type :int))

;;RLAPI void SetTraceLogExit(int logType);                          // Set the exit threshold (minimum) log level
(defcfun "SetTraceLogExit" :void
 "Set the exit threshold (minimum) log level"
 (log-type :int))

;;RLAPI void SetTraceLogCallback(TraceLogCallback callback);        // Set a trace log callback to enable custom logging
;;RLAPI void TraceLog(int logType, const char *text, ...);          // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR)
(defcfun "TraceLog" :void
 "Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR)"
 (log-type :int)
 (text :string)
 &rest)

;;RLAPI void TakeScreenshot(const char *fileName);                  // Takes a screenshot of current screen (saved a .png)
(defcfun "TakeScreenshot" :void
 "Takes a screenshot of current screen (saved a .png)"
 (file-name :string))

;;RLAPI int GetRandomValue(int min, int max);                       // Returns a random value between min and max (both included)
(defcfun "GetRandomValue" :int
 "Returns a random value between min and max (both included)"
 (min :int)
 (max :int))

;;// Files management functions
;;RLAPI bool FileExists(const char *fileName);                      // Check if file exists
(defcfun "FileExists" :boolean
 "Check if file exists"
 (filename :string))

;;RLAPI bool IsFileExtension(const char *fileName, const char *ext);// Check file extension
(defcfun "IsFileExtension" :boolean
 "Check file extension"
 (file-name :string)
 (ext :string))

;;RLAPI const char *GetExtension(const char *fileName);             // Get pointer to extension for a filename string
(defcfun "GetExtension" :string
 "Get pointer to extension for a filename string"
 (file-name :string))

;;RLAPI const char *GetFileName(const char *filePath);              // Get pointer to filename for a path string
(defcfun "GetFileName" :string
 "Get pointer to filename for a path string"
 (file-name :string))

;;RLAPI const char *GetFileNameWithoutExt(const char *filePath);    // Get filename string without extension (memory should be freed)
(defcfun "GetFileNameWithoutExt" :string
 "Get filename string without extension (memory should be freed)"
 (file-name :string))

;;RLAPI const char *GetDirectoryPath(const char *fileName);         // Get full path for a given fileName (uses static string)
(defcfun "GetDirectoryPath" :string
 "Get full path for a given fileName (uses static string)"
 (file-name :string))

;;RLAPI const char *GetWorkingDirectory(void);                      // Get current working directory (uses static string)
(defcfun "GetWorkingDirectory" :string
 "Get current working directory (uses static string)")

;;RLAPI char **GetDirectoryFiles(const char *dirPath, int *count);  // Get filenames in a directory path (memory should be freed)
(defcfun "GetDirectoryFiles" :pointer
 "Get filenames in a directory path (memory should be freed)"
 (dir-path :string)
 (count (:pointer :int)))

;;RLAPI void ClearDirectoryFiles(void);                             // Clear directory files paths buffers (free memory)
(defcfun "ClearDirectoryFiles" :void
 "Clear directory files paths buffers (free memory)")

;;RLAPI bool ChangeDirectory(const char *dir);                      // Change working directory, returns true if success
(defcfun "ChangeDirectory" :bool
 "Change working directory, returns true if success"
 (dir :string))

;;RLAPI bool IsFileDropped(void);                                   // Check if a file has been dropped into window
(defcfun "IsFileDropped" :boolean
 "Check if a file has been dropped into window")

;;RLAPI char **GetDroppedFiles(int *count);                         // Get dropped files names (memory should be freed)
(defcfun "GetDroppedFiles" :pointer
 "Get dropped files names (memory should be freed)"
 (count :int))

;;RLAPI void ClearDroppedFiles(void);                               // Clear dropped files paths buffer (free memory)
(defcfun "ClearDroppedFiles" :void
 "Clear dropped files paths buffer (free memory)")

;;RLAPI long GetFileModTime(const char *fileName);                  // Get file modification time (last write time)
(defcfun "GetFileModTime" :long
 "Get file modification time (last write time)"
 (file-name :string))

;;// Persistent storage management
;;RLAPI void StorageSaveValue(int position, int value);             // Save integer value to storage file (to defined position)
(defcfun "StorageSaveValue" :void
 "Save integer value to storage file (to defined position)"
 (position :int)
 (value :int))

;;RLAPI int StorageLoadValue(int position);                         // Load integer value from storage file (from defined position)
(defcfun "StorageLoadValue" :int
 "Load integer value from storage file (from defined position)"
 (position :int))

;;RLAPI void OpenURL(const char *url);                              // Open URL with default system browser (if available)
(defcfun "OpenURL" :void
 "Open URL with default system browser (if available)"
 (url :string))

;;//------------------------------------------------------------------------------------
;;// Input Handling Functions (Module: core)
;;//------------------------------------------------------------------------------------
;;
;;// Input-related functions: keyboard
;;RLAPI bool IsKeyPressed(int key);                             // Detect if a key has been pressed once
(defcfun "IsKeyPressed" :boolean
 "Detect if a key has been pressed once"
 (key :int))

;;RLAPI bool IsKeyDown(int key);                                // Detect if a key is being pressed
(defcfun "IsKeyDown" :boolean
 "Detect if a key is being pressed"
 (key :int))

;;RLAPI bool IsKeyReleased(int key);                            // Detect if a key has been released once
(defcfun "IsKeyReleased" :boolean
 "Detect if a key has been released once"
 (key :int))

;;RLAPI bool IsKeyUp(int key);                                  // Detect if a key is NOT being pressed
(defcfun "IsKeyUp" :boolean
 "Detect if a key is NOT being pressed"
 (key :int))

;;RLAPI int GetKeyPressed(void);                                // Get latest key pressed
(defcfun "GetKeyPressed" :int
 "Get latest key pressed")

;;RLAPI void SetExitKey(int key);                               // Set a custom key to exit program (default is ESC)
(defcfun "SetExitKey" :void
 "Set a custom key to exit program (default is ESC)"
 (key :int))

;;// Input-related functions: gamepads
;;RLAPI bool IsGamepadAvailable(int gamepad);                   // Detect if a gamepad is available
(defcfun "IsGamepadAvailable" :boolean
 "Detect if a gamepad is available"
 (gamepad :int))

;;RLAPI bool IsGamepadName(int gamepad, const char *name);      // Check gamepad name (if available)
(defcfun "IsGamepadName" :boolean
 "Check gamepad name (if available)"
 (gamepad :int)
 (name :string))

;;RLAPI const char *GetGamepadName(int gamepad);                // Return gamepad internal name id
(defcfun "GetGamepadName" :string
 "Return gamepad internal name id"
 (gamepad :int))

;;RLAPI bool IsGamepadButtonPressed(int gamepad, int button);   // Detect if a gamepad button has been pressed once
(defcfun "IsGamepadButtonPressed" :boolean
 "Detect if a gamepad button has been pressed once"
 (gamepad :int)
 (button :int))

;;RLAPI bool IsGamepadButtonDown(int gamepad, int button);      // Detect if a gamepad button is being pressed
(defcfun "IsGamepadButtonDown" :boolean
 "Detect if a gamepad button is being pressed"
 (gamepad :int)
 (button :int))

;;RLAPI bool IsGamepadButtonReleased(int gamepad, int button);  // Detect if a gamepad button has been released once
(defcfun "IsGamepadButtonReleased" :boolean
 "Detect if a gamepad button has been released once"
 (gamepad :int)
 (button :int))

;;RLAPI bool IsGamepadButtonUp(int gamepad, int button);        // Detect if a gamepad button is NOT being pressed
(defcfun "IsGamepadButtonUp" :boolean
 "Detect if a gamepad button is NOT being pressed"
 (gamepad :int)
 (button :int))

;;RLAPI int GetGamepadButtonPressed(void);                      // Get the last gamepad button pressed
(defcfun "GetGamepadButtonPressed" :int
 "Get the last gamepad button pressed")

;;RLAPI int GetGamepadAxisCount(int gamepad);                   // Return gamepad axis count for a gamepad
(defcfun "GetGamepadAxisCount" :int
 "Return gamepad axis count for a gamepad"
 (gamepad :int))

;;RLAPI float GetGamepadAxisMovement(int gamepad, int axis);    // Return axis movement value for a gamepad axis
(defcfun "GetGamepadAxisMovement" :float
 "Return axis movement value for a gamepad axis"
 (gamepad :int)
 (axis :int))

;;// Input-related functions: mouse
;;RLAPI bool IsMouseButtonPressed(int button);                  // Detect if a mouse button has been pressed once
(defcfun "IsMouseButtonPressed" :boolean
 "Detect if a mouse button has been pressed once"
 (button :int))

;;RLAPI bool IsMouseButtonDown(int button);                     // Detect if a mouse button is being pressed
(defcfun "IsMouseButtonDown" :boolean
 "Detect if a mouse button is being pressed"
 (button :int))

;;RLAPI bool IsMouseButtonReleased(int button);                 // Detect if a mouse button has been released once
(defcfun "IsMouseButtonReleased" :boolean
 "Detect if a mouse button has been released once"
 (button :int))

;;RLAPI bool IsMouseButtonUp(int button);                       // Detect if a mouse button is NOT being pressed
(defcfun "IsMouseButtonUp" :boolean
 "Detect if a mouse button is NOT being pressed"
 (button :int))

;;RLAPI int GetMouseX(void);                                    // Returns mouse position X
(defcfun "GetMouseX" :int
 "Returns mouse position X")

;;RLAPI int GetMouseY(void);                                    // Returns mouse position Y
(defcfun "GetMouseY" :int
 "Returns mouse position Y")

;;RLAPI Vector2 GetMousePosition(void);                         // Returns mouse position XY
(defcfun "GetMousePosition" (:struct %vector2)
 "Returns mouse position XY")

;;RLAPI void SetMousePosition(int x, int y);                    // Set mouse position XY
(defcfun "SetMousePosition" :void
 "Set mouse position XY"
 (x :int)
 (y :int))

;;RLAPI void SetMouseOffset(int offsetX, int offsetY);          // Set mouse offset
(defcfun "SetMouseOffset" :void
 "Set mouse offset"
 (offset-x :int)
 (offset-y :int))

;;RLAPI void SetMouseScale(float scaleX, float scaleY);         // Set mouse scaling
(defcfun "SetMouseScale" :void
 "Set mouse scaling"
 (scale-x :float)
 (scale-y :float))

;;RLAPI int GetMouseWheelMove(void);                            // Returns mouse wheel movement Y
(defcfun "GetMouseWheelMove" :int
 "Returns mouse wheel movement Y")

;;// Input-related functions: touch
;;RLAPI int GetTouchX(void);                                    // Returns touch position X for touch point 0 (relative to screen size)
(defcfun "GetTouchX" :int
 "Returns touch position X for touch point 0 (relative to screen size)")

;;RLAPI int GetTouchY(void);                                    // Returns touch position Y for touch point 0 (relative to screen size)
(defcfun "GetTouchY" :int
 "Returns touch position Y for touch point 0 (relative to screen size)")

;;RLAPI Vector2 GetTouchPosition(int index);                    // Returns touch position XY for a touch point index (relative to screen size)
(defcfun "GetTouchPosition" (:struct %vector2)
 "Returns touch position XY for a touch point index (relative to screen size)"
 (index :int))

;;//------------------------------------------------------------------------------------
;;// Gestures and Touch Handling Functions (Module: gestures)
;;//------------------------------------------------------------------------------------
;;RLAPI void SetGesturesEnabled(unsigned int gestureFlags);     // Enable a set of gestures using flags
(defcfun "SetGesturesEnabled" :void
 "Enable a set of gestures using flags"
 (gesture-flags :unsigned-int))

;;RLAPI bool IsGestureDetected(int gesture);                    // Check if a gesture have been detected
(defcfun "IsGestureDetected" :boolean
 "Check if a gesture have been detected"
 (gesture :int))

;;RLAPI int GetGestureDetected(void);                           // Get latest detected gesture
(defcfun "GetGestureDetected" :int
 "Get latest detected gesture")

;;RLAPI int GetTouchPointsCount(void);                          // Get touch points count
(defcfun "GetTouchPointsCount" :int
 "Get touch points count")

;;RLAPI float GetGestureHoldDuration(void);                     // Get gesture hold time in milliseconds
(defcfun "GetGestureHoldDuration" :float
 "Get gesture hold time in milliseconds")

;;RLAPI Vector2 GetGestureDragVector(void);                     // Get gesture drag vector
(defcfun "GetGestureDragVector" (:struct %vector2)
 "Get gesture drag vector")

;;RLAPI float GetGestureDragAngle(void);                        // Get gesture drag angle
(defcfun "GetGestureDragAngle" :float
 "Get gesture drag angle")

;;RLAPI Vector2 GetGesturePinchVector(void);                    // Get gesture pinch delta
(defcfun "GetGesturePinchVector" (:struct %vector2)
 "Get gesture pinch delta")

;;RLAPI float GetGesturePinchAngle(void);                       // Get gesture pinch angle
(defcfun "GetGesturePinchAngle" :float
 "Get gesture pinch angle")

;;//------------------------------------------------------------------------------------
;;// Camera System Functions (Module: camera)
;;//------------------------------------------------------------------------------------
;;RLAPI void SetCameraMode(Camera camera, int mode);                // Set camera mode (multiple camera modes available)
(defcfun "SetCameraMode" :void
 "Set camera mode (multiple camera modes available)"
 (camera (:struct %camera3d))
 (mode :int))

;;RLAPI void UpdateCamera(Camera *camera);                          // Update camera position for selected mode
(defcfun "UpdateCamera" :void
 (camera (:pointer (:struct %camera3d))))

;;RLAPI void SetCameraPanControl(int panKey);                       // Set camera pan key to combine with mouse movement (free camera)
(defcfun "SetCameraPanControl" :void
 (pan-key :int))

;;RLAPI void SetCameraAltControl(int altKey);                       // Set camera alt key to combine with mouse movement (free camera)
(defcfun "SetCameraAltControl" :void
 (alt-key :int))

;;RLAPI void SetCameraSmoothZoomControl(int szKey);                 // Set camera smooth zoom key to combine with mouse (free camera)
(defcfun "SetCameraSmoothZoomControl" :void
 "Set camera smooth zoom key to combine with mouse (free camera)"
 (sz-key :int))

;;RLAPI void SetCameraMoveControls(int frontKey, int backKey, int rightKey, int leftKey, int upKey, int downKey); // Set camera move controls (1st person and 3rd person cameras)
(defcfun "SetCameraMoveControls" :void
 "Set camera move controls (1st person and 3rd person cameras)"
 (front-key :int)
 (back-key :int)
 (right-key :int)
 (left-key :int)
 (up-key :int)
 (down-key :int))

;;//------------------------------------------------------------------------------------
;;// Basic Shapes Drawing Functions (Module: shapes)
;;//------------------------------------------------------------------------------------
;;
;;// Basic shapes drawing functions
;;RLAPI void DrawPixel(int posX, int posY, Color color);                                                   // Draw a pixel
(defcfun "DrawPixel" :void
 "Draw a pixel"
 (pos-x :int)
 (pos-y :int)
 (color (:struct %color)))

;;RLAPI void DrawPixelV(Vector2 position, Color color);                                                    // Draw a pixel (Vector version)
(defcfun "DrawPixelV" :void
 "Draw a pixel (Vector version)"
 (position (:struct %vector2))
 (color (:struct %color)))

;;RLAPI void DrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color color);                // Draw a line
(defcfun "DrawLine" :void
 "Draw a line"
 (start-pos-x :int)
 (start-pos-y :int)
 (end-pos-x :int)
 (end-pos-y :int))

;;RLAPI void DrawLineV(Vector2 startPos, Vector2 endPos, Color color);                                     // Draw a line (Vector version)
(defcfun "DrawLineV" :void
 "Draw a line (Vector version)"
 (start-pos (:struct %vector2))
 (end-pos (:struct %vector2))
 (color (:struct %color)))

;;RLAPI void DrawLineEx(Vector2 startPos, Vector2 endPos, float thick, Color color);                       // Draw a line defining thickness
(defcfun "DrawLineEx" :void
 "Draw a line defining thickness"
 (start-pos (:struct %vector2))
 (end-pos (:struct %vector2))
 (thick :float)
 (color (:struct %color)))

;;RLAPI void DrawLineBezier(Vector2 startPos, Vector2 endPos, float thick, Color color);                   // Draw a line using cubic-bezier curves in-out
(defcfun "DrawLineBezier" :void
 "Draw a line using cubic-bezier curves in-out"
 (start-pos (:struct %vector2))
 (end-pos (:struct %vector2))
 (thick :float)
 (color (:struct %color)))

;;RLAPI void DrawLineStrip(Vector2 *points, int numPoints, Color color);                                   // Draw lines sequence
(defcfun "DrawLineStrip" :void
  "Draw lines sequence"
  (points (:struct %vector2))
  (num-points :int)
  (color (:struct %color)))

;;RLAPI void DrawCircle(int centerX, int centerY, float radius, Color color);                              // Draw a color-filled circle
(defcfun "DrawCircle" :void
  "Draw a color-filled circle"
  (center-x :int)
  (center-y :int)
  (radius :float)
  (color (:struct %color)))

;;RLAPI void DrawCircleSector(Vector2 center, float radius, int startAngle, int endAngle, int segments, Color color);     // Draw a piece of a circle
(defcfun "DrawCircleSector" :void
  "Draw a piece of a circle"
  (center (:struct %vector2))
  (radius :float)
  (start-angle :int)
  (end-angle :int)
  (segments :int)
  (color (:struct %color)))

;;RLAPI void DrawCircleSectorLines(Vector2 center, float radius, int startAngle, int endAngle, int segments, Color color);    // Draw circle sector outline
(defcfun "DrawCircleSectorLines" :void
  "Draw circle sector outline"
  (center (:struct %vector2))
  (radius :float)
  (start-angle :int)
  (end-angle :int)
  (segments :int)
  (color (:struct %color)))

;;RLAPI void DrawCircleGradient(int centerX, int centerY, float radius, Color color1, Color color2);       // Draw a gradient-filled circle
(defcfun "DrawCircleGradient" :void
  "Draw a gradient-filled circle"
  (center-x :int)
  (center-y :int)
  (radius :float)
  (color1 (:struct %color))
  (color2 (:struct %color)))

;;RLAPI void DrawCircleV(Vector2 center, float radius, Color color);                                       // Draw a color-filled circle (Vector version)
(defcfun "DrawCircleV" :void
 "Draw a color-filled circle (Vector version)"
 (center (:struct %vector2))
 (radius :float)
 (color (:struct %color)))

;;RLAPI void DrawCircleLines(int centerX, int centerY, float radius, Color color);                         // Draw circle outline
(defcfun "DrawCircleLines" :void
  "Draw circle outline"
  (center-x :int)
  (center-y :int)
  (radius :float)
  (color (:struct %color)))

;;RLAPI void DrawRing(Vector2 center, float innerRadius, float outerRadius, int startAngle, int endAngle, int segments, Color color); // Draw ring
(defcfun "DrawRing" :void
  "Draw ring"
  (center (:struct %vector2))
  (inner-radius :float)
  (outer-radius :float)
  (start-angle :int)
  (end-angle :int)
  (segments :int)
  (color (:struct %color)))

;;RLAPI void DrawRingLines(Vector2 center, float innerRadius, float outerRadius, int startAngle, int endAngle, int segments, Color color);    // Draw ring outline
(defcfun "DrawRingLines" :void
  "Draw ring outline"
  (center (:struct %vector2))
  (inner-radius :float)
  (outer-radius :float)
  (start-angle :int)
  (end-angle :int)
  (segments :int)
  (color (:struct %color)))

;;RLAPI void DrawRectangle(int posX, int posY, int width, int height, Color color);                        // Draw a color-filled rectangle
(defcfun "DrawRectangle" :void
 "Draw a color-filled rectangle"
 (pos-x :int)
 (pos-y :int)
 (width :int)
 (height :int)
 (color (:struct %color)))

;;RLAPI void DrawRectangleV(Vector2 position, Vector2 size, Color color);                                  // Draw a color-filled rectangle (Vector version)
(defcfun "DrawRectangleV" :void
 "Draw a color-filled rectangle (Vector version)"
 (position (:struct %vector2))
 (size (:struct %vector2))
 (color (:struct %color)))

;;RLAPI void DrawRectangleRec(Rectangle rec, Color color);                                                 // Draw a color-filled rectangle
(defcfun "DrawRectangleRec" :void
 "Draw a color-filled rectangle"
 (rec (:struct %rectangle))
 (color (:struct %color)))

;;RLAPI void DrawRectanglePro(Rectangle rec, Vector2 origin, float rotation, Color color);                 // Draw a color-filled rectangle with pro parameters
(defcfun "DrawRectanglePro" :void
 "Draw a color-filled rectangle with pro parameters"
 (rec (:struct %rectangle))
 (origin (:struct %vector2))
 (rotation :float)
 (color (:struct %color)))

;;RLAPI void DrawRectangleGradientV(int posX, int posY, int width, int height, Color color1, Color color2);// Draw a vertical-gradient-filled rectangle
(defcfun "DrawRectangleGradientV" :void
 "Draw a vertical-gradient-filled rectangle"
 (pos-x :int)
 (pos-y :int)
 (width :int)
 (height :int)
 (color1 (:struct %color))
 (color2 (:struct %color)))

;;RLAPI void DrawRectangleGradientH(int posX, int posY, int width, int height, Color color1, Color color2);// Draw a horizontal-gradient-filled rectangle
(defcfun "DrawRectangleGradientH" :void
 "Draw a horizontal-gradient-filled rectangle"
 (pos-x :int)
 (pos-y :int)
 (width :int)
 (height :int)
 (color1 (:struct %color))
 (color2 (:struct %color)))

;;RLAPI void DrawRectangleGradientEx(Rectangle rec, Color col1, Color col2, Color col3, Color col4);       // Draw a gradient-filled rectangle with custom vertex colors
(defcfun "DrawRectangleGradientEx" :void
 "Draw a gradient-filled rectangle with custom vertex colors"
 (rec (:struct %rectangle))
 (col1 (:struct %color))
 (col2 (:struct %color))
 (col3 (:struct %color))
 (col4 (:struct %color)))

;;RLAPI void DrawRectangleLines(int posX, int posY, int width, int height, Color color);                   // Draw rectangle outline
(defcfun "DrawRectangleLines" :void
 "Draw rectangle outline"
 (pos-x :int)
 (pos-y :int)
 (width :int)
 (height :int)
 (color (:struct %color)))
 
;;RLAPI void DrawRectangleLinesEx(Rectangle rec, int lineThick, Color color);                              // Draw rectangle outline with extended parameters
(defcfun "DrawRectangleLinesEx" :void
 "Draw rectangle outline with extended parameters"
 (rec (:struct %rectangle))
 (line-thick :int)
 (color (:struct %color)))

;;RLAPI void DrawRectangleRounded(Rectangle rec, float roundness, int segments, Color color);              // Draw rectangle with rounded edges
(defcfun "DrawRectangleRounded" :void
 "Draw rectangle with rounded edges"
 (rec (:struct %rectangle))
 (roundness :float)
 (segments :int)
 (color (:struct %color)))

;;RLAPI void DrawRectangleRoundedLines(Rectangle rec, float roundness, int segments, int lineThick, Color color); // Draw rectangle with rounded edges outline
(defcfun "DrawRectangleRoundedLines" :void
 "Draw rectangle with rounded edges outline"
 (rec (:struct %rectangle))
 (roundness :float)
 (segments :int)
 (line-thick :int)
 (color (:struct %color)))

;;RLAPI void DrawTriangle(Vector2 v1, Vector2 v2, Vector2 v3, Color color);                                // Draw a color-filled triangle
(defcfun "DrawTriangle" :void
 "Draw a color-filled triangle"
 (v1 (:struct %vector2))
 (v2 (:struct %vector2))
 (v3 (:struct %vector2))
 (color (:struct %color)))
 
;;RLAPI void DrawTriangleLines(Vector2 v1, Vector2 v2, Vector2 v3, Color color);                           // Draw triangle outline
(defcfun "DrawTriangleLines" :void
 "Draw triangle outline"
 (v1 (:struct %vector2))
 (v2 (:struct %vector2))
 (v3 (:struct %vector2))
 (color (:struct %color)))

;;RLAPI void DrawTriangleFan(Vector2 *points, int numPoints, Color color);                                 // Draw a triangle fan defined by points
(defcfun "DrawTriangleFan" :void
 "Draw a triangle fan defined by points"
 (points (:pointer (:struct %vector2)))
 (num-points :int)
 (color (:struct %color)))

;;RLAPI void DrawPoly(Vector2 center, int sides, float radius, float rotation, Color color);               // Draw a regular polygon (Vector version)
(defcfun "DrawPoly" :void
 "Draw a regular polygon (Vector version)"
 (center (:struct %vector2))
 (sides :int)
 (radius :float)
 (rotation :float)
 (color (:struct %color)))

;;RLAPI void SetShapesTexture(Texture2D texture, Rectangle source);                                        // Define default texture used to draw shapes
(defcfun "SetShapesTexture" :void
 "Define default texture used to draw shapes"
 (texture (:struct %texture))
 (source (:struct %rectangle)))

;;// Basic shapes collision detection functions
;;RLAPI bool CheckCollisionRecs(Rectangle rec1, Rectangle rec2);                                           // Check collision between two rectangles
(defcfun "CheckCollisionRecs" :bool
 (rec1 (:struct %rectangle))
 (rec2 (:struct %rectangle)))

;;RLAPI bool CheckCollisionCircles(Vector2 center1, float radius1, Vector2 center2, float radius2);        // Check collision between two circles
(defcfun "CheckCollisionCircles" :bool
 (center1 (:struct %vector2))
 (radius1 :float)
 (center2 (:struct %vector2))
 (radius2 :float))

;;RLAPI bool CheckCollisionCircleRec(Vector2 center, float radius, Rectangle rec);                         // Check collision between circle and rectangle
(defcfun "CheckCollisionCircleRec" :bool
 (center (:struct %vector2))
 (radius :float)
 (rec (:struct %rectangle)))

;;RLAPI Rectangle GetCollisionRec(Rectangle rec1, Rectangle rec2);                                         // Get collision rectangle for two rectangles collision
(defcfun "GetCollisionRec" (:struct %rectangle)
 (rec1 (:struct %rectangle))
 (rec2 (:struct %rectangle)))

;;RLAPI bool CheckCollisionPointRec(Vector2 point, Rectangle rec);                                         // Check if point is inside rectangle
(defcfun "CheckCollisionPointRec" :bool
 (point (:struct %vector2))
 (rec (:struct %rectangle)))

;;RLAPI bool CheckCollisionPointCircle(Vector2 point, Vector2 center, float radius);                       // Check if point is inside circle
(defcfun "CheckCollisionPointCircle" :bool
 (point (:struct %vector2))
 (center (:struct %vector2))
 (radius :float))

;;RLAPI bool CheckCollisionPointTriangle(Vector2 point, Vector2 p1, Vector2 p2, Vector2 p3);               // Check if point is inside a triangle
(defcfun "CheckCollisionPointTriangle" :bool
 (point (:struct %vector2))
 (p1 (:struct %vector2))
 (p2 (:struct %vector2))
 (p3 (:struct %vector2)))

;;//------------------------------------------------------------------------------------
;;// Texture Loading and Drawing Functions (Module: textures)
;;//------------------------------------------------------------------------------------
;;
;;// Image/Texture2D data loading/unloading/saving functions
;;RLAPI Image LoadImage(const char *fileName);                                                             // Load image from file into CPU memory (RAM)
(defcfun "LoadImage" (:struct %image)
 (file-name :string))

;;RLAPI Image LoadImageEx(Color *pixels, int width, int height);                                           // Load image from Color array data (RGBA - 32bit)
(defcfun "LoadImageEx" (:struct %image)
 (pixels (:pointer (:struct %color)))
 (width :int)
 (height :int))
 
;;RLAPI Image LoadImagePro(void *data, int width, int height, int format);                                 // Load image from raw data with parameters
(defcfun "LoadImagePro" (:struct %image)
 "Load image from raw data with parameters"
 (data :pointer)
 (width :int)
 (height :int)
 (format :int))

;;RLAPI Image LoadImageRaw(const char *fileName, int width, int height, int format, int headerSize);       // Load image from RAW file data
(defcfun "LoadImageRaw" (:struct %image)
 "Load image from RAW file data"
 (filename :string)
 (width :int)
 (height :int)
 (format :int)
 (header-size :int))

;;RLAPI void ExportImage(Image image, const char *fileName);                                               // Export image data to file
(defcfun "ExportImage" :void
 "Export image data to file"
 (image (:struct %image))
 (filename :string))

;;RLAPI void ExportImageAsCode(Image image, const char *fileName);                                         // Export image as code file defining an array of bytes
(defcfun "ExportImageAsCode" :void
 "Export image as code file defining an array of bytes"
 (image (:struct %image))
 (filename :string))

;;RLAPI Texture2D LoadTexture(const char *fileName);                                                       // Load texture from file into GPU memory (VRAM)
(defcfun "LoadTexture" (:struct %texture)
 (file-name :string))

;;RLAPI Texture2D LoadTextureFromImage(Image image);                                                       // Load texture from image data
(defcfun "LoadTextureFromImage" (:struct %texture)
 "Load texture from image data"
 (image (:struct %image)))

;;RLAPI TextureCubemap LoadTextureCubemap(Image image, int layoutType);                                    // Load cubemap from image, multiple image cubemap layouts supported
(defcfun "LoadTextureCubemap" texture-cubemap
 "Load cubemap from image, multiple image cubemap layouts supported"
 (image (:struct %image))
 (layout-type :int))

;;RLAPI RenderTexture2D LoadRenderTexture(int width, int height);                                          // Load texture for rendering (framebuffer)
(defcfun "LoadRenderTexture" (:struct %render-texture)
 (width :int)
 (height :int))

;;RLAPI void UnloadImage(Image image);                                                                     // Unload image from CPU memory (RAM)
(defcfun "UnloadImage" :void
 (image (:struct %image)))

;;RLAPI void UnloadTexture(Texture2D texture);                                                             // Unload texture from GPU memory (VRAM)
(defcfun "UnloadTexture" :void
 (texture (:struct %texture)))

;;RLAPI void UnloadRenderTexture(RenderTexture2D target);                                                  // Unload render texture from GPU memory (VRAM)
(defcfun "UnloadRenderTexture" :void
 (target (:struct %render-texture)))

;;RLAPI Color *GetImageData(Image image);                                                                  // Get pixel data from image as a Color struct array
(defcfun "GetImageData" :pointer
 "Get pixel data from image as a Color struct array"
 (image (:struct %image)))

;;RLAPI Vector4 *GetImageDataNormalized(Image image);                                                      // Get pixel data from image as Vector4 array (float normalized)
(defcfun "GetImageDataNormalized" :pointer
 "Get pixel data from image as Vector4 array (float normalized)"
 (image (:struct %image)))

;;RLAPI int GetPixelDataSize(int width, int height, int format);                                           // Get pixel data size in bytes (image or texture)
(defcfun "GetPixelDataSize" :int
 "Get pixel data size in bytes (image or texture)"
 (width :int)
 (height :int)
 (format :int))

;;RLAPI Image GetTextureData(Texture2D texture);                                                           // Get pixel data from GPU texture and return an Image
(defcfun "GetTextureData" (:struct %image)
 "Get pixel data from GPU texture and return an Image"
 (texture (:struct %texture)))

;;RLAPI Image GetScreenData(void);                                                                         // Get pixel data from screen buffer and return an Image (screenshot)
(defcfun "GetScreenData" (:struct %image)
 "Get pixel data from screen buffer and return an Image (screenshot)")

;;RLAPI void UpdateTexture(Texture2D texture, const void *pixels);                                         // Update GPU texture with new data
(defcfun "UpdateTexture" :void
 "Update GPU texture with new data"
 (texture (:struct %texture))
 (pixels :pointer))

;;// Image manipulation functions
;;RLAPI Image ImageCopy(Image image);                                                                      // Create an image duplicate (useful for transformations)
(defcfun "ImageCopy" (:struct %image)
 "Create an image duplicate (useful for transformations)"
 (image (:struct %image)))

;;RLAPI void ImageToPOT(Image *image, Color fillColor);                                                    // Convert image to POT (power-of-two)
(defcfun "ImageToPOT" :void
 (image (:pointer (:struct %image)))
 (fill-color (:struct %color)))

;;RLAPI void ImageFormat(Image *image, int newFormat);                                                     // Convert image data to desired format
(defcfun "ImageFormat" :void
 (image (:pointer (:struct %image)))
 (new-format :int))

;;RLAPI void ImageAlphaMask(Image *image, Image alphaMask);                                                // Apply alpha mask to image
(defcfun "ImageAlphaMask" :void
 (image (:pointer (:struct %image)))
 (alpha-mask (:struct %image)))

;;RLAPI void ImageAlphaClear(Image *image, Color color, float threshold);                                  // Clear alpha channel to desired color
(defcfun "ImageAlphaClear" :void
 "Clear alpha channel to desired color"
 (image :pointer)
 (color (:struct %color))
 (threshold :float))

;;RLAPI void ImageAlphaCrop(Image *image, float threshold);                                                // Crop image depending on alpha value
(defcfun "ImageAlphaCrop" :void
 "Crop image depending on alpha value"
 (image :pointer)
 (threshold :float))

;;RLAPI void ImageAlphaPremultiply(Image *image);                                                          // Premultiply alpha channel
(defcfun "ImageAlphaPremultiply" :void
 "Premultiply alpha channel"
 (image :pointer))

;;RLAPI void ImageCrop(Image *image, Rectangle crop);                                                      // Crop an image to a defined rectangle
(defcfun "ImageCrop" :void
 "rop an image to a defined rectangle"
 (image :pointer)
 (crop (:struct %rectangle)))

;;RLAPI void ImageResize(Image *image, int newWidth, int newHeight);                                       // Resize image (Bicubic scaling algorithm)
(defcfun "ImageResize" :void
 "Resize image (Bicubic scaling algorithm)"
 (image :pointer)
 (new-width :int)
 (new-height :int))

;;RLAPI void ImageResizeNN(Image *image, int newWidth,int newHeight);                                      // Resize image (Nearest-Neighbor scaling algorithm)
(defcfun "ImageResizeNN" :void
 "Resize image (Nearest-Neighbor scaling algorithm)"
 (image :pointer)
 (new-width :int)
 (new-height :int))

;;RLAPI void ImageResizeCanvas(Image *image, int newWidth, int newHeight, int offsetX, int offsetY, Color color);  // Resize canvas and fill with color
(defcfun "ImageResizeCanvas" :void
 "Resize canvas and fill with color"
 (image :pointer)
 (new-width :int)
 (new-height :int)
 (offset-x :int)
 (offset-y :int)
 (color (:struct %color)))

;;RLAPI void ImageMipmaps(Image *image);                                                                   // Generate all mipmap levels for a provided image
(defcfun "ImageMipmaps" :void
 "Generate all mipmap levels for a provided image"
 (image :pointer))

;;RLAPI void ImageDither(Image *image, int rBpp, int gBpp, int bBpp, int aBpp);                            // Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
(defcfun "ImageDither" :void
 "Dither image data to 16bpp or lower (Floyd-Steinberg dithering)"
 (image :pointer)
 (r-bpp :int)
 (g-bpp :int)
 (b-bpp :int)
 (a-bpp :int))

;;RLAPI Image ImageText(const char *text, int fontSize, Color color);                                      // Create an image from text (default font)
(defcfun "ImageText" (:struct %image)
 "Create an image from text (default font)"
 (text :string)
 (font-size :int)
 (color (:struct %color)))

;;RLAPI Image ImageTextEx(Font font, const char *text, float fontSize, float spacing, Color tint);         // Create an image from text (custom sprite font)
(defcfun "ImageTextEx" (:struct %image)
 (font (:struct %font))
 (text :string)
 (font-size :float)
 (spacing :float)
 (tint (:struct %color)))
 
;;RLAPI void ImageDraw(Image *dst, Image src, Rectangle srcRec, Rectangle dstRec);                         // Draw a source image within a destination image
(defcfun "ImageDraw" :void
 "Draw a source image within a destination image"
 (dst :pointer)
 (src (:struct %image))
 (src-rec (:struct %rectangle))
 (dst-rec (:struct %rectangle)))

;;RLAPI void ImageDrawRectangle(Image *dst, Rectangle rec, Color color);                                   // Draw rectangle within an image
(defcfun "ImageDrawRectangle" :void
 "Draw rectangle within an image"
 (dst :pointer)
 (rec (:struct %rectangle))
 (color (:struct %color)))

;;RLAPI void ImageDrawRectangleLines(Image *dst, Rectangle rec, int thick, Color color);                   // Draw rectangle lines within an image
(defcfun "ImageDrawRectangleLines" :void
 "Draw rectangle lines within an image"
 (dst :pointer)
 (rec (:struct %rectangle))
 (thick :int)
 (color (:struct %color)))

;;RLAPI void ImageDrawText(Image *dst, Vector2 position, const char *text, int fontSize, Color color);     // Draw text (default font) within an image (destination)
(defcfun "ImageDrawText" :void
 (dst (:pointer (:struct %image)))
 (position (:struct %vector2))
 (text :string)
 (font-size :int)
 (color (:struct %color)))

;;RLAPI void ImageDrawTextEx(Image *dst, Vector2 position, Font font, const char *text, float fontSize, float spacing, Color color); // Draw text (custom sprite font) within an image (destination)
(defcfun "ImageDrawTextEx" :void
 "Draw text (custom sprite font) within an image (destination)"
 (dst :pointer)
 (position (:struct %vector2))
 (font (:struct %font))
 (text :string)
 (font-size :float)
 (spacing :float)
 (color (:struct %color)))

;;RLAPI void ImageFlipVertical(Image *image);                                                              // Flip image vertically
(defcfun "ImageFlipVertical" :void
 (image (:pointer (:struct %image))))

;;RLAPI void ImageFlipHorizontal(Image *image);                                                            // Flip image horizontally
(defcfun "ImageFlipHorizontal" :void
 (image (:pointer (:struct %image))))

;;RLAPI void ImageRotateCW(Image *image);                                                                  // Rotate image clockwise 90deg
(defcfun "ImageRotateCW" :void
 "Rotate image clockwise 90deg"
 (image :pointer))

;;RLAPI void ImageRotateCCW(Image *image);                                                                 // Rotate image counter-clockwise 90deg
(defcfun "ImageRotateCCW" :void
 "Rotate image counter-clockwise 90deg"
 (image :pointer))

;;RLAPI void ImageColorTint(Image *image, Color color);                                                    // Modify image color: tint
(defcfun "ImageColorTint" :void
 (image (:pointer (:struct %image)))
 (color (:struct %color)))

;;RLAPI void ImageColorInvert(Image *image);                                                               // Modify image color: invert
(defcfun "ImageColorInvert" :void
 (image (:pointer (:struct %image))))

;;RLAPI void ImageColorGrayscale(Image *image);                                                            // Modify image color: grayscale
(defcfun "ImageColorGrayscale" :void
 (image (:pointer (:struct %image))))

;;RLAPI void ImageColorContrast(Image *image, float contrast);                                             // Modify image color: contrast (-100 to 100)
(defcfun "ImageColorContrast" :void
 (image (:pointer (:struct %image)))
 (contrast :float))

;;RLAPI void ImageColorBrightness(Image *image, int brightness);                                           // Modify image color: brightness (-255 to 255)
(defcfun "ImageColorBrightness" :void
 (image (:pointer (:struct %image)))
 (brightness :int))

;;RLAPI void ImageColorReplace(Image *image, Color color, Color replace);                                  // Modify image color: replace color
(defcfun "ImageColorReplace" :void
 "Modify image color: replace color"
 (image :pointer)
 (color (:struct %color))
 (replace (:struct %color)))


;;// Image generation functions
;;RLAPI Image GenImageColor(int width, int height, Color color);                                           // Generate image: plain color
(defcfun "GenImageColor" (:struct %image)
 "Generate image: plain color"
 (width :int)
 (height :int)
 (color (:struct %color)))

;;RLAPI Image GenImageGradientV(int width, int height, Color top, Color bottom);                           // Generate image: vertical gradient
(defcfun "GenImageGradientV" (:struct %image)
 "Generate image: vertical gradient"
 (width :int)
 (height :int)
 (top (:struct %color))
 (bottom (:struct %color)))

;;RLAPI Image GenImageGradientH(int width, int height, Color left, Color right);                           // Generate image: horizontal gradient
(defcfun "GenImageGradientH" (:struct %image)
 "Generate image: horizontal gradient"
 (width :int)
 (height :int)
 (left (:struct %color))
 (right (:struct %color)))

;;RLAPI Image GenImageGradientRadial(int width, int height, float density, Color inner, Color outer);      // Generate image: radial gradient
(defcfun "GenImageGradientRadial" (:struct %image)
 "Generate image: radial gradient"
 (width :int)
 (height :int)
 (density :float)
 (inner (:struct %color))
 (outer (:struct %color)))

;;RLAPI Image GenImageChecked(int width, int height, int checksX, int checksY, Color col1, Color col2);    // Generate image: checked
(defcfun "GenImageChecked" (:struct %image)
 "Generate image: checked"
 (width :int)
 (height :int)
 (checks-x :int)
 (checks-y :int)
 (col1 (:struct %color))
 (col2 (:struct %color)))

;;RLAPI Image GenImageWhiteNoise(int width, int height, float factor);                                     // Generate image: white noise
(defcfun "GenImageWhiteNoise" (:struct %image)
 "Generate image: white noise"
 (width :int)
 (height :int)
 (factor :float))

;;RLAPI Image GenImagePerlinNoise(int width, int height, int offsetX, int offsetY, float scale);           // Generate image: perlin noise
(defcfun "GenImagePerlinNoise" (:struct %image)
 "Generate image: perlin noise"
 (width :int)
 (height :int)
 (offset-x :int)
 (offset-y :int)
 (scale :float))

;;RLAPI Image GenImageCellular(int width, int height, int tileSize);                                       // Generate image: cellular algorithm. Bigger tileSize means bigger cells
(defcfun "GenImageCellular" (:struct %image)
 "Generate image: cellular algorithm. Bigger tileSize means bigger cells"
 (width :int)
 (height :int)
 (tile-size :int))

;;// Texture2D configuration functions
;;RLAPI void GenTextureMipmaps(Texture2D *texture);                                                        // Generate GPU mipmaps for a texture
(defcfun "GenTextureMipmaps" :void
 (texture (:pointer (:struct %texture))))

;;RLAPI void SetTextureFilter(Texture2D texture, int filterMode);                                          // Set texture scaling filter mode
(defcfun "SetTextureFilter" :void
 (texture (:struct %texture))
 (filter-mode :int))

;;RLAPI void SetTextureWrap(Texture2D texture, int wrapMode);                                              // Set texture wrapping mode
(defcfun "SetTextureWrap" :void
 (texture (:struct %texture))
 (wrap-mode :int))

;;// Texture2D drawing functions
;;RLAPI void DrawTexture(Texture2D texture, int posX, int posY, Color tint);                               // Draw a Texture2D
(defcfun "DrawTexture" :void
 (texture (:struct %texture))
 (pos-x :int)
 (pos-y :int)
 (tint (:struct %color)))

;;RLAPI void DrawTextureV(Texture2D texture, Vector2 position, Color tint);                                // Draw a Texture2D with position defined as Vector2
(defcfun "DrawTextureV" :void
 (texture (:struct %texture))
 (position (:struct %vector2))
 (tint (:struct %color)))

;;RLAPI void DrawTextureEx(Texture2D texture, Vector2 position, float rotation, float scale, Color tint);  // Draw a Texture2D with extended parameters
(defcfun "DrawTextureEx" :void
 (texture (:struct %texture))
 (position (:struct %vector2))
 (rotation :float)
 (scale :float)
 (tint (:struct %color)))

;;RLAPI void DrawTextureRec(Texture2D texture, Rectangle sourceRec, Vector2 position, Color tint);         // Draw a part of a texture defined by a rectangle
(defcfun "DrawTextureRec" :void
 "Draw a part of a texture defined by a rectangle"
 (texture (:struct %texture))
 (source-rec (:struct %rectangle))
 (position (:struct %vector2))
 (tint (:struct %color)))

;;RLAPI void DrawTextureQuad(Texture2D texture, Vector2 tiling, Vector2 offset, Rectangle quad, Color tint);  // Draw texture quad with tiling and offset parameters
(defcfun "DrawTextureQuad" :void
 "Draw texture quad with tiling and offset parameters"
 (texture (:struct %texture))
 (tiling (:struct %vector2))
 (offset (:struct %vector2))
 (quad (:struct %rectangle))
 (tint (:struct %color)))

;;RLAPI void DrawTexturePro(Texture2D texture, Rectangle sourceRec, Rectangle destRec, Vector2 origin, float rotation, Color tint);       // Draw a part of a texture defined by a rectangle with 'pro' parameters
(defcfun "DrawTexturePro" :void
 "Draw a part of a texture defined by a rectangle with 'pro' parameters"
 (texture (:struct %texture))
 (source-rec (:struct %rectangle))
 (dest-rec (:struct %rectangle))
 (origin (:struct %vector2))
 (rotation :float)
 (tint (:struct %color)))

;;RLAPI void DrawTextureNPatch(Texture2D texture, NPatchInfo nPatchInfo, Rectangle destRec, Vector2 origin, float rotation, Color tint);  // Draws a texture (or part of it) that stretches or shrinks nicely
(defcfun "DrawTextureNPatch" :void
 "raws a texture (or part of it) that stretches or shrinks nicely"
 (texture (:struct %texture))
 (patch-info (:struct %patch-info))
 (dest-rec (:struct %rectangle))
 (origin (:struct %vector2))
 (rotation :float)
 (tint (:struct %color)))

;;//------------------------------------------------------------------------------------
;;// Font Loading and Text Drawing Functions (Module: text)
;;//------------------------------------------------------------------------------------
;;
;;// Font loading/unloading functions
;;RLAPI Font GetFontDefault(void);                                                            // Get the default Font
(defcfun "GetFontDefault" (:struct %font)
 "Get the default Font")

;;RLAPI Font LoadFont(const char *fileName);                                                  // Load font from file into GPU memory (VRAM)
(defcfun "LoadFont" (:struct %font)
 "Load font from file into GPU memory (VRAM)"
 (file-name :string))

;;RLAPI Font LoadFontEx(const char *fileName, int fontSize, int *fontChars, int charsCount);  // Load font from file with extended parameters
(defcfun "LoadFontEx" (:struct %font)
 "Load font from file with extended parameters"
 (file-name :string)
 (font-size :int)
 (font-chars (:pointer (:int)))
 (chars-count :int))

;;RLAPI Font LoadFontFromImage(Image image, Color key, int firstChar);                        // Load font from Image (XNA style)
(defcfun "LoadFontFromImage" (:struct %font)
 "Load font from Image (XNA style)"
 (image (:struct %image))
 (key (:struct %color))
 (first-char :int))

;;RLAPI CharInfo *LoadFontData(const char *fileName, int fontSize, int *fontChars, int charsCount, int type); // Load font data for further use
(defcfun "LoadFontData" :pointer
 "Load font data for further use"
 (file-name :string)
 (font-chars :int)
 (chars-count :int)
 (type :int))

;;RLAPI Image GenImageFontAtlas(CharInfo *chars, int charsCount, int fontSize, int padding, int packMethod);  // Generate image font atlas using chars info
(defcfun "GenImageFontAtlas" (:struct %image)
 "Generate image font atlas using chars info"
 (chars (:pointer (:struct %char-info)))
 (chars-count :int)
 (font-size :int)
 (padding :int)
 (pack-method :int))

;;RLAPI void UnloadFont(Font font);                                                           // Unload Font from GPU memory (VRAM)
(defcfun "UnloadFont" :void
 "Unload Font from GPU memory (VRAM)"
 (font (:struct %font)))
;;
;;// Text drawing functions
;;RLAPI void DrawFPS(int posX, int posY);                                                     // Shows current FPS
(defcfun "DrawFPS" :void
 "Shows current FPS"
 (pos-x :int)
 (pos-y :int))

;;RLAPI void DrawText(const char *text, int posX, int posY, int fontSize, Color color);       // Draw text (using default font)
(defcfun "DrawText" :void
 "Draw text (using default font)"
 (text :string)
 (pos-x :int)
 (pos-y :int)
 (font-size :int)
 (color (:struct %color)))

;;RLAPI void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint);                // Draw text using font and additional parameters
;;RLAPI void DrawTextRec(Font font, const char *text, Rectangle rec, float fontSize, float spacing, bool wordWrap, Color tint);   // Draw text using font inside rectangle limits
;;RLAPI void DrawTextRecEx(Font font, const char *text, Rectangle rec, float fontSize, float spacing, bool wordWrap, Color tint,
;;                         int selectStart, int selectLength, Color selectText, Color selectBack);    // Draw text using font inside rectangle limits with support for text selection
;;
;;// Text misc. functions
;;RLAPI int MeasureText(const char *text, int fontSize);                                      // Measure string width for default font
;;RLAPI Vector2 MeasureTextEx(Font font, const char *text, float fontSize, float spacing);    // Measure string size for Font
;;RLAPI int GetGlyphIndex(Font font, int character);                                          // Get index position for a unicode character on font
;;RLAPI int GetNextCodepoint(const char *text, int *count);                                   // Returns next codepoint in a UTF8 encoded string
;;                                                                                            // NOTE: 0x3f(`?`) is returned on failure, `count` will hold the total number of bytes processed
;;
;;// Text strings management functions
;;// NOTE: Some strings allocate memory internally for returned strings, just be careful!
;;RLAPI bool TextIsEqual(const char *text1, const char *text2);                               // Check if two text string are equal
;;RLAPI unsigned int TextLength(const char *text);                                            // Get text length, checks for '\0' ending
;;RLAPI unsigned int TextCountCodepoints(const char *text);                                   // Get total number of characters (codepoints) in a UTF8 encoded string
;;RLAPI const char *TextFormat(const char *text, ...);                                        // Text formatting with variables (sprintf style)
;;RLAPI const char *TextSubtext(const char *text, int position, int length);                  // Get a piece of a text string
;;RLAPI const char *TextReplace(char *text, const char *replace, const char *by);             // Replace text string (memory should be freed!)
;;RLAPI const char *TextInsert(const char *text, const char *insert, int position);           // Insert text in a position (memory should be freed!)
;;RLAPI const char *TextJoin(const char **textList, int count, const char *delimiter);        // Join text strings with delimiter
;;RLAPI const char **TextSplit(const char *text, char delimiter, int *count);                 // Split text into multiple strings
;;RLAPI void TextAppend(char *text, const char *append, int *position);                       // Append text at specific position and move cursor!
;;RLAPI int TextFindIndex(const char *text, const char *find);                                // Find first text occurrence within a string
;;RLAPI const char *TextToUpper(const char *text);                      // Get upper case version of provided string
;;RLAPI const char *TextToLower(const char *text);                      // Get lower case version of provided string
(defcfun "TextToLower" :string
 "Get lower case version of provided string"
 (text :string))

;;RLAPI const char *TextToPascal(const char *text);                     // Get Pascal case notation version of provided string
(defcfun "TextToPascal" :string
 "Get Pascal case notation version of provided string"
 (text :string))

;;RLAPI int TextToInteger(const char *text);                            // Get integer value from text (negative values not supported)
(defcfun "TextToInteger" :int
 "Get integer value from text (negative values not supported)"
 (text :string))

;;//------------------------------------------------------------------------------------
;;// Basic 3d Shapes Drawing Functions (Module: models)
;;//------------------------------------------------------------------------------------
;;
;;// Basic geometric 3D shapes drawing functions
;;RLAPI void DrawLine3D(Vector3 startPos, Vector3 endPos, Color color);                                    // Draw a line in 3D world space
(defcfun "DrawLine3D" :void
 (start-pos (:struct %vector3))
 (end-pos (:struct %vector3))
 (color (:struct %color)))

;;RLAPI void DrawCircle3D(Vector3 center, float radius, Vector3 rotationAxis, float rotationAngle, Color color); // Draw a circle in 3D world space
(defcfun "DrawCircle3D" :void
 (position (:struct %vector3))
 (radius :float)
 (rotation-axis (:struct %vector3))
 (rotation-angle :float)
 (color (:struct %color)))

;;RLAPI void DrawCube(Vector3 position, float width, float height, float length, Color color);             // Draw cube
(defcfun "DrawCube" :void
 (position (:struct %vector3))
 (width :float)
 (height :float)
 (length :float)
 (color (:struct %color)))

;;RLAPI void DrawCubeV(Vector3 position, Vector3 size, Color color);                                       // Draw cube (Vector version)
(defcfun "DrawCubeV" :void
 (position (:struct %vector3))
 (size (:struct %vector3))
 (color (:struct %color)))

;;RLAPI void DrawCubeWires(Vector3 position, float width, float height, float length, Color color);        // Draw cube wires
(defcfun "DrawCubeWires" :void
 "Draw cube wires"
 (position (:struct %vector3))
 (width :float)
 (height :float)
 (length :float)
 (color (:struct %color)))

;;RLAPI void DrawCubeWiresV(Vector3 position, Vector3 size, Color color);                                  // Draw cube wires (Vector version)
(defcfun "DrawCubeWiresV" :void
 "Draw cube wires (Vector version)"
 (position (:struct %vector3))
 (size (:struct %vector3))
 (color (:struct %color)))

;;RLAPI void DrawCubeTexture(Texture2D texture, Vector3 position, float width, float height, float length, Color color); // Draw cube textured
(defcfun "DrawCubeTexture" :void
 (texture (:struct %texture))
 (position (:struct %vector3))
 (width :float)
 (height :float)
 (length :float)
 (color (:struct %color)))

;;RLAPI void DrawSphere(Vector3 centerPos, float radius, Color color);                                     // Draw sphere
(defcfun "DrawSphere" :void
 (center-pos (:struct %vector3))
 (radius :float)
 (color (:struct %color)))

;;RLAPI void DrawSphereEx(Vector3 centerPos, float radius, int rings, int slices, Color color);            // Draw sphere with extended parameters
(defcfun "DrawSphereEx" :void
 (center-pos (:struct %vector3))
 (radius :float)
 (rings :int)
 (slices :int)
 (color (:struct %color)))

;;RLAPI void DrawSphereWires(Vector3 centerPos, float radius, int rings, int slices, Color color);         // Draw sphere wires
(defcfun "DrawSphereWires" :void
 (center-pos (:struct %vector3))
 (radius :float)
 (rings :int)
 (slices :int)
 (color (:struct %color)))

;;RLAPI void DrawCylinder(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color); // Draw a cylinder/cone
(defcfun "DrawCylinder" :void
 (position (:struct %vector3))
 (radius-top :float)
 (radius-bottom :float)
 (height :float)
 (slices :int)
 (color (:struct %color)))

;;RLAPI void DrawCylinderWires(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color); // Draw a cylinder/cone wires
(defcfun "DrawCylinderWires" :void
 (position (:struct %vector3))
 (radius-top :float)
 (radius-bottom :float)
 (height :float)
 (slices :int)
 (color (:struct %color)))

;;RLAPI void DrawPlane(Vector3 centerPos, Vector2 size, Color color);                                      // Draw a plane XZ
(defcfun "DrawPlance" :void
 (current-pos (:struct %vector3))
 (size  (:struct %vector2))
 (color (:struct %color)))

;;RLAPI void DrawRay(Ray ray, Color color);                                                                // Draw a ray line
(defcfun "DrawRay" :void
 (ray (:struct %ray))
 (color (:struct %color)))

;;RLAPI void DrawGrid(int slices, float spacing);                                                          // Draw a grid (centered at (0, 0, 0))
(defcfun "DrawGrid" :void
 (slices :int)
 (spacing :float))

;;RLAPI void DrawGizmo(Vector3 position);                                                                  // Draw simple gizmo
(defcfun "DrawGizmo" :void
 (position (:struct %vector3)))

;;//DrawTorus(), DrawTeapot() could be useful?
;;
;;//------------------------------------------------------------------------------------
;;// Model 3d Loading and Drawing Functions (Module: models)
;;//------------------------------------------------------------------------------------
;;
;;// Model loading/unloading functions
;;RLAPI Model LoadModel(const char *fileName);                                                            // Load model from files (meshes and materials)
;;RLAPI Model LoadModelFromMesh(Mesh mesh);                                                               // Load model from generated mesh (default material)
;;RLAPI void UnloadModel(Model model);                                                                    // Unload model from memory (RAM and/or VRAM)
(defcfun "UnloadModel" :void
 (model (:struct %model)))

;;// Mesh loading/unloading functions
;;RLAPI Mesh *LoadMeshes(const char *fileName, int *meshCount);                                           // Load meshes from model file
(defcfun "LoadMeshes" :pointer
 "Load meshes from model file"
 (file-name :string)
 (mesh-count :int))

;;RLAPI void ExportMesh(Mesh mesh, const char *fileName);                                                 // Export mesh data to file
(defcfun "ExportMesh" :void
 "Export mesh data to file"
 (mesh (:struct %mesh))
 (file-name :string))

;;RLAPI void UnloadMesh(Mesh *mesh);                                                                      // Unload mesh from memory (RAM and/or VRAM)
;;
;;// Material loading/unloading functions
;;RLAPI Material *LoadMaterials(const char *fileName, int *materialCount);                                // Load materials from model file
;;RLAPI Material LoadMaterialDefault(void);                                                               // Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
;;RLAPI void UnloadMaterial(Material material);                                                           // Unload material from GPU memory (VRAM)
;;RLAPI void SetMaterialTexture(Material *material, int mapType, Texture2D texture);                      // Set texture for a material map type (MAP_DIFFUSE, MAP_SPECULAR...)
;;RLAPI void SetModelMeshMaterial(Model *model, int meshId, int materialId);                              // Set material for a mesh
;;
;;// Model animations loading/unloading functions
;;RLAPI ModelAnimation *LoadModelAnimations(const char *fileName, int *animsCount);                       // Load model animations from file
;;RLAPI void UpdateModelAnimation(Model model, ModelAnimation anim, int frame);                           // Update model animation pose
;;RLAPI void UnloadModelAnimation(ModelAnimation anim);                                                   // Unload animation data
;;RLAPI bool IsModelAnimationValid(Model model, ModelAnimation anim);                                     // Check model animation skeleton match
;;
;;// Mesh generation functions
;;RLAPI Mesh GenMeshPoly(int sides, float radius);                                                        // Generate polygonal mesh
;;RLAPI Mesh GenMeshPlane(float width, float length, int resX, int resZ);                                 // Generate plane mesh (with subdivisions)
(defcfun "GenMeshPlane" (:struct %mesh)
 "Generate plane mesh (with subdivisions)"
 (width :float)
 (length :float)
 (res-x :int)
 (res-z :int))

;;RLAPI Mesh GenMeshCube(float width, float height, float length);                                        // Generate cuboid mesh
(defcfun "GenMeshCube" (:struct %mesh)
 "Generate cuboid mesh"
 (width :float)
 (height :float)
 (length :float))

;;RLAPI Mesh GenMeshSphere(float radius, int rings, int slices);                                          // Generate sphere mesh (standard sphere)
(defcfun "GenMeshSphere" (:struct %mesh)
 "Generate sphere mesh (standard sphere)"
 (radius :float)
 (rings :int)
 (slices :int))

;;RLAPI Mesh GenMeshHemiSphere(float radius, int rings, int slices);                                      // Generate half-sphere mesh (no bottom cap)
(defcfun "GenMeshHemiSphere" (:struct %mesh)
 "Generate half-sphere mesh (no bottom cap)"
 (radius :float)
 (rings :int)
 (slices :int))

;;RLAPI Mesh GenMeshCylinder(float radius, float height, int slices);                                     // Generate cylinder mesh
(defcfun "GenMeshCylinder" (:struct %mesh)
 "Generate cylinder mesh"
 (radius :float)
 (height :float)
 (slices :int))

;;RLAPI Mesh GenMeshTorus(float radius, float size, int radSeg, int sides);                               // Generate torus mesh
(defcfun "GenMeshTorus" (:struct %mesh)
 "Generate torus mesh"
 (radius :float)
 (size :float)
 (rad-seg :int)
 (sides :int))

;;RLAPI Mesh GenMeshKnot(float radius, float size, int radSeg, int sides);                                // Generate trefoil knot mesh
(defcfun "GenMeshKnot" (:struct %mesh)
 "Generate trefoil knot mesh"
 (radius :float)
 (size :float)
 (rad-seg :int)
 (sides :int))

;;RLAPI Mesh GenMeshHeightmap(Image heightmap, Vector3 size);                                             // Generate heightmap mesh from image data
(defcfun "GenMeshHeightmap" (:struct %mesh)
 "Generate heightmap mesh from image data"
 (heightmap (:struct %image))
 (size (:struct %vector3)))

;;RLAPI Mesh GenMeshCubicmap(Image cubicmap, Vector3 cubeSize);                                           // Generate cubes-based map mesh from image data
(defcfun "GenMeshCubicmap" (:struct %mesh)
 "Generate cubes-based map mesh from image data"
 (cubicmap (:struct %image))
 (cube-size (:struct %vector3)))

;;// Mesh manipulation functions
;;RLAPI BoundingBox MeshBoundingBox(Mesh mesh);                                                           // Compute mesh bounding box limits
(defcfun "MeshBoundingBox" (:struct %bounding-box)
 "Compute mesh bounding box limits"
 (mesh (:struct %mesh)))

;;RLAPI void MeshTangents(Mesh *mesh);                                                                    // Compute mesh tangents
(defcfun "MeshTangents" :void
 "Compute mesh tangents"
 (mesh :pointer))

;;RLAPI void MeshBinormals(Mesh *mesh);                                                                   // Compute mesh binormals
(defcfun "MeshBinormals" :void
 "Compute mesh binormals"
 (mesh :pointer))

;;// Model drawing functions
;;RLAPI void DrawModel(Model model, Vector3 position, float scale, Color tint);                           // Draw a model (with texture if set)
(defcfun "DrawModel" :void
  (model (:struct %model))
  (position (:struct %vector3))
  (scale (:struct %vector3))
  (tint (:struct %color)))

;;RLAPI void DrawModelEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint); // Draw a model with extended parameters
(defcfun "DrawModelEx" :void
  (model (:struct %model))
  (position (:struct %vector3))
  (rotation-axis (:struct %vector3))
  (rotation-angle :float)
  (scale (:struct %vector3))
  (tint (:struct %color)))

;;RLAPI void DrawModelWires(Model model, Vector3 position, float scale, Color tint);                      // Draw a model wires (with texture if set)
(defcfun "DrawModelWires" :void
  (model (:struct %model))
  (position (:struct %vector3))
  (scale (:struct %vector3))
  (tint (:struct %color)))

;;RLAPI void DrawModelWiresEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint); // Draw a model wires (with texture if set) with extended parameters
(defcfun "DrawModelWiresEx" :void
  (model (:struct %model))
  (position (:struct %vector3))
  (rotation-axis (:struct %vector3))
  (rotation-angle :float)
  (scale (:struct %vector3))
  (tint (:struct %color)))

;;RLAPI void DrawBoundingBox(BoundingBox box, Color color);                                               // Draw bounding box (wires)
(defcfun "DrawBoundingBox" :void
  (box (:struct %bounding-box))
  (color (:struct %color)))

;;RLAPI void DrawBillboard(Camera camera, Texture2D texture, Vector3 center, float size, Color tint);     // Draw a billboard texture
(defcfun "DrawBillboard" :void
  (camera (:struct %camera3d))
  (texture (:struct %texture))
  (center (:struct %vector3))
  (size :float)
  (tint (:struct %color)))

;;RLAPI void DrawBillboardRec(Camera camera, Texture2D texture, Rectangle sourceRec, Vector3 center, float size, Color tint); // Draw a billboard texture defined by sourceRec
(defcfun "DrawBillboardRec" :void
  (camera (:struct %camera3d))
  (texture (:struct %texture))
  (source-rec (:struct %rectangle))
  (center (:struct %vector3))
  (size :float)
  (tint (:struct %color)))

;;// Collision detection functions
;;RLAPI bool CheckCollisionSpheres(Vector3 centerA, float radiusA, Vector3 centerB, float radiusB);       // Detect collision between two spheres
(defcfun "CheckCollisionSpheres" :bool
  (center-a (:struct %vector3))
  (radius-a :float)
  (center-b (:struct %vector3))
  (radius-b :float))

;;RLAPI bool CheckCollisionBoxes(BoundingBox box1, BoundingBox box2);                                     // Detect collision between two bounding boxes
(defcfun "CheckCollisionBoxes" :bool
  (box1 (:struct %bounding-box))
  (box2 (:struct %bounding-box)))

;;RLAPI bool CheckCollisionBoxSphere(BoundingBox box, Vector3 centerSphere, float radiusSphere);          // Detect collision between box and sphere
(defcfun "CheckCollisionBoxSphere" :bool
  (box (:struct %bounding-box))
  (center-sphere (:struct %vector3))
  (sphere-radius :float))

;;RLAPI bool CheckCollisionRaySphere(Ray ray, Vector3 spherePosition, float sphereRadius);                // Detect collision between ray and sphere
(defcfun "CheckCollisionRaySphere" :bool
 "Detect collision between ray and sphere"
  (ray (:struct %ray))
  (sphere-position (:struct %vector3))
  (sphere-radius :float))

;;RLAPI bool CheckCollisionRaySphereEx(Ray ray, Vector3 spherePosition, float sphereRadius, Vector3 *collisionPoint); // Detect collision between ray and sphere, returns collision point
(defcfun "CheckCollisionRaySphereEx" :bool
  (ray (:struct %ray))
  (sphere-position (:struct %vector3))
  (sphere-radius :float)
  (collision-point (:pointer (:struct %vector3))))

;;RLAPI bool CheckCollisionRayBox(Ray ray, BoundingBox box);                                              // Detect collision between ray and box
(defcfun "CheckCollisionRayBox" :bool
  (ray (:struct %ray))
  (box (:struct %bounding-box)))

;;RLAPI RayHitInfo GetCollisionRayModel(Ray ray, Model *model);                                           // Get collision info between ray and model
(defcfun "GetCollisionRayModel" (:struct %ray-hit-info)
 "Get collision info between ray and model"
 (ray (:struct %ray))
 (model :pointer))

;;RLAPI RayHitInfo GetCollisionRayTriangle(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3);                  // Get collision info between ray and triangle
(defcfun "GetCollisionRayTriangle" (:struct %ray-hit-info)
 "Get collision info between ray and triangle"
 (ray (:struct %ray))
 (p1 (:struct %vector3))
 (p2 (:struct %vector3))
 (p3 (:struct %vector3)))

;;RLAPI RayHitInfo GetCollisionRayGround(Ray ray, float groundHeight);                                    // Get collision info between ray and ground plane (Y-normal plane)
(defcfun "GetCollisionRayGround" (:struct %ray-hit-info)
 "Get collision info between ray and ground plane (Y-normal plane)"
 (ray (:struct %ray))
 (ground-height :float))

;;//------------------------------------------------------------------------------------
;;// Shaders System Functions (Module: rlgl)
;;// NOTE: This functions are useless when using OpenGL 1.1
;;//------------------------------------------------------------------------------------
;;
;;// Shader loading/unloading functions
;;RLAPI char *LoadText(const char *fileName);                               // Load chars array from text file
(defcfun "LoadText" :pointer
 "Load chars array from text file"
 (file-name :string))

;;RLAPI Shader LoadShader(const char *vsFileName, const char *fsFileName);  // Load shader from files and bind default locations
(defcfun "LoadShader" (:struct %shader)
  (vs-file-name :string)
  (fs-file-name :string))

;;RLAPI Shader LoadShaderCode(char *vsCode, char *fsCode);                  // Load shader from code strings and bind default locations
(defcfun "LoadShaderCode" (:struct %shader)
 "Load shader from code strings and bind default locations"
 (vs-code :pointer)
 (fs-code :pointer))

;;RLAPI void UnloadShader(Shader shader);                                   // Unload shader from GPU memory (VRAM)
(defcfun "UnloadShader" :void
  (shader (:struct %shader)))

;;RLAPI Shader GetShaderDefault(void);                                      // Get default shader
(defcfun "GetShaderDefault" (:struct %shader))

;;RLAPI Texture2D GetTextureDefault(void);                                  // Get default texture
(defcfun "GetTextureDefault" (:struct %texture))

;;// Shader configuration functions
;;RLAPI int GetShaderLocation(Shader shader, const char *uniformName);      // Get shader uniform location
(defcfun "GetShaderLocation" :int
 "Get shader uniform location"
 (shader (:struct %shader))
 (uniform-name :string))

;;RLAPI void SetShaderValue(Shader shader, int uniformLoc, const void *value, int uniformType);               // Set shader uniform value
(defcfun "SetShaderValue" :void
 "Set shader uniform value"
 (shader (:struct %shader))
 (uniform-loc :int)
 (value :pointer)
 (uniform-type :int))

;;RLAPI void SetShaderValueV(Shader shader, int uniformLoc, const void *value, int uniformType, int count);   // Set shader uniform value vector
;;RLAPI void SetShaderValueMatrix(Shader shader, int uniformLoc, Matrix mat);         // Set shader uniform value (matrix 4x4)
;;RLAPI void SetShaderValueTexture(Shader shader, int uniformLoc, Texture2D texture); // Set shader uniform value for texture
;;RLAPI void SetMatrixProjection(Matrix proj);                              // Set a custom projection matrix (replaces internal projection matrix)
(defcfun "SetMatrixProjection" :void
  (proj (:struct %matrix)))

;;RLAPI void SetMatrixModelview(Matrix view);                               // Set a custom modelview matrix (replaces internal modelview matrix)
(defcfun "SetMatrixModelview" :void
  (view (:struct %matrix)))

;;RLAPI Matrix GetMatrixModelview();                                        // Get internal modelview matrix
(defcfun "GetMatrixModelview" (:struct %matrix)
 "Get internal modelview matrix")

;;// Texture maps generation (PBR)
;;// NOTE: Required shaders should be provided
;;RLAPI Texture2D GenTextureCubemap(Shader shader, Texture2D skyHDR, int size);       // Generate cubemap texture from HDR texture
(defcfun "GenTextureCubemap" (:struct %texture)
 "Generate cubemap texture from HDR texture"
 (shader (:struct %shader))
 (sky-hdr (:struct %texture))
 (size :int))

;;RLAPI Texture2D GenTextureIrradiance(Shader shader, Texture2D cubemap, int size);   // Generate irradiance texture using cubemap data
(defcfun "GenTextureIrradiance" (:struct %texture)
 "Generate irradiance texture using cubemap data"
 (shader (:struct %shader))
 (cubemap (:struct %texture))
 (size :int))

;;RLAPI Texture2D GenTexturePrefilter(Shader shader, Texture2D cubemap, int size);    // Generate prefilter texture using cubemap data
(defcfun "GenTexturePrefilter" (:struct %texture)
 "Generate prefilter texture using cubemap data"
 (shader (:struct %shader))
 (cubemap (:struct %texture))
 (size :int))

;;RLAPI Texture2D GenTextureBRDF(Shader shader, int size);                  // Generate BRDF texture
(defcfun "GenTextureBRDF" (:struct %texture)
 "Generate BRDF texture"
 (shader (:struct %shader))
 (size :int))

;;// Shading begin/end functions
;;RLAPI void BeginShaderMode(Shader shader);                                // Begin custom shader drawing
(defcfun "BeginShaderMode" :void
 "Begin custom shader drawing"
 (shader (:struct %shader)))

;;RLAPI void EndShaderMode(void);                                           // End custom shader drawing (use default shader)
(defcfun "EndShaderMode" :void
 "End custom shader drawing (use default shader)")

;;RLAPI void BeginBlendMode(int mode);                                      // Begin blending mode (alpha, additive, multiplied)
(defcfun "BeginBlendMode" :void
 "Begin blending mode (alpha, additive, multiplied)"
 (mode :int))

;;RLAPI void EndBlendMode(void);                                            // End blending mode (reset to default: alpha blending)
(defcfun "EndBlendMode" :void
 "End blending mode (reset to default: alpha blending)")

;;RLAPI void BeginScissorMode(int x, int y, int width, int height);         // Begin scissor mode (define screen area for following drawing)
(defcfun "BeginScissorMode" :void
 "Begin scissor mode (define screen area for following drawing)"
 (x :int)
 (y :int)
 (width :int)
 (height :int))

;;RLAPI void EndScissorMode(void);                                          // End scissor mode
(defcfun "EndScissorMode" :void
 "End scissor mode")

;;// VR control functions
;;RLAPI void InitVrSimulator(void);                       // Init VR simulator for selected device parameters
(defcfun "InitVrSimulator" :void
 "Init VR simulator for selected device parameters")

;;RLAPI void CloseVrSimulator(void);                      // Close VR simulator for current device
(defcfun "CloseVrSimulator" :void
 "Close VR simulator for current device")

;;RLAPI void UpdateVrTracking(Camera *camera);            // Update VR tracking (position and orientation) and camera
(defcfun "UpdateVrTracking" :void
 "Update VR tracking (position and orientation) and camera"
 (camera :pointer))

;;RLAPI void SetVrConfiguration(VrDeviceInfo info, Shader distortion);      // Set stereo rendering configuration parameters 
(defcfun "SetVrConfiguration" :void
 "Set stereo rendering configuration parameters"
 (info (:struct %vr-device-info))
 (distortion (:struct %shader)))

;;RLAPI bool IsVrSimulatorReady(void);                    // Detect if VR simulator is ready
(defcfun "IsVrSimulatorReady" :bool
 "Detect if VR simulator is ready")

;;RLAPI void ToggleVrMode(void);                          // Enable/Disable VR experience
(defcfun "ToggleVrMode" :void
 "Enable/Disable VR experience")

;;RLAPI void BeginVrDrawing(void);                        // Begin VR simulator stereo rendering
(defcfun "BeginVrDrawing" :void
 "Begin VR simulator stereo rendering")

;;RLAPI void EndVrDrawing(void);                          // End VR simulator stereo rendering
(defcfun "EndVrDrawing" :void
 "End VR simulator stereo rendering")

;;//------------------------------------------------------------------------------------
;;// Audio Loading and Playing Functions (Module: audio)
;;//------------------------------------------------------------------------------------
;;
;;// Audio device management functions
;;RLAPI void InitAudioDevice(void);                                     // Initialize audio device and context
(defcfun "InitAudioDevice" :void
 "Initialize audio device and context")

;;RLAPI void CloseAudioDevice(void);                                    // Close the audio device and context
(defcfun "CloseAudioDevice" :void
 "Close the audio device and context")

;;RLAPI bool IsAudioDeviceReady(void);                                  // Check if audio device has been initialized successfully
(defcfun "IsAudioDeviceReady" :boolean
 "Check if audio device has been initialized successfully")

;;RLAPI void SetMasterVolume(float volume);                             // Set master volume (listener)
(defcfun "SetMasterVolume" :void
 "Set master volume (listener)"
 (volume :float))

;;// Wave/Sound loading/unloading functions
;;RLAPI Wave LoadWave(const char *fileName);                            // Load wave data from file
(defcfun "LoadWave" (:struct %wave)
 "Load wave data from file"
  (file-name :string))

;;RLAPI Wave LoadWaveEx(void *data, int sampleCount, int sampleRate, int sampleSize, int channels); // Load wave data from raw array data
(defcfun "LoadWaveEx" (:struct %wave)
 "Load wave data from raw array data"
  (data :pointer)
  (sample-count :int)
  (sample-rate :int)
  (sample-size :int)
  (channels :int))

;;RLAPI Sound LoadSound(const char *fileName);                          // Load sound from file
(defcfun "LoadSound" (:struct %sound)
 "Load sound from file"
  (file-name :string))

;;RLAPI Sound LoadSoundFromWave(Wave wave);                             // Load sound from wave data
(defcfun "LoadSoundFromWave" (:struct %sound)
 "Load sound from wave data"
  (wave (:struct %wave)))

;;RLAPI void UpdateSound(Sound sound, const void *data, int samplesCount);// Update sound buffer with new data
(defcfun "UpdateSound" :void
 "Update sound buffer with new data"
  (sound (:struct %sound))
  (data :pointer)
  (samples-count :int))

;;RLAPI void UnloadWave(Wave wave);                                     // Unload wave data
(defcfun "UnloadWave" :void
 "Unload wave data"
  (wave (:struct %wave)))

;;RLAPI void UnloadSound(Sound sound);                                  // Unload sound
(defcfun "UnloadSound" :void
 "Unload sound"
 (sound (:struct %sound)))

;;RLAPI void ExportWave(Wave wave, const char *fileName);               // Export wave data to file
(defcfun "ExportWave" :void
 "Export wave data to file"
 (wave (:struct %wave))
 (file-name :string))

;;RLAPI void ExportWaveAsCode(Wave wave, const char *fileName);         // Export wave sample data to code (.h)
(defcfun "ExportWaveAsCode" :void
 "Export wave sample data to code (.h)"
 (wave (:struct %wave))
 (file-name :string))

;;// Wave/Sound management functions
;;RLAPI void PlaySound(Sound sound);                                    // Play a sound
(defcfun "PlaySound" :void
 "Play a sound"
  (sound (:struct %sound)))

;;RLAPI void PauseSound(Sound sound);                                   // Pause a sound
(defcfun "PauseSound" :void
 "Pause a sound"
  (sound (:struct %sound)))

;;RLAPI void ResumeSound(Sound sound);                                  // Resume a paused sound
(defcfun "ResumeSound" :void
 "Resume a paused sound"
  (sound (:struct %sound)))

;;RLAPI void StopSound(Sound sound);                                    // Stop playing a sound
(defcfun "StopSound" :void
 "Stop playing a sound"
  (sound (:struct %sound)))

;;RLAPI bool IsSoundPlaying(Sound sound);                               // Check if a sound is currently playing
(defcfun "IsSoundPlaying" :bool
 "Check if a sound is currently playing"
  (sound (:struct %sound)))

;;RLAPI void SetSoundVolume(Sound sound, float volume);                 // Set volume for a sound (1.0 is max level)
(defcfun "SetSoundVolume" :void
 "Set volume for a sound (1.0 is max level)"
 (sound (:struct %sound))
 (volume :float))

;;RLAPI void SetSoundPitch(Sound sound, float pitch);                   // Set pitch for a sound (1.0 is base level)
(defcfun "SetSoundPitch" :void
 "Set pitch for a sound (1.0 is base level)"
 (sound (:struct %sound))
 (pitch :float))

;;RLAPI void WaveFormat(Wave *wave, int sampleRate, int sampleSize, int channels);  // Convert wave data to desired format
(defcfun "WaveFormat" :void
 "Convert wave data to desired format"
 (wave (:pointer (:struct %wave)))
 (sample-rate :int)
 (sample-size :int)
 (channels :int))

;;RLAPI Wave WaveCopy(Wave wave);                                       // Copy a wave to a new wave
(defcfun "WaveCopy" (:struct %wave)
 "Copy a wave to a new wave"
 (wave (:struct %wave)))

;;RLAPI void WaveCrop(Wave *wave, int initSample, int finalSample);     // Crop a wave to defined samples range
(defcfun "WaveCrop" :void
 "Crop a wave to defined samples range"
 (wave (:pointer (:struct %wave)))
 (init-sample :int)
 (final-sample :int))

;;RLAPI float *GetWaveData(Wave wave);                                  // Get samples data from wave as a floats array
(defcfun "GetWaveData" (:pointer :float)
 "Get samples data from wave as a floats array"
 (wave (:struct %wave)))

;;// Music management functions
;;RLAPI Music LoadMusicStream(const char *fileName);                    // Load music stream from file
(defcfun "LoadMusicStream" music
 "Load music stream from file"
 (file-name :string))

;;RLAPI void UnloadMusicStream(Music music);                            // Unload music stream
(defcfun "UnloadMusicStream" :void
 "Unload music stream"
 (music music))

;;RLAPI void PlayMusicStream(Music music);                              // Start music playing
(defcfun "PlayMusicStream" :void
 "Start music playing"
 (music music))

;;RLAPI void UpdateMusicStream(Music music);                            // Updates buffers for music streaming
(defcfun "UpdateMusicStream" :void
 "Updates buffers for music streaming"
 (music music))

;;RLAPI void StopMusicStream(Music music);                              // Stop music playing
(defcfun "StopMusicStream" :void
 "Stop music playing"
 (music music))

;;RLAPI void PauseMusicStream(Music music);                             // Pause music playing
(defcfun "PauseMusicStream" :void
 "Pause music playing"
 (music music))

;;RLAPI void ResumeMusicStream(Music music);                            // Resume playing paused music
(defcfun "ResumeMusicStream" :void
 "Resume playing paused music"
 (music music))

;;RLAPI bool IsMusicPlaying(Music music);                               // Check if music is playing
(defcfun "IsMusicPlaying" :boolean
 "Check if music is playing"
 (music music))

;;RLAPI void SetMusicVolume(Music music, float volume);                 // Set volume for music (1.0 is max level)
(defcfun "SetMusicVolume" :void
 "Set volume for music (1.0 is max level)"
 (music music)
 (volume :float))

;;RLAPI void SetMusicPitch(Music music, float pitch);                   // Set pitch for a music (1.0 is base level)
(defcfun "SetMusicPitch" :void
 "Set pitch for a music (1.0 is base level)"
 (music music)
 (pitch :float))

;;RLAPI void SetMusicLoopCount(Music music, int count);                 // Set music loop count (loop repeats)
(defcfun "SetMusicLoopCount" :void
 "Set music loop count (loop repeats)"
 (music music)
 (count :int))

;;RLAPI float GetMusicTimeLength(Music music);                          // Get music time length (in seconds)
(defcfun "GetMusicTimeLength" :float
 "Get music time length (in seconds)"
 (music music))

;;RLAPI float GetMusicTimePlayed(Music music);                          // Get current music time played (in seconds)
(defcfun "GetMusicTimePlayed" :float
 "Get current music time played (in seconds)"
 (music music))


;;// AudioStream management functions
;;RLAPI AudioStream InitAudioStream(unsigned int sampleRate, unsigned int sampleSize, unsigned int channels); // Init audio stream (to stream raw audio pcm data)
(defcfun "InitAudioStream" (:struct %audio-stream)
 "Init audio stream (to stream raw audio pcm data)"
 (sample-rate :unsigned-int)
 (sample-size :unsigned-int)
 (channels :unsigned-int))

;;RLAPI void UpdateAudioStream(AudioStream stream, const void *data, int samplesCount); // Update audio stream buffers with data
(defcfun "UpdateAudioStream" :void
 "Update audio stream buffers with data"
 (stream (:struct %audio-stream))
 (data :pointer)
 (samples-count :int))

;;RLAPI void CloseAudioStream(AudioStream stream);                      // Close audio stream and free memory
(defcfun "CloseAudioStream" :void
 "Close audio stream and free memory"
 (stream (:struct %audio-stream)))

;;RLAPI bool IsAudioBufferProcessed(AudioStream stream);                // Check if any audio stream buffers requires refill
(defcfun "IsAudioBufferProcessed" :bool
 "Check if any audio stream buffers requires refill"
 (stream (:struct %audio-stream)))

;;RLAPI void PlayAudioStream(AudioStream stream);                       // Play audio stream
(defcfun "PlayAudioStream" :void
 "Play audio stream"
 (stream (:struct %audio-stream)))

;;RLAPI void PauseAudioStream(AudioStream stream);                      // Pause audio stream
(defcfun "PauseAudioStream" :void
 "Pause audio stream"
 (stream (:struct %audio-stream)))

;;RLAPI void ResumeAudioStream(AudioStream stream);                     // Resume audio stream
(defcfun "ResumeAudioStream" :void
 "Resume audio stream"
 (stream (:struct %audio-stream)))

;;RLAPI bool IsAudioStreamPlaying(AudioStream stream);                  // Check if audio stream is playing
(defcfun "IsAudioStreamPlaying" :bool
 "Check if audio stream is playing"
 (stream (:struct %audio-stream)))

;;RLAPI void StopAudioStream(AudioStream stream);                       // Stop audio stream
(defcfun "StopAudioStream" :void
 "Stop audio stream"
 (stream (:struct %audio-stream)))

;;RLAPI void SetAudioStreamVolume(AudioStream stream, float volume);    // Set volume for audio stream (1.0 is max level)
(defcfun "SetAudioStreamVolume" :void
 "Set volume for audio stream (1.0 is max level)"
 (stream (:struct %audio-stream))
 (volume :float))

;;RLAPI void SetAudioStreamPitch(AudioStream stream, float pitch);      // Set pitch for audio stream (1.0 is base level)
(defcfun "SetAudioStreamPitch" :void
 "Set pitch for audio stream (1.0 is base level)"
 (stream (:struct %audio-stream))
 (pitch :float))

;;
;;//------------------------------------------------------------------------------------
;;// Network (Module: network)
;;//------------------------------------------------------------------------------------
;;
;;// IN PROGRESS: Check rnet.h for reference
;;
;;#if defined(__cplusplus)
;;}
;;#endif
;;
;;#endif // RAYLIB_H

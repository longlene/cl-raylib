(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defparameter +gol-logic-shader+
  "#version 430

// Game of Life logic shader

#define GOL_WIDTH 768

layout (local_size_x = 16, local_size_y = 16, local_size_z = 1) in;

layout(std430, binding = 1) readonly restrict buffer golLayout {
    uint golBuffer[];       // golBuffer[x, y] = golBuffer[x + gl_NumWorkGroups.x * y]
};

layout(std430, binding = 2) writeonly restrict buffer golLayout2 {
    uint golBufferDest[];   // golBufferDest[x, y] = golBufferDest[x + gl_NumWorkGroups.x * y]
};

#define fetchGol(x, y) ((((x) < 0) || ((y) < 0) || ((x) > GOL_WIDTH) || ((y) > GOL_WIDTH)) ? (0) : (golBuffer[(x) + GOL_WIDTH * (y)]))

#define setGol(x, y, value) golBufferDest[(x) + GOL_WIDTH*(y)] = value

void main()
{
    uint neighbourCount = 0;
    uint x = gl_GlobalInvocationID.x;
    uint y = gl_GlobalInvocationID.y;

    neighbourCount += fetchGol(x - 1, y - 1);   // Top left
    neighbourCount += fetchGol(x, y - 1);       // Top middle
    neighbourCount += fetchGol(x + 1, y - 1);   // Top right
    neighbourCount += fetchGol(x - 1, y);       // Left
    neighbourCount += fetchGol(x + 1, y);       // Right
    neighbourCount += fetchGol(x - 1, y + 1);   // Bottom left
    neighbourCount += fetchGol(x, y + 1);       // Bottom middle   
    neighbourCount += fetchGol(x + 1, y + 1);   // Bottom right

    if (neighbourCount == 3) setGol(x, y, 1);
    else if (neighbourCount == 2) setGol(x, y, fetchGol(x, y));
    else setGol(x, y, 0);
}")

(defparameter +gol-render-shader+
  "#version 430

// Game of Life rendering shader
// Just renders the content of the ssbo at binding 1 to screen

#define GOL_WIDTH 768

// Input vertex attributes (from vertex shader)
in vec2 fragTexCoord;

// Output fragment color
out vec4 finalColor;

// Input game of life grid.
layout(std430, binding = 1) readonly buffer golLayout
{
    uint golBuffer[];
};

// Output resolution
uniform vec2 resolution;

void main()
{
    ivec2 coords = ivec2(fragTexCoord*resolution);

    if ((golBuffer[coords.x + coords.y*uvec2(resolution).x]) == 1) finalColor = vec4(1.0);
    else finalColor = vec4(0.0, 0.0, 0.0, 1.0);
}")

(defparameter +gol-transfert-shader+
  "#version 430

// Game of life transfert shader

#define GOL_WIDTH 768

// Game Of Life Update Command
// NOTE: matches the structure defined on main program
struct GolUpdateCmd {
    uint x;         // x coordinate of the gol command
    uint y;         // y coordinate of the gol command
    uint w;         // width of the filled zone
    uint enabled;   // whether to enable or disable zone
};

// Local compute unit size
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

// Output game of life grid buffer
layout(std430, binding = 1) buffer golBufferLayout
{
    uint golBuffer[]; // golBuffer[x, y] = golBuffer[x + GOL_WIDTH * y]
};

// Command buffer
layout(std430, binding = 3) readonly restrict buffer golUpdateLayout
{
    uint count;
    GolUpdateCmd commands[];
};

#define isInside(x, y) (((x) >= 0) && ((y) >= 0) && ((x) < GOL_WIDTH) && ((y) < GOL_WIDTH))
#define getBufferIndex(x, y) ((x) + GOL_WIDTH * (y))

void main()
{
    uint cmdIndex = gl_GlobalInvocationID.x;
    GolUpdateCmd cmd = commands[cmdIndex];

    for (uint x = cmd.x; x < (cmd.x + cmd.w); x++)
    {
        for (uint y = cmd.y; y < (cmd.y + cmd.w); y++)
        {
            if (isInside(x, y))
            {
                if (cmd.enabled != 0) atomicOr(golBuffer[getBufferIndex(x, y)], 1);
                else atomicAnd(golBuffer[getBufferIndex(x, y)], 0);
            }
        }
    }
}")

(defconstant +gol-width+ 768)
(defconstant +max-buffered-transfers+ 48)

(cffi:defcstruct %gol-update-cmd
  (x :uint)
  (y :uint)
  (w :uint)
  (enabled :uint))

(cffi:defcstruct %gol-update-ssbo
  (count :uint)
  (commands (:array (:struct %gol-update-cmd) 48)))

(defun main ()
  (let ((screen-width +gol-width+)
        (screen-height +gol-width+))
    (with-window (screen-width screen-height "raylib [rlgl] example - compute shader - game of life")
      (let* ((brush-size 8)
             (gol-logic-shader (rlgl::compile-shader +gol-logic-shader+ rlgl::+compute-shader+))
             (gol-logic-program (rlgl::load-compute-shader-program gol-logic-shader))
             
             (gol-render-shader (raylib::load-shader-from-memory (cffi:null-pointer) +gol-render-shader+))
             (res-uniform-loc (get-shader-location gol-render-shader "resolution"))
             
             (gol-transfert-shader (rlgl::compile-shader +gol-transfert-shader+ rlgl::+compute-shader+))
             (gol-transfert-program (rlgl::load-compute-shader-program gol-transfert-shader))

             (ssbo-a (rlgl::load-shader-buffer (* +gol-width+ +gol-width+ (cffi:foreign-type-size :uint))
                                               (cffi:null-pointer)
                                               rlgl::+dynamic-copy+))
             (ssbo-b (rlgl::load-shader-buffer (* +gol-width+ +gol-width+ (cffi:foreign-type-size :uint))
                                               (cffi:null-pointer)
                                               rlgl::+dynamic-copy+))
             (ssbo-transfert (rlgl::load-shader-buffer (cffi:foreign-type-size '(:struct %gol-update-ssbo))
                                                       (cffi:null-pointer)
                                                       rlgl::+dynamic-copy+))
             
             (white-tex (let ((white-image (gen-image-color +gol-width+ +gol-width+ :white)))
                          (unwind-protect
                               (load-texture-from-image white-image)
                            (unload-image white-image)))))
        (unwind-protect
             (progn
               (cffi:with-foreign-objects ((transfert-buffer '(:struct %gol-update-ssbo))
                                           (resolution '(:struct raylib::%vector2)))
                 (cffi:with-foreign-slots ((raylib::x raylib::y) resolution (:struct raylib::%vector2))
                   (setf raylib::x (coerce +gol-width+ 'float)
                         raylib::y (coerce +gol-width+ 'float)))
                 (loop
                   until (window-should-close)
                   do (incf brush-size (get-mouse-wheel-move))
                      (cffi:with-foreign-slots ((count (:pointer commands)) transfert-buffer (:struct %gol-update-ssbo))
                        (cond
                          ((and (or (is-mouse-button-down :mouse-button-left)
                                    (is-mouse-button-down :mouse-button-right))
                                (< count +max-buffered-transfers+))
                           (cffi:with-foreign-slots ((x y w enabled) (cffi:mem-aptr commands '(:struct %gol-update-cmd) count) (:struct %gol-update-cmd))
                             (setf x (round (- (get-mouse-x) (/ brush-size 2)))
                                   y (round (- (get-mouse-y) (/ brush-size 2)))
                                   w (round brush-size)
                                   enabled (if (is-mouse-button-down :mouse-button-left) 1 0)))
                           (incf count))
                          ((> count 0)
                           (rlgl::update-shader-buffer ssbo-transfert transfert-buffer (cffi:foreign-type-size '(:struct %gol-update-ssbo)) 0)
                           (rlgl::enable-shader gol-transfert-program)
                           (rlgl::bind-shader-buffer ssbo-a 1)
                           (rlgl::bind-shader-buffer ssbo-transfert 3)
                           (rlgl::compute-shader-dispatch count 1 1)
                           (rlgl::disable-shader)
                           (setf count 0))
                          (t
                           (rlgl::enable-shader gol-logic-program)
                           (rlgl::bind-shader-buffer ssbo-a 1)
                           (rlgl::bind-shader-buffer ssbo-b 2)
                           (rlgl::compute-shader-dispatch (/ +gol-width+ 16) (/ +gol-width+ 16) 1)
                           (rlgl::disable-shader)
                           (rotatef ssbo-a ssbo-b))))

                      (rlgl::bind-shader-buffer ssbo-a 1)
                      (set-shader-value gol-render-shader res-uniform-loc resolution :shader-uniform-vec2)
                      (with-drawing
                        (clear-background :blank)
                        (begin-shader-mode gol-render-shader)
                        (draw-texture white-tex 0 0 :blank)
                        (end-shader-mode)
                        (draw-rectangle-lines (round (- (get-mouse-x) (/ brush-size 2)))
                                              (round (- (get-mouse-y) (/ brush-size 2)))
                                              (round brush-size)
                                              (round brush-size)
                                              :red)
                        (draw-fps 20 20)))))
          (progn
            (rlgl::unload-shader-buffer ssbo-transfert)
            (rlgl::unload-shader-buffer ssbo-b)
            (rlgl::unload-shader-buffer ssbo-a)

            (unload-texture white-tex)
            
            (rlgl::unload-shader-program gol-transfert-program)
            (unload-shader gol-render-shader)
            (rlgl::unload-shader-program gol-logic-program)))))))

(main)

# cl-raylib

A pure Common Lisp implementation of [raylib](https://www.raylib.com/) (6.x).

cl-raylib is a translation of raylib's C source into Common Lisp, not a binding to the raylib
shared library. It keeps raylib's structure (`rcore`, `rlgl`, `rshapes`, `rtextures`, `rtext`,
`rmodels`, `raudio`, `raymath`, ...) and its bundled libraries (stb_truetype, stb_vorbis, dr_mp3,
cgltf, ...), and only calls into C for the platform: GLFW, OpenGL, libm and the audio system.

## Status

- All 619 functions of `raylib.h` are implemented, together with raymath, rlgl, rcamera and rgestures.
- All 226 official raylib examples are ported to Lisp (`examples/`, same file names as in raylib).
- Output is checked against C raylib: rendered frames, generated meshes, decoded audio and exported
  files are compared byte for byte.

| Platform | State |
|----------|-------|
| Linux (X11, Wayland) | Supported, audio through PulseAudio (or PipeWire's PulseAudio server) |
| macOS (Apple Silicon) | Supported, audio through Core Audio |
| Windows (x86-64) | Supported, audio through WinMM |

## Requirements

- [SBCL](https://www.sbcl.org/) (developed with SBCL 2.6) or [ECL](https://ecl.common-lisp.dev/)
  (tested with ECL 26.5 on macOS)
- [Quicklisp](https://www.quicklisp.org/)
- An OpenGL 3.3 capable GPU

GLFW does not need to be installed: cl-raylib uses the system GLFW when it is version 3.4 or newer,
and otherwise the GLFW 3.5.1 libraries shipped in `lib/` (Linux x86-64, macOS arm64, Windows x86-64), built from
the GLFW sources bundled with raylib by `lib/build-glfw.sh`.

## Installation

cl-raylib is not in Quicklisp yet, clone it into your Quicklisp local projects directory:

```bash
git clone https://github.com/longlene/cl-raylib.git ~/quicklisp/local-projects/cl-raylib
```

Then load it once to download and compile the dependencies:

```bash
sbcl --eval '(ql:quickload :cl-raylib)' --quit
```

## A first program

```lisp
(ql:quickload :cl-raylib)

(defpackage #:hello
  (:use #:cl #:raylib))
(in-package #:hello)

(with-window (800 450 "cl-raylib - hello")
  (set-target-fps 60)
  (loop until (window-should-close)
        do (with-drawing
             (clear-background +raywhite+)
             (draw-text "Congrats! You created your first window!" 190 200 20 +lightgray+))))
```

## Examples

The examples are organized like raylib's: `core`, `shapes`, `textures`, `text`, `models`, `shaders`,
`audio` and `others`. Run them from their directory, so that they find their `resources/`:

```bash
cd ~/quicklisp/local-projects/cl-raylib/examples/core
sbcl --load core_basic_window.lisp
```

## Using the API

Names follow raylib, in Lisp style:

| C raylib | cl-raylib |
|----------|-----------|
| `InitWindow(800, 450, "title")` | `(init-window 800 450 "title")` |
| `DrawCircleV(center, 10, RED)` | `(draw-circle-v center 10 +red+)` |
| `KEY_SPACE`, `FLAG_WINDOW_RESIZABLE` | `+key-space+`, `+flag-window-resizable+` |
| `Vector2`, `Vector3`, `Vector4`, `Matrix` | 3d-vectors `vec2`/`vec3`/`vec4` and 3d-matrices `mat4` |
| `Color` | a list `(r g b a)`, e.g. `(list 230 41 55 255)` |
| `Rectangle`, `Camera3D`, `Image`, `Texture2D`, ... | structures with the same fields, e.g. `(make-rectangle :x 0.0 :y 0.0 :width 10.0 :height 10.0)` |

Functions that return data through pointer arguments in C return Lisp values instead (for example
`text-split` returns a list of strings).

The `with-*` macros pair raylib's Begin/End calls and always run the End call, even on a non-local
exit: `with-window`, `with-drawing`, `with-mode-2d`, `with-mode-3d`, `with-texture-mode`,
`with-shader-mode`, `with-blend-mode`, `with-scissor-mode`, `with-audio-device`, ...

Images load from PNG, BMP, GIF, QOI, DDS, TGA, JPG and binary PPM/PGM files and export to PNG, BMP, QOI,
TGA and JPG (raylib leaves TGA, JPG and PNM disabled by default; cl-raylib always includes them). JPG
decoding goes through [cl-jpeg](https://github.com/sharplispers/cl-jpeg), which handles baseline JPEGs
only.

### macOS

Cocoa only allows windows and events on the process main thread. Run programs with
`sbcl --load program.lisp`, or from SLIME/Sly through a main thread helper such as
[trivial-main-thread](https://github.com/Shinmera/trivial-main-thread).

## License

MIT, see [LICENSE](LICENSE). The ported raylib code and its bundled libraries keep their original
licenses (zlib/libpng for raylib, see the header of each file). The GLFW libraries in `lib/` are
under the zlib/libpng license, see [lib/GLFW-LICENSE.md](lib/GLFW-LICENSE.md).

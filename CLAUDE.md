# CLAUDE.md - cl-raylib Development Guide

## Project Overview

cl-raylib is a pure Common Lisp implementation of raylib that closely follows the original C library structure and API. It talks to the system only through cffi (GLFW, OpenGL, libm, PulseAudio, X11) and uses 3d-vectors/3d-matrices for its math types.

## Project Translation Philosophy

### Core Translation Guidelines
- This is a C -> Common Lisp translation project: follow raylib's implementation and code organization as closely as possible.
- raylib C sources (6.1-dev): /home/loong0/src/raylib
- File correspondence:
```
raylib/src/rcore.c -> cl-raylib/src/core.lisp
raylib/src/platforms/rcore_desktop_glfw.c -> cl-raylib/src/glfw.lisp
raylib/src/rgestures.h -> cl-raylib/src/gestures.lisp
raylib/src/rlgl.h -> cl-raylib/src/gl.lisp
raylib/src/raylib.h -> cl-raylib/src/raylib.lisp
raylib/src/raymath.h -> cl-raylib/src/math.lisp
raylib/src/rcamera.h -> cl-raylib/src/camera3d.lisp
raylib/src/rmodels.c -> cl-raylib/src/models.lisp
raylib/src/external/par_shapes.h (subset used by rmodels.c) -> cl-raylib/src/par-shapes.lisp
raylib/src/external/tinyobj_loader_c.h -> cl-raylib/src/tinyobj.lisp
raylib/src/external/vox_loader.h -> cl-raylib/src/vox.lisp
raylib/src/external/cgltf.h (parser/loader subset used by rmodels.c) -> cl-raylib/src/gltf.lisp
raylib/src/external/m3d.h (binary importer used by rmodels.c) -> cl-raylib/src/m3d.lisp
raylib/src/rshapes.h -> cl-raylib/src/shapes.lisp
raylib/src/rtext.h -> cl-raylib/src/text.lisp
raylib/src/external/stb_truetype.h + stb_rect_pack.h -> cl-raylib/src/truetype.lisp
raylib/src/rtextures.h -> cl-raylib/src/textures.lisp
raylib/src/raudio.c -> cl-raylib/src/audio.lisp
raylib/src/external/miniaudio.h (data conversion subset + PulseAudio device) -> cl-raylib/src/miniaudio.lisp
raylib/src/external/dr_wav.h -> cl-raylib/src/wav.lisp
raylib/src/external/stb_vorbis.c -> cl-raylib/src/vorbis.lisp
raylib/src/external/dr_mp3.h -> cl-raylib/src/mp3.lisp
raylib/src/external/jar_xm.h -> cl-raylib/src/xm.lisp
raylib/src/external/jar_mod.h -> cl-raylib/src/mod.lisp
raylib/src/external/qoa.h + qoaplay.c -> cl-raylib/src/qoa.lisp
raylib/src/external/dr_flac.h -> cl-raylib/src/flac.lisp (own decoder, dr_flac output semantics)
raylib/src/utils.h -> cl-raylib/src/utils.lisp

```

## Development Memories
- cl-raylib is a Common Lisp game library translated from the C project raylib. It aims to cover all of the original library's capabilities while exposing an API close to cl-raylib.cffi (/home/loong0/.quicklisp/local-projects/cl-raylib.cffi/), a cffi binding to the raylib shared library. Because of consistency problems and the difficulty of passing structs through FFI, the project was rewritten as cl-raylib: the public API should stay as close to cl-raylib.cffi as possible (where they conflict, follow the latest raylib), while the implementation details should follow the C logic.
- Dependencies: quicklisp's glfw system (~/.quicklisp/dists/quicklisp/software/glfw-20260101-git, used through its %glfw cffi package), cffi, 3d-vectors, 3d-matrices, float-features and the others listed in cl-raylib.asd.
- When porting an API, keep the implementation close to the C version, and keep functions in the same order as in the C file where practical, so the two versions are easy to compare later.
- Do not try to fix mismatched parentheses with Python scripts; it costs more than it saves.

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
raylib/src/rshapes.c -> cl-raylib/src/shapes.lisp
raylib/src/rtext.c -> cl-raylib/src/text.lisp
raylib/src/external/stb_truetype.h + stb_rect_pack.h -> cl-raylib/src/truetype.lisp
raylib/src/rtextures.c -> cl-raylib/src/textures.lisp
raylib/src/external/stb_image_write.h (PNG/BMP writers) -> cl-raylib/src/stb-image-write.lisp
raylib/src/external/stb_image_resize2.h (stbir_resize_uint8_linear path) -> cl-raylib/src/stb-image-resize.lisp
raylib/src/raudio.c -> cl-raylib/src/audio.lisp
raylib/src/external/miniaudio.h (data conversion subset + PulseAudio device) -> cl-raylib/src/miniaudio.lisp
raylib/src/external/dr_wav.h -> cl-raylib/src/wav.lisp
raylib/src/external/stb_vorbis.c -> cl-raylib/src/vorbis.lisp
raylib/src/external/dr_mp3.h -> cl-raylib/src/mp3.lisp
raylib/src/external/jar_xm.h -> cl-raylib/src/xm.lisp
raylib/src/external/jar_mod.h -> cl-raylib/src/mod.lisp
raylib/src/external/qoa.h + qoaplay.c -> cl-raylib/src/qoa.lisp
raylib/src/external/dr_flac.h -> cl-raylib/src/flac.lisp (own decoder, dr_flac output semantics)
raylib/src/external/sdefl.h -> cl-raylib/src/sdefl.lisp
raylib/src/utils.h -> cl-raylib/src/utils.lisp
```

## Development Memories
- cl-raylib is a Common Lisp game library translated from the C project raylib. It aims to cover all of the original library's capabilities while exposing an API close to cl-raylib.cffi (/home/loong0/.quicklisp/local-projects/cl-raylib.cffi/), a cffi binding to the raylib shared library. Because of consistency problems and the difficulty of passing structs through FFI, the project was rewritten as cl-raylib: the public API should stay as close to cl-raylib.cffi as possible (where they conflict, follow the latest raylib), while the implementation details should follow the C logic.
- Dependencies: quicklisp's glfw system (~/.quicklisp/dists/quicklisp/software/glfw-20260101-git, used through its %glfw cffi package), cffi, 3d-vectors, 3d-matrices, float-features and the others listed in cl-raylib.asd.
- When porting an API, keep the implementation close to the C version, and keep functions in the same order as in the C file where practical, so the two versions are easy to compare later.
- Do not try to fix mismatched parentheses with Python scripts; it costs more than it saves.

## Porting Progress (last updated: 2026-10-04, branch pure)

### Overview
- All 619 RLAPI functions in raylib.h have a Lisp implementation (raymath, rlgl, rcamera and rgestures are complete too).
- About 44,700 lines in 35 source files of pure Common Lisp (cffi is only used to call GLFW/OpenGL/libm/PulseAudio/X11).
- Verification: outputs (pixels, meshes, audio, files) are compared byte for byte against C raylib (a GL 3.3 build in the scratchpad).

### Modules verified byte-identical to C
- rlgl (GL 3.3); rcore shaders, VR, file and path functions, CompressData (sdefl), clipboard images (X11)
- rshapes; rtextures (including PNG/BMP export via stb_image_write and ImageResize via stb_image_resize2); rtext (stb_truetype)
- rmodels: 3D shapes, GenMesh*, materials, animations, collisions, and the OBJ/MTL, IQM, VOX, glTF/GLB and M3D loaders
- raudio: WAV/OGG/MP3/QOA/FLAC/XM/MOD decoding and mixing
- raymath, rmodels and rtextures call libm sinf/cosf etc. so their trig matches C exactly; rcamera

### Examples (examples/)
- All 226 official raylib examples (core/shapes/textures/text/models/shaders/audio/others) are ported one by one, with the same file names as the C versions.
- Helper headers shipped with the examples are ported next to them: rlights.lisp (models, shaders), reasings.lisp (shapes), msf_gif.lisp (core).
- Screenshot comparison: pixel-identical to C (including variant tests for raygui interaction, recorded GIFs, etc.), except:
  - core_directory_files: the listing differs because C and Lisp run in different working directories
  - examples that depend on audio-thread timing (audio_mixed_processor, audio_raw_stream, audio_stream_callback,
    audio_spectrum_visualizer) differ between runs of the C version itself; they are verified with fixed-timing variants

### Platforms
- Linux (X11 and Wayland): complete and verified against C.
- macOS (Apple Silicon, tested on macOS 27 with SBCL 2.6.9 over `ssh msu`): all examples run. Audio uses the
  Core Audio AudioQueue backend in miniaudio.lisp; directory scanning reads the darwin dirent layout ($INODE64
  symbols on x86-64, untested). Cocoa requires InitWindow() and the main loop on the process main thread.
  GetClipboardImage() only warns, like C. Quicklisp's macOS GLFW is 3.4.0: it rejects GLFW_SCALE_FRAMEBUFFER,
  so the old GLFW_COCOA_RETINA_FRAMEBUFFER hint is used, and GLFW reports the real framebuffer size only after
  the first event poll. Large stack allocated foreign arrays (with-foreign-objects) fault on SBCL arm64 macOS.
- Windows: not supported yet (needs a FindFirstFileW directory scan, a WinMM/WASAPI audio backend and the
  win32_clipboard.h port).

### Known differences (all documented in code comments)
- Undefined behavior in C (out-of-bounds reads, uninitialized memory) is treated as 0 in Lisp; matching it is not a goal.
- LOG_FATAL does not exit the process; DecompressData uses chipz (same results for valid data).
- JPG/TGA/PNM and other formats that raylib disables by default are provided through imago.

### TODO
- Bind the GLFW functions directly (glfw3.h subset) instead of depending on quicklisp's glfw system, which pulls in
  cl-opengl: its first compile exhausts SBCL's default 1 GB heap.
- The camera2d-* helpers in camera2d.lisp and the logging/timing extensions in utils.lisp are not raylib API; consider removing them.
- raygui.lisp is only a partial port (raygui.h is not part of raylib itself).

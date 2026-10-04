#!/usr/bin/env bash
# Build the GLFW shared libraries shipped in lib/ from the GLFW sources bundled with raylib
# (raylib/src/external/glfw), so cl-raylib uses the same GLFW version as C raylib.
#
# Usage: lib/build-glfw.sh <path-to-raylib>
#
#   Linux x86-64   -> lib/linux-x86-64/libglfw.so.3   (X11 and Wayland backends)
#   macOS          -> lib/macos/libglfw.3.dylib        (Apple Silicon, macOS 11+)
#   Windows x86-64 -> lib/windows-x86-64/glfw3.dll     (MSYS2 MinGW-w64 shell, static libgcc)
#
# Requirements: CMake and a C compiler; on Linux the X11 and Wayland development packages
# and wayland-scanner (GLFW generates the Wayland protocol headers at build time).
set -euo pipefail

if [ $# -ne 1 ]; then
  echo "usage: $0 <path-to-raylib>" >&2
  exit 1
fi

GLFW_SRC="$1/src/external/glfw"
LIB_DIR="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR="$(mktemp -d)"
trap 'rm -rf "$BUILD_DIR"' EXIT

if [ ! -f "$GLFW_SRC/CMakeLists.txt" ]; then
  echo "error: GLFW sources not found in $GLFW_SRC" >&2
  exit 1
fi

COMMON=(-DBUILD_SHARED_LIBS=ON -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF -DGLFW_BUILD_DOCS=OFF
        -DCMAKE_BUILD_TYPE=Release)

case "$(uname -s)" in
  Linux)
    cmake -S "$GLFW_SRC" -B "$BUILD_DIR" "${COMMON[@]}" -DGLFW_BUILD_X11=ON -DGLFW_BUILD_WAYLAND=ON
    cmake --build "$BUILD_DIR" --parallel
    mkdir -p "$LIB_DIR/linux-x86-64"
    cp "$BUILD_DIR/src/libglfw.so.3."* "$LIB_DIR/linux-x86-64/libglfw.so.3"
    strip --strip-unneeded "$LIB_DIR/linux-x86-64/libglfw.so.3"
    OUT="$LIB_DIR/linux-x86-64/libglfw.so.3"
    ;;
  Darwin)
    cmake -S "$GLFW_SRC" -B "$BUILD_DIR" "${COMMON[@]}" \
          -DCMAKE_OSX_ARCHITECTURES=arm64 -DCMAKE_OSX_DEPLOYMENT_TARGET=11.0
    cmake --build "$BUILD_DIR" --parallel
    mkdir -p "$LIB_DIR/macos"
    cp "$BUILD_DIR/src/libglfw.3."*.dylib "$LIB_DIR/macos/libglfw.3.dylib"
    OUT="$LIB_DIR/macos/libglfw.3.dylib"
    ;;
  MINGW64*|MSYS*)
    # NOTE: Static libgcc so the DLL only depends on Windows system libraries
    cmake -S "$GLFW_SRC" -B "$BUILD_DIR" -G "MSYS Makefiles" "${COMMON[@]}" \
          -DCMAKE_SHARED_LINKER_FLAGS="-static-libgcc"
    cmake --build "$BUILD_DIR" --parallel
    mkdir -p "$LIB_DIR/windows-x86-64"
    cp "$BUILD_DIR/src/glfw3.dll" "$LIB_DIR/windows-x86-64/glfw3.dll"
    strip --strip-unneeded "$LIB_DIR/windows-x86-64/glfw3.dll"
    OUT="$LIB_DIR/windows-x86-64/glfw3.dll"
    ;;
  *)
    echo "error: unsupported system $(uname -s)" >&2
    exit 1
    ;;
esac

echo "built $OUT"

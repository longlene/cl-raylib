# cl-raylib API Completeness Report
Generated: /home/loong0/.quicklisp/local-projects/cl-raylib

## Summary
- **C raylib RLAPI functions**: 546
- **cl-raylib exported symbols**: 1391
- **Potentially missing functions**: 209

## Missing Functions by Category

### 2D Drawing (33 functions)

- `DrawCircle3D` → `draw-circle3-d`
- `DrawLine3D` → `draw-line3-d`
- `DrawLineDashed` → `draw-line-dashed`
- `DrawSplineBasis` → `draw-spline-basis`
- `DrawSplineBezierCubic` → `draw-spline-bezier-cubic`
- `DrawSplineBezierQuadratic` → `draw-spline-bezier-quadratic`
- `DrawSplineCatmullRom` → `draw-spline-catmull-rom`
- `DrawSplineLinear` → `draw-spline-linear`
- `DrawSplineSegmentBasis` → `draw-spline-segment-basis`
- `DrawSplineSegmentBezierCubic` → `draw-spline-segment-bezier-cubic`
- `DrawSplineSegmentBezierQuadratic` → `draw-spline-segment-bezier-quadratic`
- `DrawSplineSegmentCatmullRom` → `draw-spline-segment-catmull-rom`
- `DrawSplineSegmentLinear` → `draw-spline-segment-linear`
- `DrawTriangle3D` → `draw-triangle3-d`
- `DrawTriangleStrip3D` → `draw-triangle-strip3-d`
- `ImageDrawCircle` → `image-draw-circle`
- `ImageDrawCircleLines` → `image-draw-circle-lines`
- `ImageDrawCircleLinesV` → `image-draw-circle-lines-v`
- `ImageDrawCircleV` → `image-draw-circle-v`
- `ImageDrawLine` → `image-draw-line`
- `ImageDrawLineEx` → `image-draw-line-ex`
- `ImageDrawLineV` → `image-draw-line-v`
- `ImageDrawRectangle` → `image-draw-rectangle`
- `ImageDrawRectangleLines` → `image-draw-rectangle-lines`
- `ImageDrawRectangleRec` → `image-draw-rectangle-rec`
- `ImageDrawRectangleV` → `image-draw-rectangle-v`
- `ImageDrawText` → `image-draw-text`
- `ImageDrawTextEx` → `image-draw-text-ex`
- `ImageDrawTriangle` → `image-draw-triangle`
- `ImageDrawTriangleEx` → `image-draw-triangle-ex`
- `ImageDrawTriangleFan` → `image-draw-triangle-fan`
- `ImageDrawTriangleLines` → `image-draw-triangle-lines`
- `ImageDrawTriangleStrip` → `image-draw-triangle-strip`

### 3D Drawing (3 functions)

- `DrawCylinderEx` → `draw-cylinder-ex`
- `DrawCylinderWiresEx` → `draw-cylinder-wires-ex`
- `DrawModelPointsEx` → `draw-model-points-ex`

### Audio Streaming (5 functions)

- `AttachAudioMixedProcessor` → `attach-audio-mixed-processor`
- `AttachAudioStreamProcessor` → `attach-audio-stream-processor`
- `DetachAudioMixedProcessor` → `detach-audio-mixed-processor`
- `DetachAudioStreamProcessor` → `detach-audio-stream-processor`
- `SetAudioStreamBufferSizeDefault` → `set-audio-stream-buffer-size-default`

### Automation (8 functions)

- `ExportAutomationEventList` → `export-automation-event-list`
- `LoadAutomationEventList` → `load-automation-event-list`
- `PlayAutomationEvent` → `play-automation-event`
- `SetAutomationEventBaseFrame` → `set-automation-event-base-frame`
- `SetAutomationEventList` → `set-automation-event-list`
- `StartAutomationEventRecording` → `start-automation-event-recording`
- `StopAutomationEventRecording` → `stop-automation-event-recording`
- `UnloadAutomationEventList` → `unload-automation-event-list`

### Camera (1 functions)

- `GetCameraMatrix2D` → `get-camera-matrix2-d`

### Camera System (1 functions)

- `UpdateCameraPro` → `update-camera-pro`

### Collision Detection (9 functions)

- `CheckCollisionBoxSphere` → `check-collision-box-sphere`
- `CheckCollisionCircleLine` → `check-collision-circle-line`
- `CheckCollisionCircleRec` → `check-collision-circle-rec`
- `CheckCollisionLines` → `check-collision-lines`
- `CheckCollisionPointLine` → `check-collision-point-line`
- `CheckCollisionPointPoly` → `check-collision-point-poly`
- `GetRayCollisionMesh` → `get-ray-collision-mesh`
- `GetRayCollisionQuad` → `get-ray-collision-quad`
- `GetRayCollisionTriangle` → `get-ray-collision-triangle`

### Color Utilities (7 functions)

- `ColorBrightness` → `color-brightness`
- `ColorContrast` → `color-contrast`
- `ColorFromNormalized` → `color-from-normalized`
- `ColorIsEqual` → `color-is-equal`
- `ColorTint` → `color-tint`
- `ColorToInt` → `color-to-int`
- `GetColor` → `get-color`

### Cursor Management (1 functions)

- `IsCursorOnScreen` → `is-cursor-on-screen`

### Drawing Control (8 functions)

- `BeginBlendMode` → `begin-blend-mode`
- `BeginMode2D` → `begin-mode2-d`
- `BeginMode3D` → `begin-mode3-d`
- `BeginVrStereoMode` → `begin-vr-stereo-mode`
- `EndBlendMode` → `end-blend-mode`
- `EndMode2D` → `end-mode2-d`
- `EndMode3D` → `end-mode3-d`
- `EndVrStereoMode` → `end-vr-stereo-mode`

### File I/O (4 functions)

- `FileExists` → `file-exists`
- `GetFileModTime` → `get-file-mod-time`
- `IsFileExtension` → `is-file-extension`
- `UnloadFileText` → `unload-file-text`

### Gamepad Input (2 functions)

- `GetGamepadButtonPressed` → `get-gamepad-button-pressed`
- `SetGamepadMappings` → `set-gamepad-mappings`

### Gesture Input (8 functions)

- `GetGestureDetected` → `get-gesture-detected`
- `GetGestureDragAngle` → `get-gesture-drag-angle`
- `GetGestureDragVector` → `get-gesture-drag-vector`
- `GetGestureHoldDuration` → `get-gesture-hold-duration`
- `GetGesturePinchAngle` → `get-gesture-pinch-angle`
- `GetGesturePinchVector` → `get-gesture-pinch-vector`
- `IsGestureDetected` → `is-gesture-detected`
- `SetGesturesEnabled` → `set-gestures-enabled`

### Image Processing (35 functions)

- `ExportImage` → `export-image`
- `ExportImageAsCode` → `export-image-as-code`
- `GenImageText` → `gen-image-text`
- `GetImageAlphaBorder` → `get-image-alpha-border`
- `GetImageColor` → `get-image-color`
- `ImageAlphaClear` → `image-alpha-clear`
- `ImageAlphaCrop` → `image-alpha-crop`
- `ImageAlphaMask` → `image-alpha-mask`
- `ImageAlphaPremultiply` → `image-alpha-premultiply`
- `ImageColorBrightness` → `image-color-brightness`
- `ImageColorContrast` → `image-color-contrast`
- `ImageColorReplace` → `image-color-replace`
- `ImageCrop` → `image-crop`
- `ImageDither` → `image-dither`
- `ImageDraw` → `image-draw`
- `ImageDrawPixel` → `image-draw-pixel`
- `ImageDrawPixelV` → `image-draw-pixel-v`
- `ImageFromChannel` → `image-from-channel`
- `ImageKernelConvolution` → `image-kernel-convolution`
- `ImageResize` → `image-resize`
- `ImageResizeCanvas` → `image-resize-canvas`
- `ImageResizeNN` → `image-resize-nn`
- `ImageRotate` → `image-rotate`
- `ImageRotateCCW` → `image-rotate-ccw`
- `ImageRotateCW` → `image-rotate-cw`
- `ImageText` → `image-text`
- `ImageTextEx` → `image-text-ex`
- `ImageToPOT` → `image-to-pot`
- `LoadImageAnim` → `load-image-anim`
- `LoadImageAnimFromMemory` → `load-image-anim-from-memory`
- `LoadImageFromMemory` → `load-image-from-memory`
- `LoadImageFromScreen` → `load-image-from-screen`
- `LoadImageRaw` → `load-image-raw`
- `UnloadImageColors` → `unload-image-colors`
- `UnloadImagePalette` → `unload-image-palette`

### Keyboard Input (1 functions)

- `IsKeyPressedRepeat` → `is-key-pressed-repeat`

### Material System (2 functions)

- `GenMeshHemiSphere` → `gen-mesh-hemi-sphere`
- `SetModelMeshMaterial` → `set-model-mesh-material`

### Misc Core (2 functions)

- `OpenURL` → `open-url`
- `TakeScreenshot` → `take-screenshot`

### Model Animation (3 functions)

- `IsModelAnimationValid` → `is-model-animation-valid`
- `UpdateModelAnimation` → `update-model-animation`
- `UpdateModelAnimationBones` → `update-model-animation-bones`

### Model/Mesh Management (4 functions)

- `ExportMesh` → `export-mesh`
- `ExportMeshAsCode` → `export-mesh-as-code`
- `UnloadModelAnimation` → `unload-model-animation`
- `UnloadModelAnimations` → `unload-model-animations`

### Mouse Input (2 functions)

- `SetMouseOffset` → `set-mouse-offset`
- `SetMouseScale` → `set-mouse-scale`

### Music Streaming (1 functions)

- `LoadMusicStreamFromMemory` → `load-music-stream-from-memory`

### Pixel Operations (3 functions)

- `GetPixelColor` → `get-pixel-color`
- `GetPixelDataSize` → `get-pixel-data-size`
- `SetPixelColor` → `set-pixel-color`

### Shapes Texture (3 functions)

- `GetShapesTexture` → `get-shapes-texture`
- `GetShapesTextureRectangle` → `get-shapes-texture-rectangle`
- `SetShapesTexture` → `set-shapes-texture`

### Sound Management (6 functions)

- `LoadSoundAlias` → `load-sound-alias`
- `UnloadSoundAlias` → `unload-sound-alias`
- `UnloadWaveSamples` → `unload-wave-samples`
- `WaveCopy` → `wave-copy`
- `WaveCrop` → `wave-crop`
- `WaveFormat` → `wave-format`

### Texture Management (3 functions)

- `GenTextureMipmaps` → `gen-texture-mipmaps`
- `LoadTextureCubemap` → `load-texture-cubemap`
- `UpdateTextureRec` → `update-texture-rec`

### Touch Input (5 functions)

- `GetTouchPointCount` → `get-touch-point-count`
- `GetTouchPointId` → `get-touch-point-id`
- `GetTouchPosition` → `get-touch-position`
- `GetTouchX` → `get-touch-x`
- `GetTouchY` → `get-touch-y`

### Uncategorized (30 functions)

- `DrawPoint3D` → `draw-point3-d`
- `FileCopy` → `file-copy`
- `FileMove` → `file-move`
- `FileRemove` → `file-remove`
- `FileRename` → `file-rename`
- `FileTextFindIndex` → `file-text-find-index`
- `FileTextReplace` → `file-text-replace`
- `GetSplinePointBasis` → `get-spline-point-basis`
- `GetSplinePointBezierCubic` → `get-spline-point-bezier-cubic`
- `GetSplinePointBezierQuad` → `get-spline-point-bezier-quad`
- `GetSplinePointCatmullRom` → `get-spline-point-catmull-rom`
- `GetSplinePointLinear` → `get-spline-point-linear`
- `GetWorldToScreen2D` → `get-world-to-screen2-d`
- `GetWorldToScreenEx` → `get-world-to-screen-ex`
- `ImageBlurGaussian` → `image-blur-gaussian`
- `ImageClearBackground` → `image-clear-background`
- `IsAudioStreamValid` → `is-audio-stream-valid`
- `IsFileNameValid` → `is-file-name-valid`
- `IsImageValid` → `is-image-valid`
- `MakeDirectory` → `make-directory`
- `PollInputEvents` → `poll-input-events`
- `SetAudioStreamCallback` → `set-audio-stream-callback`
- `SetGamepadVibration` → `set-gamepad-vibration`
- `SwapScreenBuffer` → `swap-screen-buffer`
- `TextIsEqual` → `text-is-equal`
- `TextToFloat` → `text-to-float`
- `UnloadCodepoints` → `unload-codepoints`
- `UnloadRandomSequence` → `unload-random-sequence`
- `UnloadTextLines` → `unload-text-lines`
- `UnloadUTF8` → `unload-utf8`

### VR (2 functions)

- `LoadVrStereoConfig` → `load-vr-stereo-config`
- `UnloadVrStereoConfig` → `unload-vr-stereo-config`

### Window Management (17 functions)

- `DisableEventWaiting` → `disable-event-waiting`
- `EnableEventWaiting` → `enable-event-waiting`
- `GetClipboardImage` → `get-clipboard-image`
- `GetMonitorHeight` → `get-monitor-height`
- `GetMonitorPhysicalHeight` → `get-monitor-physical-height`
- `GetMonitorPhysicalWidth` → `get-monitor-physical-width`
- `GetMonitorPosition` → `get-monitor-position`
- `GetMonitorRefreshRate` → `get-monitor-refresh-rate`
- `GetMonitorWidth` → `get-monitor-width`
- `GetScreenToWorld2D` → `get-screen-to-world2-d`
- `GetWindowScaleDPI` → `get-window-scale-dpi`
- `IsWindowState` → `is-window-state`
- `SetWindowFocused` → `set-window-focused`
- `SetWindowIcon` → `set-window-icon`
- `SetWindowIcons` → `set-window-icons`
- `SetWindowMonitor` → `set-window-monitor`
- `ToggleBorderlessWindowed` → `toggle-borderless-windowed`

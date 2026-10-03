(defpackage #:cl-raylib
  (:nicknames #:raylib)
  (:use #:cl
        #:3d-vectors
        #:3d-matrices)
  (:import-from #:alexandria
                #:clamp)
  (:local-nicknames
   (#:glfw #:org.shirakumo.fraf.glfw)
   (#:%glfw #:org.shirakumo.fraf.glfw.cffi))
  (:export
   ;; Re-export 3d-math functions for compatibility
   #:vec2 #:vec3 #:vec4 #:vx #:vy #:vz #:vw #:vx2 #:vy2 #:vx3 #:vy3 #:vz3 #:vx4 #:vy4 #:vz4 #:vw4
   #:vec #:vec2-p #:vec3-p #:vec4-p #:vec-p #:vcopy #:vcopy2 #:vcopy3 #:vcopy4

   ;; Matrix4 functions (re-exported from 3d-matrices)
   #:meye #:mtranslation #:mscaling #:mrotation #:mlookat
   #:mperspective #:mfrustum #:mortho #:m* #:minv #:mtranspose
   #:mdet #:mat4 #:mat4-p #:marr4 #:nm* #:mcol #:mrow

   ;; Camera2D functions
   #:camera2d #:make-camera2d #:camera2d-offset #:camera2d-target #:camera2d-rotation #:camera2d-zoom
   #:make-camera-2d #:camera2d-default #:get-camera-matrix-2d #:get-world-to-screen-ex #:get-screen-to-world-ray #:get-screen-to-world-ray-ex
   #:get-screen-to-world-2d #:get-world-to-screen-2d
   #:camera2d-set-offset #:camera2d-set-target #:camera2d-set-rotation #:camera2d-set-zoom
   #:camera2d-move #:camera2d-rotate #:camera2d-zoom-by #:camera2d-zoom-to
   #:begin-mode-2d #:end-mode-2d #:with-mode-2d
   #:camera2d-get-view-rectangle #:camera2d-follow-target #:camera2d-constrain-to-bounds
   #:camera2d-lerp #:camera2d-animate-to #:camera2d-handle-pan-input #:camera2d-handle-zoom-input
   #:camera2d-handle-rotation-input #:camera2d-fit-to-bounds #:camera2d-is-point-visible
   #:camera2d-is-rectangle-visible #:camera2d-get-info #:camera2d-draw-debug-info

   ;; Camera3D functions
   #:camera3d #:make-camera3d #:camera3d-position #:camera3d-target #:camera3d-up
   #:camera3d-fovy #:camera3d-projection #:get-camera-matrix
   #:rl-set-clip-planes #:rl-get-cull-distance-near #:rl-get-cull-distance-far
   ;; rcamera.h
   #:get-camera-forward #:get-camera-up #:get-camera-right
   #:camera-move-forward #:camera-move-up #:camera-move-right #:camera-move-to-target
   #:camera-yaw #:camera-pitch #:camera-roll
   #:get-camera-view-matrix #:get-camera-projection-matrix #:update-camera #:update-camera-pro
   #:get-world-to-screen #:begin-mode-3d #:end-mode-3d #:with-mode-3d

   ;; Ray functions
   #:ray #:make-ray #:ray-position #:ray-direction #:get-mouse-ray
   #:get-screen-to-world-ray #:get-screen-to-world-ray-ex

   ;; Ray collision functions
   #:ray-collision #:make-ray-collision #:ray-collision-hit #:ray-collision-distance
   #:ray-collision-point #:ray-collision-normal

   ;; Camera constants
   #:+camera-perspective+ #:+camera-orthographic+ #:+camera-custom+ #:+camera-free+
   #:+camera-orbital+ #:+camera-first-person+ #:+camera-third-person+

   ;; Color functions
    #:color-r #:color-g #:color-b #:color-a
   #:make-color #:color-from-hsv #:color-to-hsv
   #:color-alpha #:color-alpha-blend #:color-fade #:fade #:keyword-to-color

   ;; Predefined colors
   #:+black+ #:+white+ #:+red+ #:+green+ #:+blue+ #:+yellow+
   #:+magenta+ #:+cyan+ #:+gray+ #:+lightgray+ #:+darkgray+
   #:+raywhite+ #:+blank+ #:+maroon+ #:+orange+ #:+darkgreen+
   #:+darkblue+ #:+skyblue+ #:+purple+ #:+lime+ #:+beige+ #:+brown+ #:+gold+ #:+violet+ #:+darkpurple+

   ;; 2D drawing functions
   #:draw-pixel #:draw-pixel-v
   #:draw-line #:draw-line-v #:draw-line-ex #:draw-line-strip #:draw-line-bezier
   #:draw-circle #:draw-circle-v #:draw-circle-lines #:draw-circle-lines-v
   #:draw-circle-sector #:draw-circle-sector-lines #:draw-circle-gradient
   #:draw-ellipse #:draw-ellipse-v #:draw-ellipse-lines #:draw-ellipse-lines-v
   #:draw-ring #:draw-ring-lines
   #:draw-rectangle #:draw-rectangle-v #:draw-rectangle-rec #:draw-rectangle-pro
   #:draw-rectangle-lines #:draw-rectangle-lines-ex #:draw-rectangle-rounded
   #:draw-rectangle-rounded-lines #:draw-rectangle-rounded-lines-ex
   #:draw-rectangle-gradient-v #:draw-rectangle-gradient-h #:draw-rectangle-gradient-ex
   #:draw-triangle #:draw-triangle-lines #:draw-triangle-fan #:draw-triangle-strip
   #:draw-poly #:draw-poly-lines #:draw-poly-lines-ex
   #:set-shapes-texture #:get-shapes-texture #:get-shapes-texture-rectangle
   #:draw-line-dashed #:draw-triangle-gradient #:draw-triangle-lines-ex
   #:draw-circle-sector-lines-ex #:draw-circle-lines-ex #:draw-ellipse-lines-ex #:draw-ring-lines-ex
   ;; Splines drawing functions
   #:draw-spline-linear #:draw-spline-basis #:draw-spline-catmull-rom
   #:draw-spline-bezier-quadratic #:draw-spline-bezier-cubic
   #:draw-spline-segment-linear #:draw-spline-segment-basis #:draw-spline-segment-catmull-rom
   #:draw-spline-segment-bezier-quadratic #:draw-spline-segment-bezier-cubic
   #:get-spline-point-linear #:get-spline-point-basis #:get-spline-point-catmull-rom
   #:get-spline-point-bezier-quadratic #:get-spline-point-bezier-cubic
   ;; Basic shapes collision detection functions
   #:check-collision-point-poly #:check-collision-circle-rec #:check-collision-lines
   #:check-collision-point-line #:check-collision-circle-line

   ;; rmodels: basic geometric 3D shapes drawing functions
   #:draw-line-3d #:draw-point-3d #:draw-circle-3d #:draw-triangle-3d #:draw-triangle-strip-3d
   #:draw-cube #:draw-cube-v #:draw-cube-wires #:draw-cube-wires-v
   #:draw-sphere #:draw-sphere-ex #:draw-sphere-wires
   #:draw-cylinder #:draw-cylinder-ex #:draw-cylinder-wires #:draw-cylinder-wires-ex
   #:draw-capsule #:draw-capsule-wires #:draw-plane #:draw-ray #:draw-grid

   ;; 2D collision functions (rshapes)
   #:check-collision-point-rec #:check-collision-recs #:check-collision-point-circle
   #:check-collision-circles #:get-collision-rec #:check-collision-point-triangle

   ;; rmodels: model, mesh and material types
   #:mesh #:make-mesh #:mesh-vertex-count #:mesh-triangle-count #:mesh-vertices #:mesh-texcoords
   #:mesh-texcoords2 #:mesh-normals #:mesh-tangents #:mesh-colors #:mesh-indices #:mesh-bone-count
   #:mesh-bone-indices #:mesh-bone-weights #:mesh-anim-vertices #:mesh-anim-normals #:mesh-vao-id #:mesh-vbo-id
   #:material-map #:make-material-map #:material-map-texture #:material-map-color #:material-map-value
   #:material #:make-material #:material-shader #:material-maps #:material-params
   #:transform #:make-transform #:transform-translation #:transform-rotation #:transform-scale
   #:bone-info #:make-bone-info #:bone-info-name #:bone-info-parent
   #:model-skeleton #:make-model-skeleton #:model-skeleton-bone-count #:model-skeleton-bones #:model-skeleton-bind-pose
   #:model #:make-model #:model-transform #:model-mesh-count #:model-material-count #:model-meshes
   #:model-materials #:model-mesh-material #:model-skeleton #:model-current-pose #:model-bone-matrices
   #:model-animation #:make-model-animation #:model-animation-name #:model-animation-bone-count
   #:model-animation-keyframe-count #:model-animation-keyframe-poses
   #:ray #:make-ray #:ray-position #:ray-direction
   #:bounding-box #:make-bounding-box #:bounding-box-min #:bounding-box-max

   ;; rmodels: model management functions
   #:load-model #:load-model-from-mesh #:is-model-valid #:unload-model #:get-model-bounding-box

   ;; rmodels: model drawing functions
   #:draw-model #:draw-model-ex #:draw-model-wires #:draw-model-wires-ex
   #:draw-bounding-box #:draw-billboard #:draw-billboard-rec #:draw-billboard-pro

   ;; rmodels: mesh management functions
   #:upload-mesh #:update-mesh-buffer #:unload-mesh #:draw-mesh #:draw-mesh-instanced
   #:get-mesh-bounding-box #:gen-mesh-tangents #:export-mesh #:export-mesh-as-code

   ;; rmodels: mesh generation functions
   #:gen-mesh-poly #:gen-mesh-plane #:gen-mesh-cube #:gen-mesh-sphere #:gen-mesh-hemi-sphere
   #:gen-mesh-cylinder #:gen-mesh-cone #:gen-mesh-torus #:gen-mesh-knot
   #:gen-mesh-heightmap #:gen-mesh-cubicmap

   ;; rmodels: material loading/unloading functions
   #:load-materials #:load-material-default #:is-material-valid #:unload-material
   #:set-material-texture #:set-model-mesh-material

   ;; rmodels: model animations loading/unloading functions
   #:load-model-animations #:update-model-animation #:update-model-animation-ex
   #:unload-model-animations #:is-model-animation-valid

   ;; rmodels: collision detection functions
   #:check-collision-spheres #:check-collision-boxes #:check-collision-box-sphere
   #:get-ray-collision-sphere #:get-ray-collision-box #:get-ray-collision-mesh
   #:get-ray-collision-triangle #:get-ray-collision-quad

   ;; Material map index
   #:+material-map-albedo+ #:+material-map-metalness+ #:+material-map-normal+ #:+material-map-roughness+
   #:+material-map-occlusion+ #:+material-map-emission+ #:+material-map-height+ #:+material-map-cubemap+
   #:+material-map-irradiance+ #:+material-map-prefilter+ #:+material-map-brdf+
   #:+material-map-diffuse+ #:+material-map-specular+ #:+max-material-maps+

   ;; Image/Texture structures
   #:image #:make-image #:image-data #:image-width #:image-height
   #:image-mipmaps #:image-format
   #:texture #:make-texture #:texture-id #:texture-width #:texture-height
   #:texture-mipmaps #:texture-format
   #:rectangle #:make-rectangle #:rectangle-x #:rectangle-y
   #:rectangle-width #:rectangle-height
   #:render-texture #:make-render-texture

   ;; Image loading functions
   #:load-image #:load-image-raw #:load-image-anim #:load-image-anim-from-memory
   #:load-image-from-memory #:load-image-from-screen
   #:export-image #:export-image-to-memory #:export-image-as-code
   #:load-texture-cubemap #:load-render-texture-ex #:update-texture-rec #:gen-texture-mipmaps
   #:+npatch-nine-patch+ #:+npatch-three-patch-vertical+ #:+npatch-three-patch-horizontal+

   ;; Image generation functions
   #:gen-image-color #:gen-image-gradient-linear #:gen-image-gradient-radial
   #:gen-image-checked #:gen-image-white-noise #:gen-image-perlin-noise
   #:gen-image-cellular #:gen-image-gradient-square

   ;; Image manipulation functions
   #:image-copy #:image-from-image
   #:image-color-tint #:image-color-grayscale #:image-color-invert
   #:image-flip-vertical #:image-flip-horizontal
   #:unload-image
   #:is-image-valid #:gen-image-text #:image-crop #:image-format #:image-text #:image-text-ex
   #:image-from-channel #:image-resize #:image-resize-nn #:image-resize-canvas #:image-to-pot
   #:image-alpha-crop #:image-alpha-clear #:image-alpha-mask #:image-alpha-premultiply
   #:image-blur-gaussian #:image-kernel-convolution #:image-mipmaps #:image-dither
   #:image-rotate #:image-rotate-cw #:image-rotate-ccw
   #:image-color-contrast #:image-color-brightness #:image-color-replace
   #:load-image-colors #:load-image-palette #:unload-image-colors #:unload-image-palette
   #:get-image-alpha-border #:get-image-color

   ;; Image drawing functions
   #:image-clear-background #:image-draw-pixel #:image-draw-pixel-v
   #:image-draw-line #:image-draw-line-v #:image-draw-line-ex #:image-draw-line-strip
   #:image-draw-triangle #:image-draw-triangle-gradient #:image-draw-triangle-lines
   #:image-draw-triangle-fan #:image-draw-triangle-strip
   #:image-draw-rectangle #:image-draw-rectangle-v #:image-draw-rectangle-rec #:image-draw-rectangle-pro
   #:image-draw-rectangle-lines #:image-draw-rectangle-lines-ex #:image-draw-rectangle-gradient-ex
   #:image-draw-circle #:image-draw-circle-v #:image-draw-circle-lines #:image-draw-circle-lines-v
   #:image-draw-circle-gradient
   #:image-draw-image #:image-draw-image-ex #:image-draw-image-rec #:image-draw-image-pro
   #:image-draw-text #:image-draw-text-ex #:image-draw-text-pro

   ;; Color/pixel related functions
   #:color-is-equal #:color-to-int #:color-normalize #:color-from-normalized #:color-tint
   #:color-brightness #:color-contrast #:color-lerp #:get-color
   #:get-pixel-color #:set-pixel-color #:get-pixel-data-size
   #:+pink+ #:+darkbrown+

   ;; Pixel formats
   #:+pixelformat-uncompressed-grayscale+ #:+pixelformat-uncompressed-gray-alpha+
   #:+pixelformat-uncompressed-r5g6b5+ #:+pixelformat-uncompressed-r8g8b8+
   #:+pixelformat-uncompressed-r5g5b5a1+ #:+pixelformat-uncompressed-r4g4b4a4+
   #:+pixelformat-uncompressed-r8g8b8a8+ #:+pixelformat-uncompressed-r32+
   #:+pixelformat-uncompressed-r32g32b32+ #:+pixelformat-uncompressed-r32g32b32a32+
   #:+pixelformat-uncompressed-r16+ #:+pixelformat-uncompressed-r16g16b16+
   #:+pixelformat-uncompressed-r16g16b16a16+
   #:+pixelformat-compressed-dxt1-rgb+ #:+pixelformat-compressed-dxt1-rgba+
   #:+pixelformat-compressed-dxt3-rgba+ #:+pixelformat-compressed-dxt5-rgba+
   #:+pixelformat-compressed-etc1-rgb+ #:+pixelformat-compressed-etc2-rgb+
   #:+pixelformat-compressed-etc2-eac-rgba+ #:+pixelformat-compressed-pvrt-rgb+
   #:+pixelformat-compressed-pvrt-rgba+ #:+pixelformat-compressed-astc-4x4-rgba+
   #:+pixelformat-compressed-astc-8x8-rgba+ #:+pixelformat-uncompressed-rgba+

   ;; GPU Texture functions
   #:load-texture-from-image #:load-texture
   #:is-texture-valid #:unload-texture #:update-texture
   #:set-texture-filter #:set-texture-wrap
   #:draw-texture #:draw-texture-v #:draw-texture-ex #:draw-texture-rec #:draw-texture-pro
   #:draw-texture-npatch #:get-texture-data #:get-texture-format
   #:load-image-from-texture

   ;; Render Texture functions
   #:load-render-texture #:is-render-texture-valid #:unload-render-texture
   #:begin-texture-mode #:end-texture-mode #:with-texture-mode
   #:render-texture #:render-texture-id #:render-texture-texture #:render-texture-depth

   ;; Texture constants
   #:+texture-filter-point+ #:+texture-filter-bilinear+ #:+texture-filter-trilinear+
   #:+texture-filter-anisotropic-4x+ #:+texture-filter-anisotropic-8x+ #:+texture-filter-anisotropic-16x+
   #:+texture-wrap-repeat+ #:+texture-wrap-clamp+ #:+texture-wrap-mirror-repeat+ #:+texture-wrap-mirror-clamp+
   #:+cubemap-layout-auto-detect+ #:+cubemap-layout-line-vertical+ #:+cubemap-layout-line-horizontal+
   #:+cubemap-layout-cross-three-by-four+ #:+cubemap-layout-cross-four-by-three+

   ;; NPatch info
   #:npatch-info #:make-npatch-info #:npatch-info-source #:npatch-info-left
   #:npatch-info-top #:npatch-info-right #:npatch-info-bottom #:npatch-info-layout

   ;; Window management functions
   #:set-config-flags #:init-window #:close-window #:window-should-close #:set-target-fps
   #:begin-drawing #:end-drawing #:with-drawing #:clear-background
   #:begin-scissor-mode #:end-scissor-mode #:begin-blend-mode #:end-blend-mode
   #:is-window-state #:toggle-borderless-windowed #:set-window-icon #:set-window-icons
   #:set-window-monitor #:set-window-focused #:get-window-handle #:get-monitor-position
   #:get-monitor-physical-width #:get-monitor-physical-height #:get-monitor-refresh-rate
   #:get-monitor-name #:get-window-scale-dpi #:get-clipboard-image #:enable-event-waiting
   #:disable-event-waiting #:is-cursor-on-screen #:take-screenshot #:is-key-pressed-repeat
   #:get-key-name #:get-gamepad-button-pressed #:set-mouse-offset #:set-mouse-scale
   #:get-touch-x #:get-touch-y
   ;; Automation events
   #:automation-event #:make-automation-event #:automation-event-frame #:automation-event-type
   #:automation-event-params #:automation-event-list #:make-automation-event-list
   #:automation-event-list-capacity #:automation-event-list-count #:automation-event-list-events
   #:load-automation-event-list #:unload-automation-event-list #:export-automation-event-list
   #:set-automation-event-list #:set-automation-event-base-frame #:start-automation-event-recording
   #:stop-automation-event-recording #:play-automation-event
   #:rl-read-screen-pixels #:rl-read-texture-pixels #:rl-set-blend-factors #:rl-set-blend-factors-separate
   #:+blend-alpha+ #:+blend-additive+ #:+blend-multiplied+ #:+blend-add-colors+
   #:+blend-subtract-colors+ #:+blend-alpha-premultiply+ #:+blend-custom+ #:+blend-custom-separate+
   #:get-fps #:draw-fps #:trace-log-warning
   #:with-window #:is-window-ready #:is-window-fullscreen #:is-window-hidden
   #:is-window-minimized #:is-window-maximized #:is-window-focused #:is-window-resized
   #:set-window-state #:clear-window-state #:toggle-fullscreen
   #:maximize-window #:minimize-window #:restore-window
   #:set-window-title #:set-window-position #:get-window-position
   #:set-window-size #:set-window-min-size #:set-window-max-size

   #:set-window-opacity
   #:disable-cursor #:enable-cursor #:hide-cursor #:show-cursor #:is-cursor-hidden

   ;; File drop functions
   #:file-path-list #:make-file-path-list #:file-path-list-count #:file-path-list-paths
   #:is-file-dropped #:load-dropped-files #:unload-dropped-files #:file-path-list-path
   #:get-screen-width #:get-screen-height #:get-render-width #:get-render-height
   #:get-monitor-count #:get-current-monitor
   #:set-clipboard-text #:get-clipboard-text
   #:get-monitor-width #:get-monitor-height #:swap-screen-buffer #:poll-input-events #:open-url
   #:unload-file-text #:set-gamepad-mappings #:set-gamepad-vibration
   #:get-touch-position #:get-touch-point-id #:get-touch-point-count
   #:set-gestures-enabled #:is-gesture-detected #:get-gesture-detected #:get-gesture-hold-duration
   #:get-gesture-drag-vector #:get-gesture-drag-angle #:get-gesture-pinch-vector #:get-gesture-pinch-angle
   #:unload-codepoints #:text-is-equal #:text-to-float

   ;; Window flags
   #:+flag-window-resizable+ #:+flag-window-undecorated+ #:+flag-window-hidden+
   #:+flag-window-minimized+ #:+flag-window-maximized+ #:+flag-window-unfocused+
   #:+flag-window-topmost+ #:+flag-window-always-run+ #:+flag-window-transparent+ #:+flag-fullscreen-mode+
   #:+flag-window-highdpi+ #:+flag-window-mouse-passthrough+ #:+flag-window-borderless-windowed-mode+
   #:+flag-vsync-hint+ #:+flag-msaa-4x-hint+ #:+flag-interlaced-hint+

   ;; Input functions
 #:is-key-pressed #:is-key-down #:is-key-released #:is-key-up
   #:get-key-pressed #:get-char-pressed #:set-exit-key
   #:is-mouse-button-pressed #:is-mouse-button-down #:is-mouse-button-released #:is-mouse-button-up
   #:get-mouse-position #:get-mouse-x #:get-mouse-y #:set-mouse-position
   #:get-mouse-delta #:get-mouse-wheel-move #:get-mouse-wheel-move-v
   #:set-mouse-cursor #:keyword-to-key #:keyword-to-mouse-button

   ;; Gamepad functions
   #:is-gamepad-available #:get-gamepad-name
   #:is-gamepad-button-pressed #:is-gamepad-button-down #:is-gamepad-button-released #:is-gamepad-button-up
   #:get-gamepad-axis-count #:get-gamepad-axis-movement

   ;; Key constants
   #:+key-null+ #:+key-space+ #:+key-escape+ #:+key-enter+ #:+key-tab+ #:+key-backspace+
   #:+key-insert+ #:+key-delete+ #:+key-right+ #:+key-left+ #:+key-down+ #:+key-up+
   #:+key-apostrophe+ #:+key-comma+ #:+key-minus+ #:+key-period+ #:+key-slash+
   #:+key-zero+ #:+key-one+ #:+key-two+ #:+key-three+ #:+key-four+ #:+key-five+
   #:+key-six+ #:+key-seven+ #:+key-eight+ #:+key-nine+ #:+key-semicolon+ #:+key-equal+
   #:+key-page-up+ #:+key-page-down+ #:+key-home+ #:+key-end+ #:+key-caps-lock+
   #:+key-scroll-lock+ #:+key-num-lock+ #:+key-print-screen+ #:+key-pause+
   #:+key-left-super+ #:+key-right-shift+ #:+key-right-control+ #:+key-right-alt+ #:+key-right-super+
   #:+mouse-cursor-default+ #:+mouse-cursor-arrow+ #:+mouse-cursor-ibeam+ #:+mouse-cursor-crosshair+
   #:+mouse-cursor-pointing-hand+ #:+mouse-cursor-resize-ew+ #:+mouse-cursor-resize-ns+
   #:+mouse-cursor-resize-nwse+ #:+mouse-cursor-resize-nesw+ #:+mouse-cursor-resize-all+
   #:+mouse-cursor-not-allowed+
   #:+key-a+ #:+key-b+ #:+key-c+ #:+key-d+ #:+key-e+ #:+key-f+ #:+key-g+ #:+key-h+
   #:+key-i+ #:+key-j+ #:+key-k+ #:+key-l+ #:+key-m+ #:+key-n+ #:+key-o+ #:+key-p+
   #:+key-q+ #:+key-r+ #:+key-s+ #:+key-t+ #:+key-u+ #:+key-v+ #:+key-w+ #:+key-x+
   #:+key-y+ #:+key-z+ #:+key-f1+ #:+key-f2+ #:+key-f3+ #:+key-f4+ #:+key-f5+
   #:+key-f6+ #:+key-f7+ #:+key-f8+ #:+key-f9+ #:+key-f10+ #:+key-f11+ #:+key-f12+
   #:+key-left-shift+ #:+key-left-control+ #:+key-left-alt+

   ;; Mouse constants
   #:+mouse-button-left+ #:+mouse-button-right+ #:+mouse-button-middle+
   #:+mouse-button-side+ #:+mouse-button-extra+ #:+mouse-button-forward+
   #:+mouse-button-back+
   ;; Math constants and utilities
   #:+pi+ #:+deg2rad+ #:+rad2deg+ #:+epsilon+ #:clamp
   #:degrees-to-radians #:radians-to-degrees #:lerp #:clamp-angle

   ;; Core math utility functions
   #:float-equals #:lerp #:normalize #:remap #:wrap

   ;; Vector2 math functions
   #:vector2-zero #:vector2-one #:vector2-add #:vector2-subtract #:vector2-scale
   #:vector2-multiply #:vector2-negate #:vector2-divide #:vector2-normalize
   #:vector2-length #:vector2-length-sqr #:vector2-dot-product #:vector2-distance
   #:vector2-distance-sqr #:vector2-lerp #:vector2-min #:vector2-max #:vector2-clamp
   #:vector2-add-value #:vector2-subtract-value #:vector2-cross-product #:vector2-angle
   #:vector2-line-angle #:vector2-reflect #:vector2-rotate #:vector2-move-towards
   #:vector2-invert #:vector2-clamp-value #:vector2-equals #:vector2-transform
   #:vector2-refract

   ;; Vector3 math functions
   #:vector3-zero #:vector3-one #:vector3-add #:vector3-subtract #:vector3-scale
   #:vector3-cross-product #:vector3-length #:vector3-length-sqr #:vector3-dot-product
   #:vector3-distance #:vector3-distance-sqr #:vector3-angle #:vector3-negate
   #:vector3-normalize #:vector3-lerp #:vector3-min #:vector3-max #:vector3-clamp
   #:vector3-add-value #:vector3-subtract-value #:vector3-multiply #:vector3-divide
   #:vector3-perpendicular #:vector3-project #:vector3-reject #:vector3-ortho-normalize
   #:vector3-transform #:vector3-rotate-by-quaternion #:vector3-rotate-by-axis-angle
   #:vector3-reflect #:vector3-barycenter #:vector3-unproject #:vector3-invert
   #:vector3-clamp-value #:vector3-equals #:vector3-move-towards #:vector3-cubic-hermite
   #:vector3-to-float-v #:vector3-refract

   ;; Vector4 math functions
   #:vector4-zero #:vector4-one #:vector4-add #:vector4-add-value #:vector4-subtract
   #:vector4-subtract-value #:vector4-length #:vector4-length-sqr #:vector4-dot-product
   #:vector4-distance #:vector4-distance-sqr #:vector4-scale #:vector4-multiply
   #:vector4-negate #:vector4-divide #:vector4-normalize #:vector4-min #:vector4-max
   #:vector4-lerp #:vector4-move-towards #:vector4-invert #:vector4-equals

   ;; Matrix math functions
   #:matrix-determinant #:matrix-trace #:matrix-transpose #:matrix-invert
   #:matrix-identity #:matrix-add #:matrix-subtract #:matrix-multiply
   #:matrix-translate #:matrix-rotate #:matrix-rotate-x #:matrix-rotate-y
   #:matrix-rotate-z #:matrix-rotate-xyz #:matrix-rotate-zyx #:matrix-scale
   #:matrix-frustum #:matrix-perspective #:matrix-ortho #:matrix-look-at
   #:matrix-to-float-v #:matrix-multiply-value #:matrix-compose #:matrix-decompose

   ;; Quaternion math functions
   #:quaternion-identity #:quaternion-length #:quaternion-normalize #:quaternion-invert
   #:quaternion-multiply #:quaternion-divide #:quaternion-lerp #:quaternion-nlerp
   #:quaternion-slerp #:quaternion-from-matrix #:quaternion-to-matrix
   #:quaternion-from-axis-angle #:quaternion-to-axis-angle #:quaternion-equals
   #:quaternion-scale #:quaternion-from-vector3-to-vector3 #:quaternion-from-euler
   #:quaternion-to-euler #:quaternion-transform #:quaternion-add #:quaternion-add-value
   #:quaternion-subtract #:quaternion-subtract-value #:quaternion-cubic-hermite-spline

   ;; Re-export 3d-math symbols
   #:vec #:vx #:vy #:vz #:v+ #:v- #:v* #:vunit #:vc #:vscale
   #:vx2 #:vy2   #:vx3 #:vy3 #:vz3  #:vx4 #:vy4 #:vz4 #:vw4

   ;; Timing system functions
   #:get-time #:get-frame-time #:get-fps #:set-target-fps
 #:wait-time
   #:performance-timer #:create-timer #:start-timer #:stop-timer #:get-timer-elapsed
   #:with-timer #:time-execution
   #:get-timing-info #:reset-timing #:init-timer

   ;; Logging system functions (now in utils.lisp)
   #:set-trace-log-level #:get-trace-log-level #:set-trace-log-callback #:trace-log
   #:trace-log-trace #:trace-log-debug #:trace-log-info #:trace-log-warning
   #:trace-log-error #:trace-log-fatal #:enable-file-logging #:disable-file-logging
   #:set-log-colors-enabled #:set-log-timestamp-enabled #:with-log-context
   #:+log-all+ #:+log-trace+ #:+log-debug+ #:+log-info+ #:+log-warning+ #:+log-error+ #:+log-fatal+ #:+log-none+
   #:default-trace-log #:format-timestamp #:set-log-output-stream #:log-context
   #:make-log-context #:create-log-context #:context-trace-log #:enable-performance-logging
   #:log-performance #:log-and-continue #:log-and-abort #:with-error-logging
   #:log-system-info #:cleanup-logging-system #:init-logging-system

   ;; Utility functions (utils.lisp) - Memory management and math utilities
   #:mem-alloc #:mem-realloc #:mem-free #:clamp

   ;; File I/O system functions
   #:directory-exists #:get-file-length #:get-file-extension #:is-file-extension
   #:file-exists #:is-file-hidden #:get-file-mod-time #:is-path-directory #:is-path-absolute
   #:is-file-name-valid #:make-directory #:file-rename #:file-remove #:file-copy #:file-move
   #:file-text-replace #:file-text-find-index #:get-directory-file-count #:get-directory-file-count-ex
   #:load-random-sequence #:unload-random-sequence
   #:compute-crc32 #:compute-md5 #:compute-sha1 #:compute-sha256 #:get-file-name
   #:get-file-name-without-ext #:get-directory-path #:get-working-directory #:change-directory
   #:load-file-data #:save-file-data #:unload-file-data #:load-file-text #:save-file-text

   #:get-prev-directory-path #:get-application-directory #:export-data-as-code
   #:load-directory-files #:load-directory-files-ex #:unload-directory-files
   #:is-path-file #:set-load-file-data-callback #:set-save-file-data-callback
   #:set-load-file-text-callback #:set-save-file-text-callback
   #:compress-data #:decompress-data #:encode-data-base64 #:decode-data-base64

   ;; Random system functions
   #:set-random-seed #:get-random-value

   ;; Font and glyph structures
   #:font #:make-font #:font-p #:font-base-size #:font-glyph-count #:font-glyph-padding
   #:font-texture #:font-recs #:font-glyphs
   #:glyph-info #:make-glyph-info #:glyph-info-p #:glyph-info-value #:glyph-info-offset-x
   #:glyph-info-offset-y #:glyph-info-advance-x #:glyph-info-image

   ;; Font management functions
   #:get-font-default #:is-font-valid #:load-font-default #:unload-font-default
 #:load-font #:load-font-ex #:load-font-from-image
   #:load-font-from-memory #:unload-font

   ;; Glyph functions
   #:get-glyph-index #:get-glyph-info #:get-glyph-atlas-rec

   ;; Text drawing functions
   #:draw-text #:draw-text-ex #:draw-text-pro #:draw-text-codepoint #:draw-text-codepoints
   #:draw-fps

   ;; Text measurement functions
   #:measure-text #:measure-text-ex #:measure-text-codepoints #:set-text-line-spacing
   #:+font-default+ #:+font-bitmap+ #:+font-sdf+
   #:text-remove-spaces #:get-text-between #:text-replace-alloc #:text-replace-between
   #:text-replace-between-alloc #:text-insert-alloc #:load-text-lines #:unload-text-lines
   #:load-utf8 #:unload-utf8 #:codepoint-to-utf8 #:load-codepoints

   ;; Font atlas functions
   #:gen-image-font-atlas #:load-font-data #:unload-font-data #:export-font-as-code

   ;; Font atlas system
   #:gen-image-font-atlas

   ;; Advanced text rendering
   #:color-lerp

   ;; Text manipulation functions (rtext.c)
   #:text-length #:text-subtext #:text-to-upper #:text-to-lower #:text-replace
   #:text-to-integer #:text-copy #:text-insert #:text-join #:text-split #:text-append
   #:text-find-index #:text-to-pascal #:text-to-snake #:text-to-camel #:text-format

   ;; Unicode and codepoint functions
   #:get-codepoint #:get-codepoint-next #:get-codepoint-previous #:get-codepoint-count
   #:codepoint-to-utf8 #:load-codepoints

   ;; Text line spacing
   #:set-text-line-spacing

   ;; Audio data structures
   #:wave #:sound #:music #:audio-stream
   #:make-wave #:make-sound #:make-music #:make-audio-stream
   #:wave-frame-count #:wave-sample-rate #:wave-sample-size #:wave-channels #:wave-data
   #:sound-stream #:sound-frame-count
   #:music-stream #:music-frame-count #:music-looping #:music-ctx-type #:music-ctx-data
   #:audio-stream-buffer #:audio-stream-processor #:audio-stream-sample-rate
   #:audio-stream-sample-size #:audio-stream-channels

   ;; Audio device management (raudio.c)
   #:init-audio-device #:close-audio-device #:is-audio-device-ready
   #:set-master-volume #:get-master-volume

   ;; Wave/Sound loading and management
   #:load-wave #:load-wave-from-memory #:is-wave-valid #:unload-wave
   #:load-sound #:load-sound-from-wave #:load-sound-alias #:is-sound-valid #:update-sound
   #:unload-sound #:unload-sound-alias
   #:export-wave #:export-wave-as-code

   ;; Wave/Sound management
   #:play-sound #:stop-sound #:pause-sound #:resume-sound #:is-sound-playing
   #:set-sound-volume #:set-sound-pitch #:set-sound-pan
   #:wave-copy #:wave-crop #:wave-format #:load-wave-samples #:unload-wave-samples

   ;; Music management
   #:load-music-stream #:load-music-stream-from-memory #:is-music-valid #:unload-music-stream
   #:play-music-stream #:is-music-stream-playing #:update-music-stream
   #:stop-music-stream #:pause-music-stream #:resume-music-stream
   #:seek-music-stream #:set-music-volume #:set-music-pitch #:set-music-pan
   #:get-music-time-length #:get-music-time-played

   ;; AudioStream management
   #:load-audio-stream #:is-audio-stream-valid #:unload-audio-stream
   #:update-audio-stream #:is-audio-stream-processed
   #:play-audio-stream #:pause-audio-stream #:resume-audio-stream
   #:is-audio-stream-playing #:stop-audio-stream
   #:set-audio-stream-volume #:set-audio-stream-pitch #:set-audio-stream-pan
   #:set-audio-stream-buffer-size-default #:set-audio-stream-callback
   #:attach-audio-stream-processor #:detach-audio-stream-processor
   #:attach-audio-mixed-processor #:detach-audio-mixed-processor

   ;; Compression system
    #:compress-data #:decompress-data

   ;; Shader system
   #:shader #:make-shader #:shader-id #:shader-locs
   #:load-shader #:load-shader-from-memory #:unload-shader
   #:get-shader-location #:get-shader-location-attrib
   #:set-shader-value #:set-shader-value-v #:set-shader-value-matrix #:set-shader-value-texture
   #:begin-shader-mode #:end-shader-mode #:with-shader-mode
   #:is-shader-valid #:text-format
   ;; VR stereo rendering
   #:vr-device-info #:make-vr-device-info #:vr-device-info-h-resolution #:vr-device-info-v-resolution
   #:vr-device-info-h-screen-size #:vr-device-info-v-screen-size #:vr-device-info-eye-to-screen-distance
   #:vr-device-info-lens-separation-distance #:vr-device-info-interpupillary-distance
   #:vr-device-info-lens-distortion-values #:vr-device-info-chroma-ab-correction
   #:vr-stereo-config #:make-vr-stereo-config #:vr-stereo-config-projection #:vr-stereo-config-view-offset
   #:vr-stereo-config-left-lens-center #:vr-stereo-config-right-lens-center
   #:vr-stereo-config-left-screen-center #:vr-stereo-config-right-screen-center
   #:vr-stereo-config-scale #:vr-stereo-config-scale-in
   #:begin-vr-stereo-mode #:end-vr-stereo-mode #:load-vr-stereo-config #:unload-vr-stereo-config

   ;; Shader location constants
   #:+shader-loc-vertex-position+ #:+shader-loc-vertex-texcoord01+ #:+shader-loc-vertex-texcoord02+
   #:+shader-loc-vertex-normal+ #:+shader-loc-vertex-tangent+ #:+shader-loc-vertex-color+
   #:+shader-loc-matrix-mvp+ #:+shader-loc-matrix-view+ #:+shader-loc-matrix-projection+
   #:+shader-loc-matrix-model+ #:+shader-loc-matrix-normal+ #:+shader-loc-vector-view+
   #:+shader-loc-color-diffuse+ #:+shader-loc-color-specular+ #:+shader-loc-color-ambient+
   #:+shader-loc-map-albedo+ #:+shader-loc-map-metalness+ #:+shader-loc-map-normal+
   #:+shader-loc-map-roughness+ #:+shader-loc-map-occlusion+ #:+shader-loc-map-emission+
   #:+shader-loc-map-height+ #:+shader-loc-map-cubemap+ #:+shader-loc-map-irradiance+
   #:+shader-loc-map-prefilter+ #:+shader-loc-map-brdf+ #:+shader-loc-vertex-boneids+
   #:+shader-loc-vertex-boneweights+ #:+shader-loc-matrix-bonetransforms+ #:+shader-loc-vertex-instancetransform+
   #:+shader-loc-map-diffuse+ #:+shader-loc-map-specular+

   ;; Shader uniform type constants
   #:+shader-uniform-float+ #:+shader-uniform-vec2+ #:+shader-uniform-vec3+ #:+shader-uniform-vec4+
   #:+shader-uniform-int+ #:+shader-uniform-ivec2+ #:+shader-uniform-ivec3+ #:+shader-uniform-ivec4+
   #:+shader-uniform-uint+ #:+shader-uniform-uivec2+ #:+shader-uniform-uivec3+ #:+shader-uniform-uivec4+
   #:+shader-uniform-sampler2d+
   #:+shader-attrib-float+ #:+shader-attrib-vec2+ #:+shader-attrib-vec3+ #:+shader-attrib-vec4+

   ;; Render texture system
   #:render-texture #:make-render-texture #:render-texture-id #:render-texture-texture #:render-texture-depth
   #:load-render-texture #:is-render-texture-valid #:unload-render-texture
   #:begin-texture-mode #:end-texture-mode #:with-texture-mode
   #:get-render-texture-texture #:get-render-texture-depth

   ;; GUI Functions (raygui)
   #:gui-enable #:gui-disable #:gui-lock #:gui-unlock #:gui-is-locked
   #:gui-set-alpha #:gui-set-state #:gui-get-state
   #:gui-button #:gui-label #:gui-checkbox #:gui-slider #:gui-progress-bar
   #:+gui-state-normal+ #:+gui-state-focused+ #:+gui-state-pressed+ #:+gui-state-disabled+))

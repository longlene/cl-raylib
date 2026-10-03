(defpackage #:cl-raylib
  (:nicknames #:raylib)
  (:use #:cl
        #:3d-vectors
        #:3d-matrices
        #:org.shirakumo.flare.quaternion
        #:org.shirakumo.flare.transform)
  (:import-from #:alexandria
                #:clamp)
  (:import-from #:cl-opengl
                #:with-pushed-matrix
                #:matrix-mode
                #:load-identity
                #:ortho
                #:load-matrix
                #:disable
                #:enable
                #:cull-face
                #:polygon-mode
                #:color
                #:vertex
                #:begin
                #:end
                #:point-size
                #:bind-texture
                #:tex-coord
                #:mult-matrix
                #:draw-elements
                #:draw-arrays
                #:gen-buffer
                #:bind-buffer
                #:buffer-data
                #:depth-func
                #:frustum
                #:gen-texture
                #:tex-parameter
                #:tex-image-2d
                #:generate-mipmap
                #:gen-framebuffer
                #:gen-renderbuffer
                #:bind-framebuffer
                #:viewport)
  (:import-from #:%gl
                #:enable-vertex-attrib-array
                #:vertex-attrib-pointer)
  (:import-from #:cl-glu
                #:look-at)
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
   
   ;; Quaternion functions
   #:quaternion-identity #:quaternion-from-axis-angle #:quaternion-from-euler
   #:quaternion-multiply #:quaternion-length #:quaternion-normalize
   #:quaternion-conjugate #:quaternion-inverse #:quaternion-to-matrix4
   #:quaternion-slerp
   
   ;; Camera2D functions
   #:camera2d #:make-camera2d #:camera2d-offset #:camera2d-target #:camera2d-rotation #:camera2d-zoom
   #:make-camera-2d #:camera2d-default #:get-camera2d-matrix
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
   #:camera3d-fovy #:camera3d-projection #:make-camera-3d #:camera3d-default
   #:camera3d-first-person #:camera3d-third-person #:get-camera-matrix
   #:get-camera-projection-matrix #:camera3d-get-forward #:camera3d-get-right
   #:camera3d-get-up #:camera3d-move-forward #:camera3d-move-right #:camera3d-move-up
   #:camera3d-rotate-yaw #:camera3d-rotate-pitch #:camera3d-rotate-roll
   #:set-camera-mode #:update-camera #:camera3d-set-position #:camera3d-set-target
   #:camera3d-set-up #:camera3d-set-fovy #:camera3d-get-view-ray
   #:get-world-to-screen #:begin-mode-3d #:end-mode-3d #:with-mode-3d
   
   ;; Ray functions
   #:ray #:make-ray #:ray-position #:ray-direction #:get-mouse-ray #:get-camera-ray
   #:get-screen-to-world-ray #:get-screen-to-world-ray-ex
   
   ;; Ray collision functions
   #:ray-collision #:make-ray-collision #:ray-collision-hit #:ray-collision-distance
   #:ray-collision-point #:ray-collision-normal
   
   ;; Camera constants
   #:+camera-perspective+ #:+camera-orthographic+ #:+camera-custom+ #:+camera-free+
   #:+camera-orbital+ #:+camera-first-person+ #:+camera-third-person+
   
   ;; Color functions
   #:color #:color-r #:color-g #:color-b #:color-a
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
   
   ;; 3D drawing functions
   #:draw-cube #:draw-cube-v #:draw-cube-wires #:draw-cube-wires-v
   #:draw-sphere #:draw-sphere-ex #:draw-sphere-wires #:draw-cylinder #:draw-cylinder-wires
   #:draw-capsule #:draw-capsule-wires #:draw-plane #:draw-grid #:draw-ray #:draw-line-3d #:draw-point-3d
   #:draw-triangle-3d #:push-matrix #:pop-matrix #:translate-3d #:rotate-3d
   #:scale-3d #:with-matrix
   
   ;; Collision functions
   #:check-collision-point-rec #:check-collision-recs #:check-collision-point-circle
   #:check-collision-circles #:get-collision-rec #:check-collision-point-triangle
   #:check-collision-point-box #:get-ray-collision-sphere #:get-ray-collision-box
   #:check-collision-circle-rectangle #:check-collision-line-rectangle
   #:check-collision-sphere-aabb3d #:check-collision-rectangle-rectangle
   #:init-collision-system #:cleanup-collision-system
   
   ;; 2D Geometry structures and functions
   #:circle #:make-circle #:circle-center #:circle-radius #:make-circle-at
   #:aabb #:make-aabb #:aabb-min #:aabb-max #:make-aabb-from-center-size
   #:get-aabb-center #:get-aabb-width #:get-aabb-height
   #:line-segment #:make-line-segment #:line-segment-start #:line-segment-end
   
   ;; 3D Geometry structures and functions
   #:sphere #:make-sphere #:sphere-center #:sphere-radius #:make-sphere-at
   #:aabb3d #:make-aabb3d #:aabb3d-min #:aabb3d-max #:make-aabb3d-from-center-size
   
   ;; 3D Model and Mesh structures
   #:vertex #:make-vertex #:vertex-position #:vertex-normal #:vertex-texcoord #:vertex-color
   #:mesh #:make-mesh #:mesh-vertices #:mesh-indices #:mesh-vertex-count #:mesh-triangle-count
   #:mesh-vbo-vertices #:mesh-vbo-indices #:mesh-vao #:mesh-uploaded
   #:material-map #:make-material-map #:material-map-texture #:material-map-color #:material-map-value
   #:material #:make-material #:material-shader #:material-maps #:material-params
   #:model #:make-model #:model-meshes #:model-materials #:model-mesh-count
   #:model-material-count #:model-transform #:model-bounding-box
   #:bounding-box #:make-bounding-box #:bounding-box-min #:bounding-box-max
   
   ;; Mesh creation and generation
   #:create-mesh #:create-vertex #:gen-mesh-cube #:gen-mesh-sphere #:gen-mesh-plane
   #:gen-mesh-poly #:gen-mesh-hemisphere #:gen-mesh-cylinder #:gen-mesh-cone #:gen-mesh-torus
   #:gen-mesh-knot #:gen-mesh-heightmap #:gen-mesh-cubicmap
   #:calculate-mesh-bounds #:mesh-calculate-normals #:mesh-transform
   #:create-model #:load-model-from-mesh #:create-material #:load-material-default
   #:set-material-texture #:get-material-texture #:set-material-color #:get-material-color
   #:is-material-valid #:unload-material #:material-set-texture
   
   ;; Mesh GPU functions
   #:upload-mesh #:unload-mesh #:is-mesh-valid #:is-model-valid #:cleanup-models
   
   ;; OBJ file loading
   #:load-model-obj #:export-mesh-obj #:load-model-cube #:load-model-sphere
   #:load-model-plane #:get-model-info #:get-mesh-info
   
   ;; Model rendering functions
   #:draw-model #:draw-model-ex #:draw-model-wires #:draw-model-wires-ex
   #:draw-mesh #:draw-mesh-instanced #:update-mesh-buffer #:get-mesh-bounding-box
   #:gen-mesh-tangents #:draw-cube-model #:draw-sphere-model
   #:draw-plane-model #:draw-model-billboard #:draw-model-points
   
   ;; Billboard drawing functions
   #:draw-billboard #:draw-billboard-rec #:draw-billboard-pro
   
   ;; Model loading and management
   #:load-model #:unload-model #:load-model-from-mesh
   
   ;; Model transformations
   #:transform-model #:scale-model #:translate-model #:rotate-model
   
   ;; Bounding box and collision
   #:get-model-bounding-box #:check-collision-boxes #:draw-bounding-box
   
   ;; Material map constants
   #:+material-map-albedo+ #:+material-map-metalness+ #:+material-map-normal+ #:+material-map-roughness+
   #:+material-map-occlusion+ #:+material-map-emission+ #:+material-map-height+ #:+material-map-cubemap+
   #:+material-map-irradiance+ #:+material-map-prefilter+ #:+material-map-brdf+
   #:+material-map-diffuse+ #:+material-map-specular+ #:+max-material-maps+
   
   ;; Utility functions
   #:color-normalize #:color-multiply #:set-gl-color #:matrix4-rotate-axis
   
   ;; Rendering control
   #:set-wireframe-mode #:set-lighting-enabled #:unload-model
   #:begin-batch-rendering #:end-batch-rendering #:with-batch-rendering
   
   ;; Image/Texture structures
   #:image #:make-image #:image-data #:image-width #:image-height
   #:image-mipmaps #:image-format
   #:texture #:make-texture #:texture-id #:texture-width #:texture-height
   #:texture-mipmaps #:texture-format
   #:rectangle #:make-rectangle #:rectangle-x #:rectangle-y
   #:rectangle-width #:rectangle-height
   #:render-texture #:make-render-texture
   
   ;; Image loading functions
   #:load-image
   
   ;; Image generation functions
   #:gen-image-color #:gen-image-gradient-linear #:gen-image-gradient-radial
   #:gen-image-checked #:gen-image-white-noise #:gen-image-perlin-noise
   #:gen-image-cellular #:gen-image-gradient-square
   
   ;; Image manipulation functions
   #:image-copy #:image-from-image
   #:image-color-tint #:image-color-grayscale #:image-color-invert
   #:image-flip-vertical #:image-flip-horizontal
   #:unload-image #:get-pixel #:set-pixel
   
   ;; GPU Texture functions
   #:init-texture-system #:load-texture-from-image #:load-texture 
   #:is-texture-valid #:unload-texture #:update-texture
   #:set-texture-filter #:set-texture-wrap #:bind-texture #:setup-texture-drawing
   #:draw-texture #:draw-texture-v #:draw-texture-ex #:draw-texture-rec #:draw-texture-pro
   #:draw-texture-npatch #:get-texture-data #:get-texture-format #:cleanup-texture-system
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
   #:begin-scissor-mode #:end-scissor-mode
   #:get-fps #:draw-fps #:trace-log-warning
   #:with-window #:is-window-ready #:is-window-fullscreen #:is-window-hidden
   #:is-window-minimized #:is-window-maximized #:is-window-focused #:is-window-resized
   #:set-window-state #:clear-window-state #:toggle-fullscreen
   #:maximize-window #:minimize-window #:restore-window #:hide-window #:show-window
   #:set-window-title #:set-window-position #:get-window-position
   #:set-window-size #:set-window-min-size #:set-window-max-size
   #:update-fullscreen-cooldown
   #:set-window-opacity #:get-window-opacity
   #:disable-cursor #:enable-cursor #:hide-cursor #:show-cursor #:is-cursor-hidden
   
   ;; File drop functions
   #:file-path-list #:make-file-path-list #:file-path-list-count #:file-path-list-paths
   #:is-file-dropped #:load-dropped-files #:unload-dropped-files #:file-path-list-path
   #:get-screen-width #:get-screen-height #:get-render-width #:get-render-height
   #:get-monitor-count #:get-current-monitor #:get-monitor-info
   #:set-clipboard-text #:get-clipboard-text
   
   ;; Window flags
   #:+flag-window-resizable+ #:+flag-window-undecorated+ #:+flag-window-hidden+
   #:+flag-window-minimized+ #:+flag-window-maximized+ #:+flag-window-unfocused+
   #:+flag-window-topmost+ #:+flag-window-always-run+ #:+flag-window-transparent+ #:+flag-fullscreen-mode+
   #:+flag-window-highdpi+ #:+flag-window-mouse-passthrough+ #:+flag-window-borderless-windowed-mode+
   #:+flag-vsync-hint+ #:+flag-msaa-4x-hint+ #:+flag-interlaced-hint+
   
   ;; Input functions
   #:setup-input-callbacks #:update-input #:is-key-pressed #:is-key-down #:is-key-released #:is-key-up
   #:get-key-pressed #:get-char-pressed #:set-exit-key
   #:is-mouse-button-pressed #:is-mouse-button-down #:is-mouse-button-released #:is-mouse-button-up
   #:get-mouse-position #:get-mouse-x #:get-mouse-y #:set-mouse-position
   #:get-mouse-delta #:get-mouse-wheel-move #:get-mouse-wheel-move-v
   #:set-mouse-cursor #:keyword-to-key #:keyword-to-mouse-button
   
   ;; Gamepad functions
   #:is-gamepad-available #:get-gamepad-name
   #:is-gamepad-button-pressed #:is-gamepad-button-down #:is-gamepad-button-released #:is-gamepad-button-up
   #:get-gamepad-axis-count #:get-gamepad-axis-movement #:get-gamepad-button-count
   
   ;; Key constants
   #:+key-null+ #:+key-space+ #:+key-escape+ #:+key-enter+ #:+key-tab+ #:+key-backspace+
   #:+key-insert+ #:+key-delete+ #:+key-right+ #:+key-left+ #:+key-down+ #:+key-up+
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
   ;; Alternative mouse button names for compatibility
   #:+mouse-left-button+ #:+mouse-right-button+ #:+mouse-middle-button+
   
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
   
   ;; Vector3 math functions
   #:vector3-zero #:vector3-one #:vector3-add #:vector3-subtract #:vector3-scale
   #:vector3-cross-product #:vector3-length #:vector3-length-sqr #:vector3-dot-product
   #:vector3-distance #:vector3-distance-sqr #:vector3-angle #:vector3-negate
   #:vector3-normalize #:vector3-lerp #:vector3-min #:vector3-max #:vector3-clamp
   #:vector3-add-value #:vector3-subtract-value #:vector3-multiply #:vector3-divide
   #:vector3-perpendicular #:vector3-project #:vector3-reject #:vector3-ortho-normalize
   #:vector3-transform #:vector3-rotate-by-quaternion #:vector3-rotate-by-axis-angle
   #:vector3-reflect #:vector3-barycenter #:vector3-unproject #:vector3-invert
   #:vector3-clamp-value #:vector3-equals
   
   ;; Matrix math functions
   #:matrix-determinant #:matrix-trace #:matrix-transpose #:matrix-invert
   #:matrix-identity #:matrix-add #:matrix-subtract #:matrix-multiply
   #:matrix-translate #:matrix-rotate #:matrix-rotate-x #:matrix-rotate-y
   #:matrix-rotate-z #:matrix-rotate-xyz #:matrix-rotate-zyx #:matrix-scale
   #:matrix-frustum #:matrix-perspective #:matrix-ortho #:matrix-look-at
   #:matrix-to-float-v
   
   ;; Quaternion math functions
   #:quaternion-identity #:quaternion-length #:quaternion-normalize #:quaternion-invert
   #:quaternion-multiply #:quaternion-divide #:quaternion-lerp #:quaternion-nlerp
   #:quaternion-slerp #:quaternion-from-matrix #:quaternion-to-matrix
   #:quaternion-from-axis-angle #:quaternion-to-axis-angle #:quaternion-equals
   #:quaternion-scale #:quaternion-from-vector3-to-vector3 #:quaternion-from-euler
   #:quaternion-to-euler #:quaternion-transform
   
   ;; Re-export 3d-math symbols
   #:vec #:vx #:vy #:vz #:v+ #:v- #:v* #:vunit #:vc #:vscale
   #:vx2 #:vy2 #:vz2 #:vw2 #:vx3 #:vy3 #:vz3 #:vw3 #:vx4 #:vy4 #:vz4 #:vw4
   
   ;; Timing system functions
   #:get-time #:get-frame-time #:get-fps #:get-fps-raw #:set-target-fps #:get-target-fps
   #:update-frame-timing #:wait-time #:wait-frame #:get-time-precise #:get-frame-count
   #:performance-timer #:create-timer #:start-timer #:stop-timer #:get-timer-elapsed
   #:with-timer #:time-execution #:sync-to-fps #:begin-frame #:end-frame
   #:get-timing-info #:reset-timing #:begin-frame-timing #:end-frame-timing #:init-timer
   
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
   #:directory-exists #:get-file-length #:get-file-extension #:get-file-name
   #:get-file-name-without-ext #:get-directory-path #:get-working-directory #:change-directory
   #:load-file-data #:save-file-data #:unload-file-data #:load-file-text #:save-file-text
   #:get-directory-files #:copy-file #:move-file #:delete-file-safe #:file-data
   #:get-prev-directory-path #:get-application-directory #:export-data-as-code
   #:load-directory-files #:load-directory-files-ex #:unload-directory-files
   #:is-path-file #:set-load-file-data-callback #:set-save-file-data-callback
   #:set-load-file-text-callback #:set-save-file-text-callback
   #:compress-data #:decompress-data #:encode-data-base64 #:decode-data-base64
   
   ;; Random system functions
   #:set-random-seed #:get-random-value #:get-random-float #:get-random-float-01
   #:get-random-vector2 #:get-random-vector3 #:get-random-color #:get-random-boolean
   #:get-random-choice #:shuffle-list #:get-random-angle #:random-walker #:create-random-walker
   
   ;; Font and glyph structures
   #:font #:make-font #:font-p #:font-base-size #:font-glyph-count #:font-glyph-padding
   #:font-texture #:font-recs #:font-glyphs
   #:glyph-info #:make-glyph-info #:glyph-info-p #:glyph-info-value #:glyph-info-offset-x
   #:glyph-info-offset-y #:glyph-info-advance-x #:glyph-info-image
   
   ;; Font management functions
   #:get-font-default #:is-font-valid #:load-font-default #:unload-font-default
   #:init-font-system #:init-text-system #:cleanup-text-system #:init-font-bitmap #:load-font #:load-font-ex #:load-font-from-image
   #:load-font-from-memory #:unload-font #:is-font-ready
   
   ;; Glyph functions
   #:get-glyph-index #:get-glyph-info #:get-glyph-atlas-rec
   
   ;; Text drawing functions
   #:draw-text #:draw-text-ex #:draw-text-pro #:draw-text-codepoint #:draw-text-codepoints
   #:draw-fps
   
   ;; Text measurement functions
   #:measure-text #:measure-text-ex
   
   ;; Font atlas functions
   #:gen-image-font-atlas #:load-font-data #:unload-font-data #:export-font-as-code
   
   ;; Font atlas system
   #:font-atlas #:atlas-node #:create-font-atlas #:atlas-pack-glyph
   #:gen-image-font-atlas #:get-font-atlas-info #:debug-draw-font-atlas
   
   ;; Advanced text rendering
   #:draw-text-with-shadow #:draw-text-outlined #:draw-text-gradient
   #:draw-text-wrapped #:draw-text-box #:draw-text-centered #:draw-text-right-aligned
   #:color-lerp
   
   ;; Text input system
   #:text-input #:create-text-input #:text-input-insert #:text-input-delete-char
   #:text-input-move-cursor #:text-input-text #:text-input-cursor-pos
   #:text-input-active #:text-input-max-length
   
   ;; Text manipulation functions (rtext.c)
   #:text-length #:text-subtext #:text-to-upper #:text-to-lower #:text-replace
   #:text-to-integer #:text-copy #:text-insert #:text-join #:text-split #:text-append
   #:text-find-index #:text-to-pascal #:text-to-snake #:text-to-camel #:text-format
   
   ;; Unicode and codepoint functions
   #:get-codepoint #:get-codepoint-next #:get-codepoint-previous #:get-codepoint-count 
   #:codepoint-to-utf8 #:load-codepoints
   
   ;; Text line spacing
   #:set-text-line-spacing #:get-text-line-spacing
   
   ;; Font loading system
   #:font-loader-config #:create-font-loader-config #:get-default-font-chars
   #:detect-font-format #:load-font-cached #:load-bitmap-font #:load-image-font
   #:load-truetype-font #:bitmap-font-info #:bitmap-glyph-data #:parse-fnt-file
   
   ;; Font cache management
   #:font-cache-entry #:get-font-cache-info #:clear-font-cache #:cache-font
   #:get-cached-font #:cleanup-font-cache
   
   ;; Font management utilities
   #:get-loaded-fonts #:get-font-info #:unload-font-by-name #:reload-font
   #:validate-font #:test-font-rendering #:get-supported-font-formats
   #:is-font-format-supported #:get-font-format-info
   
   ;; TTF/OTF parsing system
   #:ttf-header #:ttf-table-entry #:ttf-font-data #:detect-font-format
   #:parse-truetype-data #:parse-ttf-header #:parse-ttf-table-directory
   #:parse-essential-tables #:generate-basic-glyphs #:create-font-atlas-from-truetype
   #:create-simple-glyph-bitmap #:create-font-from-atlas #:read-uint32-be #:read-uint16-be
   #:read-int16-be #:read-tag
   
   ;; Text performance optimization
   #:text-perf-stats #:text-render-batch #:text-cache-entry #:gpu-font-atlas
   #:reset-text-perf-stats #:get-text-perf-stats #:begin-text-batch #:end-text-batch
   #:draw-text-optimized #:analyze-text-performance #:enable-text-optimizations
   #:disable-text-optimizations #:get-text-optimization-info #:cleanup-text-optimizations
   #:get-cached-text #:clear-text-cache #:create-gpu-font-atlas #:get-gpu-font-atlas
   
   ;; Advanced image processing
   #:advanced-image #:make-advanced-image #:detect-image-format #:verify-image-format
   #:load-image-advanced #:save-image-advanced #:resize-image-advanced #:apply-image-filter
   #:convert-from-opticl-image #:convert-to-opticl-image #:get-magic-bytes
   #:extract-exif-data #:get-image-metadata #:set-image-metadata #:copy-advanced-image
   #:get-supported-image-formats #:is-image-format-supported #:init-image-processing-system
   #:cleanup-image-processing-system
   
   ;; 2D Collision shapes
   #:circle #:aabb #:obb #:polygon #:line-segment
   #:make-circle #:make-aabb #:make-obb #:make-polygon #:make-line-segment
   #:make-circle-at #:make-aabb-from-points #:make-aabb-from-center-size #:make-obb-at
   #:circle-center #:circle-radius #:aabb-min #:aabb-max #:obb-center #:obb-half-extents #:obb-rotation
   
   ;; 3D Collision shapes
   #:sphere #:aabb3d #:obb3d #:plane3d #:capsule
   #:make-sphere #:make-aabb3d #:make-obb3d #:make-plane3d #:make-capsule
   #:make-sphere-at #:make-aabb3d-from-points #:make-aabb3d-from-center-size
   #:sphere-center #:sphere-radius #:aabb3d-min #:aabb3d-max
   
   ;; Shape properties
   #:get-aabb-width #:get-aabb-height #:get-aabb-center #:get-aabb3d-size #:get-aabb3d-center
   
   ;; 2D Collision detection
   #:check-collision-point-circle #:check-collision-point-rectangle #:check-collision-circles
   #:check-collision-rectangle-rectangle #:check-collision-circle-rectangle
   #:check-collision-line-circle #:check-collision-line-rectangle
   
   ;; 3D Collision detection
   #:check-collision-point-sphere #:check-collision-point-aabb3d #:check-collision-spheres
   #:check-collision-aabb3d-aabb3d #:check-collision-sphere-aabb3d
   #:check-collision-ray-sphere #:check-collision-ray-aabb3d
   
   ;; Collision information
   #:collision-info #:collision-info-3d #:make-collision-info #:make-collision-info-3d
   #:get-collision-info-circles #:get-collision-info-rectangles
   #:collision-info-colliding #:collision-info-normal #:collision-info-penetration #:collision-info-contact-point
   
   ;; Collision utilities
   #:get-aabb-from-circle #:get-aabb3d-from-sphere #:expand-aabb #:expand-aabb3d
   
   ;; Collision system compatibility (no-ops for raylib stateless design)
   #:init-collision-system #:cleanup-collision-system
   
   ;; Audio data structures
   #:wave #:sound #:music #:audio-stream
   #:make-wave #:make-sound #:make-music #:make-audio-stream
   #:wave-frame-count #:wave-sample-rate #:wave-sample-size #:wave-channels #:wave-data
   #:sound-playing #:sound-looping #:sound-volume #:sound-pitch #:sound-pan #:sound-source
   #:music-playing #:music-looping #:music-volume #:music-pitch #:music-time-played #:music-time-length
   
   ;; Audio device management
   #:init-audio-device #:close-audio-device #:is-audio-device-ready
   #:set-master-volume #:get-master-volume
   
   ;; Wave/Sound loading and management
   #:load-wave #:load-wave-from-memory #:load-wave-from-wav #:load-wave-from-ogg #:load-wave-from-mp3 #:load-wave-from-flac
   #:create-dummy-wave #:is-wave-valid #:is-wave-ready #:unload-wave
   #:load-sound #:load-sound-from-wave #:is-sound-valid #:is-sound-ready #:update-sound #:unload-sound
   #:export-wave #:export-wave-as-code
   
   ;; Sound playing management
   #:play-sound #:stop-sound #:pause-sound #:resume-sound #:is-sound-playing
   #:set-sound-volume #:set-sound-pitch #:set-sound-pan #:stop-all-sounds
   
   ;; Music management
   #:load-music-stream #:is-music-valid #:is-music-ready #:unload-music-stream
   #:play-music-stream #:is-music-stream-playing #:update-music-stream
   #:stop-music-stream #:pause-music-stream #:resume-music-stream
   #:seek-music-stream #:set-music-volume #:set-music-pitch #:set-music-pan #:set-music-looping
   #:get-music-time-length #:get-music-time-played
   
   ;; Audio stream management
   #:load-audio-stream #:is-audio-stream-ready #:unload-audio-stream
   #:update-audio-stream #:is-audio-stream-processed
   #:play-audio-stream #:pause-audio-stream #:resume-audio-stream
   #:is-audio-stream-playing #:stop-audio-stream
   #:set-audio-stream-volume #:set-audio-stream-pitch #:set-audio-stream-pan
   
   ;; Audio system utilities
   #:set-audio-buffer-size #:get-audio-buffer-size #:update-audio-system
   #:get-audio-system-info #:cleanup-audio-system
   
   ;; 3D Audio structures
   #:audio-listener #:sound-3d #:reverb-zone
   #:make-audio-listener #:make-sound-3d #:make-reverb-zone
   #:audio-listener-position #:audio-listener-velocity #:audio-listener-forward
   #:audio-listener-up #:audio-listener-right
   #:sound-3d-sound #:sound-3d-position #:sound-3d-velocity #:sound-3d-min-distance
   #:sound-3d-max-distance #:sound-3d-rolloff-factor
   
   ;; Audio listener management
   #:set-audio-listener #:get-audio-listener #:set-listener-position
   #:set-listener-velocity #:set-listener-orientation #:update-listener-from-camera
   
   ;; 3D Sound management
   #:create-sound-3d #:remove-sound-3d #:set-sound-3d-position #:set-sound-3d-velocity
   #:set-sound-3d-distance-model #:set-sound-3d-cone #:play-sound-3d #:stop-sound-3d
   #:is-sound-3d-playing #:sound-3d-calculated-volume #:sound-3d-calculated-pan
   
   ;; 3D Audio calculations
   #:calculate-3d-audio-parameters #:calculate-distance-attenuation
   #:calculate-directional-gain #:calculate-doppler-effect
   
   ;; Reverb and environmental audio
   #:create-reverb-zone #:remove-reverb-zone #:calculate-reverb-effect
   
   ;; 3D Audio system control
   #:set-3d-audio-distance-model #:set-doppler-factor #:set-speed-of-sound
   #:get-3d-audio-info #:init-3d-audio-system #:cleanup-3d-audio-system
   #:update-3d-audio-system
   
   ;; 3D Audio global variables
   #:*distance-model* #:*doppler-factor* #:*speed-of-sound*
   
   ;; Audio codec system
   #:audio-codec-info #:make-audio-codec-info #:register-audio-codec
   #:get-codec-by-extension #:get-codec-by-name #:is-codec-enabled
   #:decode-audio-file #:encode-audio-file #:decoded-audio-data #:make-decoded-audio-data
   #:init-audio-codec-system #:cleanup-audio-codec-system
   #:get-supported-audio-formats #:get-audio-codec-info #:cache-audio-data
   #:clear-audio-cache #:get-cache-info #:detect-audio-format
   
   ;; Compression system
   #:compression-format-info #:make-compression-format-info #:register-compression-format
   #:get-compression-format-by-name #:get-compression-format-by-extension
   #:is-compression-format-enabled #:compress-data #:decompress-data
   #:compress-file #:decompress-file #:detect-compression-format
   #:init-compression-system #:cleanup-compression-system
   #:get-supported-compression-formats #:get-compression-system-info
   #:clear-compression-cache #:get-compression-cache-info
   
   ;; GLTF loader system
   #:gltf-asset #:gltf-buffer #:gltf-buffer-view #:gltf-accessor #:gltf-material
   #:gltf-texture #:gltf-image #:gltf-primitive #:gltf-mesh #:gltf-node
   #:gltf-scene #:gltf-data #:load-gltf-file #:load-model-gltf #:get-gltf-info
   #:gltf-to-model #:parse-gltf-json #:validate-gltf-data
   
   ;; Shader system
   #:shader #:make-shader #:shader-id #:shader-locs
   #:load-shader #:load-shader-from-memory #:unload-shader
   #:get-shader-location #:get-shader-location-attrib
   #:set-shader-value #:set-shader-value-v #:set-shader-value-matrix #:set-shader-value-texture
   #:begin-shader-mode #:end-shader-mode #:with-shader-mode
   #:is-shader-valid #:get-current-shader #:text-format
   #:init-shader-system #:cleanup-shader-system
   
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
   #:+shader-loc-vertex-boneweights+ #:+shader-loc-bone-matrices+ #:+shader-loc-vertex-instance-tx+
   
   ;; Shader uniform type constants
   #:+shader-uniform-float+ #:+shader-uniform-vec2+ #:+shader-uniform-vec3+ #:+shader-uniform-vec4+
   #:+shader-uniform-int+ #:+shader-uniform-ivec2+ #:+shader-uniform-ivec3+ #:+shader-uniform-ivec4+
   #:+shader-uniform-uint+ #:+shader-uniform-uivec2+ #:+shader-uniform-uivec3+ #:+shader-uniform-uivec4+
   #:+shader-uniform-sampler2d+
   
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

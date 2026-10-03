(in-package #:cl-raylib)

;;; Shader System
;;; This module provides comprehensive shader functionality for cl-raylib
;;; Based on raylib's shader system (rcore.c and raylib.h)

;;; Shader location index constants (from raylib.h lines 775-806)
(defconstant +shader-loc-vertex-position+ 0 "Shader location: vertex attribute: position")
(defconstant +shader-loc-vertex-texcoord01+ 1 "Shader location: vertex attribute: texcoord01")  
(defconstant +shader-loc-vertex-texcoord02+ 2 "Shader location: vertex attribute: texcoord02")
(defconstant +shader-loc-vertex-normal+ 3 "Shader location: vertex attribute: normal")
(defconstant +shader-loc-vertex-tangent+ 4 "Shader location: vertex attribute: tangent")
(defconstant +shader-loc-vertex-color+ 5 "Shader location: vertex attribute: color")
(defconstant +shader-loc-matrix-mvp+ 6 "Shader location: matrix uniform: model-view-projection")
(defconstant +shader-loc-matrix-view+ 7 "Shader location: matrix uniform: view")
(defconstant +shader-loc-matrix-projection+ 8 "Shader location: matrix uniform: projection")
(defconstant +shader-loc-matrix-model+ 9 "Shader location: matrix uniform: model")
(defconstant +shader-loc-matrix-normal+ 10 "Shader location: matrix uniform: normal")
(defconstant +shader-loc-vector-view+ 11 "Shader location: vector uniform: view")
(defconstant +shader-loc-color-diffuse+ 12 "Shader location: vector uniform: diffuse color")
(defconstant +shader-loc-color-specular+ 13 "Shader location: vector uniform: specular color")
(defconstant +shader-loc-color-ambient+ 14 "Shader location: vector uniform: ambient color")
(defconstant +shader-loc-map-albedo+ 15 "Shader location: sampler2d texture: albedo")
(defconstant +shader-loc-map-metalness+ 16 "Shader location: sampler2d texture: metalness")
(defconstant +shader-loc-map-normal+ 17 "Shader location: sampler2d texture: normal")
(defconstant +shader-loc-map-roughness+ 18 "Shader location: sampler2d texture: roughness")
(defconstant +shader-loc-map-occlusion+ 19 "Shader location: sampler2d texture: occlusion")
(defconstant +shader-loc-map-emission+ 20 "Shader location: sampler2d texture: emission")
(defconstant +shader-loc-map-height+ 21 "Shader location: sampler2d texture: height")
(defconstant +shader-loc-map-cubemap+ 22 "Shader location: samplerCube texture: cubemap")
(defconstant +shader-loc-map-irradiance+ 23 "Shader location: samplerCube texture: irradiance")
(defconstant +shader-loc-map-prefilter+ 24 "Shader location: samplerCube texture: prefilter")
(defconstant +shader-loc-map-brdf+ 25 "Shader location: sampler2d texture: brdf")
(defconstant +shader-loc-vertex-boneids+ 26 "Shader location: vertex attribute: boneIds")
(defconstant +shader-loc-vertex-boneweights+ 27 "Shader location: vertex attribute: boneWeights")
(defconstant +shader-loc-bone-matrices+ 28 "Shader location: array of matrices uniform: boneMatrices")
(defconstant +shader-loc-vertex-instance-tx+ 29 "Shader location: vertex attribute: instanceTransform")

;;; Shader uniform data type constants (from raylib.h lines 812-826)
(defconstant +shader-uniform-float+ 0 "Shader uniform type: float")
(defconstant +shader-uniform-vec2+ 1 "Shader uniform type: vec2 (2 float)")
(defconstant +shader-uniform-vec3+ 2 "Shader uniform type: vec3 (3 float)")
(defconstant +shader-uniform-vec4+ 3 "Shader uniform type: vec4 (4 float)")
(defconstant +shader-uniform-int+ 4 "Shader uniform type: int")
(defconstant +shader-uniform-ivec2+ 5 "Shader uniform type: ivec2 (2 int)")
(defconstant +shader-uniform-ivec3+ 6 "Shader uniform type: ivec3 (3 int)")
(defconstant +shader-uniform-ivec4+ 7 "Shader uniform type: ivec4 (4 int)")
(defconstant +shader-uniform-uint+ 8 "Shader uniform type: unsigned int")
(defconstant +shader-uniform-uivec2+ 9 "Shader uniform type: uivec2 (2 unsigned int)")
(defconstant +shader-uniform-uivec3+ 10 "Shader uniform type: uivec3 (3 unsigned int)")
(defconstant +shader-uniform-uivec4+ 11 "Shader uniform type: uivec4 (4 unsigned int)")
(defconstant +shader-uniform-sampler2d+ 12 "Shader uniform type: sampler2d")

;;; Shader constants and structure are defined in raylib.lisp

;;; Global shader state
(defvar *current-shader* nil "Currently active shader")
(defvar *default-shader* nil "Default shader for basic rendering")

;;; Shader system initialization
(defun init-shader-system ()
  "Initialize shader system and create default shader"
  (setf *default-shader* (make-shader :id 0))  ; Default shader has id 0
  (setf *current-shader* *default-shader*)
  (format t "INFO: SHADER: Default shader loaded successfully~%"))

;;; File reading utilities for shaders
(defun load-file-text-shader (filename)
  "Load text from file for shader code"
  (handler-case
      (when (and filename (probe-file filename))
        (with-open-file (stream filename :direction :input)
          (let ((content (make-string (file-length stream))))
            (read-sequence content stream)
            content)))
    (error (e)
      (warn "SHADER: Failed to read shader file ~a: ~a" filename e)
      nil)))

;;; GLSL version detection
(defun get-glsl-version ()
  "Get appropriate GLSL version based on OpenGL version"
  (let ((gl-version (gl:get-string :version)))
    (cond
      ((search "OpenGL ES 2" gl-version) 100)  ; OpenGL ES 2.0
      ((search "OpenGL ES 3" gl-version) 300)  ; OpenGL ES 3.0
      (t 330))))                                ; Desktop OpenGL 3.3+

;;; Default shader source code
(defun get-default-vertex-shader-code ()
  "Get default vertex shader source code"
  (let ((version (get-glsl-version)))
    (if (< version 300)
      ;; GLSL 100 (OpenGL ES 2.0)
      "#version 100
precision mediump float;
attribute vec3 vertexPosition;
attribute vec2 vertexTexCoord;
attribute vec4 vertexColor;
varying vec2 fragTexCoord;
varying vec4 fragColor;
uniform mat4 mvp;
void main()
{
    fragTexCoord = vertexTexCoord;
    fragColor = vertexColor;
    gl_Position = mvp*vec4(vertexPosition, 1.0);
}"
      ;; GLSL 330+ (Desktop OpenGL)
      "#version 330
in vec3 vertexPosition;
in vec2 vertexTexCoord;
in vec4 vertexColor;
out vec2 fragTexCoord;
out vec4 fragColor;
uniform mat4 mvp;
void main()
{
    fragTexCoord = vertexTexCoord;
    fragColor = vertexColor;
    gl_Position = mvp*vec4(vertexPosition, 1.0);
}")))

(defun get-default-fragment-shader-code ()
  "Get default fragment shader source code"
  (let ((version (get-glsl-version)))
    (if (< version 300)
      ;; GLSL 100 (OpenGL ES 2.0)
      "#version 100
precision mediump float;
varying vec2 fragTexCoord;
varying vec4 fragColor;
uniform sampler2D texture0;
uniform vec4 colDiffuse;
void main()
{
    vec4 texelColor = texture2D(texture0, fragTexCoord);
    gl_FragColor = texelColor*colDiffuse*fragColor;
}"
      ;; GLSL 330+ (Desktop OpenGL)
      "#version 330
in vec2 fragTexCoord;
in vec4 fragColor;
out vec4 finalColor;
uniform sampler2D texture0;
uniform vec4 colDiffuse;
void main()
{
    vec4 texelColor = texture(texture0, fragTexCoord);
    finalColor = texelColor*colDiffuse*fragColor;
}")))

;;; Shader compilation utilities
(defun compile-shader (shader-code shader-type)
  "Compile shader code and return shader id"
  (handler-case
      (when shader-code
        (let ((shader-id (gl:create-shader shader-type)))
          (gl:shader-source shader-id shader-code)
          (gl:compile-shader shader-id)
          
          ;; Check compilation status
          (let ((compile-status (gl:get-shader shader-id :compile-status)))
            (if compile-status
                (progn
                  (format t "INFO: SHADER: [ID ~d] ~a shader compiled successfully~%" 
                          shader-id (if (eq shader-type :vertex-shader) "Vertex" "Fragment"))
                  shader-id)
                (let ((info-log (gl:get-shader-info-log shader-id)))
                  (warn "SHADER: [ID ~d] ~a shader compilation failed: ~a" 
                        shader-id (if (eq shader-type :vertex-shader) "Vertex" "Fragment") info-log)
                  (gl:delete-shader shader-id)
                  nil)))))
    (error (e)
      (warn "SHADER: Failed to compile ~a shader: ~a" 
            (if (eq shader-type :vertex-shader) "vertex" "fragment") e)
      nil)))

(defun link-shader-program (vertex-shader-id fragment-shader-id)
  "Link vertex and fragment shaders into a program"
  (handler-case
      (when (and vertex-shader-id fragment-shader-id)
        (let ((program-id (gl:create-program)))
          (gl:attach-shader program-id vertex-shader-id)
          (gl:attach-shader program-id fragment-shader-id)
          (gl:link-program program-id)
          
          ;; Check linking status
          (let ((link-status (gl:get-program program-id :link-status)))
            (if link-status
                (progn
                  ;; Clean up individual shaders
                  (gl:detach-shader program-id vertex-shader-id)
                  (gl:detach-shader program-id fragment-shader-id)
                  (gl:delete-shader vertex-shader-id)
                  (gl:delete-shader fragment-shader-id)
                  
                  (format t "INFO: SHADER: [ID ~d] Program linked successfully~%" program-id)
                  program-id)
                (let ((info-log (gl:get-program-info-log program-id)))
                  (warn "SHADER: [ID ~d] Program linking failed: ~a" program-id info-log)
                  (gl:delete-program program-id)
                  ;; Clean up shaders even on failure
                  (when vertex-shader-id (gl:delete-shader vertex-shader-id))
                  (when fragment-shader-id (gl:delete-shader fragment-shader-id))
                  nil)))))
    (error (e)
      (warn "SHADER: Failed to link shader program: ~a" e)
      nil)))

;;; Shader loading functions
(defun load-shader (vs-filename fs-filename)
  "Load shader from vertex and fragment shader files (raylib LoadShader)"
  (handler-case
      (let* ((vs-code (if vs-filename
                        (or (load-file-text-shader vs-filename)
                            (progn 
                              (warn "SHADER: Failed to load vertex shader file: ~a, using default" vs-filename)
                              (get-default-vertex-shader-code)))
                        (get-default-vertex-shader-code)))
             (fs-code (if fs-filename
                        (or (load-file-text-shader fs-filename)
                            (progn
                              (warn "SHADER: Failed to load fragment shader file: ~a, using default" fs-filename)
                              (get-default-fragment-shader-code)))
                        (get-default-fragment-shader-code))))
        (when (and vs-code fs-code)
          (load-shader-from-memory vs-code fs-code)))
    (error (e)
      (warn "SHADER: Failed to load shader files ~a, ~a: ~a" vs-filename fs-filename e)
      ;; Return default shader on error
      (or *default-shader* (make-shader :id 0)))))

(defun load-shader-from-memory (vs-code fs-code)
  "Load shader from memory strings (raylib LoadShaderFromMemory)"
  (handler-case
      (when (and vs-code fs-code)
        (let* ((vertex-shader-id (compile-shader vs-code :vertex-shader))
               (fragment-shader-id (compile-shader fs-code :fragment-shader))
               (program-id (link-shader-program vertex-shader-id fragment-shader-id)))
          
          ;; Check if compilation and linking was successful
          (if (and vertex-shader-id fragment-shader-id program-id (> program-id 0))
              (let ((shader (make-shader :id program-id)))
                ;; Get default uniform locations
                (setf (aref (shader-locs shader) +shader-loc-matrix-mvp+)
                      (gl:get-uniform-location program-id "mvp"))
                (setf (aref (shader-locs shader) +shader-loc-color-diffuse+)
                      (gl:get-uniform-location program-id "colDiffuse"))
                (setf (aref (shader-locs shader) +shader-loc-map-albedo+)
                      (gl:get-uniform-location program-id "texture0"))
                
                (format t "INFO: SHADER: [ID ~d] Shader loaded successfully~%" program-id)
                shader)
              (progn
                (warn "SHADER: Failed to compile or link shader from memory")
                (or *default-shader* (make-shader :id 0))))))
    (error (e)
      (warn "SHADER: Failed to compile shader from memory: ~a" e)
      ;; Return default shader on error
      (or *default-shader* (make-shader :id 0)))))

(defun unload-shader (shader)
  "Unload shader from GPU memory (raylib UnloadShader)"
  (when (and shader (> (shader-id shader) 0))
    (gl:delete-program (shader-id shader))
    (format t "INFO: SHADER: [ID ~d] Shader unloaded successfully~%" (shader-id shader))
    (setf (shader-id shader) 0)))

;;; Shader location and uniform functions
(defun get-shader-location (shader uniform-name)
  "Get shader uniform location (raylib GetShaderLocation)"
  (if (and shader (> (shader-id shader) 0))
    (gl:get-uniform-location (shader-id shader) uniform-name)
    -1))

(defun get-shader-location-attrib (shader attrib-name)
  "Get shader attribute location (raylib GetShaderLocationAttrib)"
  (if (and shader (> (shader-id shader) 0))
    (gl:get-attrib-location (shader-id shader) attrib-name)
    -1))

(defun set-shader-value (shader loc-index value uniform-type)
  "Set shader uniform value (raylib SetShaderValue)"
  (when (and shader (>= loc-index 0))
    (let ((location (if (< loc-index +max-shader-locations+)
                      (aref (shader-locs shader) loc-index)
                      loc-index)))
      (when (>= location 0)
        (gl:use-program (shader-id shader))
        (alexandria:switch (uniform-type)
          (+shader-uniform-float+
           (gl:uniformf location (if (numberp value) value (first value))))
          (+shader-uniform-vec2+
           (gl:uniformf location (first value) (second value)))
          (+shader-uniform-vec3+
           (gl:uniformf location (first value) (second value) (third value)))
          (+shader-uniform-vec4+
           (gl:uniformf location (first value) (second value) (third value) (fourth value)))
          (+shader-uniform-int+
           (gl:uniformi location (if (numberp value) value (first value))))
          (+shader-uniform-ivec2+
           (gl:uniformi location (first value) (second value)))
          (+shader-uniform-ivec3+
           (gl:uniformi location (first value) (second value) (third value)))
          (+shader-uniform-ivec4+
           (gl:uniformi location (first value) (second value) (third value) (fourth value)))
          (t (warn "SHADER: Unsupported uniform type: ~d" uniform-type)))))))

(defun set-shader-value-v (shader loc-index values uniform-type count)
  "Set shader uniform array value (raylib SetShaderValueV)"
  (when (and shader (>= loc-index 0) (> count 0))
    (let ((location (if (< loc-index +max-shader-locations+)
                      (aref (shader-locs shader) loc-index)
                      loc-index)))
      (when (>= location 0)
        (gl:use-program (shader-id shader))
        ;; Implementation would depend on specific uniform array type
        ;; This is a simplified version
        (alexandria:switch (uniform-type)
          (+shader-uniform-float+
           (dotimes (i count)
             (gl:uniformf (+ location i) (nth i values))))
          (+shader-uniform-int+
           (dotimes (i count)
             (gl:uniformi (+ location i) (nth i values))))
          (t (warn "SHADER: Unsupported uniform array type: ~d" uniform-type)))))))

(defun set-shader-value-matrix (shader loc-index mat)
  "Set shader uniform matrix value (raylib SetShaderValueMatrix)"
  (when (and shader (>= loc-index 0))
    (let ((location (if (< loc-index +max-shader-locations+)
                      (aref (shader-locs shader) loc-index)
                      loc-index)))
      (when (>= location 0)
        (gl:use-program (shader-id shader))
        ;; Convert matrix to array format for OpenGL
        (let ((matrix-array (make-array 16 :element-type 'single-float)))
          (dotimes (i 16)
            (setf (aref matrix-array i) (coerce (aref (marr4 mat) i) 'single-float)))
          (gl:uniform-matrix-4fv location matrix-array nil))))))

(defun set-shader-value-texture (shader loc-index texture)
  "Set shader uniform texture value (raylib SetShaderValueTexture)"
  (when (and shader (>= loc-index 0))
    (let ((location (if (< loc-index +max-shader-locations+)
                      (aref (shader-locs shader) loc-index)
                      loc-index)))
      (when (>= location 0)
        (gl:use-program (shader-id shader))
        (gl:uniformi location (texture-id texture))))))

;;; Shader mode management
(defun begin-shader-mode (shader)
  "Begin drawing with custom shader (raylib BeginShaderMode)"
  (when shader
    (setf *current-shader* shader)
    (gl:use-program (shader-id shader))))

(defun end-shader-mode ()
  "End drawing with custom shader, return to default (raylib EndShaderMode)"
  (setf *current-shader* *default-shader*)
  (gl:use-program (if *default-shader* (shader-id *default-shader*) 0)))

;;; Utility functions
(defun is-shader-valid (shader)
  "Check if shader is valid and ready"
  (and shader (> (shader-id shader) 0)))

(defun get-current-shader ()
  "Get currently active shader"
  *current-shader*)

;;; Shader system cleanup
(defun cleanup-shader-system ()
  "Cleanup shader system"
  (when *default-shader*
    (unload-shader *default-shader*)
    (setf *default-shader* nil))
  (setf *current-shader* nil)
  (format t "INFO: SHADER: Shader system cleaned up~%"))
